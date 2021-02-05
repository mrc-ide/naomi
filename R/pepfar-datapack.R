#' Export naomi outputs to PEPFAR Data Pack format
#'
#' @param naomi_output a naomi_output object.
#' @param path path to save Data Pack CSV.
#' @param psnu_level area_level for PEPFAR PSNU to export. If NULL,
#'    first looks in lookup table for the correct area_level, and
#'    if not defaults to the highest level of the area hierarchy.
#' @param calendar_quarter calendar_quarter to export estimates.
#'
#' @details
#'
#' The results will include Naomi outputs for area_id at the specified level,
#' irrespective of whether a Datim map_id is available.
#'
#' If a datim ID is available, the Datim map_name will be used in the column
#' `psnu`. Otherwise the Naomi `area_name` will be used for the column `psnu`.
#'
#' PEPFAR indicator codes for Data Pack are in this Datim view:
#' https://www.datim.org/api/sqlViews/DotdxKrNZxG/data.html+css?var=dataSets:j7jzezIhgPj
#'
#' PEPFAR PSNU UIDs are in this Datim view: https://www.datim.org/api/sqlViews/gsaaxFM8ZN0/data.html+css
#'
#' (Replace the extensions `.html+css` with `.csv` to download tables as CSV.)
#'
#' @export
write_datapack_csv <- function(naomi_output,
                               path,
                               psnu_level = NULL,
                               calendar_quarter = NULL) {

  stopifnot(inherits(naomi_output, "naomi_output"))
  stopifnot(calendar_quarter %in% naomi_output$meta_period$calendar_quarter)

  if (!grepl("\\.csv$", path, ignore.case = TRUE)) {
    path <- paste0(path, ".csv")
  }

  datapack_indicator_map <- naomi_read_csv(system_file("datapack", "datapack_indicator_mapping.csv"))
  datapack_age_group_map <- naomi_read_csv(system_file("datapack", "datapack_age_group_mapping.csv"))
  datapack_sex_map <- naomi_read_csv(system_file("datapack", "datapack_sex_mapping.csv"))
  datapack_psnu_map <- naomi_read_csv(system_file("datapack/datapack_psnu_area_id_map.csv"))
  datapack_psnu_level <- read_datapack_psnu()

  if (is.null(psnu_level)) {
    iso3 <- get_iso3(naomi_output$meta_area$area_id)
    if (iso3 %in% datapack_psnu_level$iso3) {
      psnu_level <- datapack_psnu_level$psnu_area_level[datapack_psnu_level$iso3 == iso3]
    } else {
      psnu_level <- max(naomi_output$meta_area$area_level)
    }
  }

  if (is.null(psnu_level) || !psnu_level %in% naomi_output$meta_area$area_level) {
    warning("PSNU level ", psnu_level, " not included in model outputs.")
  }

  if (is.null(calendar_quarter)) {
    calendar_quarter = max(naomi_output$meta_period$calendar_quarter)
  }

  tx_curr_calendar_quarter <- unique(naomi_output$meta_period$calendar_quarter)
  tx_curr_calendar_quarter <- sort(tx_curr_calendar_quarter, decreasing = TRUE)[2]


  datapack_indicator_map <- datapack_indicator_map %>%
    dplyr::rename(
             indicator_code = datapack_indicator_code,
             ) %>%
    dplyr::select(indicator, indicator_code, is_integer)


  datapack_age_group_map <- datapack_age_group_map %>%
    dplyr::transmute(
             age_group,
             age = paste0("\"", datapack_age_group_label, "\""),
             age_uid = datapack_age_group_id
           )

  datapack_sex_map <- datapack_sex_map %>%
    dplyr::rename(
             sex_naomi = sex,
             sex_datapack = datapack_sex_label,
             sex_uid = datapack_sex_id
           )

  strat <-  datapack_indicator_map %>%
    tidyr::expand_grid(datapack_age_group_map) %>%
    tidyr::expand_grid(datapack_sex_map)

  dat <- naomi_output$indicators %>%
    dplyr::rename(sex_naomi = sex) %>%
    dplyr::semi_join(
             naomi_output$meta_area %>%
             dplyr::filter(area_level == psnu_level),
             by = "area_id"
           ) %>%
    dplyr::left_join(
             dplyr::select(naomi_output$meta_indicator,
                           indicator, anc_indicator, indicator_sort_order),
             by = "indicator"
           ) %>%
    dplyr::filter(
             indicator %in% datapack_indicator_map$indicator,
             (calendar_quarter %in% {{ calendar_quarter }} |
              calendar_quarter == tx_curr_calendar_quarter & indicator == "art_current"),
             (sex_naomi %in% datapack_sex_map$sex_naomi &
              age_group %in% datapack_age_group_map$age_group |
              sex_naomi == "both" & age_group == "Y000_999" & !anc_indicator |
              sex_naomi == "female" & age_group == "Y015_049" & anc_indicator )
           )%>%
    dplyr::transmute(
             area_id,
             indicator,
             indicator_sort_order,
             sex_naomi,
             age_group,
             calendar_quarter,
             value = mean,
             rse = se / mean
           )

  dat <- dat %>%
    dplyr::filter(!age_group %in% c("Y000_999", "Y015_049")) %>%
    dplyr::rename(age_sex_rse = rse) %>%
    dplyr::left_join(
             dplyr::filter(dat, age_group %in% c("Y000_999", "Y015_049")) %>%
             dplyr::select(-indicator_sort_order, -age_group, -sex_naomi, -value) %>%
             dplyr::rename(district_rse = rse),
             by = c("area_id", "indicator", "calendar_quarter")
           ) %>%
    dplyr::left_join(
             sf::st_drop_geometry(naomi_output$meta_area) %>%
             dplyr::select(area_name, area_id),
             by = "area_id"
           ) %>%
    dplyr::arrange(calendar_quarter, indicator_sort_order, area_id, sex_naomi, age_group)

  ## Merge data pack Ids
  dat <- dplyr::left_join(dat, strat, by = c("indicator", "age_group", "sex_naomi"))

  ## Manually recode current quarter TX_CURR indicator to TX_CURR_SUBNAT.R
  dat <- dat %>%
    dplyr::mutate(
             indicator_code = dplyr::if_else(calendar_quarter == tx_curr_calendar_quarter &
                                             indicator == "art_current",
                                             "TX_CURR_SUBNAT.R",
                                             indicator_code)
           )


  ## Round integer indicators
  dat$value <- ifelse(dat$is_integer, round(dat$value), dat$value)

  ## Merge psnu_uid. If psnu_uid exists, use PSNU name, else use area_name
  psnu_map <- dplyr::select(datapack_psnu_map, area_id, map_name, psnu_uid = map_id)
  dat <- dplyr::left_join(dat, psnu_map, by = "area_id")
  dat$psnu <- ifelse(is.na(dat$map_name), dat$area_name, dat$map_name)

  datapack <- dat %>%
    dplyr::select(
             psnu,
             psnu_uid,
             area_id,
             indicator_code,
             age,
             age_uid,
             sex = sex_datapack,
             sex_uid,
             calendar_quarter,
             value,
             age_sex_rse,
             district_rse
           )

  naomi_write_csv(datapack, path)

  path
}


read_datapack_psnu <- function() {
  readr::read_csv(system_file("datapack/datapack_psnu_area_level.csv"),
                  col_types = list(readr::col_character(),
                                   readr::col_integer()))
}
