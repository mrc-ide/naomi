PEPFAR_DATAPACK_FILENAME <- "pepfar_datapack_indicators_2025.csv"

#' Export naomi outputs to PEPFAR Data Pack format
#'
#' @param naomi_output a naomi_output object.
#' @param path path to save Data Pack CSV.
#' @param psnu_level area_level for PEPFAR PSNU to export. If NULL,
#'    first looks in lookup table for the correct area_level, and
#'    if not defaults to the highest level of the area hierarchy.
#' @param dmppt2_output data frame containing the _Datapack inputs_
#'    sheet of DMPPT2 output file.
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
                               dmppt2_output = NULL) {

  if (!grepl("\\.csv$", path, ignore.case = TRUE)) {
    path <- paste0(path, ".csv")
  }

  datapack <- build_datapack_output(naomi_output, psnu_level, dmppt2_output)
  naomi_write_csv(datapack, path)

  path
}

build_datapack_output <- function(naomi_output, psnu_level, dmppt2_output) {
  stopifnot(inherits(naomi_output, "naomi_output"))

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

  ## If using the demo data, don't print this warning.
  ## The demo data only contain levels 0:2, but have ISO3 = "MWI", for which psnu_level = 3.
  ## A better way to handle this is to change the ISO3 for the demo data to an artificial
  ## code. However, some hint validation needs to be relaxed to enable this.
  is_demo_data <- "MWI_2_5_demo" %in% naomi_output$meta_area$area_id &&
    setequal(0:2, naomi_output$meta_area$area_level)
  if (is_demo_data) {
    psnu_level <- 2L
  }


  if (is.null(psnu_level) || !psnu_level %in% naomi_output$meta_area$area_level) {
    warning("PSNU level ", psnu_level, " not included in model outputs.")
  }

  datapack_indicator_map$calendar_quarter <- naomi_output$meta_period$calendar_quarter[datapack_indicator_map$time]

  datapack_indicator_map <- datapack_indicator_map %>%
    dplyr::rename(
      indicator_code = datapack_indicator_code,
      dataelement_uid = datapack_indicator_id,
    ) %>%
    dplyr::select(indicator, indicator_code, dataelement_uid, is_integer, calendar_quarter)


  datapack_age_group_map <- datapack_age_group_map %>%
    dplyr::transmute(
      age_group,
      age = paste0("=\"\"", datapack_age_group_label, "\"\""),
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

  indicators <- datapack_aggregate_1to9(naomi_output$indicators)

  ## Append VMMC indicators from DMPPT2 output to Naomi indicators
  if (!is.null(dmppt2_output)) {

    dmppt2_output <- dmppt2_output %>%
      dplyr::left_join(
        dplyr::select(datapack_indicator_map, indicator, calendar_quarter),
        by = "indicator"
      ) %>%
      dplyr::rename(mean = value) %>%
      dplyr::mutate(se = 0.0)

    indicators <- dplyr::bind_rows(indicators, dmppt2_output)
  }

  dat <- indicators %>%
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
    dplyr::semi_join(
      datapack_indicator_map,
      by = c("indicator", "calendar_quarter")
    ) %>%
    dplyr::filter(
      (sex_naomi %in% datapack_sex_map$sex_naomi &
         age_group %in% datapack_age_group_map$age_group |
         sex_naomi == "both" & age_group == "Y000_999" & !anc_indicator |
         sex_naomi == "female" & age_group == "Y015_049" & anc_indicator )
    ) %>%
    dplyr::transmute(
      area_id,
      indicator,
      indicator_sort_order,
      sex_naomi,
      age_group,
      calendar_quarter,
      value = mean,
      rse = dplyr::if_else(mean == 0, 0.0, se / mean)
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


  dat$district_rse[is.na(dat$district_rse) & dat$indicator %in% c("circ_new", "circ_ever")] <- 0.0


  ## Merge data pack Ids
  dat <- dplyr::left_join(dat, strat,
                          by = c("indicator", "calendar_quarter", "age_group", "sex_naomi"))

  ## Round integer indicators
  dat$value <- ifelse(dat$is_integer, round(dat$value), dat$value)

  ## Merge psnu_uid. If psnu_uid exists, use PSNU name, else use area_name
  psnu_map <- dplyr::select(datapack_psnu_map, area_id, map_name, psnu_uid = map_id)
  dat <- dplyr::left_join(dat, psnu_map, by = "area_id")
  dat$psnu <- ifelse(is.na(dat$map_name), dat$area_name, dat$map_name)

  dat %>%
    dplyr::select(
      psnu,
      psnu_uid,
      area_id,
      indicator_code,
      dataelement_uid,
      age,
      age_uid,
      sex = sex_datapack,
      sex_uid,
      calendar_quarter,
      value,
      age_sex_rse,
      district_rse
    )
}

build_datapack_metadata <- function(naomi_output, ids) {
  cqs <- c(naomi_output$fit$model_options$calendar_quarter_t1,
           naomi_output$fit$model_options$calendar_quarter_t2,
           naomi_output$fit$model_options$calendar_quarter_t3,
           naomi_output$fit$model_options$calendar_quarter_t4,
           naomi_output$fit$model_options$calendar_quarter_t5)
  meta_period <- data.frame(
    c("Time point", "t1", "t2", "t3", "t4", "t5"), c("Quarter", cqs)
  )

  info <- attr(naomi_output, "info")
  inputs <- read.csv(text = info$inputs.csv, header = FALSE)

  version <- data.frame("Naomi Version", utils::packageVersion("naomi"))

  if (!is.null(ids)) {
    all_data <- list(version, ids, inputs, meta_period)
  } else {
    all_data <- list(version, inputs, meta_period)
  }

  max_cols <- max(vapply(all_data, ncol, numeric(1)))
  col_names <- vapply(seq_len(max_cols), function(i) paste0("V", i), character(1))
  empty_row <- data.frame(matrix("", ncol = max_cols, nrow = 1))
  colnames(empty_row) <- col_names
  all_data <- lapply(all_data, function(df) {
    colnames(df) <- col_names[seq(1, ncol(df))]
    if (ncol(df) < max_cols) {
      df[, col_names[seq(ncol(df) + 1, max_cols)]] <- ""
    }
    df[] <- lapply(df, as.character)
    rbind.data.frame(df, empty_row)
  })

  do.call(rbind.data.frame, all_data)
}


read_datapack_psnu <- function() {
  readr::read_csv(system_file("datapack/datapack_psnu_area_level.csv"),
                  col_types = list(readr::col_character(),
                                   readr::col_integer()))
}

datapack_aggregate_1to9 <- function(indicators) {


  indicators_keep <- c("plhiv", "plhiv_attend", "untreated_plhiv_attend", "infections",
                       "population", "art_current", "art_current_residents", "aware_plhiv_num")

  indicators1to9 <- indicators %>%
    dplyr::filter(
      age_group %in% c("Y001_004", "Y005_009"),
      indicator %in% indicators_keep
    ) %>%
    dplyr::count(area_id, sex, age_group = "Y001_009", calendar_quarter, indicator,
                 wt = mean, name = "mean") %>%
    tidyr::pivot_wider(id_cols = c(area_id, sex, age_group, calendar_quarter),
                       names_from = indicator, values_from = mean)

  required_cols <- c("plhiv", "population", "art_current_residents", "infections")
  if ( any( !required_cols %in% names(indicators1to9) )) {
    missing_cols <- setdiff(required_cols, names(indicators1to9))
    warning("Required indicators not in output: ", paste(missing_cols, collapse = ", "))

    indicators1to9[missing_cols] <- NA_real_
  }


  indicators1to9 <- indicators1to9 %>%
    dplyr::mutate(
      prevalence = plhiv / population,
      art_coverage = art_current_residents / plhiv,
      incidence = infections / (population - plhiv)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::any_of(c(indicators_keep, "prevalence", "art_coverage", "incidence")),
      names_to = "indicator", values_to = "mean"
    )

  indicators1to9rse <- indicators %>%
    dplyr::filter(
      age_group %in% c("Y001_004", "Y005_009"),
      indicator %in% indicators1to9$indicator
    ) %>%
    dplyr::group_by(area_id, sex, age_group = "Y001_009", calendar_quarter, indicator) %>%
    dplyr::summarise(
      rse = mean(dplyr::if_else(mean == 0, 0.0, se / mean))
    )

  indicators1to9 <- indicators1to9 %>%
    dplyr::left_join(indicators1to9rse,
                     by = c("area_id", "sex", "age_group", "calendar_quarter", "indicator")) %>%
    dplyr::mutate(
      se = mean * rse,
      rse = NULL
    )

  indicators[names(indicators1to9)] %>%
    dplyr::bind_rows(indicators1to9)
}

transform_dmppt2 <- function(x) {

  if (any(names(x)[c(1, 4:11)] != c("area_id", "15-24", "25-34", "35-49", "50+",
                             "15-24", "25-34", "35-49", "50+"))) {
    stop("DMMPT2 output file does not have expected column names.")
  }

  names(x)[4:7] <- paste0("circ_new:", names(x)[4:7])
  names(x)[8:11] <- paste0("circ_ever:", names(x)[8:11])

  xl <- x %>%
    tidyr::pivot_longer(cols = 4:11, names_sep = ":", names_to = c("indicator", "age")) %>%
    dplyr::mutate(
      sex = "male",
      age_group = dplyr::recode(age,
                                "15-24" = "Y015_024",
                                "25-34" = "Y025_034",
                                "35-49" = "Y035_049",
                                "50+" = "Y050_999")
    ) %>%
    dplyr::select(area_id, sex, age_group, indicator, value)

  xl
}
