##' Prepare data for ART input time series plots
##'
##' Take uploaded ART and shape file paths and format as data which
##' can be used to draw input time series graphs.
##'
##' @param art Path to file containing ART data
##' @param shape Path to file containing geojson areas data
##'
##' @return Data formatted for plotting input time series containing columns
##' area_id, area_name, area_level_label, time_step, time_period, plot and
##' value
##' @export
prepare_input_time_series_art <- function(art, shape) {
  ## From "area_id", "calendar_quarter", "sex", "age_group", "art_current"

  ## To "area_id","area_name","area_level_label","time_step", "time_period",
  ## "plot", "value"
  areas <- sf::read_sf(shape)
  heirarchy <- spread_areas(areas) %>%
    sf::st_drop_geometry()
  art_number <- readr::read_csv(art) %>%
    dplyr::left_join(heirarchy)
  quarter_vec <- tidyr::crossing(year = 2010:2020, lb = "\n", q = paste0("Q", 1:4)) %>%
    dplyr::mutate(calendar_quarter = paste0(q, lb, year)) %>%
    .$calendar_quarter
  # Aggregate ART
  art_long <- art_number %>%
    dplyr::group_by(area_id0, sex, age_group, year, calendar_quarter) %>%
    dplyr::summarise(art_current = sum(art_current),
                     art_new = sum(art_new)) %>%
    dplyr::rename(area_id = area_id0) %>%
    rbind(art_number %>%
            dplyr::group_by(area_id1, sex, age_group, year, calendar_quarter) %>%
            dplyr::summarise(art_current = sum(art_current),
                             art_new = sum(art_new)) %>%
            dplyr::rename(area_id = area_id1)) %>%
    rbind(art_number %>%
            dplyr::group_by(area_id2, sex, age_group, year, calendar_quarter) %>%
            dplyr::summarise(art_current = sum(art_current),
                             art_new = sum(art_new)) %>%
            dplyr::rename(area_id = area_id2)) %>%
    rbind(art_number %>%
            dplyr::group_by(area_id3, sex, age_group, year, calendar_quarter) %>%
            dplyr::summarise(art_current = sum(art_current),
                             art_new = sum(art_new)) %>%
            dplyr::rename(area_id = area_id3)) %>%
    rbind(art_number %>%
            dplyr::group_by(area_id4, sex, age_group, year, calendar_quarter) %>%
            dplyr::summarise(art_current = sum(art_current),
                             art_new = sum(art_new)) %>%
            dplyr::rename(area_id = area_id4)) %>%
    dplyr::ungroup()

  # Create new variables
  art <- art_long %>%
    dplyr::left_join(areas %>% sf::st_drop_geometry()) %>%
    dplyr::group_by(area_id, area_level_label, calendar_quarter) %>%
    dplyr::summarise(
      art_total = sum(art_current, na.rm = TRUE),
      art_adult_f = sum(art_current * as.integer(sex == "female" & age_group == "Y015_999"), na.rm = TRUE),
      art_adult_m = sum(art_current * as.integer(sex == "male" & age_group == "Y015_999"), na.rm = TRUE),
      art_adult = sum(art_current * as.integer(age_group == "Y015_999"), na.rm = TRUE),
      art_child = sum(art_current * as.integer(age_group == "Y000_014"), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(year = year_labels(calendar_quarter_to_quarter_id(calendar_quarter))) %>%
    tidyr::separate(calendar_quarter, into=c(NA, "quarter"), sep=-2) %>%
    dplyr::mutate(quarter = factor(paste0(quarter, "\n", year), levels = quarter_vec))

  # Shape data for plot
  art %>%
    dplyr::group_by(area_id, area_level_label, year) %>%
    dplyr::summarise(
      art_total = sum(art_total),
      art_adult_f = sum(art_adult_f),
      art_adult_m = sum(art_adult_m),
      art_adult = sum(art_adult),
      art_child = sum(art_child)
    ) %>%
    dplyr::mutate(
      art_adult_sex_ratio = art_adult_f / art_adult_m,
      art_adult_child_ratio = art_adult / art_child,
      art_prop_u15 = round(art_child/(art_adult+art_child),3),
      x_axis_variable = year
    )
}

##' Prepare data for ANC input time series plots
##'
##' Take uploaded ANC and shape file paths and format as data which
##' can be used to draw input time series graphs.
##'
##' @param anc Path to file containing ANC data
##' @param shape Path to file containing geojson areas data
##'
##' @return Data formatted for plotting input time series containing columns
##' area_id, area_name, area_level, area_level_label, age_group, time_period,
##' time_step, plot and value
##' @export
prepare_input_time_series_anc <- function(anc, shape) {
  ## From "area_id", "age_group", "year", "anc_clients",
  ## "anc_known_pos", "anc_already_art", "anc_tested", "anc_tested_pos"

  ## TO "area_id","area_name","area_level","area_level_label","age_group",
  ## "time_period","time_step","plot","value"
  areas <- sf::read_sf(shape)
  heirarchy <- spread_areas(areas) %>%
    sf::st_drop_geometry()
  anc_testing <- readr::read_csv(anc) %>%
    dplyr::left_join(heirarchy)

  # Aggregate ANC
  anc_long <- anc_testing %>%
    dplyr::group_by(area_id0,age_group, year) %>%
    dplyr::summarise(anc_clients = sum(anc_clients),
                     anc_known_pos = sum(anc_known_pos),
                     anc_already_art = sum(anc_already_art),
                     anc_tested = sum(anc_tested),
                     anc_tested_pos = sum(anc_tested_pos)) %>%
    dplyr::rename(area_id = area_id0) %>%
    rbind(anc_testing %>%
            dplyr::group_by(area_id1, age_group, year) %>%
            dplyr::summarise(anc_clients = sum(anc_clients),
                             anc_known_pos = sum(anc_known_pos),
                             anc_already_art = sum(anc_already_art),
                             anc_tested = sum(anc_tested),
                             anc_tested_pos = sum(anc_tested_pos)) %>%
            dplyr::rename(area_id = area_id1)) %>%
    rbind(anc_testing %>%
            dplyr::group_by(area_id2, age_group, year) %>%
            dplyr::summarise(anc_clients = sum(anc_clients),
                             anc_known_pos = sum(anc_known_pos),
                             anc_already_art = sum(anc_already_art),
                             anc_tested = sum(anc_tested),
                             anc_tested_pos = sum(anc_tested_pos)) %>%
            dplyr::rename(area_id = area_id2)) %>%
    rbind(anc_testing %>%
            dplyr::group_by(area_id3, age_group, year) %>%
            dplyr::summarise(anc_clients = sum(anc_clients),
                             anc_known_pos = sum(anc_known_pos),
                             anc_already_art = sum(anc_already_art),
                             anc_tested = sum(anc_tested),
                             anc_tested_pos = sum(anc_tested_pos)) %>%
            dplyr::rename(area_id = area_id3)) %>%
    rbind(anc_testing %>%
            dplyr::group_by(area_id4, age_group, year) %>%
            dplyr::summarise(anc_clients = sum(anc_clients),
                             anc_known_pos = sum(anc_known_pos),
                             anc_already_art = sum(anc_already_art),
                             anc_tested = sum(anc_tested),
                             anc_tested_pos = sum(anc_tested_pos)) %>%
            dplyr::rename(area_id = area_id4)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(areas %>% dplyr::select(area_id, area_name, area_level, area_level_label,
                                             parent_area_id, area_sort_order) %>% sf::st_drop_geometry())

  # Shape data for plot
  anc_long %>%
    dplyr::mutate(
      area_label = paste0(area_name, "\n(", area_id, ")") %>%
        forcats::fct_reorder(area_sort_order),
      anc_total_pos = anc_known_pos + anc_tested_pos,
      anc_status = anc_known_pos + anc_tested,
      anc_prevalence = anc_total_pos / anc_status,
      anc_art_among_known = anc_already_art / anc_known_pos,
      anc_art_coverage = anc_already_art / anc_total_pos
    ) %>%
    dplyr::arrange(area_id, year)
}
