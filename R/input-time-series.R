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

  ## To "area_id","area_name","area_level_label", "area_level", time_step",
  ## "time_period","plot", "value"

  ## Check if data is quarterly or annual - if quarterly aggregate to annual
  areas <- sf::read_sf(shape) %>% sf::st_drop_geometry()

  art_number <- readr::read_csv(art, show_col_types = FALSE) %>%
    dplyr::left_join(areas %>% dplyr::select(area_id, area_level), by = "area_id") %>%
    dplyr::mutate(year = year_labels(calendar_quarter_to_quarter_id(calendar_quarter)))

  # Check for duplicated years in area_id/sex/age combinations
  dup <- art_number %>%
    dplyr::group_by(area_id, sex, age_group, year) %>%
    dplyr::summarise(n=n(), .groups = 'drop')

  if(unique(dup$n == 4)) {
    # If quarterly values available for all disags - aggregate to annual
    art_number_annual <- art_number %>%
      dplyr::group_by(area_id, area_name, area_level, sex, age_group, year) %>%
      dplyr::summarise(art_current = sum(art_current, na.rm = TRUE),
                .groups = 'drop') %>%
      dplyr::mutate(time_period = paste0("CY", year, "Q4"),
             time_step = "annual")

    # Bind to quarterly data set
    art_number <- art_number %>%
      dplyr::mutate(time_step = "quarterly") %>%
      dplyr::rename(time_period = calendar_quarter) %>%
      rbind(art_number_annual)
  } else if (unique(dup$n == 1)) {
    # If annual values available for all disags - don't aggregate
    art_number <- art_number %>%
      dplyr::mutate(time_step = "annual") %>%
      dplyr::rename(time_period = calendar_quarter)
  } else {
    # Throw error for duplicate annual or incomplete quarterly data
      stop(print("Quarterly data not provdied for all dissagregates/n
                 OR duplicate annual data provided for all dissagreagtes"))
    }

  ## Recursively aggregate ART data up from lowest level of programm data provided
  # Level to aggregate from
  art_level <- levels(as.factor(art_number$area_level))
  # Join ART data to hierarchy
  art_number_wide <- spread_areas(areas %>% dplyr::filter(area_level <= art_level)) %>%
    dplyr::left_join(art_number, by = "area_id")
  # Function to aggregate based on area_id[0-9]$ columns in hierarchy
  aggregate_data_art <- function(col_name) {
    df <- art_number_wide %>%
      dplyr::group_by(eval(as.name(col_name)), sex, age_group, time_step, time_period) %>%
      dplyr::summarise(art_current = sum(art_current,
                                         na.rm = TRUE), .groups = 'drop') %>%
      dplyr::rename(area_id = `eval(as.name(col_name))`)
    }
  # Aggregated data frame
  art_long <- grep("^area_id*\\s*[0-9]$", colnames(art_number_wide), value = TRUE) %>%
    lapply(function(x) aggregate_data_art(x))  %>%
    dplyr::bind_rows()

  ## Shape data for plot
  art_plot_data <- art_long %>%
    dplyr::left_join(areas, by = "area_id" ) %>%
    dplyr::group_by(area_id, area_name, area_level_label, area_level, time_step, time_period) %>%
    dplyr::summarise(
      art_total = sum(art_current, na.rm = TRUE),
      art_adult_f = sum(art_current * as.integer(sex == "female" & age_group == "Y015_999"), na.rm = TRUE),
      art_adult_m = sum(art_current * as.integer(sex == "male" & age_group == "Y015_999"), na.rm = TRUE),
      art_adult = sum(art_current * as.integer(age_group == "Y015_999"), na.rm = TRUE),
      art_child = sum(art_current * as.integer(age_group == "Y000_014"), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      art_adult_sex_ratio = art_adult_f / art_adult_m,
      art_adult_child_ratio = art_adult / art_child,
      art_prop_u15 = round(art_child/(art_adult+art_child),3)
    ) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("art"),
                        names_to = "plot",
                        values_to = "value")

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


  ## Recursively aggregate ANC data up from lowest level of programm data provided
  # Level to aggregate from
  areas <- sf::read_sf(shape) %>% sf::st_drop_geometry()

  anc_testing <- readr::read_csv(anc, show_col_types = FALSE) %>%
    dplyr::left_join(areas %>% dplyr::select(area_id, area_level), by = "area_id") %>%
    dplyr::mutate(time_step = "annual",
           time_period = paste0("CY",year, "Q4"))

  ## Recursively aggregate ART data up from lowest level of programm data provided
  # Level to aggregate from
  anc_level <- levels(as.factor(anc_testing$area_level))
  # Join ART data to hierarchy
  anc_testing_wide <- dplyr::left_join(
    anc_testing,
    spread_areas(areas %>% dplyr::filter(area_level <= anc_level)),
    by = "area_id")

  # Function to aggregate based on area_id[0-9]$ columns in hierarchy
  aggregate_data_anc <- function(col_name) {
    df <- anc_testing_wide %>%
      dplyr::group_by(eval(as.name(col_name)), age_group, time_step, time_period) %>%
      dplyr::summarise(anc_clients = sum(anc_clients, na.rm = TRUE),
                       anc_known_pos = sum(anc_known_pos, na.rm = TRUE),
                       anc_already_art = sum(anc_already_art, na.rm = TRUE),
                       anc_tested = sum(anc_tested, na.rm = TRUE),
                       anc_tested_pos = sum(anc_tested_pos, na.rm = TRUE),
                       .groups = 'drop') %>%
      dplyr::rename(area_id = `eval(as.name(col_name))`)
  }

  # Aggregated data frame
  anc_long <- grep("^area_id*\\s*[0-9]$", colnames(anc_testing_wide), value = TRUE) %>%
    lapply(function(x) aggregate_data_anc(x))  %>%
    dplyr::bind_rows()

  ## Shape data for plot
  anc_plot_data <- anc_long %>%
    dplyr::left_join(areas %>% dplyr::select(area_id, area_name,area_level,
                                             area_level_label),
                     by = "area_id") %>%
    dplyr::mutate(
      anc_total_pos = anc_known_pos + anc_tested_pos,
      anc_status = anc_known_pos + anc_tested,
      anc_prevalence = anc_total_pos / anc_status,
      anc_art_among_known = anc_already_art / anc_known_pos,
      anc_art_coverage = anc_already_art / anc_total_pos
    ) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("anc"),
                        names_to = "plot",
                        values_to = "value")
}
