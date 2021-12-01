#' Read Naomi structured input files
#'
#' @param file A path to a file.
#'
#' @export
read_population <- function(file) {

  ## !! TODO: add file format asserts

  required_cols <- c("area_id", "calendar_quarter", "sex", "age_group", "population")

  col_spec <- readr::cols_only(
                       area_id = readr::col_character(),
                       source = readr::col_character(),
                       calendar_quarter = readr::col_character(),
                       sex = readr::col_character(),
                       age_group = readr::col_character(),
                       population = readr::col_double(),
                       asfr = readr::col_double()
                     )

  val <- read_csv_partial_cols(file, col_types = col_spec)
  readr::stop_for_problems(val)

  val <- drop_na_rows(val)

  missing_cols <- setdiff(required_cols, names(val))
  if(length(missing_cols)) {
    stop(t_("POPULATION_REQUIRED_COLUMNS_MISSING",
            list(cols = paste(missing_cols, collapse = ", "))))
  }

  ## !! TODO: add validation asserts -- probably pull in hintr validation_asserts.R

  val
}

#' @rdname read_population
#' @export
read_survey_indicators <- function(file) {

  ## !! TODO: add file format asserts

  required_cols <- c("indicator", "survey_id", "area_id", "sex", "age_group",
                     "n_clusters", "n_observations", "n_eff_kish",
                     "estimate", "std_error", "ci_lower", "ci_upper")

  col_spec <- readr::cols_only(
                       indicator = readr::col_character(),
                       survey_id = readr::col_character(),
                       survey_year = readr::col_integer(),
                       survey_mid_calendar_quarter = readr::col_character(),
                       area_id = readr::col_character(),
                       sex = readr::col_character(),
                       age_group = readr::col_character(),
                       n_clusters = readr::col_integer(),
                       n_observations = readr::col_integer(),
                       n_eff_kish = readr::col_double(),
                       estimate = readr::col_double(),
                       std_error = readr::col_double(),
                       ci_lower = readr::col_double(),
                       ci_upper = readr::col_double()
                     )

  val <- read_csv_partial_cols(file, col_types = col_spec)
  readr::stop_for_problems(val)

  val <- drop_na_rows(val)

  missing_cols <- setdiff(required_cols, names(val))
  if(length(missing_cols))
    stop(paste0("Required columns not found: ", paste(missing_cols, collapse = ", ")))


  ## !! TODO: add validation asserts -- probably pull in hintr validation_asserts.R

  val

}

#' @rdname read_population
#' @export
read_art_number <- function(file) {

  ## !! TODO: add file format asserts

  required_cols <- c("area_id", "sex", "age_group", "art_current")

  col_spec <- readr::cols_only(
                       area_id = readr::col_character(),
                       sex = readr::col_character(),
                       age_group = readr::col_character(),
                       year = readr::col_integer(),
                       calendar_quarter = readr::col_character(),
                       art_current = readr::col_double(),
                       art_new = readr::col_double()
                     )

  val <- read_csv_partial_cols(file, col_types = col_spec)
  readr::stop_for_problems(val)

  val <- drop_na_rows(val)

  missing_cols <- setdiff(required_cols, names(val))
  if(length(missing_cols))
    stop(paste0("Required columns not found: ", paste(missing_cols, collapse = ", ")))

  if(!any(c("year", "calendar_quarter") %in% names(val)))
     stop(paste0("Both 'year' and 'calendar_quarter' are missing. One must be present."))

  if(!"calendar_quarter" %in% names(val))
    val$calendar_quarter <- NA_character_

  if("year" %in% names(val)) {

    if(any(!is.na(val$calendar_quarter) &
           val$year != as.integer(substr(val$calendar_quarter, 3, 6))))
      stop("Inconsistent year and calendar_quarter found in ART dataset.")

    ## If calendar quarter is not specified, assumed that represents end-of-year reporting
    val <- val %>%
      dplyr::mutate(
               calendar_quarter = dplyr::if_else(is.na(calendar_quarter), paste0("CY", year, "Q4"), calendar_quarter)
             )
  }

  ## !! TODO: check all columns are valid calendar quarters

  ## !! TODO: add validation asserts -- probably pull in hintr validation_asserts.R

  dplyr::select(val, area_id, sex, age_group, calendar_quarter, art_current)
}

#' @rdname read_population
#' @export
read_anc_testing <- function(file) {

  ## !! TODO: add file format asserts

  required_cols <- c("area_id", "age_group", "year", "anc_clients",
                     "anc_known_pos", "anc_already_art", "anc_tested", "anc_tested_pos")

  col_spec <- readr::cols_only(
                       area_id = readr::col_character(),
                       age_group = readr::col_character(),
                       year = readr::col_double(),
                       anc_clients = readr::col_double(),
                       anc_known_pos = readr::col_double(),
                       anc_already_art = readr::col_double(),
                       anc_tested = readr::col_double(),
                       anc_tested_pos = readr::col_double()
                     )

  val <- readr_read_csv(file, col_types = col_spec)
  readr::stop_for_problems(val)
  stopifnot(na.omit(val$year) %% 1 == 0)
  val$year <- as.integer(val$year)

  val <- drop_na_rows(val)

  missing_cols <- setdiff(required_cols, names(val))
  if(length(missing_cols))
    stop(paste0("Required columns not found: ", paste(missing_cols, collapse = ", ")))


  ## !! TODO: add validation asserts -- probably pull in hintr validation_asserts.R

  val
}


#' @rdname read_population
#' @export
read_area_merged <- function(file) {

  ## !! TODO: add file format asserts

  ## Note: this follows a different template than others because sf::read_sf
  ## does not allow col_types specification.

  val <- sf::read_sf(file)
  val <- dplyr::select(val, area_id, area_name, area_level, parent_area_id, spectrum_region_code, area_sort_order,
                       center_x, center_y, area_level_label, display, geometry)

  ## !! TODO: coerce column types
  ## !! TODO: add validation asserts -- probably pull in hintr validation_asserts.R

  val
}


#' Read CSV with missing columns
#'
#' This executes readr::read_csv with suppressing the warning for col_types that are
#' explicitly specified but not found.
#'
#' @param ... arguments to `readr::read_csv`.
#' @return return from `readr::read_csv`.
#'
#' @keywords internal
read_csv_partial_cols <- function(...){
  suppress_one_warning(readr_read_csv(...), "The following named parsers don't match the column names")
}

drop_na_rows <- function(x) {
  na_rows <- apply(is.na(x) | x == "", 1, all)
  x[!na_rows, ]
}
