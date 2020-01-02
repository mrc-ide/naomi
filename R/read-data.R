#' Read Naomi structured input files
#'
#' @param file A path to a file.
#' 
#' @export
read_population <- function(file) {

  ## !! TODO: add file format asserts
  
  col_spec <- readr::cols_only(
                       area_id = readr::col_character(),
                       source = readr::col_character(),
                       calendar_quarter = readr::col_character(),
                       sex = readr::col_character(),
                       age_group = readr::col_character(),
                       population = readr::col_double(),
                       asfr = readr::col_double()
                     )
                
  val <- readr::read_csv(file, col_types = col_spec)
  readr::stop_for_problems(val)
  stopifnot(names(col_spec$cols) %in% names(val))

  ## !! TODO: add validation asserts -- probably pull in hintr validation_asserts.R

  val
}

#' @rdname read_population
#' @export
read_survey_indicators <- function(file) {

  ## !! TODO: add file format asserts
  
  col_spec <- readr::cols_only(
                       indicator = readr::col_character(),
                       survey_id = readr::col_character(),
                       survey_year = readr::col_integer(),
                       area_id = readr::col_character(),
                       sex = readr::col_character(),
                       age_group = readr::col_character(),
                       n_cluster = readr::col_integer(),
                       n_obs = readr::col_integer(),
                       est = readr::col_double(),
                       se = readr::col_double(),
                       ci_l = readr::col_double(),
                       ci_u = readr::col_double()
                     )
  
  val <- readr::read_csv(file, col_types = col_spec)
  readr::stop_for_problems(val)
  stopifnot(names(col_spec$cols) %in% names(val))

  ## !! TODO: add validation asserts -- probably pull in hintr validation_asserts.R

  val

}

#' @rdname read_population
#' @export
read_art_number <- function(file) {

  ## !! TODO: add file format asserts
  
  col_spec <- readr::cols_only(
                       area_id = readr::col_character(),
                       sex = readr::col_character(),
                       age_group = readr::col_character(),
                       year = readr::col_integer(),
                       current_art = readr::col_double()
                     )
  
  val <- readr::read_csv(file, col_types = col_spec)
  readr::stop_for_problems(val)
  stopifnot(names(col_spec$cols) %in% names(val))

  ## !! TODO: add validation asserts -- probably pull in hintr validation_asserts.R

  val

}

#' @rdname read_population
#' @export
read_anc_testing <- function(file) {

  ## !! TODO: add file format asserts
  
  col_spec <- readr::cols_only(
                       area_id = readr::col_character(),
                       age_group = readr::col_character(),
                       year = readr::col_integer(),
                       anc_clients = readr::col_double(),
                       ancrt_known_pos = readr::col_double(),
                       ancrt_already_art = readr::col_double(),
                       ancrt_tested = readr::col_double(),
                       ancrt_test_pos = readr::col_double()
                     )
  
  val <- readr::read_csv(file, col_types = col_spec)
  readr::stop_for_problems(val)
  stopifnot(names(col_spec$cols) %in% names(val))
  
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
