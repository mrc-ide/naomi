#' Get JSON template representing model run options
#'
#' This reads JSON file of model run options which describe how the Naomi
#' front end should display input options. Returns template for the API to
#' enrich with model options.
#'
#' @param art If FALSE then don't return template for ART control section.
#' @param anc If FALSE then don't return template for ANC control section.
#'
#' @return Model run options template.
#' @export
#'
#' @examples
#' get_model_options_template(TRUE, TRUE)
#' get_model_options_template(FALSE, FALSE)
get_model_options_template <- function(art, anc) {
  templates <- list()
  templates$general <- paste(readLines(
    system_file("metadata", "general_run_options.json"),
    encoding = "UTF-8"), collapse = "")
  templates$survey <- paste(readLines(
    system_file("metadata", "survey_run_options.json"),
    encoding = "UTF-8"), collapse = "")
  if (art) {
    templates$art <- paste(readLines(
      system_file("metadata", "art_run_options.json"),
      encoding = "UTF-8"), collapse = "")
  }
  if (anc) {
    templates$anc <- paste(readLines(
      system_file("metadata", "anc_run_options.json"),
      encoding = "UTF-8"), collapse = "")
  }
  templates
}

#' Validate a set of model options
#'
#' This validates that a set of model options can be used to run the model
#'
#' @param data The set of input data for the model run
#' @param options Key-value list of model options
#'
#' @return TRUE if valid otherwise throw an error
#' @export
validate_model_options <- function(data, options) {
  ## This must return TRUE if valid - otherwise throw an error
  ## TODO: Add real validation
  TRUE
}
