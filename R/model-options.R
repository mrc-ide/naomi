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
  templates$general <- read_options("general")
  templates$survey <- read_options("survey")
  if (art) {
    templates$art <- read_options("art")
  }
  if (anc) {
    templates$anc <-  read_options("anc")
  }
  templates$advanced <- read_options("advanced")
  templates
}

read_options <- function(type) {
  paste(readLines(
    system_file("metadata", sprintf("%s_run_options.json", type)),
    encoding = "UTF-8"), collapse = "")
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
  ## TODO: mrc-795 Add real validation
  TRUE
}
