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

  required_options <- c("area_scope", "area_level",
                        "calendar_quarter_t1", "calendar_quarter_t2",
                        "survey_prevalence")

  if(!all(required_options %in% names(options)))
    stop(paste("Required model options not supplied:",
               paste(setdiff(required_options, names(options)), collapse = ", ")))

  ## TODO: better approach to check file exists and is valid?
  if(is.null(data$art_number) &&
     (!is.null(options$include_art_t1) && options$include_art_t1 == "true" ||
      !is.null(options$include_art_t2) && options$include_art_t2 == "true"))
    stop("ART dataset not provided. ART data cannot be selected Yes to include.")


  ## # Population inputs

  ## TODO:
  ## * completeness: contains m/f for all age/sex
  ## * does not contain "both" sexes

  ## Calibration options

  if(!options$spectrum_population_calibration %in%
     c("national", "subnational", "none"))
    stop(paste0("Spectrum population calibration option \"",
                options$spectrum_population_calibration, "\" not found."))
  
  if(!is.null(options$spectrum_plhiv_0to14_calibration) &&
     options$spectrum_plhiv_0to14_calibration %in% c("national", "subnational"))
    stop("Spectrum PLHIV age 0-14 calibration not yet implemented")

  if(!is.null(options$spectrum_plhiv_15plus_calibration) &&
     options$spectrum_plhiv_15plus_calibration %in% c("national", "subnational"))
    stop("Spectrum PLHIV 15+ calibration not yet implemented")

  if(!is.null(options$spectrum_artnum_0to14_calibration) &&
     options$spectrum_artnum_0to14_calibration %in% c("national", "subnational"))
    stop("Spectrum ART age 0-14 calibration not yet implemented")

  if(!is.null(options$spectrum_artnum_15plus_calibration) &&
     options$spectrum_artnum_15plus_calibration %in% c("national", "subnational"))
    stop("Spectrum ART 15+ calibration not yet implemented")
  
  TRUE
}
