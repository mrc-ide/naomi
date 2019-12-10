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
  if (anc) {
    templates$anc <-  read_options("anc")
  }
  if (art) {
    templates$art <- read_options("art")
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
  calib_level_options <- c("national", "subnational", "none")
  calib_strat_options <- c("age_coarse", "sex_age_coarse", "age_group", "sex_age_group")

  ## !! TODO: Makes sure NULLs are handled in a way that doesn't break anything!!
  ## if(!options$spectrum_population_calibration %in% calib_level_options)
  ##   stop(paste0("Spectrum population calibration option \"",
  ##               options$spectrum_population_calibration, "\" not found."))

  ## if(!options$spectrum_plhiv_calibration_level %in% calib_level_options)
  ##   stop(paste0("Spectrum PLHIV calibration level \"",
  ##               options$spectrum_plhiv_calibration_level, "\" not found."))

  ## if(!options$spectrum_artnum_calibration_level %in% calib_level_options)
  ##   stop(paste0("Spectrum ART number calibration level \"",
  ##               options$spectrum_artnum_calibration_level, "\" not found."))

  ## if(!options$spectrum_plhiv_calibration_strat %in% calib_strat_options)
  ##   stop(paste0("Spectrum PLHIV calibration stratification \"",
  ##               options$spectrum_plhiv_calibration_strat, "\" not found."))

  ## if(!options$spectrum_artnum_calibration_strat %in% calib_strat_options)
  ##   stop(paste0("Spectrum ART number calibration stratification \"",
  ##               options$spectrum_artnum_calibration_strat, "\" not found."))

  ## ART attendance model options
  if (!is.null(data$art_number)) {
    if (as.logical(options$artattend) &&
       ((is.null(options$include_art_t1) || options$include_art_t1 == "false") &&
       (is.null(options$include_art_t2) || options$include_art_t2 == "false"))) {
      stop(t_("art_attendance_impossible"))
    }
  }

  TRUE
}
