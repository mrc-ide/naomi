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
  options <- paste(
    brio::readLines(system_file("metadata", sprintf("%s_run_options.json", type))),
    collapse = ""
  )
  traduire::translator()$replace(options)
}

#' Get JSON representing model calibration options
#'
#' @return Model calibration options.
#' @export
#'
#' @examples
#' get_model_calibration_options()
get_model_calibration_options <- function() {
  read_options("calibration")
}


#' Map calibration option ID to JSON calibration option labels
#'
#' @param options Key-value (calibration option name - calibration option ID)
#' list of model options to be mapped.
#'
#' @return Mapped key-value (calibration option name - calibration option label)
#' list of model options
get_calibration_option_labels <- function(options) {
  ## This is not ideal that we are maintaining this list here and in the
  ## calibration options themselves but the alternative of parsing this
  ## from the calibration json is far worse
  ## TODO: Improve how this is being built see mrc-2022
  calibration_option_map <- list(
    none = "t_(OPTIONS_CALIBRATION_ADJUST_TO_SPECTRUM_NONE)",
    national= "t_(OPTIONS_CALIBRATION_ADJUST_TO_SPECTRUM_NATIONAL)",
    subnational = "t_(OPTIONS_CALIBRATION_ADJUST_TO_SPECTRUM_SUBNATIONAL)",
    age_coarse = "t_(OPTIONS_CALIBRATION_ADJUST_TO_SPECTRUM_AGE_COARSE_LABEL)",
    sex_age_coarse = "t_(OPTIONS_CALIBRATION_ADJUST_TO_SPECTRUM_SEX_AGE_COARSE_LABEL)",
    age_group = "t_(OPTIONS_CALIBRATION_ADJUST_TO_SPECTRUM_AGE_GROUP_LABEL)",
    sex_age_group = "t_(OPTIONS_CALIBRATION_ADJUST_TO_SPECTRUM_SEX_AGE_GROUP_LABEL)",
    logistic = "t_(OPTIONS_CALIBRATE_METHOD_LOGISTIC_LABEL)",
    proportional = "t_(OPTIONS_CALIBRATE_METHOD_PROPORTIONAL_LABEL)"
  )
  calibration_option_map <- traduire::translator()$replace(
    calibration_option_map)
  map_option <- function(option_name) {
    if (!(options[[option_name]]) %in% names(calibration_option_map)) {
      stop(sprintf("Failed to find calibration option for name %s and id %s",
                   option_name, options[[option_name]]))
    }
    calibration_option_map[[options[[option_name]]]]
  }
  as.list(vapply(names(options), map_option, character(1), USE.NAMES = TRUE))
}

#' Validate a set of model options
#'
#' This validates that a set of model options can be used to run the model
#'
#' @param data The set of input data for the model run
#' @param options Key-value list of model options
#'
#' @return TRUE if valid otherwise throw an error
#'
#' @export
validate_model_options <- function(data, options) {

  required_options <- c("area_scope", "area_level",
                        "calendar_quarter_t1",
                        "calendar_quarter_t2",
                        "calendar_quarter_t3",
                        "survey_prevalence")

  data <- format_data_input(data)

  if(!all(required_options %in% names(options)))
    stop(t_("MISSING_OPTIONS", list(missing_options =
      paste(setdiff(required_options, names(options)), collapse = ", "))))

  ## TODO: better approach to check file exists and is valid?
  if(is.null(data$art_number) &&
     (!is.null(options$include_art_t1) && options$include_art_t1 == "true" ||
      !is.null(options$include_art_t2) && options$include_art_t2 == "true"))
    stop(t_("MISSING_ART_DATA"))

  area_merged <- read_area_merged(data$shape$path)
  population <- read_population(data$population$path)
  survey <- read_survey_indicators(data$survey$path)

  ## # Area selection
  ## !!! TODO: temporary check. More comprehensive validation should be done
  ##     with overhauling the data.tree stuff.
  if(options$area_level == 0)
    stop(t_("NO_COUNTRY_LEVEL_FIT"))

  ## Check time T2 is after T1
  if(calendar_quarter_to_quarter_id(options$calendar_quarter_t1) >=
     calendar_quarter_to_quarter_id(options$calendar_quarter_t2))
    stop("Estimates quarter (time 2) must be after survey quarter (time 1)")

    ## Check time T3 is after T2
  if(calendar_quarter_to_quarter_id(options$calendar_quarter_t2) >=
     calendar_quarter_to_quarter_id(options$calendar_quarter_t3))
    stop("Projection quarter (time 3) must be after estimates quarter (time 2)")

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
      stop(t_("ART_ATTENDANCE_IMPOSSIBLE"))
    }
  }

  if (all(is.na(area_merged$spectrum_region_code))) {
    stop(t_("SHAPE_SPECTRUM_REGION_ALL_NA"))
  }

  ## ## Validate PJNZ

  pjnz_list <- unroll_pjnz(data$pjnz$path)
  spectrum_region_codes <- vapply(pjnz_list, read_spectrum_region_code, integer(1))

  missing_spectrum_regions <- !all(is.na(area_merged$spectrum_region_code) |
                                   area_merged$spectrum_region_code %in% spectrum_region_codes)
  ## !! TODO: return names and codes of missing regions
  if (missing_spectrum_regions) {
    stop(t_("PJNZ_SHAPE_CODE_MISMATCH"))
  }

  if (as.logical(options$output_aware_plhiv)) {
    has_shiny90 <- vapply(pjnz_list, assert_pjnz_shiny90, logical(1))
    if (any(!has_shiny90)) {
      projname <- vapply(pjnz_list[!has_shiny90], read_spectrum_projection_name, character(1))
      stop(t_("ERROR_SHINY90_MISSING_1"), ": ",
           paste0(projname, collapse = ", "),
           ". ", t_("ERROR_SHINY90_MISSING_2"))
    }
  }

  TRUE
}
