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
  handle_naomi_warnings(do_validate_model_options(data, options))
}

do_validate_model_options <- function(data, options) {
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

    if (as.logical(options$artattend_t2) &&
        ((is.null(options$include_art_t1) || options$include_art_t1 == "false") ||
         (is.null(options$include_art_t2) || options$include_art_t2 == "false"))) {
      stop(t_("TIME_VARYING_ART_ATTENDANCE_IMPOSSIBLE"))
    }

    # Add warning is ART attendance is not selected
    if(!(as.logical(options$artattend))) {
      if(options$include_art_t1 == "true" || options$include_art_t2 == "true"){
        naomi_warning(t_("WARNING_OPTIONS_MISSING_ARTATTEND"),
                      c("model_options"))
      }
    }
  }

  area_merged <- read_area_merged(data$shape$path)
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

  list(valid = TRUE)
}
