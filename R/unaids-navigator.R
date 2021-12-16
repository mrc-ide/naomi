#' Write UNAIDS Estimates Navigator checklist CSV
#'
#' @param naomi_output a naomi_output object.
#' @param path path to save Navigator checklist CSV.
#'
#' @details
#'
#' @export
write_navigator_checklist <- function(naomi_output,
                                      path) {


  stopifnot(inherits(naomi_output, "naomi_output"))

  if (!grepl("\\.csv$", path, ignore.case = TRUE)) {
    path <- paste0(path, ".csv")
  }

  key <- c("ART_is_Spectrum",
           "ANC_is_Spectrum",
           "Package_created",
           "Package_has_all_data",
           "Opt_recent_qtr",
           "Opt_future_proj_qtr",
           "Opt_area_ID_selected",
           "Opt_calendar_survey_match",
           "Opt_recent_survey_only",
           "Opt_ART_coverage",
           "Opt_ANC_data",
           "Opt_ART_data",
           "Opt_ART_attendance_yes",
           "Model_fit",
           "Cal_Population",
           "Cal_PLHIV",
           "Cal_ART",
           "Cal_KOS",
           "Cal_new_infections",
           "Cal_method")

  label <- c(t_("NAVIGATOR_ART_IS_SPECTRUM_DESC"),
             t_("NAVIGATOR_ANC_IS_SPECTRUM_DESC"),
             t_("NAVIGATOR_PACKAGE_CREATED_DESC"),
             t_("NAVIGATOR_PACKAGE_HAS_ALL_DATA_DESC"),
             t_("NAVIGATOR_OPT_RECENT_QTR_DESC"),
             t_("NAVIGATOR_OPT_FUTURE_PROJ_QTR_DESC"),
             t_("NAVIGATOR_OPT_AREA_ID_SELECTED_DESC"),
             t_("NAVIGATOR_OPT_CALENDAR_SURVEY_MATCH_DESC"),
             t_("NAVIGATOR_OPT_RECENT_SURVEY_ONLY_DESC"),
             t_("NAVIGATOR_OPT_ART_COVERAGE_DESC"),
             t_("NAVIGATOR_OPT_ANC_DATA_DESC"),
             t_("NAVIGATOR_OPT_ART_DATA_DESC"),
             t_("NAVIGATOR_OPT_ART_ATTENDANCE_YES_DESC"),
             t_("NAVIGATOR_MODEL_FIT_DESC"),
             t_("NAVIGGATOR_CAL_POPULATION_DESC"),
             t_("NAVIGGATOR_CAL_PLHIV_DESC"),
             t_("NAVIGATOR_CAL_ART_DESC"),
             t_("NAVIGATOR_CAL_KOS_DESC"),
             t_("NAVIGATOR_CAL_NEW_INFECTIONS_DESC"),
             t_("NAVIGATOR_CAL_METHOD_DESC"))

  v <- data.frame(NaomiCheckPermPrimKey = key,
                  NaomiCheckDes = label,
                  TrueFalse = NA)

  ## Do checks
  ## These checks are always 'TRUE'
  always_true_checks <- c("Package_created", "Package_has_all_data", "Model_fit")
  v$TrueFalse[v$NaomiCheckPermPrimKey %in% always_true_checks] <- TRUE

  ## Check for correct model options selection
  valid_opt <- yaml::read_yaml(system.file("metadata/navigator_validation.yml", package = "naomi"))

  model_options <- naomi_output$fit$model_options
  data_options <- naomi_output$fit$data_options
  calibration_options <- naomi_output$fit$calibration_options

  if (!is.null(model_options)) {

    ## Is most recent calendar quarter selected
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_recent_qtr"] <-
      naomi_output$fit$model_options$calendar_quarter_t2 == valid_opt$calendar_quarter_t2

    ## Is future projection quarter selected
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_future_proj_qtr"] <-
      naomi_output$fit$model_options$calendar_quarter_t3 == valid_opt$calendar_quarter_t3

    ## Is scope set to national ID
    national_area_id <- naomi_output$meta_area$area_id[naomi_output$meta_area$area_level == 0]
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_area_ID_selected"] <-
      all(model_options$area_scope == national_area_id)

  }

  if (!is.null(data_options)) {

    ## Compare aggregated naomi inputs to national Spectrum totals
    v$TrueFalse[v$NaomiCheckPermPrimKey == "ART_is_Spectrum"] <- data_options$art_number_spectrum_aligned
    v$TrueFalse[v$NaomiCheckPermPrimKey == "ANC_is_Spectrum"] <- data_options$anc_testing_spectrum_aligned

    ## Check that all surveys used are from most recent quarter available
    most_recent_survey_available  <- max(data_options$prev_survey_available_quarters)
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_recent_survey_only"] <-
      all(data_options$prev_survey_quarters == most_recent_survey_available)

    # Is survey ART coverage included if available
    is_artcov_notavail <- length(data_options$artcov_survey_available) == 0   ## Not available -> TRUE
    is_artcov_used <- data_options$artcov_survey_ids %in% data_options$artcov_survey_available
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_ART_coverage"] <- is_artcov_notavail || is_artcov_used

    ## ## Check ART inputs
    ## Is ART data included for at least one time point
    has_art_t1 <- !is.null(naomi_output$fit$data_options$artnum_calendar_quarter_t1)
    has_art_t2 <- !is.null(naomi_output$fit$data_options$artnum_calendar_quarter_t2)
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_ART_data"] <- has_art_t1 || has_art_t2


    ## ## Check ANC inputs
    ## Is ANC data included for at least one time point
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_ANC_data"] <-
      !is.null(naomi_output$fit$data_options$anc_prev_year_t1)||
      !is.null(naomi_output$fit$data_options$anc_prev_year_t2)

    if (!is.null(model_options)) {

      ## Check survey
      # Does T1 calendar quarter match one survey
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_calendar_survey_match"] <-
        model_options$calendar_quarter_t1 %in% data_options$prev_survey_quarters

      ## ## Is ART attendance selected
      ## * If ART data at both time points; artattend and artattend_t1 should be both TRUE
      ## * If ART data at only one time point; only artattend should be TRUE
      ## * If no ART data; both should be FALSE
      if (has_art_t1 && has_art_t2) {
        v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_ART_attendance_yes"] <- model_options[["artattend"]] && model_options[["artattend_t2"]]
      } else if (has_art_t1 && !has_art_t2 || !has_art_t1 && has_art_t2 ) {
        v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_ART_attendance_yes"] <- model_options[["artattend"]] && !model_options[["artattend_t2"]]
      } else {
        v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_ART_attendance_yes"] <- !model_options[["artattend"]] && !model_options[["artattend_t2"]]
      }
    }
  }

  if (!is.null(calibration_options)) {

    if (all(naomi_output$meta_area$spectrum_region_code == 0)) {
        spec_level <- "national"
      } else {
        spec_level <- "subnational"
      }

    v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_Population"] <-
      calibration_options$spectrum_population_calibration == spec_level
    
    if (setequal(names(calibration_options), "spectrum_population_calibration")) {

      ## If naomi_output is uncalibrated, only the spectrum_population_calibration
      ## is completed. Since no calibrations are applied, return FALSE for calibration
      ## checks.

      calib_check_keys <- c("Cal_method", "Cal_PLHIV", "Cal_ART", "Cal_KOS", "Cal_new_infections")
      v$TrueFalse[v$NaomiCheckPermPrimKey %in% calib_check_keys] <- FALSE

    } else {

      ## Check for correct calibration options selection
      ## Is logistic calibration selected

      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_method"] <- calibration_options$calibrate_method == "logistic"

      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_PLHIV"] <-
        calibration_options$spectrum_plhiv_calibration_level == spec_level &&
        calibration_options$spectrum_plhiv_calibration_strat == "sex_age_coarse"

      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_ART"] <-
        calibration_options$spectrum_artnum_calibration_level == spec_level &&
        calibration_options$spectrum_artnum_calibration_strat == "sex_age_coarse"

      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_KOS"] <-
        calibration_options$spectrum_aware_calibration_level == spec_level &&
        calibration_options$spectrum_aware_calibration_strat == "sex_age_coarse"

      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_new_infections"] <-
        calibration_options$spectrum_infections_calibration_level == spec_level &&
        calibration_options$spectrum_infections_calibration_strat == "sex_age_coarse"
    }
  }

  ## Using write.csv() instead of naomi_write_csv() because writing na = "NA"
  write.csv(v, path, row.names = FALSE, na = "NA")
}
