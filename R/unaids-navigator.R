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
             t_("NAVIGGATOR_CAL_PLHIV_DESC"),
             t_("NAVIGATOR_CAL_ART_DESC"),
             t_("NAVIGATOR_CAL_KOS_DESC"),
             t_("NAVIGATOR_CAL_NEW_INFECTIONS_DESC"),
             t_("NAVIGATOR_CAL_METHOD_DESC"))

  v <- data.frame(NaomiCheckPermPrimKey = key,
                  NaomiCheckDes = label,
                  TrueFalse = FALSE)

  ## Do checks
  ## These checks are always 'TRUE'
  always_true_checks <- c("Package_created", "Package_has_all_data", "Model_fit")
  v$TrueFalse[v$NaomiCheckPermPrimKey %in% always_true_checks] <- TRUE

  # TODO: Compare aggregated naomi inputs to national Spectrum totals
  # This is dummy code

  spec_art <- 1000
  naomi_art <- 1000
  spec_anc <- 500
  naomi_anc <- 500

  if (spec_art == naomi_art) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "ART_is_Spectrum"] <- TRUE
  }

  if (spec_anc == naomi_anc) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "ANC_is_Spectrum"] <- TRUE
  }



  ## Check for correct model options selection
  valid_opt <- yaml::read_yaml(system.file("metadata/navigator_validation.yml", package = "naomi"))

  model_options <- naomi_output$fit$model_options
  data_options <- naomi_output$fit$data_options
  calibration_options <- naomi_output$fit$calibration_options

  ## Is most recent calendar quarter selected
  if (naomi_output$fit$model_options$calendar_quarter_t2 == valid_opt$calendar_quarter_t2) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_recent_qtr"] <- TRUE
  }

  ## Is future projection quarter selected
  if (naomi_output$fit$model_options$calendar_quarter_t3 == valid_opt$calendar_quarter_t3) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_future_proj_qtr"] <- TRUE
  }

  ## Is scope set to national ID
  national_area_id <- naomi_output$meta_area$area_id[naomi_output$meta_area$area_level == 0]
  if (all(model_options$area_scope == national_area_id)) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_area_ID_selected"] <- TRUE
  }

  ## Check survey
  # Does T1 calendar quarter match one survey
  if (model_options$calendar_quarter_t1 %in% data_options$prev_survey_quarters) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_calendar_survey_match"] <- TRUE
  }

  # Is most recent survey in input data selected in options
  prev_survey_available <- data_options$prev_survey_available
  max_prev_survey_available <- max(gsub(".*?([0-9]+).*", "\\1", prev_survey_available))
  prev_survey_ids <- gsub(".*?([0-9]+).*", "\\1", data_options$prev_survey_ids)
  if (max_prev_survey_available %in% prev_survey_ids) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_recent_survey_only"] <- TRUE
  }

  # Is survey ART coverage included if available
  if (length(data_options$artcov_survey_available) == 0 ||   ## Not available -> TRUE
      data_options$artcov_survey_ids %in% data_options$artcov_survey_available) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_ART_coverage"] <- TRUE
  }

  ## ## Check ART inputs
  ## Is ART data included for at least one time point
  has_art_t1 <- !is.null(naomi_output$fit$data_options$artnum_calendar_quarter_t1)
  has_art_t2 <- !is.null(naomi_output$fit$data_options$artnum_calendar_quarter_t2)
  
  if ( has_art_t1 || has_art_t2 ) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_ART_data"] <- TRUE
  }

  ## ## Check ANC inputs
  ## Is ANC data included for at least one time point
  if (!is.null(naomi_output$fit$data_options$anc_prev_year_t1)||
      !is.null(naomi_output$fit$data_options$anc_prev_year_t2)) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_ANC_data"] <- TRUE
  }

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
  

  ## Check for correct calibration options selection
  # Is logistic calibration selected

  calibration_options <- naomi_output$fit$calibration_options

  if (calibration_options$calibrate_method == "logistic") {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_method"] <- TRUE
  }


  if (all(naomi_output$meta_area$spectrum_region_code == 0)) {
    spec_level <- "nat"
  } else {
    spec_level <- "subnat"
  }

  if (spec_level == "nat") {
    if (calibration_options$spectrum_plhiv_calibration_level == "national") {
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_PLHIV"] <- TRUE
    }

    if (calibration_options$spectrum_artnum_calibration_level == "national") {
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_ART"] <- TRUE
    }

    if (calibration_options$spectrum_aware_calibration_level == "national") {
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_KOS"] <- TRUE
    }

    if (calibration_options$spectrum_infections_calibration_level == "national") {
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_new_infections"] <- TRUE
    }
  }

  if (spec_level == "subnat") {
    if (calibration_options$spectrum_plhiv_calibration_level == "subnational") {
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_PLHIV"] <- TRUE
    }

    if (calibration_options$spectrum_artnum_calibration_level == "subnational") {
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_ART"] <- TRUE
    }

    if (calibration_options$spectrum_aware_calibration_level == "subnational") {
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_KOS"] <- TRUE
    }

    if (calibration_options$spectrum_infections_calibration_level == "subnational") {
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_new_infections"] <- TRUE
    }
  }


  ## Using write.csv() instead of naomi_write_csv() because writing na = "NA"
  write.csv(v, path, row.names = FALSE, na = "NA")
}
