#' Write UNAIDS Estimates Navigator checklist CSV
#'
#' @param naomi_output a naomi_output object.
#' @param path path to save Navigator checlist CSV.
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
                  TrueFalse = NA)

  ## These checks are always 'TRUE'
  always_true_checks <- c("Package_created", "Package_has_all_data", "Model_fit")
  v$TrueFalse[v$NaomiCheckPermPrimKey %in% always_true_checks] <- TRUE

  ## Using write.csv() instead of naomi_write_csv() because writing na = "NA"
  write.csv(v, path, row.names = FALSE, na = "NA")
}
