#' Write UNAIDS Estimates Navigator checklist CSV
#'
#' @param naomi_output a naomi_output object.
#' @param options naomi model options
#' @param path path to save Navigator checklist CSV.
#'
#' @details
#'
#' @export
write_navigator_checklist <- function(naomi_output,
                                      options,
                                      data,
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


  ## Check for correct model options selection
  valid_opt <- yaml::read_yaml(system.file("metadata/navigator_validation.yml", package = "naomi"))

  # Is most recent calendar quarter selected
  if(options$calendar_quarter_t2 == valid_opt$calendar_quarter_t2){
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_recent_qtr"] <- TRUE}

  # Is future projection quarter selected
  if(options$calendar_quarter_t2 == valid_opt$calendar_quarter_t2){
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_future_proj_qtr"] <- TRUE}

  # Is ART attendance selected
  if(options$artattend){v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_ART_attendance_yes"] <- TRUE}

  # Is scope set to national ID
  scope <- options$area_scope
  national_id <- naomi_output$meta_area$area_id[naomi_output$meta_area$area_level == 0]
  if(scope == national_id){v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_area_ID_selected"] <- TRUE}

  ## Check survey
  # Does T1 calendar quarter match one survey
  t1 <- gsub(".*?([0-9]+).*", "\\1", options$calendar_quarter_t1)
  surveys <- gsub(".*?([0-9]+).*", "\\1", options$survey_prevalence)
  if(t1 %in% surveys){v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_calendar_survey_match"] <- TRUE}

  # Is most recent survey in input data selected in options
  input_survey <- read_csv(data$survey$path, show_col_types = FALSE)
  input_survey_ids <- unique(input_survey$survey_id)
  max_inputs_survey_year <- max(gsub(".*?([0-9]+).*", "\\1", input_survey_ids))
  options_survey_year <- gsub(".*?([0-9]+).*", "\\1", options$survey_prevalence)
  if(max_inputs_survey_year %in% options_survey_year) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_recent_survey_only"] <- TRUE}

  # Is survey ART coverage included if available
  input_survey_ind <- unique(input_survey$indicator)
  if("art_coverage" %in% input_survey_ind && !is.null(options$survey_art_coverage)) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_ART_coverage"] <- TRUE}

  ## Check ART inputs
  # Is ART data included if available
  art_options <- c(options$include_art_t1, options$include_art_t2)

  if(TRUE || "true" %in% art_options && !is.null(data$art_number)) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_ART_data"] <- TRUE}

  ## Check ANC inputs
  # Is ANC data included if available
  anc_options <- c(options$anc_prevalence_year1, options$anc_prevalence_year2)

  if(TRUE || "true" %in% anc_options && !is.null(data$anc_testing)) {
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Opt_ANC_data"] <- TRUE}

  ## Check for correct calibration options selection
  # Is logistic calibration selected

  if(options$calibrate_method == "logistic"){
    v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_method"] <- TRUE}

  spectrum_code <- unique(naomi_output$fit$spectrum_calibration$spectrum_region_code)
  if(0 %in% spectrum_code){spec_level <- "nat"} else {spec_level <- "subnat"}

  if(spec_level == "nat"){
    if(options$spectrum_plhiv_calibration_level == "national"){
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_PLHIV"] <- TRUE}

    if(options$spectrum_artnum_calibration_level == "national"){
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_ART"] <- TRUE}

    if(options$spectrum_aware_calibration_level == "national"){
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_KOS"] <- TRUE}

    if(options$spectrum_infections_calibration_level == "national"){
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_new_infections"] <- TRUE}
  }

  if(spec_level == "subnat"){
    if(options$spectrum_plhiv_calibration_level == "subnational"){
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_PLHIV"] <- TRUE}

    if(options$spectrum_artnum_calibration_level == "subnational"){
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_ART"] <- TRUE}

    if(options$spectrum_aware_calibration_level == "subnational"){
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_KOS"] <- TRUE}

    if(options$spectrum_infections_calibration_level == "subnational"){
      v$TrueFalse[v$NaomiCheckPermPrimKey == "Cal_new_infections"] <- TRUE}
  }


  ## Using write.csv() instead of naomi_write_csv() because writing na = "NA"
  write.csv(v, path, row.names = FALSE, na = "NA")
}
