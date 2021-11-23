#' Write UNAIDS Estimates Navigator checklist CSV
#'
#' @param naomi_output a naomi_output object.
#' @param path path to save Navigator checlist CSV.
#'
#' @details
#'
#' @export
write_navigator_checks <- function(naomi_output,
                                   path) {


  key <- c("NaomiCheckPermPrimKey", "ART_is_Spectrum", "ANC_is_Spectrum",
           "Package_created", "Package_has_all_data", "Adv_recent_qtr",
           "Adv_future_proj_qtr", "Adv_area_ID_selected", "Adv_calendar_survey_match",
           "Adv_recent_survey_only", "Adv_ART_coverage", "Adv_ANC_data", "Adv_ART_data",
           "Adv_ART_attendance_yes", "Model_fit",           
           "Cal_PLHIV", "Cal_ART", "Cal_KOS", "Cal_new_infections", "Cal_method")


Naomi num on ART 2015-Pres is Spectrum national total on ART
Naomi num ANC clients 2015-Pres is Spectrum ANC testing cascade
Naomi package created
Naomi package has all data from upload or ADR
"Adv Options - Most recent calendar quarter selected"
Adv Options - Future projection quarter selected
Adv Options - National area_id selected
Adv Options - T1 calendar quarter matches one survey
Adv Options - Only most recent survey included (also TRUE for known special cases)
Adv Options - ART coverage included if availble 
Adv Options - ANC data included if avaialble
Adv Options - ART data included if available
Adv Options - ART attendance = "Yes"
Naomi output returned (model was fit)
Calibration - Spectrum PLHIV natl (subnatl if Spectrum subnatl)
Calibration - Spectrum ART natl (subnatl if Spectrum subnatl)
Calibration - Spectrum KOS natl (subnatl if Spectrum subnatl)
Calibration - Spectrum new infections natl (subnatl if Spectrum subnatl)
Calibration - method 
}
