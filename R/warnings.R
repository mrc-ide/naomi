naomi_warning <- function(text, locations) {
  ## If you want to add a new location here then let one of the developers know
  ## we will need to make sure the front end knows where to display the
  ## new location
  match_values(locations, c("model_options", "model_fit", "model_calibrate",
                           "review_output", "download_results"))
  warn <- list(
    text = text,
    locations = locations
  )
  signalCondition(structure(
    warn,
    class = c("naomi_warning", "condition")))
}

handle_naomi_warnings <- function(expr) {
  naomi_warnings <- new.env(parent = emptyenv())
  naomi_warnings$warnings <- list()
  out <- withCallingHandlers(
    force(expr),
    naomi_warning = function(e) {
      naomi_warnings$warnings <- c(naomi_warnings$warnings, list(e))
    }
  )
  out$warnings <- naomi_warnings$warnings
  out
}

##' Generate naomi warning for specific strata of simulated outputs
##'
##'
##' @param naomi_output
##' @param ind Naomi output indicator
##' @param threshold Numerical threshold to trigger warning
##' @param locations Location where warning should be displayed in front end

output_naomi_warning <- function(naomi_output, ind, threshold, locations) {

  val <- naomi_output$indicators$mean[naomi_output$indicators$indicator == ind]

  if(max(val) > threshold) {

    v <- naomi_output$indicators %>%
      dplyr::filter(indicator == ind, mean > threshold) %>%
      dplyr::mutate(disag = paste(calendar_quarter, area_id, sex, age_group, sep = " "))

    if (ind == "prevalence") {
      key <- "WARNING_OUTPUTS_PREV_EXCEEDS_THRESHOLD"
    } else if (ind == "art_coverage") {
      key <- "WARNING_OUTPUTS_ARTCOV_EXCEEDS_THRESHOLD"
    } else {
      stop("Invalid indicator, can only return warning for prevalence or art_coverage") ## or some generic warning here?
    }

    msg <- t_(key, list(rows = paste0(v$disag, collapse = ", ")))

    naomi_warning(msg, locations)
  }

}




