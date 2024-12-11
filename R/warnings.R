naomi_warning <- function(text, locations) {
  ## If you want to add a new location here then let one of the developers know
  ## we will need to make sure the front end knows where to display the
  ## new location
  match_values(locations, c("review_inputs", "model_options", "model_fit",
                            "model_calibrate", "review_output",
                            "download_results"))
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
##' @param naomi_output Naomi output object
##' @param ind Naomi output indicator
##' @param threshold Numerical threshold to trigger warning
##' @param locations Location where warning should be displayed in front end
output_naomi_warning <- function(naomi_output, ind, threshold, locations) {


  val <- naomi_output$indicators$mean[naomi_output$indicators$indicator == ind]

  if(max(val) > threshold) {

    to_upper_first <- function(x) {
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }

    v <- naomi_output$indicators %>%
      dplyr::filter(indicator == ind, mean > threshold) %>%
      dplyr::left_join(naomi_output$meta_age_group, by = "age_group") %>%
      dplyr::left_join(naomi_output$meta_area %>% sf::st_drop_geometry(), by = "area_id") %>%
      dplyr::mutate(calendar_quarter = calendar_quarter_labels(calendar_quarter),
                    sex = to_upper_first(sex)) %>%
      dplyr::mutate(disag = paste(calendar_quarter, area_name, sex, age_group_label, sep = ", "))

    if (ind == "prevalence") {
      key <- "WARNING_OUTPUTS_PREV_EXCEEDS_THRESHOLD"
    } else if (ind == "art_coverage") {
      key <- "WARNING_OUTPUTS_ARTCOV_EXCEEDS_THRESHOLD"
    } else {
      stop("Invalid indicator, can only return warning for prevalence or art_coverage") ## or some generic warning here?
    }

    msg <- t_(key, list(rows = paste0(v$disag, collapse = "; ")))

    naomi_warning(msg, locations)
  }

}


##' Warning for aggregated subnational data input snot equal to spectrum totals
##'
##' Generate warning if aggregated subnational totals do not match spectrum totals
##'
##' @param naomi_spectrum_comparison Comparison table of aggregated subnational
##' Naomi and national Spectrum programme data created by
##' prepare_art_spectrum_comparison()
##'
##' @keywords internal
art_programme_data_warning <- function(art_naomi_spectrum_comparison) {

  df <- art_naomi_spectrum_comparison |>
    dplyr::group_by(year, indicator) |>
    dplyr::summarise(
      value_naomi = sum(value_naomi),
      value_spectrum_adjusted = sum(value_spectrum_adjusted), .groups = "drop") |>
    dplyr::mutate(total_diff = value_naomi - value_spectrum_adjusted) |>
    dplyr::filter(total_diff > 0) |>
    dplyr::group_by(indicator) |>
    dplyr::summarise(years = paste0(year, collapse = ";"), .groups = "drop") |>
    dplyr::mutate(text = paste(indicator, years, sep = ": "))


  if(nrow(df) > 0) {
    msg <- t_("WARNING_PROGRAMME_DATA_NOT_EQUAL_TO_SPECTRUM", list(years = paste(df$text, collapse = "\n")))
    naomi_warning(msg, c("model_calibrate", "review_output"))
  }

}

##' Warning for aggregated subnational data input snot equal to spectrum totals
##'
##' Generate warning if aggregated subnational totals do not match spectrum totals
##'
##' @param naomi_spectrum_comparison Comparison table of aggregated subnational
##' Naomi and national Spectrum programme data created by
##' prepare_art_spectrum_comparison()
##'
##' @keywords internal
anc_programme_data_warning <- function(anc_naomi_spectrum_comparison) {

  df <- anc_naomi_spectrum_comparison |>
    dplyr::group_by(year, indicator) |>
    dplyr::summarise(
      total_diff = sum(abs(difference)), .groups = "drop") |>
    dplyr::filter(total_diff > 0) |>
    dplyr::group_by(indicator) |>
    dplyr::summarise(years = paste0(year, collapse = ";"), .groups = "drop") |>
    dplyr::mutate(text = paste(indicator, years, sep = ": "))


  if(nrow(df) > 0) {
    msg <- t_("WARNING_PROGRAMME_DATA_NOT_EQUAL_TO_SPECTRUM", list(years = paste(df$text, collapse = "\n")))
    naomi_warning(msg, c("model_calibrate", "review_output"))
  }

}

##' Run validation for subnational programme data input
##'
##' This can throw validation errors or warnings which will be shown to user
##' in naomi web app
##'
##' @param naomi_spectrum_comparison Comparison table of aggregated subnational
##' Naomi and national Spectrum programme data created by
##' prepare_art_spectrum_comparison() or prepare_anc_spectrum_comparison()
##'
##' @export
hintr_validate_programme_data <- function(naomi_spectrum_comparison) {
  handle_naomi_warnings(programme_data_warning(naomi_spectrum_comparison))
}






