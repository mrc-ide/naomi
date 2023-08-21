#' Get data for hintr calibrate plot
#'
#' Takes hintr_output object, reads data and prepares data in format needed
#' for plotting calibrate barchart to compare calibrated, spectrum and
#' unadjusted estimates for a set of indicators.
#'
#' @param output A hintr_output object
#'
#' @return Calibrated, unadjusted and spectrum estimates of indicators
#' @export
hintr_calibrate_plot <- function(output) {

  assert_model_output_version(output, "2.5.7")
  calibration_path <- output$model_output_path
  calibration_data <- read_hintr_output(calibration_path)

  df <- calibration_data$output_package$fit$spectrum_calibration
  ## Could be NULL if called with uncalibrated model output
  if (is.null(df)) {
    stop(t_("INVALID_CALIBRATE_OBJECT"))
  }

  ## Only return indicators for T1, T2, T3
  cq_t1t2t3 <- sort(calibration_data$output_package$meta_period$calendar_quarter)[1:3]
  df <- dplyr::filter(df, calendar_quarter %in% cq_t1t2t3)

  dflong <- df %>%
    dplyr::mutate(population_denominator = population_calibrated) %>%
    tidyr:: pivot_longer(c(tidyselect::ends_with("raw"),
                           tidyselect::ends_with("spectrum"),
                           tidyselect::ends_with("calibrated")),
                         names_to = c("indicator", "data_type"),
                         names_pattern = "(.*)_(.*)")

  ## Add population_denominator to indicator for each data_type
  dflong <- dflong %>%
    tidyr::pivot_wider(names_from = indicator) %>%
    tidyr::pivot_longer(
      c(population_denominator, population, plhiv, art_current, unaware, infections),
      names_to = "indicator"
    )

  ## Rename indicators to match Naomi output indicators
  dflong$indicator <- dplyr::recode(dflong$indicator, "unaware" = "unaware_plhiv_num")

  ## Remove births_artpop, birth_artpop; not Naomi indicators
  ## (Maybe later add a comparison of these with ANC testing outputs)
  dflong <- dplyr::filter(dflong, !indicator %in% c("births_artpop", "births_hivpop"))

  ## Code copied from naomi_output_frame()

  regions <- unique(dflong$spectrum_region_code)
  age_groups <- unique(dflong$age_group)
  sexes <- unique(dflong$sex)

  region_out <- unique(df[c("spectrum_region_code", "spectrum_region_name")]) %>%
    dplyr::mutate(
      spectrum_region_code_out = spectrum_region_code,
      spectrum_region_name_out = spectrum_region_name,
      spectrum_region_name = NULL
    )

  if (nrow(region_out) > 1) {
    region_out <- region_out %>%
      dplyr::bind_rows(
        region_out %>%
          dplyr::mutate(spectrum_region_code_out = 0,
                        spectrum_region_name_out = "National")
      )
  }


  sex_out <- get_sex_out(sexes)

  sex_join <- data.frame(sex_out = c("male", "female", "both", "both", "both"),
                         sex = c("male", "female", "male", "female", "both"),
                         stringsAsFactors = FALSE) %>%
    dplyr::filter(sex %in% sexes, sex_out %in% !!sex_out)

  age_group_out <- get_age_group_out(age_groups)

  age_group_join <- get_age_groups() %>%
    dplyr::filter(age_group %in% age_group_out) %>%
    stats::setNames(paste0(names(.), "_out")) %>%
    tidyr::crossing(get_age_groups() %>%
                      dplyr::filter(age_group %in% age_groups)) %>%
    dplyr::filter(age_group_start_out <= age_group_start,
                  age_group_span_out == Inf |
                    (age_group_start + age_group_span) <=
                    (age_group_start_out + age_group_span_out)) %>%
    dplyr::select(age_group_out, age_group)

  dfexpand <- dflong %>%
    dplyr::inner_join(region_out, by = "spectrum_region_code", multiple = "all",
                      relationship = "many-to-many") %>%
    dplyr::inner_join(sex_join, by = "sex", multiple = "all",
                      relationship = "many-to-many") %>%
    dplyr::inner_join(age_group_join, by = "age_group", multiple = "all",
                      relationship = "many-to-many") %>%
    dplyr::count(spectrum_region_code = spectrum_region_code_out,
                 spectrum_region_name = spectrum_region_name_out,
                 sex = sex_out,
                 age_group = age_group_out,
                 calendar_quarter,
                 indicator,
                 data_type,
                 wt = value,
                 name = "value")

  ## Calculate rate indicators

  dfw <- dfexpand %>%
    tidyr::pivot_wider(names_from = indicator, values_from = value) %>%
    dplyr::mutate(
      prevalence = plhiv / dplyr::if_else(data_type == "spectrum", population, population_denominator),
      art_coverage = art_current / plhiv,
      aware_plhiv_prop = 1 - unaware_plhiv_num / plhiv,
      incidence = infections / (population - plhiv),
      population_denominator = NULL
    ) %>%
    tidyr::pivot_longer(cols = art_current:incidence, names_to = "indicator")

  ## Calculate calibration ratio
  val <- dfw %>%
    tidyr::pivot_wider(names_from = data_type, values_from = value) %>%
    dplyr::mutate(calibration_ratio = calibrated / raw) %>%
    tidyr::pivot_longer(cols = c(calibrated, raw, spectrum, calibration_ratio),
                        names_to = "data_type")

  val <- val %>%
    dplyr::select(data_type, spectrum_region_code, spectrum_region_name,
                  sex, age_group, calendar_quarter, indicator, mean = value)

  val
}

#' Get data for hintr comparison plot
#'
#' Takes hintr_output object, reads data and prepares data in format needed
#' for plotting comparison barchart to compare input and output data.
#'
#' @param output A hintr_output object
#'
#' @return Calibrated, unadjusted and spectrum estimates of indicators
#' @export
hintr_comparison_plot <- function(output) {
  assert_model_output_version(output, "2.7.1")
  output_path <- output$model_output_path
  output_data <- read_hintr_output(output_path)
  if (is.null(output_data$output_package$inputs_outputs)) {
    ## This can happen if a user has an old model output, then recalibrates
    ## it will update the version to latest but this output will not exist
    stop(t_("OLD_MODEL_OUTPUT"))
  }

  ## Only return indicators for T1, T2, T3 for plotting
  cq_t1t2t3 <- sort(output_data$output_package$meta_period$calendar_quarter)[1:3]
  plot_inputs_outputs <- output_data$output_package$inputs_outputs
  plot_inputs_outputs <- dplyr::filter(plot_inputs_outputs, calendar_quarter %in% cq_t1t2t3)

  plot_inputs_outputs
}
