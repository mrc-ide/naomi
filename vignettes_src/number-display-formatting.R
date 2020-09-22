#' ---
#' title: "Naomi Model Workflow Example"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Naomi Model Workflow Example}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---

##+ include = FALSE
knitr::opts_chunk$set(
                    collapse = TRUE,
                    comment = "#>"
                  )
unlink("outputs", recursive = TRUE)
#'
#'

##+ setup, message = FALSE
library(naomi)
library(dplyr)
library(tidyr)


#' ## Fit Naomi model
#'
#' Fit the model to Malawi testing data. For speed, ART programme data are omitted
#' from the model fitting.
#' 

##+ cache = FALSE

shape_file <- system.file("extdata/areas/area_merged.geojson", package = "naomi")
population_file <- system.file("extdata/population/population_agesex.csv", package = "naomi")
survey_file <- system.file("extdata/survey/survey_hiv_indicators.csv", package = "naomi")
art_file <- system.file("extdata/programme/art_number.csv", package = "naomi")
anc_file <- system.file("extdata/programme/anc_testing.csv", package = "naomi")
pjnz_file <- system.file("extdata/mwi2019.PJNZ", package = "naomi")

data <- list(shape = list(path = shape_file),
             population = list(path = population_file),
             survey = list(path = survey_file),
             art = list(path = art_file),
             anc = list(path = anc_file),
             pjnz = list(path = pjnz_file))

options <- list(
  area_scope = "MWI",
  area_level = "4",
  calendar_quarter_t1 = "CY2016Q1",
  calendar_quarter_t2 = "CY2019Q4",
  calendar_quarter_t3 = "CY2020Q3",
  survey_prevalence = c("MWI2015DHS", "MWI2016PHIA"),
  survey_art_coverage = "MWI2016PHIA",
  survey_recently_infected = NULL,
  include_art_t1 = "false",
  include_art_t2 = "false",
  anc_prevalence_year1 = 2016,
  anc_prevalence_year2 = 2019,
  anc_art_coverage_year1 = 2016,
  anc_art_coverage_year2 = 2019,
  spectrum_population_calibration = "national",
  spectrum_plhiv_calibration_level = "national",
  spectrum_plhiv_calibration_strat = "sex_age_coarse",
  spectrum_artnum_calibration_level = "national",
  spectrum_artnum_calibration_strat = "sex_age_coarse",
  spectrum_infections_calibration_level = "national",
  spectrum_infections_calibration_strat = "sex_age_coarse",
  artattend = "false",
  artattend_t2 = "false",
  artattend_log_gamma_offset = -4,
  rng_seed = 28,
  no_of_samples = 100,
  max_iter = 250,
  permissive = "true",
  outer_verbose = FALSE
)
  
             
naomi_data <- naomi_prepare_data(data, options)
tmb_inputs <- prepare_tmb_inputs(naomi_data)
fit <- fit_tmb(tmb_inputs, outer_verbose = options$outer_verbose)
fit <- sample_tmb(fit)

#' Generate output package
#' 
out <- output_package(fit, naomi_data)

#' ## Display a table
#' 
#' Display a table of 15-49 national 15-49 indicators
#' 

tab <- out$indicators %>%
  filter(age_group == "15-49",
         sex == "both",
         area_id == "MWI",
         calendar_quarter == "CY2019Q4")

#' The `meta_indicator` table contains columns _accuracy_, _scale_, _prefix_, and _suffix_ to define
#' formatting. These are modelled after the the `scales::number()` function.
#'
#' * `accuracy` determines the accuracy with which to display results, e.g. '0.01' for
#'    two decimal places. Accuracy is applied after any scaling.
#' * `scale` is scaling factor to multiply the value by, e.g. 100 to display a proportion as a percentage.
#' * `prefix` a string pasted to the front of the number, e.g. `$` for dollars currency.
#' * `suffix` a string pasted to the end of the number, e.g. `%` for a percentage.
#' 

select(out$meta_indicator, indicator, indicator_label, accuracy, scale, prefix, suffix) %>%
  knitr::kable(format.args = list(scientific = FALSE))

#' The function `display_number()` formats numerical values for display.
#'
#' ```
#' > display_number
#' function(x, accuracy = 0.0001, scale = 1, prefix = "", suffix = "") {
#'   ...
#'   val <- x * scale
#'   val <- round( x / accuracy ) * accuracy
#'   paste0(prefix, val, suffix)
#' }
#' ```
#'
#' Join the output table to the `meta_indicators` table identify default parameters
#' for each indicator and call `display_number()` to format.

tab <- tab %>%
  left_join(
    select(out$meta_indicator, indicator, indicator_label, accuracy, scale, prefix, suffix),
    by = "indicator"
  ) %>%
  mutate(mean_formatted = display_number(mean, accuracy, scale, prefix, suffix))

select(tab, indicator, indicator_label, mean, mean_formatted) %>%
  knitr::kable(format.args = list(scientific = FALSE), align = "llcc")



#' ## Questions
#'
#' * Do we want accuracy to be be number representing the displayed digits as in `scales::number()` (e.g. 0.01, 10, 100, 0.001), or the number of decimal places to display as in Excel / Spectrum (e.g. `4` would display 718.3571, `-1` would display 720).
#' * `scales::number()` also offers arguments for `big.mark` and `decimal.mark`. Are these needed?
#' * `scales::number()` uses a hueristic to automatically choose the `accuracy` if no argument is provided. Do we want this?
#' * Default settings per indicator may not scale well with different settings or stratifications (similar to the challenges we had with color scales last year). Do we want more granular defaults? My proposal is 'no', but leave open an option to add feature in future for the user to adjust the number of digits displayed.
#' 
