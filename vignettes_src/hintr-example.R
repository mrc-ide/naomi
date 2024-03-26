#' ---
#' title: "Emulate a hintr model run"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{`hintr` example run}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---

##+ preamble, include = FALSE
knitr::opts_chunk$set(
                    collapse = TRUE,
                    comment = "#>"
                  )
unlink("outputs", recursive = TRUE)

#'
#'
#' This vignette provides example

##+ setup, message = FALSE

library(naomi)

##+ fit_model
hintr_data <- list(
  pjnz = system.file("extdata/demo-subnational-pjnz/demo_mwi2019_region-pjnz.zip", package = "naomi"),
  population = system.file("extdata/demo-subnational-pjnz/demo_population_zone.csv", package = "naomi"),
  shape = system.file("extdata/demo-subnational-pjnz/demo_areas_region-pjnz.geojson", package = "naomi"),
  survey = system.file("extdata/demo_survey_hiv_indicators.csv", package = "naomi"),
  art_number = system.file("extdata/demo-subnational-pjnz/demo_art_number_zone.csv", package = "naomi"),
  anc_testing = system.file("extdata/demo-subnational-pjnz/demo_anc_testing_zone.csv", package = "naomi")
)

hintr_options <- list(
  area_scope = "MWI",
  area_level = "2",
  calendar_quarter_t1 = "CY2016Q1",
  calendar_quarter_t2 = "CY2018Q4",
  calendar_quarter_t3 = "CY2019Q2",
  calendar_quarter_t4 = "CY2022Q3",
  calendar_quarter_t5 = "CY2023Q3",
  survey_prevalence = c("DEMO2016PHIA", "DEMO2015DHS"),
  survey_art_coverage = "DEMO2016PHIA",
  survey_recently_infected = "DEMO2016PHIA",
  include_art_t1 = "true",
  include_art_t2 = "true",
  anc_clients_year2 = 2018,
  anc_clients_year2_num_months = "9",
  anc_prevalence_year1 = 2016,
  anc_prevalence_year2 = 2018,
  anc_art_coverage_year1 = 2016,
  anc_art_coverage_year2 = 2018,
  spectrum_population_calibration = "national",
  artattend = "true",
  artattend_t2 = "false",
  artattend_log_gamma_offset = -4L,
  anchor_home_district = TRUE,
  output_aware_plhiv = "true",
  rng_seed = 17,
  no_of_samples = 20,
  max_iter = 250,
  use_kish_prev = "true",
  deff_prev = 1.0,
  use_kish_artcov = "true",
  deff_artcov = 1.0,
  use_kish_recent = "true",
  deff_recent = 1.0,
  use_survey_aggregate = "false",
  psnu_level = NULL
)

calibration_options <- list(
  spectrum_plhiv_calibration_level = "subnational",
  spectrum_plhiv_calibration_strat = "sex_age_group",
  spectrum_artnum_calibration_level = "subnational",
  spectrum_artnum_calibration_strat = "sex_age_coarse",
  spectrum_aware_calibration_level = "national",
  spectrum_aware_calibration_strat = "age_coarse",
  spectrum_infections_calibration_level = "none",
  spectrum_infections_calibration_strat = "age_coarse",
  calibrate_method = "logistic"
)

hintr_options$outer_verbose <- TRUE

hintr_paths <- hintr_run_model(hintr_data, hintr_options)
calibrated_paths <- hintr_calibrate(hintr_paths, calibration_options)
spectrum_download <- hintr_prepare_spectrum_download(calibrated_paths)

coarse_download <- hintr_prepare_coarse_age_group_download(calibrated_paths)

#' TO DO: add summary report download

#' Read output package and generate datapack export

##+ read_output
naomi_output <- read_output_package(spectrum_download$path)

datapack_path <- tempfile(fileext = ".csv")
write_datapack_csv(naomi_output, datapack_path)

navigator_path <- tempfile(fileext = ".csv")
write_navigator_checklist(naomi_output, navigator_path)
