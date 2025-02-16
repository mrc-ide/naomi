#' ---
#' title: "Emulate a hintr model run using zone-level demo data"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{`hintr` example run}
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
#' This vignette demonstrates a hintr model fit using alternate demonstration data:
#'
#' * Three subnational PJNZ files (northern / central / southern regions).
#' * Level 2 input data for five health zones.
#'
#' Fitting at five zones is for model testing data that fits quickly but represents
#' national epidemic.
#'

##+ setup, message = FALSE

library(naomi)

##+ fit model
hintr_data <- list(
  pjnz = system.file("extdata/demo-subnational-pjnz/demo_mwi2024_region-pjnz.zip", package = "naomi"),
  population = system.file("extdata/demo-subnational-pjnz/demo_population_zone.csv", package = "naomi"),
  shape = system.file("extdata/demo-subnational-pjnz/demo_areas_region-pjnz.geojson", package = "naomi"),
  survey = system.file("extdata/demo_survey_hiv_indicators.csv", package = "naomi"),
  art_number = system.file("extdata/demo-subnational-pjnz/demo_art_number_zone.csv", package = "naomi"),
  anc_testing = system.file("extdata/demo-subnational-pjnz/demo_anc_testing_zone.csv", package = "naomi")
)

hintr_options <- list(
  area_scope = "MWI",
  area_level = "2",
  calendar_quarter_t1 = "CY2020Q3",
  calendar_quarter_t2 = "CY2023Q4",
  calendar_quarter_t3 = "CY2024Q3",
  calendar_quarter_t4 = "CY2025Q3",
  calendar_quarter_t5 = "CY2026Q3",
  survey_prevalence = "DEMO2020PHIA",
  survey_art_coverage = "DEMO2020PHIA",
  survey_recently_infected = "DEMO2020PHIA",
  include_art_t1 = "true",
  include_art_t2 = "true",
  anc_clients_year2 = 2023,
  anc_clients_year2_num_months = "12",
  anc_prevalence_year1 = 2020,
  anc_prevalence_year2 = 2023,
  anc_art_coverage_year1 = 2020,
  anc_art_coverage_year2 = 2023,
  artattend = "true",
  artattend_t2 = "true",
  artattend_log_gamma_offset = -4L,
  spectrum_population_calibration = "subnational",
  output_aware_plhiv = "true",
  rng_seed = 17,
  no_of_samples = 20,
  max_iter = 250
)

calibration_options <- list(
  spectrum_plhiv_calibration_level = "subnational",
  spectrum_plhiv_calibration_strat = "sex_age_coarse",
  spectrum_artnum_calibration_level = "subnational",
  spectrum_artnum_calibration_strat = "sex_age_coarse",
  spectrum_aware_calibration_level = "subnational",
  spectrum_aware_calibration_strat = "sex_age_coarse",
  spectrum_infections_calibration_level = "subnational",
  spectrum_infections_calibration_strat = "sex_age_coarse",
  calibrate_method = "logistic"
)

hintr_options$outer_verbose <- TRUE

hintr_paths <- hintr_run_model(hintr_data, hintr_options)
calibrated_paths <- hintr_calibrate(hintr_paths, calibration_options)
spectrum_download <- hintr_prepare_spectrum_download(calibrated_paths)

calibrate_plot_data <- hintr_calibrate_plot(calibrated_paths)
comparison_plot_data <- hintr_comparison_plot(calibrated_paths)

#' Read output package and generate datapack export
naomi_output <- read_output_package(spectrum_download$path)

datapack_path <- tempfile(fileext = ".csv")
write_datapack_csv(naomi_output, datapack_path)


#' # Step-by-step workflow

area_merged <- read_area_merged(hintr_data$shape)
pop_agesex <- read_population(hintr_data$population)
spec <- extract_pjnz_naomi(hintr_data$pjnz)
survey_hiv_indicators <- read_survey_indicators(hintr_data$survey)
anc_testing <- read_anc_testing(hintr_data$anc_testing)
art_number <- read_art_number(hintr_data$art_number)

naomi_mf <- naomi_model_frame(area_merged,
                              pop_agesex,
                              spec,
                              scope = hintr_options$area_scope,
                              level = hintr_options$area_level,
                              calendar_quarter1 = hintr_options$calendar_quarter_t1,
                              calendar_quarter2 = hintr_options$calendar_quarter_t2,
                              calendar_quarter3 = hintr_options$calendar_quarter_t3,
                              calendar_quarter4 = hintr_options$calendar_quarter_t4,
                              calendar_quarter5 = hintr_options$calendar_quarter_t5,
                              adjust_area_growth = TRUE)


#' Prepare data inputs

naomi_data <- select_naomi_data(naomi_mf = naomi_mf,
                                survey_hiv_indicators = survey_hiv_indicators,
                                anc_testing = anc_testing,
                                art_number = art_number,
                                prev_survey_ids = hintr_options$survey_prevalence,
                                artcov_survey_ids = hintr_options$survey_art_coverage,
                                recent_survey_ids = hintr_options$survey_recently_infected,
                                vls_survey_ids = NULL,
                                artnum_calendar_quarter_t1 = hintr_options$calendar_quarter_t1,
                                artnum_calendar_quarter_t2 = hintr_options$calendar_quarter_t2,
                                anc_clients_year_t2 = hintr_options$anc_clients_year2,
                                anc_clients_year_t2_num_months = as.numeric(hintr_options$anc_clients_year2_num_months),
                                anc_prev_year_t1 = hintr_options$anc_prevalence_year1,
                                anc_prev_year_t2 = hintr_options$anc_prevalence_year2,
                                anc_artcov_year_t1 = hintr_options$anc_art_coverage_year1,
                                anc_artcov_year_t2 = hintr_options$anc_art_coverage_year2)


#' 5. Fit model
tmb_inputs <- prepare_tmb_inputs(naomi_data)
fit <- fit_tmb(tmb_inputs)
fit <- sample_tmb(fit, 100)

outputs <- output_package(fit, naomi_data)

outputs_calib <- calibrate_outputs(outputs, naomi_mf,
                                   spectrum_plhiv_calibration_level = "national",
                                   spectrum_plhiv_calibration_strat = "sex_age_coarse",
                                   spectrum_artnum_calibration_level = "national",
                                   spectrum_artnum_calibration_strat = "sex_age_coarse",
                                   spectrum_aware_calibration_level = "national",
                                   spectrum_aware_calibration_strat = "sex_age_coarse",
                                   spectrum_infections_calibration_level = "national",
                                   spectrum_infections_calibration_strat = "sex_age_coarse")

outputs_calib <- disaggregate_0to4_outputs(outputs_calib, naomi_mf)


d <- tmb_inputs$data
par_val <- fit$par.full %>% split(names(.))
p <- tmb_inputs$par_init
p[names(par_val)] <- par_val


v <- naomi_objective_function_r(d, p)

expect_setequal(names(v$report), names(fit$mode))
expect_equal(v$report, fit$mode[names(v$report)])

expect_equal(v$val, fit$objective)
