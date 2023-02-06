
## A single set of valid model options and data, update once instead of copying
## for every test.

a_hintr_data <- list(
  pjnz = system_file("extdata/demo-subnational-pjnz/demo_mwi2019_region-pjnz.zip"),
  population = system_file("extdata/demo-subnational-pjnz/demo_population_zone.csv"),
  shape = system_file("extdata/demo-subnational-pjnz/demo_areas_region-pjnz.geojson"),
  survey = system_file("extdata/demo_survey_hiv_indicators.csv"),
  art_number = system_file("extdata/demo-subnational-pjnz/demo_art_number_zone.csv"),
  anc_testing = system_file("extdata/demo-subnational-pjnz/demo_anc_testing_zone.csv")
)

a_hintr_options <- list(
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

a_hintr_output <- hintr_run_model(a_hintr_data, a_hintr_options)

a_hintr_options_bad <- a_hintr_options
a_hintr_options_bad$calendar_quarter_t2 <- NULL

a_hintr_calibration_options <- list(
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

a_hintr_output_calibrated <- hintr_calibrate(a_hintr_output,
                                             a_hintr_calibration_options)

## Use fit.RDS if it exists locally, otherwise just use the actual functions
## fit.RDS not on git because it is pretty massive ~ 220MB
if (file.exists("testdata/fit.RDS")) {
  model_output <- readRDS("testdata/fit.RDS")
  fit <- mockery::mock(model_output, cycle = TRUE)
  sample <- mockery::mock(model_output, cycle = TRUE)
} else {
  fit <- fit_tmb
  sample <- sample_tmb
}
