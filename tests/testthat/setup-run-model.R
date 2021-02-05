
## A single set of valid model options and data, update once instead of copying
## for every test.

a_hintr_data <- list(
  pjnz = system_file("extdata/demo_mwi2019.PJNZ"),
  population = system_file("extdata/demo_population_agesex.csv"),
  shape = system_file("extdata/demo_areas.geojson"),
  survey = system_file("extdata/demo_survey_hiv_indicators.csv"),
  art_number = system_file("extdata/demo_art_number.csv"),
  anc_testing = system_file("extdata/demo_anc_testing.csv")
)

a_hintr_options <- list(
  area_scope = "MWI_1_2_demo",
  area_level = "4",
  calendar_quarter_t1 = "CY2016Q1",
  calendar_quarter_t2 = "CY2018Q3",
  calendar_quarter_t3 = "CY2019Q2",
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
  spectrum_population_calibration = "none",
  artattend = "true",
  artattend_t2 = "false",
  artattend_log_gamma_offset = -4L,
  output_aware_plhiv = "true",
  rng_seed = 17,
  no_of_samples = 20,
  max_iter = 250,
  permissive = "false",
  use_kish_prev = "true",
  deff_prev = 1.0,
  use_kish_artcov = "true",
  deff_artcov = 1.0,
  use_kish_recent = "true",
  deff_recent = 1.0,
  use_survey_aggregate = "false"
)

a_hintr_output <- hintr_run_model(a_hintr_data, a_hintr_options)

a_hintr_options_bad <- a_hintr_options
a_hintr_options_bad$calendar_quarter_t2 <- NULL

a_hintr_calibration_options <- list(
  spectrum_plhiv_calibration_level = "subnational",
  spectrum_plhiv_calibration_strat = "sex_age_group",
  spectrum_artnum_calibration_level = "national",
  spectrum_artnum_calibration_strat = "age_coarse",
  spectrum_aware_calibration_level = "national",
  spectrum_aware_calibration_strat = "age_coarse",
  spectrum_infections_calibration_level = "none",
  spectrum_infections_calibration_strat = "age_coarse",
  calibrate_method = "logistic"
)

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

naomi_evaluate_promise <- function (code, print = FALSE) {
  warnings <- testthat:::Stack$new()
  handle_warning <- function(condition) {
    warnings$push(condition)
    invokeRestart("muffleWarning")
  }
  messages <- testthat:::Stack$new()
  handle_message <- function(condition) {
    messages$push(condition)
    invokeRestart("muffleMessage")
  }
  progress <- testthat:::Stack$new()
  handle_progress <- function(condition) {
    progress$push(condition)
    invokeRestart("muffleProgress")
  }
  temp <- tempfile()
  result <- withr::with_output_sink(
    temp,
    withCallingHandlers(withVisible(code),
                        warning = handle_warning,
                        message = handle_message,
                        progress = handle_progress))
  if (result$visible && print) {
    withr::with_output_sink(temp, print(result$value))
  }
  output <- brio::read_file(temp)
  list(result = result$value,
       output = output,
       warnings = testthat:::get_messages(warnings$as_list()),
       messages = testthat:::get_messages(messages$as_list()),
       progress = progress$as_list())
}

MockProgress <- R6::R6Class(
  "MockProgress",
  inherit = Progress,
  cloneable = FALSE,
  public = list(
    ## Wrap print message in a with restarts so we can capture messages for
    ## testing
    print = function() {
      withRestarts({
        super$print()
      }, muffleProgress = function(...) NULL)
    }
  )
)

MockSimpleProgress <- R6::R6Class(
  "MockSimpleProgress",
  inherit = SimpleProgress,
  cloneable = FALSE,
  public = list(
    ## Wrap print message in a with restarts so we can capture messages for
    ## testing
    print = function() {
      withRestarts({
        super$print()
      }, muffleProgress = function(...) NULL)
    }
  )
)

expect_file_equivalent <- function(path_object, path_expected) {
  object_md5 <- tools::md5sum(path_object)
  expected_md5 <- tools::md5sum(path_expected)
  expect_equal(object_md5, expected_md5, check.attributes = FALSE)
}

expect_file_different <- function(path_object, path_expected) {
  object_md5 <- tools::md5sum(path_object)
  expected_md5 <- tools::md5sum(path_expected)
  expect_false(isTRUE(all.equal(object_md5, expected_md5,
                                check.attributes = FALSE)))
}
