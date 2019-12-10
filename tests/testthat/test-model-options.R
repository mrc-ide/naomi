context("model-options")

test_that("can get valid model run options template", {
  options <- get_model_options_template(TRUE, TRUE)
  expect_length(options, 5)
  expect_equal(names(options), c("general", "survey", "anc", "art", "advanced"))
  expect_true(any(grepl("General", options$general)))
  expect_true(any(grepl("<\\+area_scope_options\\+>", options$general)))
  expect_true(any(grepl("<\\+area_level_options\\+>", options$general)))
  expect_true(any(grepl("<\\+calendar_quarter_t2_options\\+>", options$general)))

  expect_true(any(grepl("Survey", options$survey)))
  expect_true(any(grepl("<\\+calendar_quarter_t1_options\\+>", options$survey)))
  expect_true(any(grepl("<\\+survey_prevalence_options\\+>", options$survey)))
  expect_true(any(grepl("<\\+survey_art_coverage_options\\+>", options$survey)))
  expect_true(any(grepl("<\\+survey_recently_infected_options\\+>", options$survey)))

  expect_true(any(grepl("ART", options$art)))
  expect_true(any(grepl("ANC", options$anc)))
  expect_true(any(grepl("<\\+anc_prevalence_year1_options\\+>", options$anc)))
  expect_true(any(grepl("<\\+anc_prevalence_year2_options\\+>", options$anc)))
  expect_true(any(grepl("<\\+anc_art_coverage_year1_options\\+>", options$anc)))
  expect_true(any(grepl("<\\+anc_art_coverage_year2_options\\+>", options$anc)))

  expect_true(any(grepl("Advanced", options$advanced)))
})

test_that("art and anc data can be omitted from model run options", {
  options <- get_model_options_template(FALSE, FALSE)
  expect_length(options, 3)
  expect_equal(names(options), c("general", "survey", "advanced"))
  expect_true(any(grepl("General", options$general)))
  expect_true(any(grepl("<\\+area_scope_options\\+>", options$general)))
  expect_true(any(grepl("<\\+area_level_options\\+>", options$general)))
  expect_true(any(grepl("<\\+calendar_quarter_t2_options\\+>", options$general)))

  expect_true(any(grepl("Survey", options$survey)))
  expect_true(any(grepl("<\\+calendar_quarter_t1_options\\+>", options$survey)))
  expect_true(any(grepl("<\\+survey_prevalence_options\\+>", options$survey)))
  expect_true(any(grepl("<\\+survey_art_coverage_options\\+>", options$survey)))
  expect_true(any(grepl("<\\+survey_recently_infected_options\\+>", options$survey)))

  expect_false(any(grepl("ART", options$art)))
  expect_false(any(grepl("ANC", options$anc)))

  expect_true(any(grepl("Advanced", options$advanced)))
})

test_that("validate model options returns true", {
  expect_true(validate_model_options(a_hintr_data, a_hintr_options))
})

test_that("validate model options returns error for invalid", {
  expect_error(validate_model_options(a_hintr_data, a_hintr_options_bad))

  options <- a_hintr_options
  options$artattend <- "true"
  options$include_art_t1 <- "false"
  options$include_art_t2 <- "false"
  expect_error(validate_model_options(a_hintr_data, options),
               "ART attendance model can only be estimated if ART programme data are used.")

})

test_that("validate_model_options() handles NULL include_art_tX", {

  data <- a_hintr_data

  options <- a_hintr_options
  options$include_art_t1 <- NULL
  options$include_art_t2 <- NULL

  expect_error(validate_model_options(data, options),
               "ART attendance model can only be estimated if ART programme data are used")

  options$artattend <- "false"
  expect_true(validate_model_options(data, options))
})

test_that("validation error for invalid area selection", {
  options <- a_hintr_options
  options$area_scope <- "MWI"
  options$area_level <- 0

  expect_error(hintr_run_model(a_hintr_data, options),
               "Cannot fit model at country level. Choose a different level.")
})

test_that("error message translation", {
  options <- a_hintr_options
  options$artattend <- "true"
  options$include_art_t1 <- "false"
  options$include_art_t2 <- "false"

  err_en <- "ART attendance model can only be estimated if ART programme data are used."
  err_fr <- "Le modèle de participation aux TAR ne peut être estimé si les données du programme de TAR sont utilisées"

  expect_error(validate_model_options(a_hintr_data, options),
               err_en)
  reset <- naomi_set_language("fr")
  on.exit(reset())
  expect_error(validate_model_options(a_hintr_data, options),
               err_fr)
  reset()
  expect_error(validate_model_options(a_hintr_data, options),
               err_en)
})
