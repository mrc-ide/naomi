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
  err_fr <- "Le modèle d’assiduité à l’ART peut uniquement être estimé si les données du programme ART sont utilisées."

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

test_that("check for required model options", {
  options <- list(
    area_scope = "MWI",
    area_level = "4"
  )
  expect_error(validate_model_options(a_hintr_data, options),
               "Required model options not supplied: calendar_quarter_t1, calendar_quarter_t2, calendar_quarter_t3, survey_prevalence")
})

test_that("model options template can be translated", {
  reset <- naomi_set_language("fr")
  on.exit(reset())

  options <- get_model_options_template(TRUE, TRUE)
  expect_true(any(grepl("Généralités", options$general)))
  expect_false(any(grepl("General", options$general)))
  expect_true(any(grepl("Sélectionnez les options générales du modèle :", options$general)))
  expect_false(any(grepl("Select general model options:", options$general)))

  reset()
  options <- get_model_options_template(TRUE, TRUE)
  expect_false(any(grepl("Généralités", options$general)))
  expect_true(any(grepl("General", options$general)))
  expect_false(any(grepl("Sélectionnez les options générales du modèle :", options$general)))
  expect_true(any(grepl("Select general model options:", options$general)))
})

test_that("artattend_t2 option produces model frames", {

  options_TRUE <- a_hintr_options
  options_TRUE$artattend_t2 <- "true"
  data_TRUE <- naomi_prepare_data(a_hintr_data, options_TRUE)
  inputs_TRUE <- prepare_tmb_inputs(data_TRUE)

  options_NULL <- a_hintr_options
  options_NULL$artattend_t2 <- NULL
  data_NULL <- naomi_prepare_data(a_hintr_data, options_NULL)
  inputs_NULL <- prepare_tmb_inputs(data_NULL)

  options_FALSE <- a_hintr_options
  options_FALSE$artattend_t2 <- "false"
  data_FALSE <- naomi_prepare_data(a_hintr_data, options_FALSE)
  inputs_FALSE <- prepare_tmb_inputs(data_FALSE)

  n_attend_nb <- nrow(data_TRUE$mf_artattend)
  n_areas <- nrow(data_TRUE$mf_areas)

  expect_equal(ncol(inputs_TRUE$data$Xgamma_t2), n_areas)
  expect_equal(nrow(inputs_TRUE$data$Xgamma_t2), n_attend_nb)
  expect_equal(inputs_TRUE$par_init$log_or_gamma_t1t2, numeric(n_areas))

  expect_equal(ncol(inputs_FALSE$data$Xgamma_t2), 0)
  expect_equal(nrow(inputs_FALSE$data$Xgamma_t2), n_attend_nb)
  expect_equal(inputs_FALSE$par_init$log_or_gamma_t1t2, numeric(0))

  expect_equal(ncol(inputs_NULL$data$Xgamma_t2), 0)
  expect_equal(nrow(inputs_NULL$data$Xgamma_t2), n_attend_nb)
  expect_equal(inputs_NULL$par_init$log_or_gamma_t1t2, numeric(0))

})
