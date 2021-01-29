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

  expect_false(any(grepl("ART", options$art)))
  expect_false(any(grepl("ANC", options$anc)))

  expect_true(any(grepl("Advanced", options$advanced)))
})

test_that("validate model options returns true", {
  expect_true(
    validate_model_options(format_data_input(a_hintr_data), a_hintr_options))
})

test_that("validate model options returns error for invalid", {
  expect_error(
    validate_model_options(format_data_input(a_hintr_data), a_hintr_options_bad))

  options <- a_hintr_options
  options$artattend <- "true"
  options$include_art_t1 <- "false"
  options$include_art_t2 <- "false"
  expect_error(validate_model_options(format_data_input(a_hintr_data), options),
               "ART attendance model can only be estimated if ART programme data are used.")

})

test_that("validate_model_options() handles NULL include_art_tX", {

  data <- a_hintr_data

  options <- a_hintr_options
  options$include_art_t1 <- NULL
  options$include_art_t2 <- NULL

  expect_error(validate_model_options(format_data_input(data), options),
               "ART attendance model can only be estimated if ART programme data are used")

  options$artattend <- "false"
  expect_true(validate_model_options(format_data_input(data), options))
})

test_that("validation error for invalid area selection", {
  options <- a_hintr_options
  options$area_scope <- "MWI"
  options$area_level <- 0

  expect_error(hintr_run_model(format_data_input(a_hintr_data), options),
               "Cannot fit model at country level. Choose a different level.")
})

test_that("error message translation", {
  options <- a_hintr_options
  options$artattend <- "true"
  options$include_art_t1 <- "false"
  options$include_art_t2 <- "false"

  err_en <- "ART attendance model can only be estimated if ART programme data are used."
  err_fr <- "Le modèle d’assiduité à l’ART peut uniquement être estimé si les données du programme ART sont utilisées."

  expect_error(validate_model_options(format_data_input(a_hintr_data), options),
               err_en)
  reset <- naomi_set_language("fr")
  on.exit(reset())
  expect_error(validate_model_options(format_data_input(a_hintr_data), options),
               err_fr)
  reset()
  expect_error(validate_model_options(format_data_input(a_hintr_data), options),
               err_en)
})

test_that("check for required model options", {
  options <- list(
    area_scope = "MWI",
    area_level = "4"
  )
  expect_error(validate_model_options(format_data_input(a_hintr_data), options),
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

  input_data <- format_data_input(a_hintr_data)
  options_TRUE <- a_hintr_options
  options_TRUE$artattend_t2 <- "true"

  options_TRUE <- format_options(options_TRUE)
  data_TRUE <- naomi_prepare_data(input_data, options_TRUE)

  inputs_TRUE <- prepare_tmb_inputs(data_TRUE)

  options_NULL <- a_hintr_options
  options_NULL$artattend_t2 <- NULL
  options_NULL <- format_options(options_NULL)
  data_NULL <- naomi_prepare_data(input_data, options_NULL)
  inputs_NULL <- prepare_tmb_inputs(data_NULL)

  options_FALSE <- a_hintr_options
  options_FALSE$artattend_t2 <- "false"
  options_FALSE <- format_options(options_FALSE)
  data_FALSE <- naomi_prepare_data(input_data, options_FALSE)
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

test_that("validation check for spectrum region code returns error", {

  expect_true(
    validate_model_options(format_data_input(a_hintr_data), a_hintr_options))

  areas_na_spectrum_region_code <- read_area_merged(a_hintr_data$shape)
  areas_na_spectrum_region_code$spectrum_region_code <- NA_integer_
  tmp_areas_na_spectrum_region_code <- tempfile(fileext = ".geojson")
  sf::write_sf(areas_na_spectrum_region_code, tmp_areas_na_spectrum_region_code)

  data_na_spectrum_region_code <- a_hintr_data
  data_na_spectrum_region_code$shape <- tmp_areas_na_spectrum_region_code

  expect_error(validate_model_options(
    format_data_input(data_na_spectrum_region_code), a_hintr_options),
               "Spectrum region code column is all missing in your shape file. Update the shape file with Spectrum region code for the estimation level.")

  areas_bad_spectrum_region_code <- read_area_merged(a_hintr_data$shape)
  areas_bad_spectrum_region_code$spectrum_region_code[nrow(areas_bad_spectrum_region_code)] <- 1L
  tmp_areas_bad_spectrum_region_code <- tempfile(fileext = ".geojson")
  sf::write_sf(areas_bad_spectrum_region_code, tmp_areas_bad_spectrum_region_code)

  data_bad_spectrum_region_code <- a_hintr_data
  data_bad_spectrum_region_code$shape <- tmp_areas_bad_spectrum_region_code

  expect_error(validate_model_options(
    format_data_input(data_bad_spectrum_region_code), a_hintr_options),
               "Some spectrum region code in your shape file are not in PJNZ extracts. Please update your shape file to include the correct codes")

})

test_that("can get model calibration options", {
  options <- get_model_calibration_options()
  expect_length(options, 1)
  expect_true(any(grepl("Calibration options", options)))
})

test_that("can get model calibration options label from ID", {
  options <- list(spectrum_population_calibration_level = "subnational",
                  spectrum_plhiv_calibration_strat = "sex_age_group" ,
                  spectrum_artnum_calibration_level = "none",
                  spectrum_artnum_calibration_strat = "age_coarse",
                  spectrum_aware_calibration_level = "none",
                  spectrum_aware_calibration_strat = "age_coarse",
                  spectrum_infections_calibration_level = "none",
                  spectrum_infections_calibration_strat ="age_coarse",
                  calibrate_method = "logistic")
  options_map <- get_calibration_option_labels(options)

  expect_length(options_map, length(options))
  expect_equal(options_map, list(
    spectrum_population_calibration_level = "Subnational",
    spectrum_plhiv_calibration_strat = "Sex and 5-year age group",
    spectrum_artnum_calibration_level = "None",
    spectrum_artnum_calibration_strat = "Age <15 / 15+",
    spectrum_aware_calibration_level = "None",
    spectrum_aware_calibration_strat = "Age <15 / 15+",
    spectrum_infections_calibration_level = "None",
    spectrum_infections_calibration_strat ="Age <15 / 15+",
    calibrate_method = "Logistic"))
})

test_that("validate_model_options() returns error if missing .shiny90", {

  temp_pjnz <- tempfile(fileext = ".pjnz")
  file.copy(a_hintr_data$pjnz, temp_pjnz)
  utils::zip(temp_pjnz, "malawi.zip.shiny90", flags="-d", extras = "-q")
  expect_false(assert_pjnz_shiny90(temp_pjnz))

  bad_data <- a_hintr_data
  bad_data$pjnz <- temp_pjnz
  bad_data <- format_data_input(bad_data)

  expect_error(validate_model_options(bad_data, a_hintr_options),
               "^\\.shiny90 file is not present for the following projection\\(s\\)")

  opts <- a_hintr_options
  opts$output_aware_plhiv <- "false"
  expect_true(validate_model_options(bad_data, opts))
})

test_that("use_survey_aggregate option affects selected data", {

  a_hintr_options <- format_options(a_hintr_options)
  options_aggregate <- a_hintr_options
  options_aggregate$use_survey_aggregate <- "true"

  aggregate_survey <- dplyr::filter(demo_survey_hiv_indicators,
                                    age_group %in% c("Y000_014", "Y015_049"),
                                    sex == "both",
                                    area_id == "MWI_1_2_demo")

  aggregate_survey_file <- tempfile(fileext = ".csv")
  write.csv(aggregate_survey, aggregate_survey_file, row.names = FALSE)

  input_data <- format_data_input(a_hintr_data)

  input_data_aggregate <- input_data
  input_data_aggregate$survey$path <- aggregate_survey_file
  
  expect_error(
    naomi_prepare_data(input_data, options_aggregate),
    "Aggregate survey data selected. Stratifications included in dataset which are not in model scope for indicator prevalence"
  )

  naomi_data_aggregate <- naomi_prepare_data(input_data_aggregate, options_aggregate)
  expect_equal(nrow(naomi_data_aggregate$prev_dat), 3)
  expect_equal(nrow(naomi_data_aggregate$artcov_dat), 2)
  expect_equal(nrow(naomi_data_aggregate$recent_dat), 1)
 
  ## Aggregate data with standard model options -- returns no data and an error.
  expect_error(
    naomi_prepare_data(input_data_aggregate, a_hintr_options),
    "No prevalence survey data found for survey: DEMO2016PHIA, DEMO2015DHS. Prevalence data are required for Naomi. Check your selections."
  )
               
})
