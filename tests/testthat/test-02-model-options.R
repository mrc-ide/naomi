test_that("validate model options returns true", {
  out <- validate_model_options(format_data_input(a_hintr_data),
                                a_hintr_options)
  expect_true(out$valid)
  expect_equal(out$warnings, list())
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
  expect_true(validate_model_options(format_data_input(data), options)$valid)
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
  err_fr <- "Le modèle d’assiduité de TARV peut uniquement être estimé si les données du programme TARV sont utilisées."

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
    validate_model_options(format_data_input(a_hintr_data),
                           a_hintr_options)$valid)

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

test_that("validate_model_options() returns error if missing .shiny90", {

  temp_pjnz <- tempfile(fileext = ".pjnz")
  file.copy(system_file("extdata/demo_mwi2019.PJNZ"), temp_pjnz)
  utils::zip(temp_pjnz, "malawi.zip.shiny90", flags="-d", extras = "-q")
  expect_false(assert_pjnz_shiny90(temp_pjnz))

  bad_data <- a_hintr_data
  bad_data$pjnz <- temp_pjnz
  bad_data$shape <- system_file("extdata/demo_areas.geojson")
  bad_data <- format_data_input(bad_data)

  expect_error(validate_model_options(bad_data, a_hintr_options),
               "^\\.shiny90 file is not present for the following projection\\(s\\)")

  opts <- a_hintr_options
  opts$output_aware_plhiv <- "false"
  expect_true(validate_model_options(bad_data, opts)$valid)
})

test_that("use_survey_aggregate option affects selected data", {

  a_hintr_options <- format_options(a_hintr_options)
  options_aggregate <- a_hintr_options
  options_aggregate$use_survey_aggregate <- "true"
  options_aggregate$survey_prevalence <- c("DEMO2016PHIA", "DEMO2015DHS")
  options_aggregate$survey_art_coverage <- "DEMO2016PHIA"
  options_aggregate$survey_recently_infected <- "DEMO2016PHIA"

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
    "No prevalence survey data found for survey: DEMO2020PHIA. Prevalence data are required for Naomi. Check your selections."
  )

})

test_that("Option adjust_area_growth affects projection matrices", {

  a_hintr_data <- format_data_input(a_hintr_data)

  a_hintr_options <- format_options(a_hintr_options)
  options_null <- a_hintr_options
  options_null$adjust_area_growth <- NULL

  naomi_data_null <- naomi_prepare_data(a_hintr_data, options_null)

  ## If adjust_area_growth not provided, defaults to FALSE -> no net growth factors
  expect_true(all(naomi_data_null$Lproj_t1t2$Lproj_netgrow == 0 |
                  naomi_data_null$Lproj_t1t2$Lproj_netgrow == 1))
  expect_true(all(naomi_data_null$Lproj_t2t3$Lproj_netgrow == 0 |
                  naomi_data_null$Lproj_t2t3$Lproj_netgrow == 1))

  options_TRUE <- a_hintr_options
  options_TRUE$adjust_area_growth <- TRUE
  naomi_data_TRUE <- naomi_prepare_data(a_hintr_data, options_TRUE)

  ## If adjust_area_growth = TRUE, non-zero growth factors
  expect_true(all(diag(as.matrix(naomi_data_TRUE$Lproj_t1t2$Lproj_netgrow)) != 1))
  expect_true(all(diag(as.matrix(naomi_data_TRUE$Lproj_t2t3$Lproj_netgrow)) != 1))

  options_FALSE <- a_hintr_options
  options_FALSE$adjust_area_growth <- FALSE
  naomi_data_FALSE <- naomi_prepare_data(a_hintr_data, options_FALSE)

  ## If adjust_area_growth = FALSE, non-zero growth factors
  expect_true(all(diag(as.matrix(naomi_data_FALSE$Lproj_t1t2$Lproj_netgrow)) == 1))
  expect_true(all(diag(as.matrix(naomi_data_FALSE$Lproj_t2t3$Lproj_netgrow)) == 1))

  expect_equal(naomi_data_null$Lproj_t1t2$Lproj_hivpop, naomi_data_FALSE$Lproj_t1t2$Lproj_hivpop)
  expect_equal(naomi_data_null$Lproj_t2t3$Lproj_hivpop, naomi_data_FALSE$Lproj_t2t3$Lproj_hivpop)
  expect_equal(naomi_data_null$Lproj_t1t2$Lproj_paed, naomi_data_FALSE$Lproj_t1t2$Lproj_paed)
  expect_equal(naomi_data_null$Lproj_t2t3$Lproj_paed, naomi_data_FALSE$Lproj_t2t3$Lproj_paed)
  expect_equal(naomi_data_null$Lproj_t1t2$Lproj_incid, naomi_data_FALSE$Lproj_t1t2$Lproj_incid)
  expect_equal(naomi_data_null$Lproj_t2t3$Lproj_incid, naomi_data_FALSE$Lproj_t2t3$Lproj_incid)

  skip("Net growth ratio effect not implemented in v2.6.0; target for v2.6.1")

  ## adjust_area_growth TRUE vs. FALSE affects projections for
  ## hivpop, paediatric, and infections

  expect_false(all(naomi_data_TRUE$Lproj_t1t2$Lproj_hivpop == naomi_data_FALSE$Lproj_t1t2$Lproj_hivpop))
  expect_false(all(naomi_data_TRUE$Lproj_t2t3$Lproj_hivpop == naomi_data_FALSE$Lproj_t2t3$Lproj_hivpop))
  expect_false(all(naomi_data_TRUE$Lproj_t1t2$Lproj_paed == naomi_data_FALSE$Lproj_t1t2$Lproj_paed))
  expect_false(all(naomi_data_TRUE$Lproj_t2t3$Lproj_paed == naomi_data_FALSE$Lproj_t2t3$Lproj_paed))
  expect_false(all(naomi_data_TRUE$Lproj_t1t2$Lproj_incid == naomi_data_FALSE$Lproj_t1t2$Lproj_incid))
  expect_false(all(naomi_data_TRUE$Lproj_t2t3$Lproj_incid == naomi_data_FALSE$Lproj_t2t3$Lproj_incid))
})


test_that("Option adjust_area_growth handles cases with projection_dur >5 years", {

  a_hintr_data <- format_data_input(a_hintr_data)

  a_hintr_options <- format_options(a_hintr_options)
  options_longdur <- a_hintr_options
  options_longdur$adjust_area_growth <- TRUE
  options_longdur$calendar_quarter_t2 = "CY2023Q4"
  options_longdur$calendar_quarter_t3 = "CY2024Q2"
  options_longdur$calendar_quarter_t4 = "CY2024Q3"
  options_longdur$calendar_quarter_t5 = "CY2024Q4"
  options_longdur$include_art_t2 <- "false"
  options_longdur$artattend_t2 <- "false"

  naomi_data_longdur <- naomi_prepare_data(a_hintr_data, options_longdur)

  expect_equal(sum(is.na(naomi_data_longdur$Lproj_t1t2$Lproj_netgrow)), 0)
  expect_equal(sum(is.na(naomi_data_longdur$Lproj_t1t2$Lproj_hivpop)), 0)
  expect_equal(sum(is.na(naomi_data_longdur$Lproj_t1t2$Lproj_paed)), 0)
  expect_equal(sum(is.na(naomi_data_longdur$Lproj_t1t2$Lproj_incid)), 0)
})

test_that("Handle backwards regression when T4 and T5 options are missing", {

  a_hintr_data <- format_data_input(a_hintr_data)
  a_hintr_options <- format_options(a_hintr_options)

  options_old <- a_hintr_options

  # Check that T4 and T5 are 24- and 36-months after T3 when options are not specificed
  options_old$calendar_quarter_t4 <- NULL
  options_old$calendar_quarter_t5 <- NULL
  naomi_data <- naomi_prepare_data(a_hintr_data, options_old)

  t3 <- calendar_quarter_to_quarter_id(naomi_data$model_options$calendar_quarter_t3)
  t4 <- calendar_quarter_to_quarter_id(naomi_data$model_options$calendar_quarter_t4)
  t5 <- calendar_quarter_to_quarter_id(naomi_data$model_options$calendar_quarter_t5)

  expect_equal(t4 - t3, 6)
  expect_equal(t5 - t3, 9)

})

test_that("Population data available for area level set in model options", {

  expect_error(naomi_model_frame(a_area_merged,
                             demo_population_agesex,
                             a_spec,
                             scope = "MWI_1_1_demo",
                             level = 3,
                             calendar_quarter1 = "CY2016Q1",
                             calendar_quarter2 = "CY2018Q4",
                             calendar_quarter3 = "CY2019Q2",
                             calendar_quarter4 = "CY2022Q3",
                             calendar_quarter5 = "CY2023Q3",
                             artattend = FALSE,
                             spectrum_population_calibration = "none",
                             psnu_level = NULL),
               paste("Unable to generate model estimates at the District level because",
                     "population data only available at the District + Metro level/s.",
                     "Please review model options or population data inputs."))


})
