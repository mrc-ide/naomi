test_that("R implementation matches C++", {

  ## TODO: Replace the test in setup-model-frame.R with this

  b_area_merged <- read_area_merged(a_hintr_data$shape)
  b_spec <- extract_pjnz_naomi(a_hintr_data$pjnz)
  b_pop_agesex <- read_population(a_hintr_data$population)

  b_survey <- read_survey_indicators(a_hintr_data$survey)
  b_art_number <- read_art_number(a_hintr_data$art_number)
  b_anc_testing <- read_anc_testing(a_hintr_data$anc_testing)

  b_naomi_mf <- naomi_model_frame(b_area_merged,
                                  b_pop_agesex,
                                  b_spec,
                                  scope = "MWI",
                                  level = 2,
                                  calendar_quarter1 = "CY2016Q1",
                                  calendar_quarter2 = "CY2018Q4",
                                  calendar_quarter3 = "CY2019Q2",
                                  calendar_quarter4 = "CY2022Q3",
                                  calendar_quarter5 = "CY2023Q3",
                                  artattend = TRUE,
                                  spectrum_population_calibration = "subnational")

  b_naomi_data <- select_naomi_data(b_naomi_mf,
                                    b_survey,
                                    b_anc_testing,
                                    b_art_number,
                                    prev_survey_ids = c("DEMO2016PHIA", "DEMO2015DHS"),
                                    artcov_survey_ids = "DEMO2016PHIA",
                                    recent_survey_ids = "DEMO2016PHIA")


  b_tmb_inputs <- prepare_tmb_inputs(b_naomi_data)
  b_fit <- fit_tmb(b_tmb_inputs, outer_verbose = FALSE)

  d <- b_tmb_inputs$data
  par_val <- b_fit$par.full %>% split(names(.))
  p <- b_tmb_inputs$par_init
  p[names(par_val)] <- par_val

  v <- naomi_objective_function_r(d, p)

  ## Check that REPORT() values are equal
  expect_setequal(names(v$report), names(b_fit$mode))
  expect_equal(v$report, b_fit$mode[names(v$report)])

  ## Check that objective function is equal
  skip("Skipping test for objective function value; AR1 not yet implemented")
  expect_equal(v$val, b_fit$objective)
})
