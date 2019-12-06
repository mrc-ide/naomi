context("test-model-fit")

test_that("setting rng_seed returns same outputs", {

  a_fit_sample2 <- sample_tmb(a_fit, nsample = 30, rng_seed = 28)
  a_output2 <- output_package(a_fit_sample2, a_naomi_mf, a_area_merged)

  expect_equal(a_fit_sample$sample, a_fit_sample2$sample)
  expect_equal(a_output$indicators$mean, a_output2$indicators$mean)
})

test_that("setting different rng_seed returns different output", {

  a_fit_sample3 <- sample_tmb(a_fit, nsample = 30, rng_seed = 1)
  expect_true(
    a_fit_sample$sample$artnum_t1_out[1] !=
    a_fit_sample3$sample$artnum_t1_out[1]
  )

  a_fit_sample_null <- sample_tmb(a_fit, nsample = 30)
  expect_true(
    a_fit_sample$sample$artnum_t1_out[1] !=
    a_fit_sample_null$sample$artnum_t1_out[1]
  )
})

test_that("exceeding maximum iterations throws a warning", {
  expect_warning(fit_tmb(a_tmb_inputs, outer_verbose = FALSE, max_iter = 5),
                 "iteration limit reached")
})

test_that("model fits with differing number of ANC observations T1 and T2", {

  ancdat <- mwi_anc_testing %>%
    dplyr::group_by(year) %>%
    dplyr::filter(year == 2016 |
                  year == 2018 & dplyr::row_number() == 1) %>%
    dplyr::ungroup()
  
  naomi_data <- select_naomi_data(a_naomi_mf,
                                  mwi_survey_hiv_indicators,
                                  anc_testing = ancdat,
                                  mwi_art_number,
                                  prev_survey_ids = c("MWI2016PHIA", "MWI2015DHS"),
                                  artcov_survey_ids = "MWI2016PHIA",
                                  recent_survey_ids = "MWI2016PHIA",
                                  anc_prev_year_t1 = 2016,
                                  anc_prev_year_t2 = 2018,
                                  anc_artcov_year_t1 = 2016,
                                  anc_artcov_year_t2 = 2018)

  tmb_inputs <- prepare_tmb_inputs(naomi_data)
  a_fit <- fit_tmb(tmb_inputs, outer_verbose = FALSE)

  expect_equal(a_fit$convergence, 0)
  
})
