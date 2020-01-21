  context("test-model-frames")

test_that("get_age_group_out() returns expected groups", {
  expect_setequal(get_age_group_out("15-49"), "15-49")
  expect_setequal(get_age_group_out(c("15-49", "50-64", "65+")),
               c("15-49", "15-64", "15+", "50+", "50-64", "65+"))
  expect_setequal(
    get_age_group_out(c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29",
                        "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                        "60-64", "65-69", "70-74", "75-79", "80+")),
    c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34",
      "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
      "70-74", "75-79", "80+", "15-49", "15-64", "15+", "50+", "00+",
      "00-64", "00-14", "15-24", "25-34", "35-49", "50-64", "65+")
  )
  expect_setequal(
    get_age_group_out(c("15-19", "20-24", "25-29",
                        "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                        "60-64", "65-69", "70-74", "75-79", "80+")),
    c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
      "55-59", "60-64", "65-69", "70-74", "75-79", "80+", "15-49", "15-64", "15+",
      "50+", "15-24", "25-34", "35-49", "50-64", "65+")
  )
})

test_that("artnum_mf() returns expected number of records", {
  expect_equal(nrow(artnum_mf("CY2016Q1", NULL, a_naomi_mf)), 0L)
  expect_equal(nrow(artnum_mf(NULL, mwi_art_number, a_naomi_mf)), 0L)
  expect_equal(nrow(artnum_mf("CY2016Q1", NULL, a_naomi_mf)), 0L)
  expect_named(artnum_mf(NULL, mwi_art_number, a_naomi_mf),
               c("area_id", "sex", "age_group", "artnum_idx", "current_art"))
  expect_equal(nrow(artnum_mf("CY2016Q1", mwi_art_number, a_naomi_mf)), 14L)
  expect_named(artnum_mf("CY2016Q1", mwi_art_number, a_naomi_mf),
               c("area_id", "sex", "age_group", "artnum_idx", "current_art"))
})

test_that("artnum_mf() throws errors for invalid inputs", {
  expect_error(artnum_mf("CY1924Q4", mwi_art_number, a_naomi_mf),
               "No ART data found for quarter CY1924Q4.\nSet 'Include ART data' to 'No' if you intend to include no ART data.")
  expect_error(artnum_mf("CY2016Q1", mwi_art_number, "jibberish"))
  expect_error(artnum_mf(c("CY2016Q1", "CY2016Q2"), mwi_art_number, "jibberish"))
})


test_that("population calibration options", {


  mf_none <- naomi_model_frame(a_area_merged,
                               mwi_population_agesex,
                               a_spec,
                               scope = "MWI",
                               level = 4,
                               calendar_quarter1 = "CY2016Q1",
                               calendar_quarter2 = "CY2018Q3",
                               calendar_quarter3 = "CY2019Q2",
                               spectrum_population_calibration = "none")

  expect_true(all(mf_none$spectrum_calibration$population_calibration == 1))
  expect_equal(mf_none$spectrum_calibration$population_raw,
               mf_none$spectrum_calibration$population)
  expect_false(all(mf_none$spectrum_calibration$population_spectrum ==
                   mf_none$spectrum_calibration$population))
  expect_equal(sum(mf_none$mf_model$population_t1 + mf_none$mf_model$population_t2 + mf_none$mf_model$population_t3),
               sum(mf_none$spectrum_calibration$population))
  expect_equal(sum(mf_none$mf_model$population_t1 + mf_none$mf_model$population_t2 + + mf_none$mf_model$population_t3),
               sum(mf_none$spectrum_calibration$population_raw))


  ## TODO: Test on subnational Spectrum files that national calibration
  ##       gives a different result than subnational calibtration.

  mf_nat <- naomi_model_frame(a_area_merged,
                              mwi_population_agesex,
                              a_spec,
                              scope = "MWI",
                              level = 4,
                              calendar_quarter1 = "CY2016Q1",
                              calendar_quarter2 = "CY2018Q3",
                              calendar_quarter3 = "CY2019Q2",
                              spectrum_population_calibration = "national")

  expect_false(all(mf_nat$spectrum_calibration$population_calibration == 1))
  expect_false(sum(mf_nat$spectrum_calibration$population_raw) ==
               sum(mf_nat$spectrum_calibration$population))
  expect_equal(sum(mf_nat$spectrum_calibration$population_spectrum),
               sum(mf_nat$spectrum_calibration$population))
  expect_equal(sum(mf_nat$mf_model$population_t1 + mf_nat$mf_model$population_t2 + mf_nat$mf_model$population_t3),
               sum(mf_nat$spectrum_calibration$population))
  expect_equal(sum(mf_nat$mf_model$population_t1 + mf_nat$mf_model$population_t2 + mf_nat$mf_model$population_t3),
               sum(mf_nat$spectrum_calibration$population_spectrum))


  mf_subnat <- naomi_model_frame(a_area_merged,
                                 mwi_population_agesex,
                                 a_spec,
                                 scope = "MWI",
                                 level = 4,
                                 calendar_quarter1 = "CY2016Q1",
                                 calendar_quarter2 = "CY2018Q3",
                                 calendar_quarter3 = "CY2019Q2",
                                 spectrum_population_calibration = "subnational")

  expect_false(all(mf_subnat$spectrum_calibration$population_calibration == 1))
  expect_false(sum(mf_subnat$spectrum_calibration$population_raw) ==
               sum(mf_subnat$spectrum_calibration$population))
  expect_equal(mf_subnat$spectrum_calibration$population_spectrum,
               mf_subnat$spectrum_calibration$population)
  expect_equal(sum(mf_subnat$mf_model$population_t1 + mf_subnat$mf_model$population_t2 + mf_subnat$mf_model$population_t3),
               sum(mf_subnat$spectrum_calibration$population))
  expect_equal(sum(mf_subnat$mf_model$population_t1 + mf_subnat$mf_model$population_t2 + mf_subnat$mf_model$population_t3),
               sum(mf_subnat$spectrum_calibration$population_spectrum))

  expect_error(
    naomi_model_frame(a_area_merged,
               mwi_population_agesex,
               a_spec,
               scope = "MWI",
               level = 4,
               calendar_quarter1 = "CY2016Q1",
               calendar_quarter2 = "CY2018Q3",
               calendar_quarter3 = "CY2019Q2",
               spectrum_population_calibration = "jibberish"),
    "spectrum_calibration_option \"jibberish\" not found."
  )

})


test_that("survey design effect scales effect sample size", {

  mf1 <- survey_mf("MWI2016PHIA", "prev", mwi_survey_hiv_indicators, a_naomi_mf, deff = 1.0)
  mf2 <- survey_mf("MWI2016PHIA", "prev", mwi_survey_hiv_indicators, a_naomi_mf, deff = 2.5)

  expect_equal(mf1$n, mf1$n_eff)
  expect_equal(mf2$n, mf2$n_eff * 2.5)
  expect_equal(mf1$n_eff, 2.5 * mf2$n_eff)
  expect_equal(mf1$x_eff, 2.5 * mf2$x_eff)
})

test_that("select_naomi_data() returns expected stratifications", {

  prev_age_groups <- dplyr::filter(get_age_groups(),
                                   age_group %in% get_five_year_age_groups(),
                                   age_group_start + age_group_span - 1 < 65) %>%
    .$age_group

  artcov_age_groups <- dplyr::filter(get_age_groups(),
                                     age_group %in% get_five_year_age_groups(),
                                     age_group_start + age_group_span - 1 < 65) %>%
    .$age_group

  recent_age_groups <- dplyr::filter(get_age_groups(),
                                     age_group %in% get_five_year_age_groups(),
                                     age_group_start >= 15,
                                     age_group_start + age_group_span - 1 < 65) %>%
    .$age_group

  expect_setequal(a_naomi_data$prev_dat$age_group, prev_age_groups)
  expect_setequal(a_naomi_data$artcov_dat$age_group, artcov_age_groups)
  expect_setequal(a_naomi_data$recent_dat$age_group, recent_age_groups)

})
