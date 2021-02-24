  context("test-model-frames")

test_that("get_age_group_out() returns expected groups", {
  expect_setequal(get_age_group_out("Y015_049"), "Y015_049")
  expect_setequal(get_age_group_out(c("Y015_049", "Y050_064", "Y065_999")),
               c("Y015_049", "Y015_064", "Y015_999", "Y050_999", "Y050_064", "Y065_999"))
  expect_setequal(
    get_age_group_out(c("Y000_004", "Y005_009", "Y010_014", "Y015_019", "Y020_024", "Y025_029",
                        "Y030_034", "Y035_039", "Y040_044", "Y045_049", "Y050_054", "Y055_059",
                        "Y060_064", "Y065_069", "Y070_074", "Y075_079", "Y080_999")),
    c("Y000_004", "Y005_009", "Y010_014", "Y015_019", "Y020_024", "Y025_029", "Y030_034",
      "Y035_039", "Y040_044", "Y045_049", "Y050_054", "Y055_059", "Y060_064", "Y065_069",
      "Y070_074", "Y075_079", "Y080_999", "Y015_049", "Y015_064", "Y015_999", "Y050_999", "Y000_999",
      "Y000_064", "Y000_014", "Y015_024", "Y025_034", "Y035_049", "Y050_064", "Y065_999")
  )
  expect_setequal(
    get_age_group_out(c("Y015_019", "Y020_024", "Y025_029",
                        "Y030_034", "Y035_039", "Y040_044", "Y045_049", "Y050_054", "Y055_059",
                        "Y060_064", "Y065_069", "Y070_074", "Y075_079", "Y080_999")),
    c("Y015_019", "Y020_024", "Y025_029", "Y030_034", "Y035_039", "Y040_044", "Y045_049", "Y050_054",
      "Y055_059", "Y060_064", "Y065_069", "Y070_074", "Y075_079", "Y080_999", "Y015_049", "Y015_064", "Y015_999",
      "Y050_999", "Y015_024", "Y025_034", "Y035_049", "Y050_064", "Y065_999")
  )
})

test_that("artnum_mf() returns expected number of records", {
  expect_equal(nrow(artnum_mf("CY2016Q1", NULL, a_naomi_mf)), 0L)
  expect_equal(nrow(artnum_mf(NULL, demo_art_number, a_naomi_mf)), 0L)
  expect_equal(nrow(artnum_mf("CY2016Q1", NULL, a_naomi_mf)), 0L)
  expect_named(artnum_mf(NULL, demo_art_number, a_naomi_mf),
               c("area_id", "sex", "age_group", "artnum_idx", "art_current"))
  expect_equal(nrow(artnum_mf("CY2016Q1", demo_art_number, a_naomi_mf)), 14L)
  expect_named(artnum_mf("CY2016Q1", demo_art_number, a_naomi_mf),
               c("area_id", "sex", "age_group", "artnum_idx", "art_current"))
})

test_that("artnum_mf() throws errors for invalid inputs", {
  expect_error(artnum_mf("CY1924Q4", demo_art_number, a_naomi_mf),
               "No ART data found for quarter CY1924Q4.\nSet 'Include ART data' to 'No' if you intend to include no ART data.")
  expect_error(artnum_mf("CY2016Q1", demo_art_number, "jibberish"))
  expect_error(artnum_mf(c("CY2016Q1", "CY2016Q2"), demo_art_number, "jibberish"))
})

test_that("artnum_mf() works with single quarter ART data", {
  data_art_single_quarter <- dplyr::filter(demo_art_number, calendar_quarter == "CY2017Q4")
  expect_equal(nrow(artnum_mf("CY2017Q4", data_art_single_quarter, a_naomi_mf)), 14L)
})




test_that("population calibration options", {


  mf_none <- naomi_model_frame(a_area_merged,
                               demo_population_agesex,
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
                              demo_population_agesex,
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
                                 demo_population_agesex,
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
                      demo_population_agesex,
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

  mf1 <- survey_mf("DEMO2016PHIA", "prevalence", demo_survey_hiv_indicators, a_naomi_mf, use_kish = FALSE, deff = 1.0)
  mf2 <- survey_mf("DEMO2016PHIA", "prevalence", demo_survey_hiv_indicators, a_naomi_mf, use_kish = FALSE, deff = 2.5)

  expect_equal(mf1$n, mf1$n_eff)
  expect_equal(mf2$n, mf2$n_eff * 2.5)
  expect_equal(mf1$n_eff, 2.5 * mf2$n_eff)
  expect_equal(mf1$x_eff, 2.5 * mf2$x_eff)
})

test_that("use_kish returns Kish effective sample size", {

  mf_default <- survey_mf("DEMO2016PHIA",
                          "prevalence",
                          demo_survey_hiv_indicators,
                          a_naomi_mf)

  mf_srs <- survey_mf("DEMO2016PHIA",
                      "prevalence",
                      demo_survey_hiv_indicators,
                      a_naomi_mf,
                      use_kish = FALSE)
  
  mf_kish <- survey_mf("DEMO2016PHIA",
                       "prevalence",
                       demo_survey_hiv_indicators,
                       a_naomi_mf,
                       use_kish = TRUE)

  mf_kish_scaled <- survey_mf("DEMO2016PHIA",
                              "prevalence",
                              demo_survey_hiv_indicators,
                              a_naomi_mf,
                              use_kish = TRUE,
                              deff = 2.5)

  expect_equal(mf_default$n_eff, mf_kish$n_eff)
  expect_true(all(mf_kish$n_eff < mf_srs$n_eff | mf_srs$n_eff == 1))
  expect_equal(mf_kish_scaled$n_eff * 2.5, mf_kish$n_eff)
  
  expect_equal(mf_default$x_eff, mf_kish$x_eff)
  expect_true(all(mf_kish$x_eff < mf_srs$x_eff | mf_srs$x_eff == 0))
  expect_equal(mf_kish_scaled$x_eff * 2.5, mf_kish$x_eff)
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


test_that("survey_mf(..., use_aggregate) option returns expected results", {

  aggregate_survey <- dplyr::filter(demo_survey_hiv_indicators,
                                    age_group %in% c("Y000_014", "Y015_049"),
                                    sex == "both",
                                    grepl("MWI_1", area_id))

  expect_equal(nrow(survey_mf("DEMO2016PHIA", "prevalence", aggregate_survey, a_naomi_mf)), 0)
  expect_equal(nrow(survey_mf("DEMO2016PHIA", "prevalence", aggregate_survey, a_naomi_mf, use_aggregate = FALSE)), 0)
  expect_error(nrow(survey_mf("DEMO2016PHIA", "prevalence", aggregate_survey, a_naomi_mf, use_aggregate = TRUE)),
               "Aggregate survey data selected. Stratifications included in dataset which are not in model scope for indicator prevalence")

  expect_equal(nrow(survey_mf("DEMO2016PHIA", "art_coverage", aggregate_survey, a_naomi_mf)), 0)
  expect_equal(nrow(survey_mf("DEMO2016PHIA", "art_coverage", aggregate_survey, a_naomi_mf, use_aggregate = FALSE)), 0)
  expect_error(nrow(survey_mf("DEMO2016PHIA", "art_coverage", aggregate_survey, a_naomi_mf, use_aggregate = TRUE)),
               "Aggregate survey data selected. Stratifications included in dataset which are not in model scope for indicator art_coverage")

  aggregate_survey_good <- dplyr::filter(aggregate_survey, area_id == "MWI_1_1_demo")
  expect_equal(nrow(survey_mf("DEMO2016PHIA", "prevalence", aggregate_survey_good, a_naomi_mf, use_aggregate = TRUE)),
               nrow(dplyr::filter(aggregate_survey_good, indicator == "prevalence", survey_id == "DEMO2016PHIA")))

  expect_equal(nrow(survey_mf("DEMO2016PHIA", "art_coverage", aggregate_survey_good, a_naomi_mf, use_aggregate = TRUE)),
               nrow(dplyr::filter(aggregate_survey_good, indicator == "art_coverage", survey_id == "DEMO2016PHIA")))

  expect_equal(nrow(survey_mf("DEMO2016PHIA", "recent_infected", aggregate_survey_good, a_naomi_mf,
                              min_age = 15, max_age = 50, use_aggregate = TRUE)), 1)
})
