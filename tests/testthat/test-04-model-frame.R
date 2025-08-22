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
      "Y000_064", "Y000_014", "Y015_024", "Y025_034", "Y035_049", "Y050_064", "Y065_999",
      "Y010_019", "Y025_049")
  )
  expect_setequal(
    get_age_group_out(c("Y015_019", "Y020_024", "Y025_029",
                        "Y030_034", "Y035_039", "Y040_044", "Y045_049", "Y050_054", "Y055_059",
                        "Y060_064", "Y065_069", "Y070_074", "Y075_079", "Y080_999")),
    c("Y015_019", "Y020_024", "Y025_029", "Y030_034", "Y035_039", "Y040_044", "Y045_049", "Y050_054",
      "Y055_059", "Y060_064", "Y065_069", "Y070_074", "Y075_079", "Y080_999", "Y015_049", "Y015_064", "Y015_999",
      "Y050_999", "Y015_024", "Y025_034", "Y035_049", "Y050_064", "Y065_999", "Y025_049")
  )
})

test_that("artnum_mf() returns expected number of records", {
  mf <- artnum_mf("CY2016Q1", NULL, a_naomi_mf)
  expect_equal(nrow(mf$model_input), 0L)
  mf <- artnum_mf(NULL, demo_art_number, a_naomi_mf)
  expect_equal(nrow(mf$model_input), 0L)
  mf <- artnum_mf("CY2016Q1", NULL, a_naomi_mf)
  expect_equal(nrow(mf$model_input), 0L)
  mf <- artnum_mf(NULL, demo_art_number, a_naomi_mf)
  expect_named(mf$model_input,
               c("area_id", "sex", "age_group", "art_current"))
  mf <- artnum_mf("CY2016Q1", demo_art_number, a_naomi_mf)
  expect_equal(nrow(mf$model_input), 14L)
  mf <- artnum_mf("CY2016Q1", demo_art_number, a_naomi_mf)
  expect_named(mf$model_input,
               c("area_id", "sex", "age_group", "art_current"))
})

test_that("artnum_mf() throws errors for invalid inputs", {
  expect_error(artnum_mf("CY1924Q4", demo_art_number, a_naomi_mf),
               "No ART data found for quarter CY1924Q4.\nSet 'Include ART data' to 'No' if you intend to include no ART data.")
  expect_error(artnum_mf("CY2016Q1", demo_art_number, "jibberish"))
  expect_error(artnum_mf(c("CY2016Q1", "CY2016Q2"), demo_art_number, "jibberish"))
})

test_that("artnum_mf() works with single quarter ART data", {
  data_art_single_quarter <- dplyr::filter(demo_art_number, calendar_quarter == "CY2017Q4")
  mf <- artnum_mf("CY2017Q4", data_art_single_quarter, a_naomi_mf)
  expect_equal(nrow(mf$model_input), 14L)
})


test_that("artnum_mf() selects adjusted number on ART", {

  mf <- artnum_mf("CY2023Q4", demo_art_number, a_naomi_mf)

  art_data_raw <- demo_art_number |>
    dplyr::filter(calendar_quarter == "CY2023Q4") |>
    dplyr::select(area_id, sex, age_group,
                  art_current_input = art_current,
                  art_current_adjusted_input =  art_current_adjusted)

  dat <- mf$model_input |>
    dplyr::left_join(art_data_raw, by = dplyr::join_by(area_id, sex, age_group))

  dat_014 <- dat[dat$age_group == "Y000_014",]
  dat_015plus <- dat[dat$age_group == "Y015_999",]

  # In test data, ARt adjustments only applied to 15+ population
  # Expect art_current used in model to match adjusted ART in input data
  expect_identical(dat$art_current, dat$art_current_adjusted_input)
  # Expect art_current <15 used in model to match reported ART in input data (adjustment factor = 1)
  expect_identical(dat_014$art_current, dat_014$art_current_input)
  # Expect art_current 15+ used in model **not** to match reported ART in input data (adjustment factor = 0.95)
  expect_true(all(unlist(dat_015plus$art_current != dat_015plus$art_current_input)))

})


test_that("population calibration options", {

  mf_none <- naomi_model_frame(a_area_merged,
                               demo_population_agesex,
                               a_spec,
                               scope = "MWI",
                               level = 4,
                               calendar_quarter1 = "CY2016Q1",
                               calendar_quarter2 = "CY2018Q4",
                               calendar_quarter3 = "CY2019Q2",
                               calendar_quarter4 = "CY2022Q3",
                               spectrum_population_calibration = "none")

  expect_equal(mf_none$spectrum_calibration$population_raw,
               mf_none$spectrum_calibration$population_calibrated)
  expect_false(all(mf_none$spectrum_calibration$population_spectrum ==
                   mf_none$spectrum_calibration$population_calibrated))
  expect_equal(sum(mf_none$mf_model$population_t1 +
                     mf_none$mf_model$population_t2 +
                     mf_none$mf_model$population_t3 +
                     mf_none$mf_model$population_t4
                   ),
               sum(mf_none$spectrum_calibration$population_calibrated))
  expect_equal(sum(mf_none$mf_model$population_t1 +
                     mf_none$mf_model$population_t2 +
                     mf_none$mf_model$population_t3 +
                     mf_none$mf_model$population_t4
                   ),
               sum(mf_none$spectrum_calibration$population_raw))


  ## TODO: Test on subnational Spectrum files that national calibration
  ##       gives a different result than subnational calibtration.

  mf_nat <- naomi_model_frame(a_area_merged,
                              demo_population_agesex,
                              a_spec,
                              scope = "MWI",
                              level = 4,
                              calendar_quarter1 = "CY2016Q1",
                              calendar_quarter2 = "CY2018Q4",
                              calendar_quarter3 = "CY2019Q2",
                              calendar_quarter4 = "CY2022Q3",
                              spectrum_population_calibration = "national")

  expect_false(sum(mf_nat$spectrum_calibration$population_raw) ==
               sum(mf_nat$spectrum_calibration$population_calibrated))
  expect_equal(sum(mf_nat$spectrum_calibration$population_spectrum),
               sum(mf_nat$spectrum_calibration$population_calibrated))
  expect_equal(sum(mf_nat$mf_model$population_t1 +
                     mf_nat$mf_model$population_t2 +
                     mf_nat$mf_model$population_t3 +
                     mf_nat$mf_model$population_t4
                   ),
               sum(mf_nat$spectrum_calibration$population_calibrated))
  expect_equal(sum(mf_nat$mf_model$population_t1 +
                     mf_nat$mf_model$population_t2 +
                     mf_nat$mf_model$population_t3 +
                     mf_nat$mf_model$population_t4
                   ),
               sum(mf_nat$spectrum_calibration$population_spectrum))


  mf_subnat <- naomi_model_frame(a_area_merged,
                                 demo_population_agesex,
                                 a_spec,
                                 scope = "MWI",
                                 level = 4,
                                 calendar_quarter1 = "CY2016Q1",
                                 calendar_quarter2 = "CY2018Q4",
                                 calendar_quarter3 = "CY2019Q2",
                                 calendar_quarter4 = "CY2022Q3",
                                 spectrum_population_calibration = "subnational")

  expect_false(sum(mf_subnat$spectrum_calibration$population_raw) ==
               sum(mf_subnat$spectrum_calibration$population_calibrated))
  expect_equal(mf_subnat$spectrum_calibration$population_spectrum,
               mf_subnat$spectrum_calibration$population_calibrated)
  expect_equal(sum(mf_subnat$mf_model$population_t1 +
                     mf_subnat$mf_model$population_t2 +
                     mf_subnat$mf_model$population_t3 +
                     mf_subnat$mf_model$population_t4
                   ),
               sum(mf_subnat$spectrum_calibration$population_calibrated))
  expect_equal(sum(mf_subnat$mf_model$population_t1 +
                     mf_subnat$mf_model$population_t2 +
                     mf_subnat$mf_model$population_t3 +
                     mf_subnat$mf_model$population_t4
                   ),
               sum(mf_subnat$spectrum_calibration$population_spectrum))

  expect_error(
    naomi_model_frame(a_area_merged,
                      demo_population_agesex,
                      a_spec,
                      scope = "MWI",
                      level = 4,
                      calendar_quarter1 = "CY2016Q1",
                      calendar_quarter2 = "CY2018Q4",
                      calendar_quarter3 = "CY2019Q2",
                      calendar_quarter4 = "CY2022Q3",
                      spectrum_population_calibration = "jibberish"),
    "spectrum_calibration_option \"jibberish\" not found."
  )

})


test_that("survey design effect scales effect sample size", {

  mf1 <- survey_mf("DEMO2016PHIA", "prevalence", demo_survey_hiv_indicators, a_naomi_mf, use_kish = FALSE, deff = 1.0)
  mf2 <- survey_mf("DEMO2016PHIA", "prevalence", demo_survey_hiv_indicators, a_naomi_mf, use_kish = FALSE, deff = 2.5)

  expect_equal(mf1$model_input$n, mf1$model_input$n_eff)
  expect_equal(mf2$model_input$n, mf2$model_input$n_eff * 2.5)
  expect_equal(mf1$model_input$n_eff, 2.5 * mf2$model_input$n_eff)
  expect_equal(mf1$model_input$x_eff, 2.5 * mf2$model_input$x_eff)
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

  expect_equal(mf_default$model_input$n_eff, mf_kish$model_input$n_eff)
  expect_true(all(mf_kish$model_input$n_eff < mf_srs$model_input$n_eff | mf_srs$model_input$n_eff == 1))
  expect_equal(mf_kish_scaled$model_input$n_eff * 2.5, mf_kish$model_input$n_eff)

  expect_equal(mf_default$model_input$x_eff, mf_kish$model_input$x_eff)
  expect_true(all(mf_kish$model_input$x_eff < mf_srs$model_input$x_eff | mf_srs$model_input$x_eff == 0))
  expect_equal(mf_kish_scaled$model_input$x_eff * 2.5, mf_kish$model_input$x_eff)
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
  mf <- survey_mf("DEMO2016PHIA", "prevalence", aggregate_survey, a_naomi_mf)
  expect_equal(nrow(mf$model_input), 0)
  mf <- survey_mf("DEMO2016PHIA", "prevalence", aggregate_survey, a_naomi_mf, use_aggregate = FALSE)
  expect_equal(nrow(mf$model_input), 0)
  expect_error(nrow(survey_mf("DEMO2016PHIA", "prevalence", aggregate_survey, a_naomi_mf, use_aggregate = TRUE)),
               "Aggregate survey data selected. Stratifications included in dataset which are not in model scope for indicator prevalence")

  mf <- survey_mf("DEMO2016PHIA", "art_coverage", aggregate_survey, a_naomi_mf)
  expect_equal(nrow(mf$model_input), 0)
  mf <- survey_mf("DEMO2016PHIA", "art_coverage", aggregate_survey, a_naomi_mf, use_aggregate = FALSE)
  expect_equal(nrow(mf$model_input), 0)
  expect_error(nrow(survey_mf("DEMO2016PHIA", "art_coverage", aggregate_survey, a_naomi_mf, use_aggregate = TRUE)),
               "Aggregate survey data selected. Stratifications included in dataset which are not in model scope for indicator art_coverage")

  aggregate_survey_good <- dplyr::filter(aggregate_survey, area_id == "MWI_1_1_demo")
  mf <- survey_mf("DEMO2016PHIA", "prevalence", aggregate_survey_good, a_naomi_mf, use_aggregate = TRUE)
  expect_equal(nrow(mf$model_input),
               nrow(dplyr::filter(aggregate_survey_good, indicator == "prevalence", survey_id == "DEMO2016PHIA")))
  mf <- survey_mf("DEMO2016PHIA", "art_coverage", aggregate_survey_good, a_naomi_mf, use_aggregate = TRUE)
  expect_equal(nrow(mf$model_input),
               nrow(dplyr::filter(aggregate_survey_good, indicator == "art_coverage", survey_id == "DEMO2016PHIA")))
  mf <- survey_mf("DEMO2016PHIA", "recent_infected", aggregate_survey_good, a_naomi_mf,
                       min_age = 15, max_age = 50, use_aggregate = TRUE)
  expect_equal(nrow(mf$model_input), 1)
})

test_that("naomi_model_frame() interpolated population depends on quarter specification", {

  areas_zone <- read_area_merged(system_file("extdata/demo-subnational-pjnz/demo_areas_region-pjnz.geojson"))
  pop_zone <- read_population(system_file("extdata/demo-subnational-pjnz/demo_population_zone.csv"))

  ## Test files created with Spectrum v5.87 -- mid-year population projection
  pjnz_old <- system_file("extdata/demo-subnational-pjnz/demo_mwi2019_region-pjnz.zip")
  spec_old  <- extract_pjnz_naomi(pjnz_old)

  expect_true(all(spec_old$quarter == 2))

  mf_old <- naomi_model_frame(areas_zone,
                              pop_zone,
                              spec_old,
                              scope = "MWI",
                              level = 2,
                              calendar_quarter1 = "CY2016Q1",
                              calendar_quarter2 = "CY2018Q4",
                              calendar_quarter3 = "CY2019Q2",
                              calendar_quarter4 = "CY2022Q3",
                              spectrum_population_calibration = "subnational")


  ## Outputs should match for T3 (CY2019Q2 -- mid-year)
  expect_equal(sum(dplyr::filter(spec_old, year == 2019)$totpop), sum(mf_old$mf_model$population_t3))
  expect_equal(sum(dplyr::filter(spec_old, year == 2019)$hivpop),
               sum(dplyr::filter(mf_old$spectrum_calibration, calendar_quarter == "CY2019Q2")$plhiv_spectrum))
  expect_equal(sum(dplyr::filter(spec_old, year == 2019)$artpop),
               sum(dplyr::filter(mf_old$spectrum_calibration, calendar_quarter == "CY2019Q2")$art_current_spectrum))

  ## Test files created with Spectrum v6.2 Beta 25 -- calendar year population projection
  pjnz_new <- system_file("extdata/demo-subnational-pjnz/demo_mwi2019_region-pjnz_v6.2.zip")
  spec_new  <- extract_pjnz_naomi(pjnz_new)

  expect_true(all(spec_new$quarter == 4))

  mf_new <- naomi_model_frame(areas_zone,
                              pop_zone,
                              spec_new,
                              scope = "MWI",
                              level = 2,
                              calendar_quarter1 = "CY2016Q1",
                              calendar_quarter2 = "CY2018Q4",
                              calendar_quarter3 = "CY2019Q2",
                              calendar_quarter4 = "CY2022Q3",
                              spectrum_population_calibration = "subnational")


  ## Outputs should match for T2 (CY2018Q4 -- mid-year)
  expect_equal(sum(dplyr::filter(spec_new, year == 2018)$totpop), sum(mf_new$mf_model$population_t2))
  expect_equal(sum(dplyr::filter(spec_new, year == 2018)$hivpop),
               sum(dplyr::filter(mf_new$spectrum_calibration, calendar_quarter == "CY2018Q4")$plhiv_spectrum))

  ## Note: For Q4 ART pop -- should match to Dec 31 ART input
  expect_equal(sum(dplyr::filter(spec_new, year == 2018)$artpop_dec31),
               sum(dplyr::filter(mf_new$spectrum_calibration, calendar_quarter == "CY2018Q4")$art_current_spectrum))

  ## Test that calibrates to internal ART population if artpop_dec31 not specified

  spec_alt  <- spec_new
  spec_alt$artpop_dec31 <- NA_real_

  expect_true(all(spec_alt$quarter == 4))

  mf_alt <- naomi_model_frame(areas_zone,
                              pop_zone,
                              spec_alt,
                              scope = "MWI",
                              level = 2,
                              calendar_quarter1 = "CY2016Q1",
                              calendar_quarter2 = "CY2018Q4",
                              calendar_quarter3 = "CY2019Q2",
                              calendar_quarter4 = "CY2022Q3",
                              spectrum_population_calibration = "subnational")

  ## Outputs should match for T2 (CY2018Q4 -- mid-year)
  expect_equal(sum(dplyr::filter(spec_alt, year == 2018)$totpop), sum(mf_alt$mf_model$population_t2))
  expect_equal(sum(dplyr::filter(spec_alt, year == 2018)$hivpop),
               sum(dplyr::filter(mf_alt$spectrum_calibration, calendar_quarter == "CY2018Q4")$plhiv_spectrum))

  ## Note: For Q4 ART pop -- should match to internal ART total
  expect_equal(sum(dplyr::filter(spec_alt, year == 2018)$artpop),
               sum(dplyr::filter(mf_alt$spectrum_calibration, calendar_quarter == "CY2018Q4")$art_current_spectrum))

})
