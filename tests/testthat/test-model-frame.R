context("test-model-frames")

test_that("get_age_group_id_out() returns expected groups", {
  expect_equal(get_age_group_id_out(18), 18)
  expect_equal(get_age_group_id_out(c(18, 28, 29)), c(18:21, 28:29))
  expect_equal(get_age_group_id_out(1:17), 1:29)
  expect_equal(get_age_group_id_out(4:17), c(4:21, 25:29))
})

test_that("artnum_mf() returns expected number of records", {
  expect_equal(nrow(artnum_mf("CY2016Q1", NULL, a_naomi_mf)), 0L)
  expect_equal(nrow(artnum_mf(NULL, mwi_art_number, a_naomi_mf)), 0L)
  expect_equal(nrow(artnum_mf("CY2016Q1", NULL, a_naomi_mf)), 0L)
  expect_named(artnum_mf(NULL, mwi_art_number, a_naomi_mf), 
               c("area_id", "sex", "age_group_id", "artnum_idx", "current_art"))
  expect_equal(nrow(artnum_mf("CY2016Q1", mwi_art_number, a_naomi_mf)), 14L)
  expect_named(artnum_mf("CY2016Q1", mwi_art_number, a_naomi_mf), 
               c("area_id", "sex", "age_group_id", "artnum_idx", "current_art"))
})

test_that("artnum_mf() throws errors for invalid inputs", {
  expect_error(artnum_mf("CY1924Q4", mwi_art_number, a_naomi_mf))
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
                               spectrum_population_calibration = "none")

  expect_true(all(mf_none$spectrum_calibration$population_calibration == 1))
  expect_equal(mf_none$spectrum_calibration$population_raw,
               mf_none$spectrum_calibration$population)
  expect_false(all(mf_none$spectrum_calibration$population_spectrum == 
                   mf_none$spectrum_calibration$population))
  expect_equal(sum(mf_none$mf_model$population_t1 + mf_none$mf_model$population_t2),
               sum(mf_none$spectrum_calibration$population))
  expect_equal(sum(mf_none$mf_model$population_t1 + mf_none$mf_model$population_t2),
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
                              spectrum_population_calibration = "national")

  expect_false(all(mf_nat$spectrum_calibration$population_calibration == 1))
  expect_false(sum(mf_nat$spectrum_calibration$population_raw) == 
               sum(mf_nat$spectrum_calibration$population))
  expect_equal(sum(mf_nat$spectrum_calibration$population_spectrum),
               sum(mf_nat$spectrum_calibration$population))
  expect_equal(sum(mf_nat$mf_model$population_t1 + mf_nat$mf_model$population_t2),
               sum(mf_nat$spectrum_calibration$population))
  expect_equal(sum(mf_nat$mf_model$population_t1 + mf_nat$mf_model$population_t2),
               sum(mf_nat$spectrum_calibration$population_spectrum))

  
  mf_subnat <- naomi_model_frame(a_area_merged,
                                 mwi_population_agesex,
                                 a_spec,
                                 scope = "MWI",
                                 level = 4,
                                 calendar_quarter1 = "CY2016Q1",
                                 calendar_quarter2 = "CY2018Q3",
                                 spectrum_population_calibration = "subnational")

  expect_false(all(mf_subnat$spectrum_calibration$population_calibration == 1))
  expect_false(sum(mf_subnat$spectrum_calibration$population_raw) == 
               sum(mf_subnat$spectrum_calibration$population))
  expect_equal(mf_subnat$spectrum_calibration$population_spectrum,
               mf_subnat$spectrum_calibration$population)
  expect_equal(sum(mf_subnat$mf_model$population_t1 + mf_subnat$mf_model$population_t2),
               sum(mf_subnat$spectrum_calibration$population))
  expect_equal(sum(mf_subnat$mf_model$population_t1 + mf_subnat$mf_model$population_t2),
               sum(mf_subnat$spectrum_calibration$population_spectrum))
 
  expect_error(
    naomi_model_frame(a_area_merged,
               mwi_population_agesex,
               a_spec,
               scope = "MWI",
               level = 4,
               calendar_quarter1 = "CY2016Q1",
               calendar_quarter2 = "CY2018Q3",
               spectrum_population_calibration = "jibberish"),
    "spectrum_calibration_option \"jibberish\" not found."
  )

})
               
