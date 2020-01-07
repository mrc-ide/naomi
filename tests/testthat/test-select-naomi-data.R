context("test-select-naomi-data")

test_that("select ANC programme data returns expected rows", {

  anc_testing <- read_anc_testing(a_hintr_data$anc_testing)

  expect_equal(nrow(anc_testing_prev_mf(2017, anc_testing, a_naomi_mf)),
                    nrow(a_naomi_mf$mf_areas))
  expect_equal(nrow(anc_testing_artcov_mf(2017, anc_testing, a_naomi_mf)),
                    nrow(a_naomi_mf$mf_areas))

  ## Year not in data
  expect_error(nrow(anc_testing_prev_mf(2000, anc_testing, a_naomi_mf)),
               "ANC testing data not found for year 2000")
  expect_error(nrow(anc_testing_artcov_mf(2000, anc_testing, a_naomi_mf)),
               "ANC testing data not found for year 2000")

  ## NULL data provided
  expect_equal(nrow(anc_testing_prev_mf(2017, NULL, a_naomi_mf)), 0)
  expect_equal(nrow(anc_testing_artcov_mf(2017, NULL, a_naomi_mf)), 0)

  ## NULL year provided
  expect_equal(nrow(anc_testing_prev_mf(NULL, anc_testing, a_naomi_mf)), 0)
  expect_equal(nrow(anc_testing_artcov_mf(NULL, anc_testing, a_naomi_mf)), 0)

})
