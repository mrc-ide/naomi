context("test-select-naomi-data")

test_that("select ANC programme data returns expected rows", {

  anc_testing <- read_anc_testing(system_file("extdata/demo_anc_testing.csv"))

  anc_testing_prev_mf <- anc_testing_prev_mf(2017, anc_testing, a_naomi_mf)
  expect_equal(nrow(anc_testing_prev_mf$model_input), nrow(a_naomi_mf$mf_areas))

  anc_testing_artcov_mf <- anc_testing_artcov_mf(2017, anc_testing, a_naomi_mf)
  expect_equal(nrow(anc_testing_artcov_mf$model_input),nrow(a_naomi_mf$mf_areas))

  ## Year not in data
  expect_error(nrow(anc_testing_prev_mf(2000, anc_testing, a_naomi_mf)),
               "ANC testing data not found for year 2000")
  expect_error(nrow(anc_testing_artcov_mf(2000, anc_testing, a_naomi_mf)),
               "ANC testing data not found for year 2000")

  ## NULL data provided
  anc_testing_prev_mf <- anc_testing_prev_mf(2017, NULL, a_naomi_mf)
  expect_equal(nrow(anc_testing_prev_mf$model_input), 0)
  anc_testing_artcov_mf <- anc_testing_artcov_mf(2017, NULL, a_naomi_mf)
  expect_equal(nrow(anc_testing_artcov_mf$model_input), 0)

  ## NULL year provided
  anc_testing_prev_mf <- anc_testing_prev_mf(NULL, anc_testing, a_naomi_mf)
  expect_equal(nrow(anc_testing_prev_mf$model_input), 0)
  anc_testing_artcov_mf <- anc_testing_artcov_mf(NULL, anc_testing, a_naomi_mf)
  expect_equal(nrow(anc_testing_artcov_mf$model_input), 0)

})

