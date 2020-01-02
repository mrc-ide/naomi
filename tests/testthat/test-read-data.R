context("test-read-data")

test_that("read data functions parse expected columns", {

  expect_s3_class(read_population(a_hintr_data$population), "data.frame")
  expect_s3_class(read_survey_indicators(a_hintr_data$survey), "data.frame")
  expect_s3_class(read_art_number(a_hintr_data$art_number), "data.frame")
  expect_s3_class(read_anc_testing(a_hintr_data$anc_testing), "data.frame")
})

test_that("read data throws error if required columns not found", {

  expect_error(read_population(a_hintr_data$survey),
               "names\\(col_spec\\$cols\\) %in% names\\(val\\) are not all TRUE")
})
