context("test-read-data")

test_that("read data functions parse expected columns", {

  expect_s3_class(read_population(a_hintr_data$population), "data.frame")
  expect_s3_class(read_survey_indicators(a_hintr_data$survey), "data.frame")
  expect_s3_class(read_art_number(a_hintr_data$art_number), "data.frame")
  expect_s3_class(read_anc_testing(a_hintr_data$anc_testing), "data.frame")
})

test_that("read data throws error if required columns not found", {
  expect_error(read_population(a_hintr_data$survey),
               "Required columns not found: calendar_quarter, population")
})


test_that("read data works with optional column specs", {

  pop <- read_population(a_hintr_data$population)
  pop$asfr <- NULL
  no_asfr_col <- tempfile(fileext = ".csv")
  readr::write_csv(pop, no_asfr_col, na = "")
  expect_s3_class(read_population(no_asfr_col), "data.frame")

  pop <- read_population(a_hintr_data$population)
  pop$area_id <- NULL
  no_area_id_col <- tempfile(fileext = ".csv")
  readr::write_csv(pop, no_area_id_col, na = "")
  expect_error(read_population(no_area_id_col), "Required columns not found: area_id")
})
