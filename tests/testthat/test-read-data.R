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

test_that("read_art_number() allows year or calendar_quarter entry", {

  raw <- readr_read_csv(a_hintr_data$art_number)
  v0 <- read_art_number(a_hintr_data$art_number)
  
  f1 <- tempfile(fileext = ".csv")
  dplyr::mutate(raw, year = NULL, calendar_quarter = NULL) %>%
    readr::write_csv(f1, na = "")
  expect_error(read_art_number(f1), "Both 'year' and 'calendar_quarter' are missing. One must be present.")

  f2 <- tempfile(fileext = ".csv")
  dplyr::mutate(raw, year = year + 1) %>%
    readr::write_csv(f2, na = "")
  expect_error(read_art_number(f2), "Inconsistent year and calendar_quarter found in ART dataset.")

  f3 <- tempfile(fileext = ".csv")
  dplyr::mutate(raw, year = NULL) %>%
    readr::write_csv(f3, na = "")
  expect_equal(read_art_number(f3), v0)

  f4 <- tempfile(fileext = ".csv")
  dplyr::mutate(raw, calendar_quarter = NULL) %>%
    readr::write_csv(f4, na = "")
  expect_equal(read_art_number(f4), v0)
})

test_that("read_*()functions drop NA rows", {

  raw <- readr_read_csv(a_hintr_data$anc_testing)
  dat_with_na <- rbind(raw[1:10, ], NA, raw[11:nrow(raw), ], NA, "", c("", NA, ""), NA)
  
  f1 <- tempfile(fileext = ".csv")
  readr::write_csv(dat_with_na, f1, na = "")
  
  v1 <- read_anc_testing(a_hintr_data$anc_testing)
  v2 <- read_anc_testing(f1)

  expect_equal(nrow(raw), nrow(v2))
  expect_equal(v1, v2)
})
