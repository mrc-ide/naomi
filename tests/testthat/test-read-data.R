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

test_that("reading utils can handle files with , in numeric columns", {
  art <- read.csv(a_hintr_data$art_number)
  art$art_current <- as.character(art$art_current)
  art$art_current[1] <- "2,031"
  art$art_current[2] <- "2.031"
  art <- art[1:25, ]
  t_en <- tempfile(fileext = ".csv")
  t_fr <- tempfile(fileext = ".csv")
  write.csv(art, t_en, row.names = FALSE)
  write.csv2(art, t_fr, row.names = FALSE)
  expect_no_error(art_en <- read_art_number(t_en))
  expect_no_error(art_fr <- read_art_number(t_fr))
  expect_type(art_en$art_current, "double")
  expect_type(art_fr$art_current, "double")
  expect_equal(art_en$art_current[1], 2031)
  expect_equal(art_en$art_current[2], 2.031)
  expect_equal(art_fr$art_current[1], 2.031)
  expect_equal(art_fr$art_current[2], 2031)
})

test_that("read_anc_testing() handles data set without 'anc_known_neg' column", {

  raw <- read_anc_testing(system_file("extdata/demo_anc_testing.csv"))

  ## Column anc_known_neg missing
  new1 <- raw
  new1$anc_tested <- raw$anc_tested + raw$anc_known_neg
  new1$anc_known_neg <- NULL
  new1$births_facility <- NULL

  f1 <- tempfile(fileext = ".csv")
  readr::write_csv(new, f1, na = "")

  anc_missing_known_neg <- read_anc_testing(f1)

  expect_equal(anc_missing_known_neg$anc_known_neg, rep(0.0, nrow(anc_new)))
  expect_null(anc_missing_known_neg[["births_facility"]])

  ## TODO: Insert test that calculating anc_prevalence using `raw` and `anc_missing_known_neg` gives same results
  expect_true(FALSE)  ## Dummy test failure as placeholder for prevalence calculation check


  ## Column anc_known_neg exists, but all values NA
  new2 <- raw
  new2$anc_tested <- raw$anc_tested + raw$anc_known_neg
  new2$anc_known_neg <- NA_real_
  new2$births_facility <- NULL

  f2 <- tempfile(fileext = ".csv")
  readr::write_csv(new, f2, na = "")

  anc_na_known_neg <- read_anc_testing(f2)

  expect_equal(anc_na_known_neg$anc_known_neg, rep(0.0, nrow(anc_new)))
  expect_null(anc_na_known_neg[["births_facility"]])

  ## TODO: Insert test that calculating anc_prevalence using `raw` and `anc_na_known_neg` gives same results
  expect_true(FALSE)  ## Dummy test failure as placeholder for prevalence calculation check

})
