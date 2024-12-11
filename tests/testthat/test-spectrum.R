test_that("extract_shiny90_age_sex() returns expected results", {

  pjnz <- system_file("extdata/demo_mwi2019.PJNZ")
  shiny90dir <- tempfile()
  utils::unzip(pjnz, exdir = shiny90dir)
  shiny90_path <- file.path(shiny90dir, "malawi.zip.shiny90")

  res1 <- extract_shiny90_age_sex(shiny90_path, year = 2010:2019)

  expect_named(res1, c("area", "year", "sex", "agegr", "hivstatus",
                       "plhiv", "aware", "artnum"))
  expect_setequal(res1$area, "Malawi")
  expect_setequal(res1$year, 2010:2019)
  expect_setequal(res1$sex, c("male", "female"))
  expect_setequal(res1$agegr, c("15-19", "20-24", "25-29", "30-34",
                                "35-39", "40-44", "45-49", "50-99"))
  expect_setequal(res1$hivstatus, "positive")
  expect_true(all(res1$plhiv > res1$aware))
  expect_true(all(res1$aware > res1$artnum))
  expect_true(all(res1$artnum > 0))

  res2 <- extract_shiny90_age_sex(shiny90_path)
  expect_setequal(res2$year, 1970:2025)

  expect_error(extract_shiny90_age_sex(shiny90_path, year = c(2010:2013, 2035, 2038)),
               "Ouput years not contained in shiny90 projection: 2035, 2038")
})

test_that("extract_shiny90_age_sex() returns for Spectrum internal .shiny90", {

  pjnz_shiny90_internal <- system_file("extdata/demo_mwi2019_v6.21-shiny90.PJNZ")
  shiny90dir <- tempfile()
  utils::unzip(pjnz_shiny90_internal, exdir = shiny90dir)
  shiny90_internal_path <- file.path(shiny90dir, "Malawi.zip.shiny90")

  ## pjnz_path argument = NULL (default)
  expect_error(extract_shiny90_age_sex(shiny90_internal_path, year = 2010:2019),
               "PJNZ file required for .shiny90 created by Spectrum")

  ## pjnz_path does not exist
  expect_error(extract_shiny90_age_sex(shiny90_internal_path, pjnz_path = "./does_not_exist.pjnz", year = 2010:2019),
               "PJNZ file required for .shiny90 created by Spectrum")

  res1 <- extract_shiny90_age_sex(shiny90_internal_path, pjnz_shiny90_internal, year = 2010:2019)

  expect_named(res1, c("area", "year", "sex", "agegr", "hivstatus",
                       "plhiv", "aware", "artnum"))
  expect_setequal(res1$area, "Malawi")
  expect_setequal(res1$year, 2010:2019)
  expect_setequal(res1$sex, c("male", "female"))
  expect_setequal(res1$agegr, c("15-19", "20-24", "25-29", "30-34",
                                "35-39", "40-44", "45-49", "50-99"))
  expect_setequal(res1$hivstatus, "positive")
  expect_true(all(res1$plhiv > res1$aware))
  expect_true(all(res1$aware >= res1$artnum))
  expect_true(all(res1$artnum > 0))

})

test_that("assert_pjnz_shiny90 validates shiny90 case-insensitively", {

  pjnz <- system_file("extdata/demo_mwi2019.PJNZ")
  expect_true(assert_pjnz_shiny90(pjnz))

  ## Zip does not contain .shiny90
  tmp_not_shiny90 <- tempfile(fileext = ".anything")
  file.create(tmp_not_shiny90)
  tmp_not_shiny90z <- tempfile(fileext = ".PJNZ")
  utils::zip(tmp_not_shiny90z, tmp_not_shiny90, extras = "-jq")

  expect_false(assert_pjnz_shiny90(tmp_not_shiny90z))

  ## Case insensitive
  tmp_shiny90 <- tempfile(fileext = ".sHiNy90")
  file.create(tmp_shiny90)
  tmp_shiny90z <- tempfile(fileext = ".PJNZ")
  utils::zip(tmp_shiny90z, tmp_shiny90, extras = "-jq")

  expect_true(assert_pjnz_shiny90(tmp_shiny90z))
})

test_that("read_spectrum_projection_name() returns projection name", {

  pjnz <- system_file("extdata/demo_mwi2019.PJNZ")
  expect_equal(read_spectrum_projection_name(pjnz), "Malawi_2019_v22_MM_BF")
})

test_that("error thrown if zip contains non PJNZ files", {
  error <- expect_error(unroll_pjnz(file.path("refdata", "invalid_files.zip")))
  expect_equal(error$message, "Zip contains no PJNZ files")
})


test_that("extract_pjnz_naomi() returns country and region name", {

  ## Test when using subnational Spectrum file
  pjnz_zone <- system_file("extdata/demo-subnational-pjnz/demo_mwi2019_region-pjnz.zip")
  spec <- extract_pjnz_naomi(pjnz_zone)

  expect_setequal(spec$iso3, "MWI")
  expect_setequal(spec$spectrum_country, "Malawi")
  expect_setequal(spec$spectrum_region_code, c(10, 11, 12))
  expect_setequal(spec$spectrum_region_name, c("Northern Region", "Central Region", "Southern Region"))

  ## Test when using national Spectrum file
  pjnz_nat <- system_file("extdata/demo_mwi2019.PJNZ")
  spec <- extract_pjnz_naomi(pjnz_nat)

  expect_setequal(spec$iso3, "MWI")
  expect_setequal(spec$spectrum_country, "Malawi")
  expect_setequal(spec$spectrum_region_code, 0)
  expect_setequal(spec$spectrum_region_name, "Malawi")
})


test_that("extract_pjnz_program_data() returns complete data", {

  ## Test files created with Spectrum v5.87 -- mid-year population projection
  pjnz_old <- system_file("extdata/demo_mwi2019.PJNZ")
  dat_old <- extract_pjnz_program_data(pjnz_old)

  expect_true(all(!is.na(dat_old$art_dec31$art_dec31)))
  expect_setequal(dat_old$art_dec31$sex, c("both", "female", "male"))
  expect_setequal(dat_old$art_dec31$year, 1970:2025)
  expect_setequal(dat_old$art_dec31$age_group, c("Y000_014", "Y015_999"))
  expect_setequal(dat_old$art_dec31$spectrum_region_code, 0)

  expect_setequal(dat_old$anc_testing$spectrum_region_code, 0)
  expect_setequal(dat_old$anc_testing$indicator[dat_old$anc_testing$year == 2018],
                  c("anc_clients", "anc_tested", "anc_tested_pos", "anc_known_pos", "anc_already_art"))
  expect_setequal(dat_old$anc_testing$indicator[dat_old$anc_testing$year == 2019],
                  c("anc_already_art"))
  expect_true(all(!is.na(dat_old$anc_testing$value)))


  ## Test files created with Spectrum v6.2 Beta 25 -- calendar year population projection
  pjnz_new <- system_file("extdata/demo_mwi2019_v6.2.PJNZ")
  dat_new <- extract_pjnz_program_data(pjnz_new)

  expect_true(all(!is.na(dat_new$art_dec31$art_dec31)))
  expect_setequal(dat_new$art_dec31$sex, c("both", "female", "male"))
  expect_setequal(dat_new$art_dec31$year, 1970:2025)
  expect_setequal(dat_new$art_dec31$age_group, c("Y000_014", "Y015_999"))
  expect_setequal(dat_new$art_dec31$spectrum_region_code, 0)

  expect_setequal(dat_new$anc_testing$spectrum_region_code, 0)
  expect_setequal(dat_new$anc_testing$indicator[dat_new$anc_testing$year == 2018],
                  c("anc_clients", "anc_tested", "anc_tested_pos", "anc_known_pos", "anc_known_neg", "anc_already_art"))
  expect_setequal(dat_new$anc_testing$indicator[dat_new$anc_testing$year == 2019],
                  c("anc_already_art"))
  expect_true(all(!is.na(dat_new$anc_testing$value)))

})


test_that("extract_pjnz_naomi(..., extract_shiny90 = FALSE) returns PLHIV - ART", {

  ## Test when using national Spectrum file
  pjnz_nat <- system_file("extdata/demo_mwi2019.PJNZ")
  spec <- extract_pjnz_naomi(pjnz_nat, extract_shiny90 = FALSE)

  expect_equal(spec$unaware, spec$hivpop - spec$artpop)
})


test_that("get_pjnz_shiny90_filename() handles multiple .shiny90 files", {

  pjnz_test <- system_file("extdata/demo_mwi2019_v6.21-shiny90.PJNZ")

  tmpd <- tempfile()
  dir.create(tmpd)
  shiny90file <- get_pjnz_shiny90_filename(pjnz_test)
  utils::unzip(pjnz_test, shiny90file, exdir = tmpd)

  shiny90dup <- "Malawi.zip duplicated.shiny90"
  file.copy(file.path(tmpd, shiny90file), file.path(tmpd, shiny90dup))
  shiny90dup_zip <- tempfile(fileext = ".PJNZ")
  file.copy(pjnz_test, shiny90dup_zip)
  zip(shiny90dup_zip, file.path(tmpd, shiny90dup), flags = "-j")

  func <- function() {
    list(shiny90new = get_pjnz_shiny90_filename(shiny90dup_zip))
  }
  w <- handle_naomi_warnings(func())

  expect_equal(w$warnings[[1]]$text, "Multiple .shiny90 files found: Malawi.zip.shiny90, Malawi.zip duplicated.shiny90
Using file: Malawi.zip.shiny90")
  expect_equal(w$warnings[[1]]$locations, c("model_options", "model_fit"))
  expect_s3_class(w$warnings[[1]], "naomi_warning")
  expect_s3_class(w$warnings[[1]], "condition")

  expect_equal(w$shiny90new, "Malawi.zip.shiny90")
})

test_that("ART adjustments properly extracted from Spectrum", {

  dp_path <- file.path("refdata/ART_adjustment_test.DP")

  dp <- as.data.frame(
    readr::read_csv(dp_path,
                    name_repair = "minimal",
                    col_types = readr::cols(.default = "c")))

  spec_art <- read_dp_art_dec31(dp)
  rownames(spec_art) <- seq_len(nrow(spec_art))

  # Test ART adjustments correctly read from test file with
  expect_equal(colnames(spec_art), c("sex", "age_group", "year",
                                     "art_dec31_reported", "art_dec31_attend","art_dec31_reside"))
  # Adults
  expect_setequal(spec_art[166,], c("male", "Y015_999", "2022", "450799", "387903", "377903"))
  expect_setequal(spec_art[167,], c("female", "Y015_999", "2022", "857994", "719095", "707095"))
  # Children
  expect_setequal(spec_art[52,], c("both", "Y000_014", "2021", "59853", "49368", "44368"))

})
