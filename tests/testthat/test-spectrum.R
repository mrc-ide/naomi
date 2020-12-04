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
