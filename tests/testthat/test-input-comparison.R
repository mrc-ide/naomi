test_that("ART data is properly aggreagted for Spectrum comparison table", {


  # Create test data with sex disaggreagted adults on ART
  art <- a_hintr_data$art_number
  art_dat <- naomi::read_art_number(art)
  art_adult_female <- art_dat |> dplyr::filter(age_group == "Y015_999") |>
    dplyr::mutate(sex = "female", art_current = 0.60 * art_current)
  art_adult_male <- art_dat |> dplyr::filter(age_group == "Y015_999") |>
    dplyr::mutate(sex = "male", art_current = 0.40* art_current)
  art_pead <- art_dat |> dplyr::filter(age_group == "Y000_014")
  art_sexdiff <- dplyr::bind_rows(art_adult_female, art_adult_male, art_pead)

  # Test that aggregation works with subnational pjnz and sex disaggreagted adults on ART
  shape <- a_hintr_data$shape
  pjnz <- a_hintr_data$pjnz

  x <- prepare_art_spectrum_comparison(art_sexdiff, shape, pjnz)

  expect_equal(unique(x$group), c("art_children", "art_adult_female",
                                  "art_adult_male"))
  expect_equal(unique(x$area_name), c("Northern", "Central", "Southern"))

  # Test that aggregation works with national pjnz and sex aggregated adults on ART
  shape <- system_file("extdata/demo_areas.geojson")
  pjnz <- system_file("extdata/demo_mwi2019.PJNZ")

  x <- prepare_art_spectrum_comparison(art, shape, pjnz)

  expect_equal(unique(x$group), c("art_children", "art_adult_both"))
  expect_equal(unique(x$area_name), c("Malawi - Demo"))

  # TO DO: Ensure ART adjustments in Spectrum are applied
  # Make a test "pjnz" file

})

test_that("ANC data is properly aggreagted for Spectrum comparison table", {

  # Test that aggregation works with subnational pjnz
  shape <- a_hintr_data$shape
  pjnz <- a_hintr_data$pjnz
  anc <- a_hintr_data$anc_testing

  x <- prepare_anc_spectrum_comparison(anc, shape, pjnz)

  expect_equal(unique(x$indicator), c("anc_already_art", "anc_clients", "anc_known_neg",
                                      "anc_known_pos", "anc_tested" , "anc_tested_pos"))
  expect_equal(unique(x$group), c("anc_adult_female"))
  expect_equal(unique(x$area_name), c("Northern", "Central", "Southern"))

  # Test that aggregation works with national pjnz
  shape <- system_file("extdata/demo_areas.geojson")
  pjnz <- system_file("extdata/demo_mwi2019.PJNZ")

  x <- prepare_anc_spectrum_comparison(anc, shape, pjnz)


  expect_equal(unique(x$indicator), c("anc_already_art", "anc_clients",
                                      "anc_known_pos", "anc_tested" , "anc_tested_pos"))
  expect_equal(unique(x$group), c("anc_adult_female"))
  expect_equal(unique(x$area_name), c("Malawi - Demo"))

})

test_that("Comparison wrapper function works with missing programme data", {

  # Test wrapper function with all programme data supplied
  shape <- a_hintr_data$shape
  pjnz <- a_hintr_data$pjnz
  anc <- a_hintr_data$anc_testing
  art <- a_hintr_data$art_number

  x <- prepare_spectrum_naomi_comparison(art, anc, shape, pjnz)

  expect_equal(unique(x$indicator), c("number_on_art", "anc_already_art", "anc_clients",
                                      "anc_known_neg", "anc_known_pos", "anc_tested" , "anc_tested_pos"))
  expect_equal(unique(x$group), c("art_children", "art_adult_both", "anc_adult_female"))

  # Test wrapper function with no ART
  art <- NULL

  x <- prepare_spectrum_naomi_comparison(art, anc, shape, pjnz)

  expect_equal(unique(x$indicator), c("anc_already_art", "anc_clients", "anc_known_neg",
                                      "anc_known_pos", "anc_tested" , "anc_tested_pos"))
  expect_equal(unique(x$group), c("anc_adult_female"))

  # Test wrapper function with no ANC
  art <- a_hintr_data$art_number
  anc <- NULL

  x <- prepare_spectrum_naomi_comparison(art, anc, shape, pjnz)

  expect_equal(unique(x$indicator), c("number_on_art"))
  expect_equal(unique(x$group), c("art_children", "art_adult_both"))

  # Test wrapper function with no programme data
  art <- NULL

  x <- prepare_spectrum_naomi_comparison(art, anc, shape, pjnz)
  expect_equal(nrow(x), 0)

})

test_that("art data comparison returns value for only last CQ within each year", {
  # Create test data with a 2 rows for the same area, sex, and age group
  # but with a different calendar quarter. We should be returning the latest
  # quarter of these
  art <- a_hintr_data$art_number
  art_dat <- naomi::read_art_number(art)
  art_dat_row <- art_dat[art_dat$area_id == "MWI_2_1_demo" &
                                art_dat$sex == "both" &
                                art_dat$age_group == "Y000_014" &
                                art_dat$calendar_quarter == "CY2011Q4", ]
  expected_value <- art_dat_row$art_current
  art_dat_row$calendar_quarter <- quarter_id_to_calendar_quarter(
    calendar_quarter_to_quarter_id(art_dat_row$calendar_quarter) - 1)
  ## We're expecting we don't see this in the aggregated data
  art_dat_row$art_current <- 10000

  art_test <- dplyr::bind_rows(art_dat, art_dat_row)

  # Test that aggregation works with subnational pjnz and sex disaggreagted adults on ART
  shape <- a_hintr_data$shape
  pjnz <- a_hintr_data$pjnz

  x <- prepare_art_spectrum_comparison(art_test, shape, pjnz)

  modified_row <- x[x$indicator == "number_on_art" &
                      x$year == "2011" &
                      x$group == "art_children" &
                      x$area_name == "Northern", ]

  expect_equal(modified_row$value_naomi, expected_value)
})
