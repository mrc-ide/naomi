context("metadata")

test_that("can retrieve indicator colour scales for a country", {
  scale <- get_colour_scale("prevalence", "Malawi")
  expect_equal(scale$colour, "interpolateMagma")
  expect_equal(scale$invert_scale, TRUE)
  expect_equal(scale$min, 0)
  expect_equal(scale$max, 0.5)
})

test_that("getting scale for missing indicator or country returns error", {
  expect_error(get_colour_scale("missing", "Malawi"),
    "Can't retrieve colour scale for country Malawi and indicator missing.
Indicator and country combination not found in configuration.")
  expect_error(get_colour_scale("prevalence", "missing"),
               "Can't retrieve colour scale for country missing and indicator prevalence.
Indicator and country combination not found in configuration.")
})

test_that("return useful error if scale is misconfigured", {
  mock_naomi_read_csv <- mockery::mock(data.frame(
    country = c("Malawi", "Malawi"),
    indicator = c("prevalence", "prevalence")
  ))
  with_mock("naomi:::naomi_read_csv" = mock_naomi_read_csv, {
    expect_error(get_colour_scale("prevalence", "Malawi"),
                 "Found more than one colour scale for country Malawi and indicator prevalence.
Check configuration, each country and indicator combination should have 1 and only 1 entry.")
  })
})
