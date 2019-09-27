context("metadata")

test_that("can retrieve indicator colour scales for a country", {
  scale <- get_colour_scale("Malawi")
  expect_true(all(c("country", "indicator", "colour", "min", "max",
                    "invert_scale") %in% names(scale)))
  expect_true(all(scale$country == "Malawi"))
})

test_that("getting scale for missing indicator or country returns error", {
  expect_error(get_colour_scale("missing"),
    "Can't retrieve colour scale for country missing. Country not found in configuration.")
})

test_that("can get plot metadata for a country", {
  metadata <- get_plotting_metadata("Malawi")
  expect_true(all(
    c("indicator", "data_type", "plot_type", "value_column", "indicator_column",
      "indicator_value", "name", "colour", "min", "max", "invert_scale") %in%
      names(metadata)))
  expect_true(all(unique(metadata$indicator) %in%
                    c("art_coverage", "current_art",  "prevalence")))
})
