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

test_that("colour scales metadata is well formed", {
  scales <- naomi_read_csv(system_file("extdata", "meta", "colour_scales.csv"))
  expect_true(all(scales$indicator %in%
    c("art_coverage", "current_art", "prevalence", "vls", "recent", "plhiv", "incidence")))
  expect_equal(nrow(unique(scales[, c("country", "indicator")])), nrow(scales))
  expect_true(is.numeric(scales$min))
  expect_true(is.numeric(scales$max))
  expect_true(all(scales$min >= 0))
  expect_true(all(scales$min < scales$max))
  expect_type(scales$invert_scale, "logical")
  expect_type(scales$colour, "character")
  ## No NULLs, NAs or empty strings
  non_empty <- function(x) {
    !is.null(x) & !is.na(x) & !(x == "")
  }
  expect_true(all(
    vapply(colnames(scales), function(column) {
      all(non_empty(column))
    }, logical(1))))
})

test_that("metadata is well formed", {
  meta <- get_metadata()
  expect_true(all(meta$indicator %in%
    c("art_coverage", "current_art", "prevalence", "vls", "recent", "plhiv", "incidence")))
  expect_equal(nrow(unique(meta[, c("data_type", "plot_type", "indicator")])),
               nrow(meta))
  expect_true(all(meta$plot_type == "choropleth"))
  expect_true(all(meta$data_type %in% c("survey", "anc", "programme", "output")))
  expect_true(all(meta$name %in% c("Prevalence", "ART coverage", "Number on ART")))
  ## No NULLs, NAs or empty strings except for indicator_column and
  ## indicator_value columns
  non_empty_columns <- colnames(
    meta[, !(colnames(meta) %in% c("indicator_column", "indicator_value"))])
  non_empty <- function(x) {
    !is.null(x) & !is.na(x) & !(x == "")
  }
  expect_true(all(
    vapply(non_empty_columns, function(column) {
      all(non_empty(meta[, column]))
    }, logical(1))))
})
