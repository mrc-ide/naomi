context("metadata")

test_that("can retrieve indicator colour scales for a country", {
  scale <- get_colour_scale("MWI")
  expect_true(all(c("iso3", "indicator", "colour", "min", "max",
                    "invert_scale") %in% names(scale)))
  expect_true(all(scale$iso3 == "MWI"))
})

test_that("can retrieve default colour scales", {
  scale <- get_colour_scale()
  expect_true(all(c("iso3", "indicator", "colour", "min", "max",
                    "invert_scale") %in% names(scale)))
  expect_true(all(scale$iso3 == "default"))
})

test_that("getting scale for missing country returns error empty data", {
  missing <- get_colour_scale("missing")
  expect_equal(names(missing),
               c("iso3", "indicator", "colour", "min", "max","invert_scale"))
  expect_equal(nrow(missing), 0)
})

test_that("default configuration missing throws an error", {
  mock_naomi_read_csv <- mockery::mock(
    data.frame(iso3 = c("MWI"), indicator = c("Test"),
               stringsAsFactors = FALSE))
  with_mock("naomi:::naomi_read_csv" = mock_naomi_read_csv, {
    expect_error(get_colour_scale(),
                 "Can't retrieve default colour scale. Check configuration.")
  })
})

test_that("can get plot metadata for a country", {
  metadata <- get_plotting_metadata("MWI")
  expect_true(all(
    c("indicator", "data_type", "plot_type", "value_column", "error_low_column",
      "error_high_column", "indicator_column", "indicator_value", "name",
      "colour", "min", "max", "invert_scale") %in%
      names(metadata)))
  expect_true(all(unique(metadata$indicator) %in%
                  c("art_coverage", "current_art", "receiving_art",
                    "prevalence", "art_number",
                    "incidence", "new_infections", "plhiv", "population",
                    "recent", "vls",
                    "anc_prevalence", "anc_art_coverage")))
})

test_that("can get plot metadata for missing country with defaults", {
  metadata <- testthat::evaluate_promise(get_plotting_metadata("missing"))
  expect_equal(metadata$messages,
    "Country with iso3 code missing not in metadata - returning default colour scales.\n")
  expect_true(all(
    c("indicator", "data_type", "plot_type", "value_column", "error_low_column",
      "error_high_column", "indicator_column", "indicator_value", "name",
      "colour", "min", "max", "invert_scale") %in%
      names(metadata$result)))
  expect_true(all(unique(metadata$result$indicator) %in%
                  c("art_coverage", "current_art", "receiving_art",
                    "prevalence", "art_number",
                    "incidence", "new_infections", "plhiv", "population",
                    "recent", "vls",
                    "anc_prevalence", "anc_art_coverage")))
})

test_that("colour scales metadata is well formed", {
  scales <- naomi_read_csv(system_file("metadata", "colour_scales.csv"))
  expect_true(all(scales$indicator %in%
    c("art_coverage", "current_art", "receiving_art", "prevalence", "vls", "recent",
      "art_number", "plhiv", "incidence", "population", "new_infections",
      "anc_prevalence", "anc_art_coverage")))
  expect_equal(nrow(unique(scales[, c("iso3", "indicator")])), nrow(scales))
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
    c("art_coverage", "current_art", "prevalence", "vls", "recent", "plhiv",
      "incidence", "art_number", "population", "incidence", "new_infections",
      "receiving_art", "anc_prevalence", "anc_art_coverage")))
  expect_equal(nrow(unique(meta[, c("data_type", "plot_type", "indicator")])),
               nrow(meta))
  expect_true(all(meta$plot_type %in% c("choropleth", "barchart")))
  expect_true(all(meta$data_type %in% c("survey", "anc", "programme", "output")))
  expect_true(all(meta$name %in%
                    c("HIV prevalence", "ART coverage", "Viral load suppression",
                      "Proportion recently infected", "PLHIV", "Population",
                      "New infections", "HIV incidence", "ART number (residents)",
                      "ART number (attending)", "ANC HIV prevalence",
                      "ANC prior ART coverage")))
  expect_equal(
    colnames(meta),
    c("data_type", "plot_type", "indicator", "value_column", "error_low_column",
      "error_high_column", "indicator_column", "indicator_value", "name",
      "scale", "accuracy", "format"))
  ## No NULLs, NAs or empty strings except for indicator_column,
  ## indicator_value and accuracy columns
  non_empty_columns <- colnames(
    meta[, !(colnames(meta) %in% c("error_low_column", "error_high_column",
                                   "indicator_column", "indicator_value",
                                   "accuracy"))])
  non_empty <- function(x) {
    !is.null(x) & !is.na(x) & !(x == "")
  }
  expect_true(all(
    vapply(non_empty_columns, function(column) {
      all(non_empty(meta[, column]))
    }, logical(1))))
})

test_that("can get 5 year age groups", {
  age_groups <- get_five_year_age_groups()
  expect_length(age_groups, 17)
  expect_equal(age_groups[1], "00-04")
})

## !!!! TODO: metadata.csv and the meta_indicator data.frame  should not
## exist separately. Long-term refactor to consolidate.
test_that("metadata synced with meta_indicator", {

  metadata <- get_metadata()

  check <- metadata %>%
    dplyr::filter(indicator_column == "indicator_id") %>%
    dplyr::distinct(name, indicator_value) %>%
    dplyr::mutate(indicator_value = as.integer(indicator_value)) %>%
    dplyr::full_join(
             get_meta_indicator() %>%
             dplyr::select(indicator_id, indicator_label),
             by = c("indicator_value" = "indicator_id")
           )

  expect_equal(tolower(check$name), tolower(check$indicator_label))
})

test_that("metadata can be translated", {
  reset <- naomi_set_language("fr")
  on.exit(reset())

  metadata <- get_metadata()
  expect_equal(metadata[1, "name"], "Prévalence du VIH")

  reset()
  metadata <- get_metadata()
  expect_equal(metadata[1, "name"], "HIV prevalence")
})

test_that("uncertainty metadata set for all model output data", {
  metadata <- get_metadata()
  output_meta <- metadata[metadata$data_type == "output", ]
  expect_true(all(!(output_meta$error_low_column == "")))
  expect_true(all(!(output_meta$error_high_column == "")))
})
