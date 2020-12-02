context("metadata")

test_that("can retrieve indicator colour scales for a country", {
  scale <- get_colour_scale("MWI")
  expect_true(all(c("iso3", "indicator", "colour", "min", "max",
                    "invert_scale") %in% names(scale)))
})

test_that("can retrieve default colour scales", {
  scale <- get_colour_scale()
  expect_true(all(c("iso3", "indicator", "colour", "min", "max",
                    "invert_scale") %in% names(scale)))
  expect_true(all(scale$iso3 == "default"))
})

test_that("color scales are retrieved for all output indicators", {

  scale_default <-  get_colour_scale()
  scale_mwi <- get_colour_scale(iso3 = "junk")

  expect_true(all(get_meta_indicator()$indicator %in% scale_default$indicator))
  expect_true(all(get_meta_indicator()$indicator %in% scale_mwi$indicator))
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
                  c("art_coverage", "art_current", "art_current_residents",
                    "prevalence",
                    "incidence", "infections", "plhiv", "population",
                    "recent_infected", "viral_suppression_plhiv",
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
  expect_setequal(metadata$result$indicator,
                  c("art_coverage", "art_current", "art_current_residents",
                    "prevalence",
                    "incidence", "infections", "plhiv", "population",
                    "recent_infected", "viral_suppression_plhiv",
                    "anc_prevalence", "anc_art_coverage",
                    "anc_clients", "anc_plhiv", "anc_already_art",
                    "anc_art_new", "anc_known_pos", "anc_tested_pos",
                    "anc_tested_neg"))
})

test_that("colour scales metadata is well formed", {
  scales <- naomi_read_csv(system_file("metadata", "colour_scales.csv"))
  expect_setequal(scales$indicator,
    c("art_coverage", "art_current", "art_current_residents", "prevalence",
      "viral_suppression_plhiv", "recent_infected",
      "plhiv", "incidence", "population", "infections",
      "anc_prevalence", "anc_art_coverage", "anc_clients", "anc_plhiv",
      "anc_already_art", "anc_art_new", "anc_known_pos", "anc_tested_pos",
      "anc_tested_neg"))
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
  expect_setequal(meta$indicator,
                  c("art_coverage", "art_current", "art_current_residents", "prevalence",
                    "viral_suppression_plhiv", "recent_infected", "plhiv",
                    "population", "incidence", "infections", "anc_prevalence",
                    "anc_art_coverage", "anc_clients", "anc_plhiv", "anc_already_art",
                    "anc_art_new", "anc_known_pos", "anc_tested_pos", "anc_tested_neg"))
  expect_equal(nrow(unique(meta[, c("data_type", "plot_type", "indicator")])),
               nrow(meta))
  expect_true(all(meta$plot_type %in% c("choropleth", "barchart")))
  expect_true(all(meta$data_type %in% c("survey", "anc", "programme", "output")))
  expect_setequal(meta$name,
                  c("HIV prevalence", "ART coverage", "Viral load suppression",
                    "Proportion recently infected", "PLHIV", "Population",
                    "New infections", "HIV incidence", "ART number (residents)",
                    "ART number (attending)", "ANC HIV prevalence",
                    "ANC prior ART coverage", "ANC clients",
                    "HIV positive ANC attendees",
                    "ANC attendees already on ART", "ART initiations at ANC",
                    "ANC known positive", "ANC tested positive", "ANC tested negative"))
  expect_equal(
    colnames(meta),
    c("data_type", "plot_type", "indicator", "value_column", "error_low_column",
      "error_high_column", "indicator_column", "indicator_value", "name",
      "scale", "accuracy", "format", "indicator_sort_order"))
  ## No NULLs, NAs or empty strings except for indicator_column,
  ## indicator_value and accuracy columns
  non_empty_columns <- colnames(
    meta[, !(colnames(meta) %in% c("error_low_column", "error_high_column",
                                   "indicator_column", "indicator_value",
                                   "accuracy", "indicator_sort_order"))])
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
  expect_equal(age_groups[1], "Y000_004")
})

## !!!! TODO: metadata.csv and the meta_indicator data.frame  should not
## exist separately. Long-term refactor to consolidate.
test_that("metadata synced with meta_indicator", {

  metadata <- get_metadata()

  check <- metadata %>%
    dplyr::filter(data_type == "output") %>%
    dplyr::distinct(name, indicator_value) %>%
    dplyr::full_join(
             get_meta_indicator() %>%
             dplyr::select(indicator, indicator_label),
             by = c("indicator_value" = "indicator")
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

test_that("metadata format column hasn't been messed by Excel", {

  ## When opening inst/metadata/metadata.csv in MS Excel, the format column is
  ## 'helpfully' parsed and converts 0.0% to a generic percentage formatted cell.
  ## The value 0.0% is displayed as 0.00% (perhaps dependent on local settings),
  ## and when resaved as CSV 0.0% is saved as 0.00%.
  ##
  ## This test exist to make sure this hasn't happened inadvertently.
  ## Be thoughtful before idly updating the values in these tests to make the test
  ## pass!

  meta <- get_metadata()

  expect_setequal(meta$format[meta$indicator == "prevalence"], "0.0%")
  expect_setequal(meta$format[meta$indicator == "art_coverage"], "0.0%")
  expect_setequal(meta$format[meta$indicator == "anc_prevalence"], "0.0%")
  expect_setequal(meta$format[meta$indicator == "anc_art_coverage"], "0.0%")
  expect_setequal(meta$format[meta$indicator == "recent_infected"], "0.00%")
})
