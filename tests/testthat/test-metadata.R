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

  ## Add a test country to metadata
  meta <- naomi_read_csv(system_file("metadata", "colour_scales.csv"))
  meta <- rbind(meta, data.frame(iso3 = rep("test", 2),
                                 indicator = c("art_current", "prevalence"),
                                 colour = rep("colour", 2),
                                 min = rep(0, 2),
                                 max = rep(100, 2),
                                 invert_scale = rep(FALSE, 2),
                                 stringsAsFactors = FALSE))
  mockery::stub(get_colour_scale, "naomi_read_csv", meta)

  scale_test <- get_colour_scale(iso3 = "TEST")

  expect_true(all(get_meta_indicator()$indicator %in% scale_default$indicator))
  expect_true(all(get_meta_indicator()$indicator %in% scale_test$indicator))

  ## colour scale uses default when indicator missing
  expect_equal(sum(scale_test$iso3 == "test"), 2)
  expect_equal(sum(scale_test$iso3 == "default"), nrow(scale_test) - 2)
})

test_that("getting scale for missing country returns default data", {
  missing <- get_colour_scale("missing")
  expect_equal(names(missing),
               c("iso3", "indicator", "colour", "min", "max","invert_scale"))
  expect_equal(missing, get_colour_scale("default"))
})

test_that("default configuration missing throws an error", {
  mock_naomi_read_csv <- mockery::mock(
    data.frame(iso3 = c("MWI"), indicator = c("Test"),
               stringsAsFactors = FALSE))
  with_mock(naomi_read_csv = mock_naomi_read_csv, {
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
  expect_true(all(metadata$indicator %in% get_metadata()$indicator))
  expect_true(all(get_meta_indicator()$indicator %in% metadata$indicator))
})

test_that("can get plot metadata for missing country with defaults", {
  metadata <- testthat::evaluate_promise(get_plotting_metadata("missing"))
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
                    "untreated_plhiv_num", "plhiv_attend", "untreated_plhiv_attend",
                    "aware_plhiv_prop", "aware_plhiv_num", "unaware_plhiv_num",
                    "aware_plhiv_attend", "unaware_plhiv_attend",
                    "anc_prevalence", "anc_art_coverage",
                    "anc_clients", "anc_plhiv", "anc_already_art",
                    "anc_art_new", "anc_known_pos", "anc_tested_pos",
                    "anc_tested_neg", "art_new", "vl_tested_12mos",
                    "vl_suppressed_12mos", "population_ratio", "plhiv_ratio",
                    "infections_ratio", "art_current_ratio",
                    "unaware_plhiv_num_ratio", "prevalence_ratio",
                    "art_coverage_ratio", "aware_plhiv_prop_ratio",
                    "incidence_ratio", "anc_prevalence_age_matched",
                    "anc_art_coverage_age_matched", "number_on_art",
                    "anc_tested", "population_proportion"))
})

test_that("colour scales metadata is well formed", {
  scales <- naomi_read_csv(system_file("metadata", "colour_scales.csv"))
  expect_setequal(scales$indicator,
    c("art_coverage", "art_current", "art_current_residents", "prevalence",
      "viral_suppression_plhiv", "recent_infected",
      "plhiv", "incidence", "population", "infections",
      "untreated_plhiv_num", "plhiv_attend", "untreated_plhiv_attend",
      "aware_plhiv_num", "aware_plhiv_prop", "unaware_plhiv_num",
      "aware_plhiv_attend", "unaware_plhiv_attend",
      "anc_prevalence", "anc_art_coverage", "anc_clients", "anc_plhiv",
      "anc_already_art", "anc_art_new", "anc_known_pos", "anc_tested_pos",
      "anc_tested_neg", "art_new", "vl_tested_12mos", "vl_suppressed_12mos",
      "population_ratio", "plhiv_ratio", "infections_ratio",
      "art_current_ratio", "unaware_plhiv_num_ratio", "prevalence_ratio",
      "art_coverage_ratio", "aware_plhiv_prop_ratio", "incidence_ratio",
      "anc_prevalence_age_matched", "anc_art_coverage_age_matched",
      "number_on_art", "anc_tested", "population_proportion"))
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
  expect_setequal(meta$indicator, c(
    "art_coverage", "art_current", "art_current_residents", "prevalence",
    "viral_suppression_plhiv", "recent_infected", "plhiv",
    "population", "incidence", "infections",
    "untreated_plhiv_num", "plhiv_attend", "untreated_plhiv_attend",
    "aware_plhiv_prop", "aware_plhiv_num", "unaware_plhiv_num",
    "aware_plhiv_attend", "unaware_plhiv_attend",
    "anc_prevalence", "anc_art_coverage", "anc_clients",
    "anc_plhiv", "anc_already_art", "anc_art_new", "anc_known_pos",
    "anc_tested_pos", "anc_tested_neg", "art_new", "vl_tested_12mos",
    "vl_suppressed_12mos", "population_ratio", "plhiv_ratio",
    "infections_ratio", "art_current_ratio", "unaware_plhiv_num_ratio",
    "prevalence_ratio", "art_coverage_ratio", "aware_plhiv_prop_ratio",
    "incidence_ratio", "anc_prevalence_age_matched",
    "anc_art_coverage_age_matched", "number_on_art", "anc_tested",
    "population_proportion"))
  expect_equal(nrow(unique(meta[, c("data_type", "plot_type", "indicator")])),
               nrow(meta))
  expect_true(all(meta$plot_type %in% c("choropleth", "barchart", "pyramid")))
  expect_true(all(meta$data_type %in%
                    c("survey", "anc", "programme", "output", "calibrate",
                      "comparison", "input_comparison", "population")))
  expect_setequal(meta$name,
                  c("HIV prevalence", "ART coverage", "Viral load suppression",
                    "Proportion recently infected", "PLHIV", "Population",
                    "New infections", "HIV incidence", "ART number (residents)",
                    "ART number (attending)",
                    "PLHIV not on ART", "PLHIV (ART catchment)", "Untreated PLHIV (ART catchment)",
                    "Number PLHIV aware", "Proportion PLHIV aware", "Number PLHIV unaware",
                    "Number Aware PLHIV (ART catchment)", "Number unaware PLHIV (ART catchment)",
                    "ANC HIV prevalence",
                    "ANC prior ART coverage", "ANC clients",
                    "HIV positive ANC attendees",
                    "ANC attendees already on ART", "ART initiations at ANC",
                    "ANC known positive", "ANC tested positive", "ANC tested negative",
                    "ART new", "VL tested", "VL tests suppressed",
                    "Population ratio", "PLHIV ratio", "New infections ratio",
                    "ART number (attending) ratio",
                    "Number PLHIV unaware ratio", "HIV prevalence ratio",
                    "ART coverage ratio", "Proportion PLHIV aware ratio",
                    "Incidence ratio", "ANC prevalence age matched",
                    "ANC ART coverage age matched", "Number on ART",
                    "ANC tested", "Population proportion"))
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

  ## meta_indicator is used for the output zip and naomi needs
  ## metadata is used for hintr plotting. We want to include
  ## info about all output values in meta_indicator, but not
  ## input metadata so strip out any input data types
  check <- metadata %>%
    dplyr::filter(!(data_type %in% c("survey", "programme", "anc"))) %>%
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

  reset <- naomi_set_language("pt")
  on.exit(reset())
  metadata <- get_metadata()
  expect_equal(metadata[1, "name"], "Prevalência de VIH")
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
  expect_setequal(meta$format[meta$indicator == "aware_plhiv_prop"], "0.0%")

  ## Also, 0.0 gets parsed by Excel as 0. To avoid this, 0.0 for the incidence
  ## indicator is wrapped in quotes. This checks regression against that.
  expect_setequal(meta$format[meta$indicator == "incidence"], "0.0")

})

test_that("time series metadata format column hasn't been messed by Excel", {

  ## When opening inst/metadata/time_series_plot_metadata.csv in MS Excel,
  ## the format column is 'helpfully' parsed and converts 0.0% to a generic
  ## percentage formatted cell.
  ## The value 0.0% is displayed as 0.00% (perhaps dependent on local settings),
  ## and when resaved as CSV 0.0% is saved as 0.00%.
  ##
  ## Also, 0.0 gets parsed by Excel as 0. To avoid this, 0.0 for the incidence
  ## indicator is wrapped in quotes. This checks regression against that.
  ##
  ## This test exist to make sure this hasn't happened inadvertently.
  ## Be thoughtful before idly updating the values in these tests to make the
  ## test pass!

  meta <- get_plot_type_column_metadata("art_current")
  expect_equal(meta[[1]]$format, "0,0")
  meta <- get_plot_type_column_metadata("anc_prevalence")
  expect_equal(meta[[1]]$format, "0.0%")
  meta <- get_plot_type_column_metadata("anc_art_coverage")
  expect_equal(meta[[1]]$format, "0%")
  meta <- get_plot_type_column_metadata("vl_prop_suppressed_total")
  expect_equal(meta[[1]]$format, "0%")
  meta <- get_plot_type_column_metadata("vl_prop_suppressed_adult_f")
  expect_equal(meta[[1]]$format, "0%")
  meta <- get_plot_type_column_metadata("vl_prop_suppressed_adult_m")
  expect_equal(meta[[1]]$format, "0%")
  meta <- get_plot_type_column_metadata("vl_prop_suppressed_adult")
  expect_equal(meta[[1]]$format, "0%")
  meta <- get_plot_type_column_metadata("vl_prop_suppressed_child")
  expect_equal(meta[[1]]$format, "0%")
  meta <- get_plot_type_column_metadata("vl_coverage_total")
  expect_equal(meta[[1]]$format, "0%")
  meta <- get_plot_type_column_metadata("vl_coverage_adult_f")
  expect_equal(meta[[1]]$format, "0%")
  meta <- get_plot_type_column_metadata("vl_coverage_adult_m")
  expect_equal(meta[[1]]$format, "0%")
  meta <- get_plot_type_column_metadata("vl_coverage_adult")
  expect_equal(meta[[1]]$format, "0%")
  meta <- get_plot_type_column_metadata("vl_coverage_child")
  expect_equal(meta[[1]]$format, "0%")
  meta <- get_plot_type_column_metadata("births_clients_ratio")
  expect_equal(meta[[1]]$format, "0%")
})

test_that("there is metadata for each indicator once per plot type", {
  metadata <- get_metadata()
  ## Each indicator is displayed only once
  all <- metadata[, c("data_type", "plot_type", "value_column",
                      "indicator_column", "indicator_value")]
  expect_equal(nrow(all), nrow(unique(all)))
})
