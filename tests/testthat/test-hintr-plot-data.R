test_that("calibrate plot data can be generated", {
  plot_data <- hintr_calibrate_plot(a_hintr_output)
  expect_setequal(names(plot_data),
                  c("spectrum_region_code", "spectrum_region_name", "sex",
                    "age_group", "calendar_quarter", "indicator", "mean",
                    "data_type"))
  expect_setequal(unique(plot_data$spectrum_region_name),
                  c("National", "Northern Region", "Central Region",
                    "Southern Region"))
  expect_setequal(unique(plot_data$indicator),
                  c("art_current",
                    "infections", "plhiv", "population", "unaware_plhiv_num",
                    "prevalence", "art_coverage",
                    "aware_plhiv_prop", "incidence"))

  indicators <- read_hintr_output(a_hintr_output$model_output_path)$output_package$indicators
  expect_true(all(plot_data$indicator %in% indicators$indicator))
})

test_that("comparison plot data can be generated", {
  plot_data <- hintr_comparison_plot(a_hintr_output)
  expect_true(all(c("area_id", "area_name", "age_group", "sex",
                    "calendar_quarter", "indicator", "source", "mean",
                    "lower", "upper") %in% colnames(plot_data)))
  expect_true(nrow(plot_data) > 200)
})

test_that("comparison plot returns useful error if run with old naomi output", {
  hintr_output <- a_hintr_output
  hintr_output$version <- "2.7.0"
  expect_error(hintr_comparison_plot(hintr_output),
               "Model output out of date please re-run model and try again.")
})

test_that("comparison plot returns useful error if no input output data", {
  t <- tempfile(fileext = ".qs")
  output_data <- read_hintr_output(a_hintr_output$model_output_path)
  output_data$output_package$inputs_outputs <- NULL
  hintr_save(output_data, t)
  hintr_output <- a_hintr_output
  hintr_output$model_output_path <- t
  expect_error(hintr_comparison_plot(hintr_output),
               "Model output out of date please re-run model and try again.")
})

test_that("there is metadata for every indicator in comparison data", {
  plot_data <- hintr_comparison_plot(a_hintr_output)
  indicators <- unique(plot_data$indicator)

  metadata <- get_metadata()
  comparison <- metadata[metadata$data_type == "comparison" &
                           metadata$plot_type == "barchart", ]
  expect_setequal(indicators, comparison$indicator)
})
