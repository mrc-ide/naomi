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

test_that("hintr data can be saved and read as qs or duckdb type", {
  t_qs <- tempfile(fileext = ".qs")
  t_db <- tempfile(fileext = ".duckdb")
  t_rds <- tempfile(fileext = ".rds")
  d <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))

  hintr_save(d, t_qs)
  hintr_save(d, t_db)
  ## Can't use hintr_save for rds as we're no longer serialising as rds
  ## but we could still have historic data saved as rds so we need to
  ## be able to read it
  saveRDS(d, t_rds)
  expect_equal(read_hintr_output(t_rds), d)
  expect_equal(read_hintr_output(t_qs), read_hintr_output(t_db))
  expect_equal(read_hintr_output(t_rds), read_hintr_output(t_qs))

  t <- tempfile(fileext = ".thing")
  expect_error(hintr_save(d, t),
               "Cannot save as type 'thing', must be 'qs' or 'duckdb'.")

  x <- list(1, 2, 3)
  expect_error(hintr_save(x, t_db),
               paste("Trying to save invalid object as duckdb database.",
                     "Only data frames can be saved as database."))

  expect_error(read_hintr_output(t),
               paste("Cannot read hintr data of invalid type, got 'thing',",
                     "must be one of rds, qs or duckdb."))
})
