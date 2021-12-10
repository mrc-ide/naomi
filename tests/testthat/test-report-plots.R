context("report-plots")

test_that("map plot can be generated", {
  expect_no_error(map_outputs(a_output_indicators,
                              indicator = "plhiv",
                              age = "Y015_999",
                              sex_disag = "both",
                              calendar_quarter = "CY2018Q4",
                              colour_palette = "red",
                              fig_title = "title PLHIV",
                              legend_title = "legend PLHIV"))
})

test_that("pop pyramid plot can be generated", {
  model_output <- readRDS(a_hintr_output$model_output_path)
  expect_no_error(pop_pyramid_outputs(model_output$output_package,
                                      indicator = "plhiv",
                                      colour_palette = "red",
                                      x_title = "PLHIV",
                                      fig_title = "legend PLHIV"))
})

test_that("map plot can be generated", {
  model_output <- readRDS(a_hintr_output$model_output_path)
  expect_no_error(district_barplot(model_output$output_package,
                                   indicator = "prevalence",
                                   age = "Y015_049",
                                   sex_disag = "both",
                                   label_format = scales::percent_format(1),
                                   colour_palette  = "red",
                                   x_title = "x axis HIV",
                                   fig_title = "title HIV",
                                   legend_title = "legend HIV"))
})
