context("report-plots")

test_that("map plot can be generated", {
  expect_no_error(map_outputs(a_output_indicators,
                              indicator = "plhiv",
                              age = "Y015_999",
                              sex_disag = "both",
                              start_colour_scale = "#8C000038",
                              end_colour_scale = "red4",
                              fig_title = "title PLHIV",
                              legend_title = "legend PLHIV"))
})
