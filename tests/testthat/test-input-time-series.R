test_that("data can be formatted for ART input time series", {
  data <- prepare_input_time_series_art(a_hintr_data$art_number,
                                        a_hintr_data$shape)

  expect_true(nrow(data) > 100) ## Check that we have read out some data
  expect_setequal(colnames(data),
                  c("area_id", "area_name", "area_level_label",
                    "time_step", "time_period", "plot", "value"))
})

test_that("data can be formatted for ANC input time series", {
  data <- prepare_input_time_series_anc(a_hintr_data$anc_testing,
                                        a_hintr_data$shape)
  expect_true(nrow(data) > 100) ## Check that we have read out some data
  expect_setequal(colnames(data),
                  c("area_id", "area_name", "area_level", "area_level_label",
                    "age_group", "time_period", "time_step", "plot", "value"))
})
