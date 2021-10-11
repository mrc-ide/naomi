test_that("naomi warning can be raised", {
  w <- capture_warning(
    naomi_warning("my warning", c("fit_model", "review_output")))
  expect_equal(w$message, "my warning")
  expect_equal(w$location, c("fit_model", "review_output"))
  expect_s3_class(w, "naomi_warning")
  expect_s3_class(w, "warning")
  expect_s3_class(w, "condition")
})

test_that("error thrown if location unknown", {
  expect_error(naomi_warning("my warning", "custom_location"),
               paste0("locations must be one of 'model_options', ",
                      "'fit_model', 'calibrate_model', 'review_output'"))
})
