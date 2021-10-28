test_that("naomi warning can be raised", {
  w <- capture_condition(
    naomi_warning("my warning", c("model_fit", "review_output")))
  expect_equal(w$text, "my warning")
  expect_equal(w$locations, c("model_fit", "review_output"))
  expect_s3_class(w, "naomi_warning")
  expect_s3_class(w, "condition")
})

test_that("error thrown if location unknown", {
  expect_error(naomi_warning("my warning", "custom_location"),
               paste0("locations must be one of 'model_options', ",
                      "'model_fit', 'model_calibrate', 'review_output', ",
                      "'download_results'"))
})

test_that("naomi warnings can be handled", {
  func <- function(x) {
    naomi_warning("my warning", c("model_fit", "review_output"))
    naomi_warning("second warning", "download_results")
    list(
      output = "return value"
    )
  }
  out <- handle_naomi_warnings(func())
  expect_equal(names(out), c("output", "warnings"))
  expect_equal(out$output, "return value")
  expect_length(out$warnings, 2)
  expect_equal(out$warnings[[1]]$text, "my warning")
  expect_equal(out$warnings[[1]]$locations, c("model_fit", "review_output"))
  expect_equal(out$warnings[[2]]$text, "second warning")
  expect_equal(out$warnings[[2]]$locations, "download_results")
})

test_that("naomi warning handler returns empty list when no warnings", {
  func <- function(x) {
    list(
      output = "return value"
    )
  }
  out <- handle_naomi_warnings(func())
  expect_equal(names(out), c("output", "warnings"))
  expect_equal(out$output, "return value")
  expect_length(out$warnings, 0)
})
