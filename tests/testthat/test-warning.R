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


test_that("warning raised after false convergence", {
  a_fit_bad <- a_fit
  a_fit_bad$convergence <- 1
  a_fit_bad$message <- "false convergence (8)"

  mock_fit_tmb <- mockery::mock(a_fit_bad)
  mock_sample_tmb <- mockery::mock(a_fit_sample)
  mock_output_package <- mockery::mock(a_output)

  with_mock(
    "naomi:::fit_tmb" = mock_fit_tmb,
    "naomi:::sample_tmb" = mock_sample_tmb,
    "naomi:::output_package" = mock_output_package, {
      out <- hintr_run_model(a_hintr_data, a_hintr_options, validate = FALSE)
    })

  expect_length(out$warnings, 1)
  expect_equal(out$warnings[[1]]$text,
               "Convergence error: false convergence (8)")
})


test_that("warning raised if art attend is not selected", {
  options <- a_hintr_options
  options$artattend <- "false"
  out <- validate_model_options(a_hintr_data, options)

  expect_length(out$warnings, 1)
  expect_equal(out$warnings[[1]]$text,
               paste0("You have chosen to fit model without estimating ",
                      "neighbouring ART attendance. You may wish to review",
                      " your selection to include this option."))
})

test_that("warning raised if outputs exceed threshold", {
  mock_fit_tmb <- mockery::mock(a_fit)
  mock_sample_tmb <- mockery::mock(a_fit_sample)

  output <- a_output
  output$indicators$mean[
    output$indicators$indicator == "prevalence"][1] <- 0.5
  output$indicators$mean[
    output$indicators$indicator == "art_coverage"][1] <- 1.2
  mock_output_package <- mockery::mock(output)

  with_mock(
    "naomi:::fit_tmb" = mock_fit_tmb,
    "naomi:::sample_tmb" = mock_sample_tmb,
    "naomi:::output_package" = mock_output_package, {
      out <- hintr_run_model(a_hintr_data, a_hintr_options, validate = FALSE)
    })

  expect_length(out$warnings, 2)
  expect_equal(
    out$warnings[[1]]$text,
    "HIV prevalence is higher than 40% for: CY2016Q1 MWI_1_1_demo both Y000_004")
  expect_equal(
    out$warnings[[2]]$text,
    "ART coverage is higher than 100% for: CY2016Q1 MWI_1_1_demo both Y000_004")
})
