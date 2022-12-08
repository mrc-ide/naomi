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
               paste0("locations must be one of 'review_inputs', ",
                      "'model_options', 'model_fit', 'model_calibrate', ",
                      "'review_output', 'download_results'"))
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

  expect_length(out$warnings, 4)
  expect_match(out$warnings[[1]]$text,
               "Naomi ART current not equal to Spectrum")
  expect_match(out$warnings[[2]]$text,
               "Naomi ANC testing not equal to Spectrum")
  expect_match(out$warnings[[3]]$text,
               "Naomi ANC tested positive not equal to Spectrum")
  expect_equal(out$warnings[[4]]$text,
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
    output$indicators$indicator == "prevalence"][1] <- 0.6
  output$indicators$mean[
    output$indicators$indicator == "art_coverage"][1] <- 1.2
  mock_output_package <- mockery::mock(output)

  with_mock(
    "naomi:::fit_tmb" = mock_fit_tmb,
    "naomi:::sample_tmb" = mock_sample_tmb,
    "naomi:::output_package" = mock_output_package, {
      out <- hintr_run_model(a_hintr_data, a_hintr_options, validate = FALSE)
    })

  expect_length(out$warnings, 5)
  msgs <- lapply(out$warnings, function(x) x$text)
  expect_true(any(grepl("Naomi ART current not equal to Spectrum", msgs)))
  expect_true(any(grepl("Naomi ANC testing not equal to Spectrum", msgs)))
  expect_true(any(grepl("Naomi ANC tested positive not equal to Spectrum",
                        msgs)))
  expect_equal(
    out$warnings[[4]]$text,
    "HIV prevalence is higher than 50% for: March 2016, Northern, Both, 0-4")
  expect_equal(
    out$warnings[[5]]$text,
    "ART coverage is higher than 100% for: March 2016, Northern, Both, 0-4")
})

test_that("ART warning raised if spectrum totals do not match naomi data", {

  data <- list(
    pjnz = system.file("extdata/demo_mwi2019.PJNZ", package = "naomi"),
    shape = system.file("extdata/demo_areas.geojson", package = "naomi"),
    art_number = system.file("extdata/demo_art_number.csv", package = "naomi")
  )

  # National warnings for national pjnz file
  art0 <- hintr_validate_art(data$art_number,
                             data$shape,
                             data$pjnz)

  expect_length(art0$warnings, 1)
  expect_equal(art0$warnings[[1]]$locations,
               c("model_calibrate", "review_output"))
  expect_true(grepl("Naomi ART current not equal to Spectrum",
                    art0$warnings[[1]]$text))
  expect_true(grepl("2018 Y000_999 Malawi - Demo naomi",
                    art0$warnings[[1]]$text))
  expect_true(grepl("and \\d+ more",
                    art0$warnings[[1]]$text))

  art1 <- hintr_validate_art(a_hintr_data$art_number,
                            a_hintr_data$shape,
                            a_hintr_data$pjnz)

  expect_length(art1$warnings, 1)
  expect_equal(art1$warnings[[1]]$locations,
               c("model_calibrate", "review_output"))
  expect_true(grepl("Naomi ART current not equal to Spectrum",
                    art1$warnings[[1]]$text))
  expect_true(grepl("2018 Y000_999 Northern",
                    art1$warnings[[1]]$text))
  expect_true(grepl("2018 Y000_999 Central",
                    art1$warnings[[1]]$text))
  expect_true(grepl("2018 Y000_999 Southern",
                    art1$warnings[[1]]$text))
  expect_true(grepl("and \\d+ more",
                    art1$warnings[[1]]$text))

  })

test_that("ANC warning raised if spectrum totals do not match naomi data", {

  data <- list(
    pjnz = system.file("extdata/demo_mwi2019.PJNZ", package = "naomi"),
    shape = system.file("extdata/demo_areas.geojson", package = "naomi"),
    anc_testing = system.file("extdata/demo_anc_testing.csv", package = "naomi")
  )

  # National warnings for national pjnz file
  anc0 <- hintr_validate_anc(data$anc_testing,
                             data$shape,
                             data$pjnz)

  expect_length(anc0$warnings, 2)

  expect_equal(anc0$warnings[[1]]$locations,
               c("model_calibrate", "review_output"))
  expect_true(grepl("Naomi ANC testing not equal to Spectrum",
                    anc0$warnings[[1]]$text))
  expect_true(grepl("2018 Malawi - Demo",
                    anc0$warnings[[1]]$text))
  expect_true(grepl("and \\d+ more",
                    anc0$warnings[[1]]$text))

  expect_equal(anc0$warnings[[2]]$locations,
               c("model_calibrate", "review_output"))
  expect_true(grepl("Naomi ANC tested positive not equal to Spectrum",
                    anc0$warnings[[2]]$text))
  expect_true(grepl("2018 Malawi - Demo",
                    anc0$warnings[[2]]$text))
  expect_true(grepl("and \\d+ more",
                    anc0$warnings[[2]]$text))

  # Sub national level warnings for national pjnz file
  anc1 <- hintr_validate_anc(a_hintr_data$anc_testing,
                             a_hintr_data$shape,
                             a_hintr_data$pjnz)

  expect_length(anc1$warnings, 2)
  expect_equal(anc1$warnings[[1]]$locations,
               c("model_calibrate", "review_output"))
  expect_true(grepl("Naomi ANC testing not equal to Spectrum",
                    anc1$warnings[[1]]$text))
  expect_true(grepl("2018 Northern",
                    anc1$warnings[[1]]$text))
  expect_true(grepl("2018 Central",
                    anc1$warnings[[1]]$text))
  expect_true(grepl("2018 Southern",
                    anc1$warnings[[1]]$text))
  expect_true(grepl("and \\d+ more",
                    anc1$warnings[[1]]$text))

  expect_equal(anc1$warnings[[2]]$locations,
               c("model_calibrate", "review_output"))
  expect_true(grepl("Naomi ANC tested positive not equal to Spectrum",
                    anc1$warnings[[2]]$text))
  expect_true(grepl("2018 Northern",
                    anc1$warnings[[2]]$text))
  expect_true(grepl("2018 Central",
                    anc1$warnings[[2]]$text))
  expect_true(grepl("2018 Southern",
                    anc1$warnings[[2]]$text))
  expect_true(grepl("and \\d+ more",
                    anc1$warnings[[2]]$text))
})

