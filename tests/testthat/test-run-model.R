test_that("model can be run", {
  output_path <- tempfile(fileext = ".qs")
  model_run <- hintr_run_model(a_hintr_data,
                               a_hintr_options,
                               output_path)
  expect_s3_class(model_run, "hintr_output")
  expect_equal(names(model_run),
               c("plot_data_path", "model_output_path", "version", "warnings"))
  expect_equal(model_run$version, packageVersion("naomi"))
  expect_null(model_run$plot_data_path)

  output <- read_hintr_output(model_run$model_output_path)
  expect_equal(names(output),
               c("output_package", "naomi_data", "info", "warnings"))
  expect_s3_class(output$output_package, "naomi_output")
  expect_s3_class(output$naomi_data, "naomi_data")
  expect_s3_class(output$naomi_data, "naomi_mf")
  expect_equal(names(output$info),
               c("inputs.csv", "options.yml", "packages.csv"))
  expect_equal(output$warnings$model_fit, model_run$warnings)

  expect_length(model_run$warnings, 3)
  msgs <- lapply(model_run$warnings, function(x) x$text)
  expect_true(any(grepl("Naomi ART current not equal to Spectrum", msgs)))
  expect_true(any(grepl("Naomi ANC testing not equal to Spectrum", msgs)))
  expect_true(any(grepl("Naomi ANC tested positive not equal to Spectrum",
                        msgs)))
})

test_that("model can be run without programme data", {
  data <- a_hintr_data
  data$art_number <- NULL
  data$anc_testing <- NULL

  options <- a_hintr_options
  options$include_art_t1 <- NULL
  options$include_art_t2 <- NULL
  options$anc_clients_year2 <- NULL
  options$anc_clients_year2_num_months <- NULL
  options$anc_prevalence_year1 <- NULL
  options$anc_prevalence_year2 <- NULL
  options$anc_art_coverage_year1 <- NULL
  options$anc_art_coverage_year2 <- NULL
  options$artattend <- NULL
  options$artattend_t2 <- NULL
  options$artattend_log_gamma_offset <- NULL

  output_path <- tempfile(fileext = ".qs")
  model_run <- hintr_run_model(data, options, output_path)
  expect_equal(names(model_run),
               c("plot_data_path", "model_output_path", "version", "warnings"))
  expect_null(model_run$plot_data_path)

  output <- read_hintr_output(model_run$model_output_path)
  expect_equal(names(output),
               c("output_package", "naomi_data", "info", "warnings"))
  expect_s3_class(output$output_package, "naomi_output")
  expect_s3_class(output$naomi_data, "naomi_data")
  expect_s3_class(output$naomi_data, "naomi_mf")
  expect_equal(names(output$info),
               c("inputs.csv", "options.yml", "packages.csv"))
  expect_equal(output$warnings$model_fit, model_run$warnings)
})

test_that("model fit without survey ART and survey recency data", {

  options <- a_hintr_options
  options$survey_art_coverage <- NULL
  expect_error(hintr_run_model(a_hintr_data, options), NA)

  options <- a_hintr_options
  options$survey_recently_infected <- NULL
  expect_error(hintr_run_model(a_hintr_data, options), NA)

  ## No survey ART coverage or ART programme data
  options <- a_hintr_options
  options$survey_art_coverage <- NULL
  options$include_art_t1 = "false"
  options$include_art_t2 = "false"
  options$artattend <- "false"
  expect_error(hintr_run_model(a_hintr_data, options), NA)
}
)

test_that("progress messages are printed", {
  skip_on_covr()
  mock_new_progress <- mockery::mock(MockProgress$new())

  output_path <- tempfile(fileext = ".qs")
  with_mock(new_progress = mock_new_progress,
            fit_tmb = fit, sample_tmb = sample, {
              model_run <- naomi_evaluate_promise(
                hintr_run_model(a_hintr_data, a_hintr_options, output_path))
            })
  ## If using mock fit here there will only be 5, if using real
  ## fit_tmb there will be many more
  expect_true(length(model_run$progress) >= 5)
  first_message <- model_run$progress[[1]]
  expect_equal(first_message[[1]]$name, "Preparing input data")
  expect_equal(first_message[[2]]$name, "Fitting the model")
  expect_equal(first_message[[3]]$name, "Generating uncertainty ranges")
  expect_equal(first_message[[4]]$name, "Preparing outputs")
  ## 4 different states
  expect_equal(length(first_message), 4)
  expect_true(first_message[[1]]$started)
  expect_false(first_message[[1]]$complete)
  expect_false(first_message[[2]]$started)
  expect_false(first_message[[2]]$complete)

  second_message <- model_run$progress[[2]]
  expect_equal(length(second_message), 4)
  expect_true(second_message[[1]]$started)
  expect_true(second_message[[1]]$complete)
  expect_true(second_message[[2]]$started)
  expect_false(second_message[[2]]$complete)

  skip_if(!all.equal(fit, fit_tmb),
          "Using mock fit result, skipping progress tests")
  ## Help text gets printed at some point
  model_help <- lapply(model_run$progress, function(msg) {
    msg$fit_model$helpText
  })
  have_iteration <- grepl("Iteration \\d+ - [\\d.m\\s]+[sm] elapsed", model_help,
                          perl = TRUE)
  expect_true(any(have_iteration))
  expect_false(all(have_iteration))
  ## Iteration message updates
  expect_false(identical(model_help[[which(have_iteration)[1]]],
                      model_help[[which(have_iteration)[2]]]))

  ## Final messages has completed message
  final_message <- model_run$progress[[length(model_run$progress)]]
  expect_match(final_message$fit_model$helpText,
               "\\d+ iterations in [\\d.m\\s]+[sm]",
               perl = TRUE)
})

test_that("model run throws error for invalid inputs", {
  output_path <- tempfile(fileext = ".qs")
  expect_error(
    hintr_run_model(data, a_hintr_options_bad, output_path)
  )
})


test_that("setting rng_seed returns same output", {

  data <- a_hintr_data
  options <- a_hintr_options
  options$survey_prevalence = "DEMO2016PHIA"
  options$survey_art_coverage <- "DEMO2016PHIA"
  options$survey_recently_infected <- NULL
  options$include_art_t1 = "false"
  options$include_art_t2 = "false"
  options$artattend <- "false"
  options$spectrum_plhiv_calibration_level <- "none"
  options$spectrum_artnum_calibration_level <- "none"
  options$spectrum_aware_calibration_level <- "none"
  options$spectrum_infections_calibration_level <- "none"
  options$calibrate_method <- "logistic"

  output_path <- tempfile(fileext = ".qs")
  model_run <- hintr_run_model(data, options, output_path)

  options2 <- options
  options2$rng_seed <- 17

  output_path2 <- tempfile(fileext = ".qs")
  model_run2 <- hintr_run_model(data, options2, output_path2)

  options3 <- options
  options3$rng_seed <- NULL

  output_path3 <- tempfile(fileext = ".qs")
  model_run3 <- hintr_run_model(data, options3, output_path3)

  output <- read_hintr_output(model_run$model_output_path)
  output2 <- read_hintr_output(model_run2$model_output_path)
  output3 <- read_hintr_output(model_run3$model_output_path)

  expect_equal(output, output2)

  expect_equal(nrow(output), nrow(output3))
  output_indicators <- output$output_package$indicators
  output_indicators3 <- output3$output_package$indicators
  expect_true(
    output_indicators$mean[output_indicators$indicator == "prevalence"][1] !=
      output_indicators3$mean[output_indicators3$indicator == "prevalence"][1])
})

test_that("exceeding max_iterations raises convergence warning", {

  data <- a_hintr_data

  options <- a_hintr_options
  options$survey_prevalence = "DEMO2016PHIA"
  options$survey_art_coverage <- NULL
  options$survey_recently_infected <- NULL
  options$include_art_t1 = "false"
  options$include_art_t2 = "false"
  options$artattend <- "false"
  options$max_iterations <- 5

  output_path <- tempfile(fileext = ".qs")
  out <- hintr_run_model(data, options, output_path)

  expect_length(out$warnings, 4)

  expect_equal(out$warnings[[1]]$text,
               paste0("Naomi ART current not equal to Spectrum: 2018 Y000_999 Northern naomi: 78974 spectrum: 57913; 2018 Y000_999 Central naomi: 226728 spectrum: 236140; 2018 Y000_999 Southern naomi: 493159 spectrum: 496708 and 21 more"))

  expect_equal(out$warnings[[4]]$text,
               paste0("Convergence error: iteration limit reached without convergence (10)"))

  msgs <- lapply(out$warnings, function(x) x$text)
  expect_true(any(grepl("Naomi ART current not equal to Spectrum", msgs)))
  expect_true(any(grepl("Naomi ANC testing not equal to Spectrum", msgs)))
  expect_true(any(grepl("Naomi ANC tested positive not equal to Spectrum",
                        msgs)))
})

test_that("invalid time sequencing returns an error", {

  options <- a_hintr_options
  options$calendar_quarter_t2 <- a_hintr_options$calendar_quarter_t1
  expect_error(hintr_run_model(a_hintr_data, options),
               "Estimates quarter \\(time 2\\) must be after survey quarter \\(time 1\\)")

})


test_that("model works with empty string for ANC year", {

  options <- a_hintr_options

  options$anc_clients_year2 <- ""
  options$anc_prevalence_year1 <- ""
  options$anc_prevalence_year2 <- ""
  options$anc_art_coverage_year1 <- ""
  options$anc_art_coverage_year2 <- ""

  model_run <- hintr_run_model(a_hintr_data, options)

  expect_equal(names(model_run),
               c("plot_data_path", "model_output_path", "version", "warnings"))
})

test_that("input data types can be formatted", {
  path1 <- tempfile()
  writeLines("test", path1)
  path2 <- tempfile()
  writeLines("test2", path2)
  formatted <- format_data_input(list(
    file1 = path1,
    file2 = path2
  ))

  expect_equal(names(formatted), c("file1", "file2"))
  expect_equal(names(formatted$file1), c("path", "hash", "filename"))
  expect_equal(formatted$file1$path, path1)
  expect_equal(names(formatted$file2), c("path", "hash", "filename"))
  expect_equal(formatted$file2$path, path2)

  ## Correctly formatted data is unchanged
  test_data <- list(
    file1 = list(
      path = "path/to/file1",
      hash = "hash1",
      filename = "file1"
    ),
    file2 = list(
      path = "path/to/file2",
      hash = "hash2",
      filename = "file2"
    )
  )
  expect_equal(format_data_input(test_data), test_data)

  expect_error(format_data_input(2),
               "Unsupported input data type, must be a list of file paths or list of file metadata")
})

test_that("model run can be calibrated", {

  ## Calibration makes no modification of existing files.
  output_hash <- tools::md5sum(a_hintr_output$model_output_path)
  expect_null(a_hintr_output$plot_data_path)

  plot_data_path <- tempfile(fileext = ".qs")
  calibration_output_path <- tempfile(fileext = ".qs")
  calibrated_output <- hintr_calibrate(a_hintr_output,
                                       a_hintr_calibration_options,
                                       plot_data_path,
                                       calibration_output_path)

  expect_s3_class(calibrated_output, "hintr_output")
  expect_equal(calibrated_output$plot_data_path, plot_data_path)
  expect_equal(calibrated_output$model_output_path, calibration_output_path)

  ## Calibration does not modify original files
  expect_equal(tools::md5sum(a_hintr_output$model_output_path),
               output_hash)
  expect_null(a_hintr_output$plot_data_path)

  ## Output has been calibrated
  expect_true(!is.null(calibrated_output$plot_data_path))
  indicators_output <- read_hintr_output(calibrated_output$plot_data_path)
  ## Total population outputs:
  ## * 33 age groups
  ## * 3 sexes
  ## * 3 output times
  ## * 9 areas
  ## * 14 indicators
  ##
  ## ANC indicators outputs
  ## 3 = number or output times
  ## 9 = number of ANC indicators
  ## 9 = number of areas
  ## 12 = number of ANC age groups
  expect_equal(nrow(indicators_output), 33 * 3 * 3 * 9 * 14 + 3 * 9 * 9 * 12)

  expect_file_different(calibrated_output$model_output_path,
                        a_hintr_output$model_output_path)
  expect_length(calibrated_output$warnings, 0)

  output <- read_hintr_output(calibrated_output$model_output_path)
  expect_equal(names(output),
               c("output_package", "naomi_data", "info", "warnings"))
  expect_s3_class(output$output_package, "naomi_output")
  expect_s3_class(output$naomi_data, "naomi_data")
  expect_s3_class(output$naomi_data, "naomi_mf")
  expect_equal(
    names(output$info),
    c("inputs.csv", "options.yml", "packages.csv", "calibration_options.yml"))
  expect_setequal(names(output$warnings), c("model_fit", "calibrate"))
  expect_equal(output$warnings$calibrate, calibrated_output$warnings)

  ## Cannot calibrate multiple times
  calibration_options <- list(
    spectrum_plhiv_calibration_level = "subnational",
    spectrum_plhiv_calibration_strat = "sex_age_coarse",
    spectrum_artnum_calibration_level = "subnational",
    spectrum_artnum_calibration_strat = "age_coarse",
    spectrum_aware_calibration_level = "subnational",
    spectrum_aware_calibration_strat = "age_coarse",
    spectrum_infections_calibration_level = "none",
    spectrum_infections_calibration_strat = "age_coarse",
    calibrate_method = "logistic"
  )
  expect_error(
    hintr_calibrate(calibrated_output, calibration_options),
    "Calibration cannot be re-run for this model fit please re-run fit step.")
})

test_that("calibrating model with 'none' returns same results", {

  none_calibration_options <- list(
    spectrum_plhiv_calibration_level = "none",
    spectrum_plhiv_calibration_strat = "sex_age_coarse",
    spectrum_artnum_calibration_level = "none",
    spectrum_artnum_calibration_strat = "age_coarse",
    spectrum_aware_calibration_level = "none",
    spectrum_aware_calibration_strat = "age_coarse",
    spectrum_infections_calibration_level = "none",
    spectrum_infections_calibration_strat = "age_coarse",
    calibrate_method = "logistic"
  )

  plot_data_path <- tempfile(fileext = ".qs")
  calibration_output_path <- tempfile(fileext = ".qs")
  calibrated_output <- hintr_calibrate(a_hintr_output,
                                       none_calibration_options,
                                       plot_data_path,
                                       calibration_output_path)

  out_raw <- read_hintr_output(a_hintr_output$model_output_path)
  out_raw_disag <- disaggregate_0to4_outputs(out_raw$output_package, out_raw$naomi_data)

  out_calib <- read_hintr_output(calibrated_output$model_output_path)

  expect_equal(out_raw_disag$indicators, out_calib$output_package$indicators, tolerance = 1e-6)
  expect_equal(out_raw$output_package$art_attendance, out_calib$output_package$art_attendance)
})

test_that("re-calibrating an already calibrated output throws error", {

  plot_data_path1 <- tempfile(fileext = ".qs")
  calibration_output_path1 <- tempfile(fileext = ".qs")
  calibrated_output1 <- hintr_calibrate(a_hintr_output,
                                        a_hintr_calibration_options,
                                        plot_data_path1,
                                        calibration_output_path1)

  ## Calibrate again with same options using the outputs of calibration 1
  plot_data_path2 <- tempfile(fileext = ".qs")
  calibration_output_path2 <- tempfile(fileext = ".qs")
  expect_error(hintr_calibrate(calibrated_output1,
                               a_hintr_calibration_options,
                               plot_data_path2,
                               calibration_output_path2),
               "Calibration cannot be re-run for this model fit please re-run fit step.")
})

test_that("useful error returned when model output can't be calibrated", {
  expect_error(hintr_calibrate(NULL, list(test = "option")),
               "Model output out of date please re-run model and try again.")
})

test_that("progress can report on model fit", {
  progress <- MockProgress$new()
  expect_null(progress$progress$fit_model$helpText)
  messages1 <- naomi_evaluate_promise(
    progress$iterate_fit()
  )
  messages2 <- naomi_evaluate_promise(
    progress$iterate_fit()
  )
  messages3 <- naomi_evaluate_promise(
    progress$iterate_fit()
  )
  reset <- naomi_set_language("fr")
  on.exit(reset())
  messages4 <- naomi_evaluate_promise(
    progress$iterate_fit()
  )
  messages5 <- naomi_evaluate_promise({
    progress$finalise_fit()
    progress$complete("fit_model")
    progress$print()
  })

  expect_equal(progress$start_time, progress$now)
  expect_equal(messages1$progress[[1]]$fit_model$helpText,
               "Iteration 1 - 30s elapsed")
  expect_equal(messages2$progress[[1]]$fit_model$helpText,
               "Iteration 2 - 2m elapsed")
  expect_equal(messages3$progress[[1]]$fit_model$helpText,
               "Iteration 3 - 1h 2m elapsed")
  expect_equal(messages4$progress[[1]]$fit_model$helpText,
               "Itération 4 - 1h 5m 8s écoulées")
  expect_equal(messages5$progress[[1]]$fit_model$helpText,
               "4 itérations en 1h 5m 8s")
})


test_that("Model can be run without .shiny90 file", {

  ## Remove .shiny90 from PJNZ and set 'output_aware_plhiv = FALSE'
  temp_pjnz <- tempfile(fileext = ".pjnz")
  file.copy(system_file("extdata/demo_mwi2019.PJNZ"), temp_pjnz)
  utils::zip(temp_pjnz, "malawi.zip.shiny90", flags="-d", extras = "-q")
  expect_false(assert_pjnz_shiny90(temp_pjnz))

  data <- a_hintr_data
  data$pjnz <- temp_pjnz
  data$shape <- system_file("extdata/demo_areas.geojson")
  data <- format_data_input(data)

  opts <- a_hintr_options
  opts$output_aware_plhiv <- "false"
  expect_true(validate_model_options(data, opts)$valid)

  ## Fit model without .shiny90 in PJNZ
  output_path <- tempfile(fileext = ".qs")

  model_run <- hintr_run_model(data,
                               opts,
                               output_path)

  expect_s3_class(model_run, "hintr_output")
  expect_equal(names(model_run),
               c("plot_data_path", "model_output_path", "version", "warnings"))

  output <- read_hintr_output(model_run$model_output_path)
  expect_equal(names(output),
               c("output_package", "naomi_data", "info", "warnings"))
  expect_s3_class(output$output_package, "naomi_output")
  expect_s3_class(output$naomi_data, "naomi_data")
  expect_s3_class(output$naomi_data, "naomi_mf")
  expect_equal(names(output$info),
               c("inputs.csv", "options.yml", "packages.csv"))
  expect_equal(output$warnings$model_fit, model_run$warnings)

  expect_setequal(names(output$output_package$meta_area),
                   c("area_level", "area_level_label", "area_id", "area_name",
                     "parent_area_id", "spectrum_region_code",
                     "area_sort_order", "center_x", "center_y", "geometry")
  )

  ## ## Calibrate model

  calibrated_output <- hintr_calibrate(model_run, a_hintr_calibration_options)

  expect_s3_class(calibrated_output, "hintr_output")

  indicators_output <- read_hintr_output(calibrated_output$model_output_path)
  ## Check there is some data
  ## 11 indicators (3 fewer because missing awareness of status indicators
  expect_equal(nrow(indicators_output$output_package$indicators),
               33 * 3 * 3 * 9 * 11 + 3 * 9 * 9 * 12)
})

test_that("hintr_run_model can skip validation", {
  options <- a_hintr_options
  options$area_scope <- "MWI"
  options$area_level <- 0

  mock_validate_model_options <- mockery::mock(TRUE)

  with_mock(do_validate_model_options = mock_validate_model_options, {
    ## Don't really care about result here, just using some test that will
    ## complete relatively quickly so we can test model validation is skipped
    expect_error(hintr_run_model(format_data_input(a_hintr_data), options,
                                 validate = FALSE))
  })
  mockery::expect_called(mock_validate_model_options, 0)

  with_mock(do_validate_model_options = mock_validate_model_options, {
    ## Don't really care about result here, just using some test that will
    ## complete relatively quickly so we can test model validation is skipped
    expect_error(hintr_run_model(format_data_input(a_hintr_data), options))
  })
  mockery::expect_called(mock_validate_model_options, 1)
})

test_that("simple progress", {
  progress <- MockSimpleProgress$new()
  messages1 <- naomi_evaluate_promise(
    progress$update_progress("PROGRESS_CALIBRATE")
  )
  messages2 <- naomi_evaluate_promise(
    progress$update_progress("PROGRESS_CALIBRATE")
  )
  reset <- naomi_set_language("fr")
  on.exit(reset())
  messages3 <- naomi_evaluate_promise(
    progress$update_progress("PROGRESS_CALIBRATE")
  )

  expect_equal(progress$start_time, progress$now)
  expect_equal(messages1$progress[[1]]$message,
               "Calibrating outputs - 30s elapsed")
  expect_equal(messages2$progress[[1]]$message,
               "Calibrating outputs - 2m elapsed")
  expect_equal(messages3$progress[[1]]$message,
               "Calibrage des sorties - 1h 2m écoulées")
})

test_that("calibration reports simple progress", {
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  with_mock(new_simple_progress = mock_new_simple_progress, {
    messages <- naomi_evaluate_promise(
      hintr_calibrate(a_hintr_output, a_hintr_calibration_options))
  })
  expect_length(messages$progress, 2)
  progress <- messages$progress
  expect_equal(progress[[1]]$message,
               "Calibrating outputs - 30s elapsed")
  expect_equal(progress[[2]]$message,
               "Saving outputs - 2m elapsed")
})

test_that("validate_calibrate_options errors if required options are missing", {
  expect_error(validate_calibrate_options(list(
    "spectrum_plhiv_calibration_level" = "none",
    "spectrum_plhiv_calibration_level" = "none",
    "spectrum_plhiv_calibration_strat" = "none",
    "spectrum_artnum_calibration_level" = "none",
    "spectrum_artnum_calibration_strat" = "none",
    "spectrum_aware_calibration_level" = "none",
    "spectrum_aware_calibration_strat" = "none",
    "calibrate_method" = "logistic")),
    paste0("Calibration cannot be run, missing options for ",
           "spectrum_infections_calibration_level, ",
           "spectrum_infections_calibration_strat."))


  expect_error(validate_calibrate_options(list(
    "spectrum_plhiv_calibration_level" = "none",
    "spectrum_plhiv_calibration_level" = "none",
    "spectrum_plhiv_calibration_strat" = "none",
    "spectrum_artnum_calibration_level" = "none",
    "spectrum_artnum_calibration_strat" = "none",
    "spectrum_aware_calibration_level" = "none",
    "spectrum_aware_calibration_strat" = "none",
    "spectrum_infections_calibration_level" = "none",
    "calibrate_method" = "logistic")),
    paste0("Calibration cannot be run, missing options for ",
           "spectrum_infections_calibration_strat."))

  expect_true(validate_calibrate_options(list(
    "spectrum_plhiv_calibration_level" = "none",
    "spectrum_plhiv_calibration_level" = "none",
    "spectrum_plhiv_calibration_strat" = "none",
    "spectrum_artnum_calibration_level" = "none",
    "spectrum_artnum_calibration_strat" = "none",
    "spectrum_aware_calibration_level" = "none",
    "spectrum_aware_calibration_strat" = "none",
    "spectrum_infections_calibration_level" = "none",
    "spectrum_infections_calibration_strat" = "none",
    "calibrate_method" = "logistic")))

  expect_error(validate_calibrate_options(list(
    "spectrum_plhiv_calibration_level" = "none",
    "spectrum_plhiv_calibration_level" = "none",
    "spectrum_plhiv_calibration_strat" = "none",
    "spectrum_artnum_calibration_level" = "none",
    "spectrum_artnum_calibration_strat" = "none",
    "spectrum_aware_calibration_level" = "none",
    "spectrum_aware_calibration_strat" = "none",
    "spectrum_infections_calibration_level" = "none",
    "spectrum_infections_calibration_strat" = "none")),
    paste0("Calibration cannot be run, missing options for ",
           "calibrate_method."))

  expect_error(validate_calibrate_options(list(
    "spectrum_plhiv_calibration_level" = "none",
    "spectrum_plhiv_calibration_level" = "none",
    "spectrum_plhiv_calibration_strat" = "none",
    "spectrum_artnum_calibration_level" = "none",
    "spectrum_artnum_calibration_strat" = "none",
    "spectrum_aware_calibration_level" = "none",
    "spectrum_aware_calibration_strat" = "none",
    "spectrum_infections_calibration_level" = "none",
    "spectrum_infections_calibration_strat" = "none",
    "calibrate_method" = "JIBBERISH")),
    paste0("calibrate_method must be either \"logistic\" or \"proportional\""))

})

test_that("assert_model_output_version ensures model version up to date", {
  expect_true(assert_model_output_version(a_hintr_output))
  expect_error(assert_model_output_version(list(version = "123")),
               "Model output out of date please re-run model and try again.")
  output <- a_hintr_output
  output$version <- "2.5.3"
  expect_error(assert_model_output_version(output, "2.5.4"),
               "Model output out of date please re-run model and try again.")
  expect_true(assert_model_output_version(output, "2.5.3"))
  expect_true(assert_model_output_version(output))
})

test_that("can get data_type labels", {
  labels <- data_type_labels()
  expect_length(labels, 4)
  expect_equal(labels[[1]], list(
    id = "spectrum",
    label = "Spectrum"
  ))
  expect_equal(labels[[2]], list(
    id = "calibrated",
    label = "Calibrated"
  ))
  expect_equal(labels[[3]], list(
    id = "raw",
    label = "Unadjusted"
  ))
  expect_equal(labels[[4]], list(
    id = "calibration_ratio",
    label = "Calibration ratio"
  ))
})

test_that("trying to calibrate incompatible model output returns error", {

  ## Calibration makes no modification of existing files.
  plot_data_path <- tempfile(fileext = ".qs")
  calibration_output_path <- tempfile(fileext = ".qs")
  hintr_output <- list(
    plot_data_path = NULL,
    model_output_path = "refdata/naomi-2.5.5/output_data_2.5.5.qs",
    version = "2.5.5"
  )
  class(hintr_output) <- "hintr_output"
  expect_error(hintr_calibrate(hintr_output,
                               a_hintr_calibration_options,
                               plot_data_path,
                               calibration_output_path),
               "Model output out of date please re-run model and try again.")
})

