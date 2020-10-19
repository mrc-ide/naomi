context("run-model")

test_that("model can be run", {

  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  coarse_output_path <- tempfile(fileext = ".zip")
  summary_report_path = tempfile(fileext = ".html")
  calibration_path <- tempfile(fileext = ".rds")
  model_run <- hintr_run_model(a_hintr_data,
                               a_hintr_options,
                               output_path,
                               output_spectrum,
                               coarse_output_path,
                               summary_report_path,
                               calibration_path)
  expect_s3_class(model_run, "hintr_output")
  expect_equal(names(model_run),
               c("output_path", "spectrum_path", "coarse_output_path",
                 "summary_report_path", "calibration_path", "metadata"))

  output <- readRDS(model_run$output_path)
  expect_equal(colnames(output),
               c("area_level", "area_level_label", "area_id", "area_name",
                 "sex", "age_group", "age_group_id", "age_group_label",
                 "calendar_quarter", "quarter_id", "quarter_label",
                 "indicator", "indicator_id", "indicator_label",
                 "mean", "se", "median", "mode", "lower", "upper"))
  expect_true(nrow(output) == 16368 * 3 + 2*2*10)
  expect_equal(model_run$spectrum_path, output_spectrum)
  file_list <- unzip(model_run$spectrum_path, list = TRUE)
  ## Note that this test is likely quite platform specific
  info <- naomi_info(format_data_input(a_hintr_data), a_hintr_options)
  info_names <- paste0("info/", names(info))
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "info/", info_names,
      "fit/", "fit/spectrum_calibration.csv", "fit/calibration_options.csv")
  )

  expect_equal(model_run$coarse_output_path, coarse_output_path)
  file_list <- unzip(model_run$coarse_output_path, list = TRUE)
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "info/", info_names,
      "fit/", "fit/spectrum_calibration.csv", "fit/calibration_options.csv")
  )

  tmp <- tempfile()
  unzip(model_run$spectrum_path, exdir = tmp, files = info_names)
  expect_equal(dir(tmp), "info")
  expect_equal(dir(file.path(tmp, "info")), names(info))

  outputs <- read_output_package(model_run$spectrum_path)

  expect_true(
    all(c("area_level", "area_level_label", "area_id", "area_name", "parent_area_id",
          "spectrum_region_code", "area_sort_order", "geometry") %in%
        names(outputs$meta_area))
  )

  tmpf <- tempfile()
  unzip(model_run$spectrum_path, "boundaries.geojson", exdir = tmpf)
  output_boundaries <- sf::read_sf(file.path(tmpf, "boundaries.geojson"))

  ## Column 'name' added in boundaries.geojson during save_output() for Spectrum
  expect_true(
    all(c("area_level", "area_level_label", "area_id", "area_name", "parent_area_id",
      "spectrum_region_code", "area_sort_order", "name", "geometry") %in%
      names(output_boundaries))
  )

  ## Check coarse age outputs saved in coarse_output_path
  coarse_ages <- c("15-49", "15-64", "15+", "50+", "00+", "00-64", "00-14",
                   "15-24", "25-34", "35-49", "50-64", "65+")
  coarse_age_outputs <- read_output_package(model_run$coarse_output_path)
  expect_setequal(coarse_age_outputs$meta_age_group$age_group, coarse_ages)
  expect_setequal(coarse_age_outputs$indicators$age_group, coarse_ages)

  ## Metadata has been saved
  expect_equal(model_run$metadata$areas, "MWI_1_2")

  ## Summary report has been generated
  expect_true(file.size(summary_report_path) > 2000)
  expect_true(any(grepl("MWI2016PHIA MWI2015DHS", readLines(summary_report_path))))
  expect_true(any(grepl(basename(a_hintr_data$pjnz), readLines(summary_report_path))))
  expect_true(any(grepl("Central", readLines(summary_report_path))))

  ## Calibration data is stored
  expect_true(!is.null(model_run$calibration_path))
  calibration_data <- readRDS(model_run$calibration_path)
  expect_equal(names(calibration_data),
               c("output_package", "naomi_data", "info"))
  expect_s3_class(calibration_data$output_package, "naomi_output")
  expect_s3_class(calibration_data$naomi_data, "naomi_data")
  expect_s3_class(calibration_data$naomi_data, "naomi_mf")
  expect_equal(names(calibration_data$info),
               c("inputs.csv", "options.yml", "packages.csv"))
})

test_that("model can be run without programme data", {
  data <- a_hintr_data
  data$art_number <- NULL
  data$anc_testing <- NULL

  options <- list(
    area_scope = "MWI_1_2",
    area_level = "4",
    calendar_quarter_t1 = "CY2016Q1",
    calendar_quarter_t2 = "CY2018Q3",
    calendar_quarter_t3 = "CY2019Q2",
    survey_prevalence = c("MWI2016PHIA", "MWI2015DHS"),
    survey_art_coverage = "MWI2016PHIA",
    survey_recently_infected = "MWI2016PHIA",
    spectrum_population_calibration = "national",
    spectrum_plhiv_calibration_level = "subnational",
    spectrum_plhiv_calibration_strat = "sex_age_group",
    spectrum_artnum_calibration_level = "national",
    spectrum_artnum_calibration_strat = "age_coarse",
    spectrum_infections_calibration_level = "national",
    spectrum_infections_calibration_strat = "age_coarse",
    rng_seed = 17,
    no_of_samples = 20,
    max_iter = 250,
    permissive = "false"
  )

  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  coarse_output_path <- tempfile(fileext = ".zip")
  model_run <- hintr_run_model(data, options, output_path, output_spectrum,
                               coarse_output_path)
  expect_equal(names(model_run),
               c("output_path", "spectrum_path", "coarse_output_path",
                 "summary_report_path", "calibration_path", "metadata"))

  output <- readRDS(model_run$output_path)
  expect_equal(colnames(output),
               c("area_level", "area_level_label", "area_id", "area_name",
                 "sex", "age_group", "age_group_id", "age_group_label",
                 "calendar_quarter", "quarter_id", "quarter_label",
                 "indicator", "indicator_id", "indicator_label",
                 "mean", "se", "median", "mode", "lower", "upper"))
  expect_true(nrow(output) == 16368 * 3 + 2*2*10)

  expect_equal(model_run$spectrum_path, output_spectrum)
  file_list <- unzip(model_run$spectrum_path, list = TRUE)

  info <- naomi_info(format_data_input(data), options)
  info_names <- paste0("info/", names(info))
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "info/", info_names,
      "fit/", "fit/spectrum_calibration.csv", "fit/calibration_options.csv")
  )

  ## TODO: replace with checks for spectrum digest once function to create
  ## that has been added mrc-636
  expect_equal(model_run$coarse_output_path, coarse_output_path)
  file_list <- unzip(model_run$coarse_output_path, list = TRUE)
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "info/", info_names,
      "fit/", "fit/spectrum_calibration.csv", "fit/calibration_options.csv")
  )

})

test_that("model fit without survey ART and survey recency data", {

  options <- a_hintr_options
  options$survey_art_coverage <- NULL
  expect_error(hintr_run_model(a_hintr_data, options),
               NA)

  options <- a_hintr_options
  options$survey_recently_infected <- NULL
  expect_error(hintr_run_model(a_hintr_data, options),
               NA)

  ## No survey ART coverage or ART programme data
  options <- a_hintr_options
  options$survey_art_coverage <- NULL
  options$include_art_t1 = "false"
  options$include_art_t2 = "false"
  options$artattend <- "false"
  expect_error(hintr_run_model(a_hintr_data, options),
               NA)
}
)

test_that("progress messages are printed", {
  skip_on_covr()
  mock_new_progress <- mockery::mock(MockProgress$new())

  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  coarse_output_path <- tempfile(fileext = ".zip")
  with_mock("naomi:::new_progress" = mock_new_progress,
            "naomi::fit_tmb" = fit, "naomi::sample_tmb" = sample, {
    model_run <- naomi_evaluate_promise(
      hintr_run_model(a_hintr_data, a_hintr_options,
                      output_path, output_spectrum, coarse_output_path))
  })
  expect_equal(length(model_run$progress), 6)
  for (step in model_run$progress) {
    expect_equal(step[[1]]$name, "Validating inputs and options")
    expect_equal(step[[2]]$name, "Preparing input data")
    expect_equal(step[[3]]$name, "Fitting the model")
    expect_equal(step[[4]]$name, "Generating uncertainty ranges")
    expect_equal(step[[5]]$name, "Preparing outputs")
  }
  first_message <- model_run$progress[[1]]
  ## 5 different states
  expect_equal(length(first_message), 5)
  expect_true(first_message[[1]]$started)
  expect_false(first_message[[1]]$complete)
  expect_false(first_message[[2]]$started)
  expect_false(first_message[[2]]$complete)

  second_message <- model_run$progress[[2]]
  expect_equal(length(second_message), 5)
  expect_true(second_message[[1]]$started)
  expect_true(second_message[[1]]$complete)
  expect_true(second_message[[2]]$started)
  expect_false(second_message[[2]]$complete)
})

test_that("model run throws error for invalid inputs", {
  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  coarse_output_path <- tempfile(fileext = ".zip")
  expect_error(
    hintr_run_model(data, a_hintr_options_bad,
                    output_path, output_spectrum, coarse_output_path)

  )
})


test_that("setting rng_seed returns same output", {

  data <- a_hintr_data

  options <- a_hintr_options
  options$survey_prevalence = "MWI2016PHIA"
  options$survey_art_coverage <- "MWI2016PHIA"
  options$survey_recently_infected <- NULL
  options$include_art_t1 = "false"
  options$include_art_t2 = "false"
  options$artattend <- "false"
  options$spectrum_plhiv_calibration_level <- "none"
  options$spectrum_artnum_calibration_level <- "none"
  options$spectrum_infections_calibration_level <- "none"

  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  coarse_output_path <- tempfile(fileext = ".zip")

  model_run <- hintr_run_model(data, options,
                               output_path, output_spectrum,
                               coarse_output_path)

  options2 <- options
  options2$rng_seed <- 17

  output_path2 <- tempfile()
  output_spectrum2 <- tempfile(fileext = ".zip")
  coarse_output_path2 <- tempfile(fileext = ".zip")

  model_run2 <- hintr_run_model(data, options2, output_path2,
                                output_spectrum2, coarse_output_path2)

  options3 <- options
  options3$rng_seed <- NULL

  output_path3 <- tempfile()
  output_spectrum3 <- tempfile(fileext = ".zip")
  coarse_output_path3 <- tempfile(fileext = ".zip")

  model_run3 <- hintr_run_model(data, options3, output_path3,
                                output_spectrum3, coarse_output_path3)

  output <- readRDS(model_run$output_path)
  output2 <- readRDS(model_run2$output_path)
  output3 <- readRDS(model_run3$output_path)

  expect_equal(output, output2)

  expect_equal(nrow(output), nrow(output3))
  expect_true(output$mean[output$indicator == "prevalence"][1] !=
              output3$mean[output3$indicator == "prevalence"][1])
})

test_that("exceeding max_iterations convergence error or warning", {

  data <- a_hintr_data

  options <- a_hintr_options
  options$survey_prevalence = "MWI2016PHIA"
  options$survey_art_coverage <- NULL
  options$survey_recently_infected <- NULL
  options$include_art_t1 = "false"
  options$include_art_t2 = "false"
  options$artattend <- "false"
  options$max_iterations <- 5

  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  coarse_output_path <- tempfile(fileext = ".zip")

  expect_error(hintr_run_model(data, options,
                               output_path, output_spectrum,
                               coarse_output_path))

  options$permissive <- "true"
  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  coarse_output_path <- tempfile(fileext = ".zip")
  expect_warning(hintr_run_model(data, options,
                                 output_path, output_spectrum,
                                 coarse_output_path))
})

test_that("invalid time sequencing returns an error", {

  options <- a_hintr_options
  options$calendar_quarter_t2 <- a_hintr_options$calendar_quarter_t1
  expect_error(hintr_run_model(a_hintr_data, options),
               "Estimates quarter \\(time 2\\) must be after survey quarter \\(time 1\\)")

})


test_that("model works with empty string for ANC year", {

  options <- a_hintr_options
  options$anc_prevalence_year1 <- ""
  options$anc_prevalence_year2 <- ""
  options$anc_art_coverage_year1 <- ""
  options$anc_art_coverage_year2 <- ""

  model_run <- hintr_run_model(a_hintr_data, options)

  expect_equal(names(model_run),
               c("output_path", "spectrum_path", "coarse_output_path",
                 "summary_report_path", "calibration_path","metadata"))
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
  ## Calibration modifies files in place. We want to make sure we don't modify
  ## the test setup output and introduce race condition in tests.
  ## Create a copy and calibrate that
  output <- clone_output(a_hintr_output)
  calibrated_output <- hintr_calibrate(output, a_hintr_calibration_options)

  expect_s3_class(calibrated_output, "hintr_output")

  ## Output has been calibrated
  expect_file_different(calibrated_output$output_path,
                        a_hintr_output$output_path)
  indicators_output <- readRDS(calibrated_output$output_path)
  ## Check there is some data
  expect_true(nrow(indicators_output) == 16368 * 3 + 2*2*10)

  ## Spectrum file has been calibrated
  expect_file_different(calibrated_output$spectrum_path,
                        a_hintr_output$spectrum_path)
  file_list <- unzip(calibrated_output$spectrum_path, list = TRUE)
  ## Note that this test is likely quite platform specific
  info <- naomi_info(format_data_input(a_hintr_data), a_hintr_options)
  info_names <- paste0("info/", names(info))
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv",
      "meta_period.csv", "info/", "info/calibration_options.yml", info_names,
      "fit/", "fit/spectrum_calibration.csv", "fit/calibration_options.csv")
  )

  ## Coarse age group output file has been calibrated
  expect_file_different(calibrated_output$coarse_output_path,
                        a_hintr_output$coarse_output_path)
  file_list <- unzip(calibrated_output$coarse_output_path, list = TRUE)
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv",
      "meta_period.csv", "info/", "info/calibration_options.yml", info_names,
      "fit/", "fit/spectrum_calibration.csv", "fit/calibration_options.csv")
  )

  ## Summary report has been calibrated
  summary_report <- calibrated_output$summary_report_path
  ## Content of report is the same so check that it has been regenerated
  expect_true(file.info(summary_report)$ctime >
                file.info(a_hintr_output$summary_report_path)$ctime)
  ## Options & filename are available to calibrated report
  expect_true(any(grepl("MWI2016PHIA MWI2015DHS", readLines(summary_report))))
  expect_true(any(grepl("mwi2019.PJNZ", readLines(summary_report))))

  ## calibration data: info has been updated but everything else unchanged
  expect_file_different(calibrated_output$calibration_path,
                        a_hintr_output$calibration_path)
  pre_calibration_data <- readRDS(a_hintr_output$calibration_path)
  post_calibration_data <- readRDS(calibrated_output$calibration_path)
  expect_equal(post_calibration_data$output_package,
               pre_calibration_data$output_package)
  expect_equal(post_calibration_data$naomi_data,
               pre_calibration_data$naomi_data)
  expect_equal(names(post_calibration_data$info),
              c("inputs.csv", "options.yml", "packages.csv",
                "calibration_options.yml"))
  expect_equal(post_calibration_data$info$inputs.csv,
               pre_calibration_data$info$inputs.csv)
  expect_equal(post_calibration_data$info$options.yml,
               pre_calibration_data$info$options.yml)
  expect_equal(post_calibration_data$info$packages.csv,
               pre_calibration_data$info$packages.csv)
  expect_equal(post_calibration_data$info$calibration_options.yml,
               yaml::as.yaml(a_hintr_calibration_options))

  ## metadata is unchanged
  expect_equal(calibrated_output$metadata, a_hintr_output$metadata)

  ## Can calibrate multiple times
  ## Copy files again as calibration modified in place and we want to test
  ## that recalibrating returns us new results
  clone <- clone_output(calibrated_output)
  calibration_options <- list(
    spectrum_plhiv_calibration_level = "national",
    spectrum_plhiv_calibration_strat = "sex_age_coarse",
    spectrum_artnum_calibration_level = "subnational",
    spectrum_artnum_calibration_strat = "age_coarse",
    spectrum_infections_calibration_level = "none",
    spectrum_infections_calibration_strat = "age_coarse"
  )
  calibrated_output_2 <- hintr_calibrate(clone, calibration_options)

  expect_s3_class(calibrated_output_2, "hintr_output")

  ## Output has been calibrated
  expect_file_different(calibrated_output_2$output_path,
                        a_hintr_output$output_path)
  expect_file_different(calibrated_output_2$output_path,
                        calibrated_output$output_path)
  indicators_output <- readRDS(calibrated_output_2$output_path)
  ## Check there is some data
  expect_true(nrow(indicators_output) == 16368 * 3 + 2*2*10)

  ## Spectrum file has been calibrated
  expect_file_different(calibrated_output_2$spectrum_path,
                        a_hintr_output$spectrum_path)
  expect_file_different(calibrated_output_2$spectrum_path,
                        calibrated_output$spectrum_path)
  ## There is some data
  expect_true(file.size(calibrated_output_2$spectrum_path) > 2000)

  ## Coarse age-group output file has been calibrated
  expect_file_different(calibrated_output_2$coarse_output_path,
                        a_hintr_output$coarse_output_path)
  expect_file_different(calibrated_output_2$coarse_output_path,
                        calibrated_output$coarse_output_path)
  expect_true(file.size(calibrated_output_2$coarse_output_path) > 2000)

  ## Summary report has been calibrated
  summary_report_2 <- calibrated_output_2$summary_report_path
  expect_true(file.info(summary_report_2)$ctime >
                file.info(a_hintr_output$summary_report_path)$ctime)
  ## Options & filename are available to calibrated report
  expect_true(any(grepl("MWI2016PHIA MWI2015DHS", readLines(summary_report_2))))
  expect_true(any(grepl("mwi2019.PJNZ", readLines(summary_report_2))))

  ## calibration data: info has been updated but everything else unchanged
  expect_file_different(calibrated_output_2$calibration_path,
                        a_hintr_output$calibration_path)
  expect_file_different(calibrated_output_2$calibration_path,
                        calibrated_output$calibration_path)
  post_calibration_2_data <- readRDS(calibrated_output_2$calibration_path)
  expect_equal(post_calibration_2_data$output_package,
               pre_calibration_data$output_package)
  expect_equal(post_calibration_2_data$naomi_data,
               pre_calibration_data$naomi_data)
  expect_equal(names(post_calibration_2_data$info),
              c("inputs.csv", "options.yml", "packages.csv",
                "calibration_options.yml"))
  expect_equal(post_calibration_2_data$info$inputs.csv,
               pre_calibration_data$info$inputs.csv)
  expect_equal(post_calibration_2_data$info$options.yml,
               pre_calibration_data$info$options.yml)
  expect_equal(post_calibration_2_data$info$packages.csv,
               pre_calibration_data$info$packages.csv)
  expect_equal(post_calibration_2_data$info$calibration_options.yml,
               yaml::as.yaml(calibration_options))

  ## metadata is unchanged
  expect_equal(calibrated_output_2$metadata, a_hintr_output$metadata)
})

test_that("useful error returned when model output can't be calibrated", {
  expect_error(hintr_calibrate(NULL, list(test = "option")),
               paste0("Can't calibrate this model output please re-run model",
               " and try calibration again"))
})
