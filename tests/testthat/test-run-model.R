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
                 "sex", "age_group", "age_group_label",
                 "calendar_quarter", "quarter_label",
                 "indicator", "indicator_label",
                 "mean", "se", "median", "mode", "lower", "upper"))

  ## Total population outputs:
  ## * 31 age groups
  ## * 3 sexes
  ## * 3 output times
  ## * 22 areas
  ## * 12 indicators
  ##
  ## ANC indicators outputs
  ## 3 = number or output times
  ## 9 = number of ANC indicators
  ## 22 = number of areas
  ## 11 = number of ANC age groups
  expect_equal(nrow(output), 31 * 3 * 3 * 22 * 12 + 3 * 9 * 22 * 11)
  expect_equal(model_run$spectrum_path, output_spectrum)
  file_list <- unzip(model_run$spectrum_path, list = TRUE)
  ## Note that this test is likely quite platform specific
  info <- naomi_info(format_data_input(a_hintr_data), a_hintr_options)
  info_names <- paste0("info/", names(info))
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "pepfar_datapack_indicators_2021.csv",
      "info/", info_names,
      "fit/", "fit/spectrum_calibration.csv", "fit/calibration_options.csv")
  )

  expect_equal(model_run$coarse_output_path, coarse_output_path)
  file_list <- unzip(model_run$coarse_output_path, list = TRUE)
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "pepfar_datapack_indicators_2021.csv",
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
  coarse_ages <- c("Y015_049", "Y015_064", "Y015_999", "Y050_999", "Y000_999", "Y000_064",
                   "Y000_014", "Y015_024", "Y025_034", "Y035_049", "Y050_064", "Y065_999")

  coarse_age_outputs <- read_output_package(model_run$coarse_output_path)
  expect_setequal(coarse_age_outputs$meta_age_group$age_group, coarse_ages)
  expect_setequal(coarse_age_outputs$indicators$age_group, coarse_ages)

  ## Metadata has been saved
  expect_equal(model_run$metadata$areas, "MWI_1_2_demo")
  expect_type(model_run$metadata$output_description, "character")
  expect_length(model_run$metadata$output_description, 1)
  expect_type(model_run$metadata$summary_report_description, "character")
  expect_length(model_run$metadata$summary_report_description, 1)

  ## Summary report has been generated
  expect_true(file.size(summary_report_path) > 2000)
  expect_true(any(grepl("DEMO2016PHIA, DEMO2015DHS", brio::readLines(summary_report_path))))
  expect_true(any(grepl(basename(a_hintr_data$pjnz), brio::readLines(summary_report_path))))
  expect_true(any(grepl("Central", brio::readLines(summary_report_path))))

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
                 "sex", "age_group", "age_group_label",
                 "calendar_quarter", "quarter_label",
                 "indicator", "indicator_label",
                 "mean", "se", "median", "mode", "lower", "upper"))
  expect_equal(nrow(output), 31 * 3 * 3 * 22 * 12 + 3 * 9 * 22 * 11)

  expect_equal(model_run$spectrum_path, output_spectrum)
  file_list <- unzip(model_run$spectrum_path, list = TRUE)

  info <- naomi_info(format_data_input(data), options)
  info_names <- paste0("info/", names(info))
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "pepfar_datapack_indicators_2021.csv",
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
      "pepfar_datapack_indicators_2021.csv",
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
  have_iteration <- grepl("Iteration \\d+ - [\\d.m\\s]+s elapsed", model_help,
                          perl = TRUE)
  expect_true(any(have_iteration))
  expect_false(all(have_iteration))
  ## Iteration message updates
  expect_false(identical(model_help[[which(have_iteration)[1]]],
                      model_help[[which(have_iteration)[2]]]))

  ## Final messages has completed message
  final_message <- model_run$progress[[length(model_run$progress)]]
  expect_match(final_message$fit_model$helpText,
               "\\d+ iterations in [\\d.m\\s]+s",
               perl = TRUE)
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
  options$survey_prevalence = "DEMO2016PHIA"
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

  options$anc_clients_year2 <- ""
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

  ## Calibration makes no modification of existing files.
  output_hash <- tools::md5sum(a_hintr_output$output_path)
  spectrum_hash <- tools::md5sum(a_hintr_output$spectrum_path)
  coarse_output_hash <- tools::md5sum(a_hintr_output$coarse_output_path)
  summary_report_hash <- tools::md5sum(a_hintr_output$summary_report_path)
  calibration_hash <- tools::md5sum(a_hintr_output$calibration_path)

  output_path <- tempfile()
  spectrum_path <- tempfile(fileext = ".zip")
  coarse_output_path <- tempfile(fileext = ".zip")
  summary_report_path = tempfile(fileext = ".html")
  calibration_path <- tempfile(fileext = ".rds")
  calibrated_output <- hintr_calibrate(a_hintr_output,
                                       a_hintr_calibration_options,
                                       output_path,
                                       spectrum_path,
                                       coarse_output_path,
                                       summary_report_path,
                                       calibration_path)

  expect_s3_class(calibrated_output, "hintr_output")
  expect_equal(calibrated_output$output_path, output_path)
  expect_equal(calibrated_output$spectrum_path, spectrum_path)
  expect_equal(calibrated_output$coarse_output_path, coarse_output_path)
  expect_equal(calibrated_output$summary_report_path, summary_report_path)
  expect_equal(calibrated_output$calibration_path, calibration_path)
  expect_equal(calibrated_output$metadata, a_hintr_output$metadata)

  ## Calibration does not modify original files
  expect_equal(tools::md5sum(a_hintr_output$output_path),
               output_hash)
  expect_equal(tools::md5sum(a_hintr_output$spectrum_path),
               spectrum_hash)
  expect_equal(tools::md5sum(a_hintr_output$coarse_output_path),
               coarse_output_hash)
  expect_equal(tools::md5sum(a_hintr_output$summary_report_path),
               summary_report_hash)
  expect_equal(tools::md5sum(a_hintr_output$calibration_path),
               calibration_hash)

  ## Output has been calibrated
  expect_file_different(calibrated_output$output_path,
                        a_hintr_output$output_path)
  indicators_output <- readRDS(calibrated_output$output_path)
  ## Check there is some data
  expect_equal(nrow(indicators_output), 31 * 3 * 3 * 22 * 12 + 3 * 9 * 22 * 11)

  ## Spectrum file has been calibrated & original files unchanged
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
      "meta_period.csv",
      "pepfar_datapack_indicators_2021.csv",
      "info/", "info/calibration_options.yml", info_names,
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
      "meta_period.csv",
      "pepfar_datapack_indicators_2021.csv",
      "info/", "info/calibration_options.yml", info_names,
      "fit/", "fit/spectrum_calibration.csv", "fit/calibration_options.csv")
  )

  ## Summary report has been calibrated
  summary_report <- calibrated_output$summary_report_path
  ## Content of report is the same so check that it has been regenerated
  expect_true(file.info(summary_report)$ctime >
                file.info(a_hintr_output$summary_report_path)$ctime)
  ## Options & filename are available to calibrated report
  expect_true(any(grepl("DEMO2016PHIA, DEMO2015DHS", brio::readLines(summary_report))))
  expect_true(any(grepl("demo_mwi2019.PJNZ", brio::readLines(summary_report))))

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
  expect_type(calibrated_output$metadata$output_description, "character")
  expect_length(calibrated_output$metadata$output_description, 1)
  expect_type(calibrated_output$metadata$summary_report_description,
              "character")
  expect_length(calibrated_output$metadata$summary_report_description, 1)

  ## Can calibrate multiple times
  calibration_options <- list(
    spectrum_plhiv_calibration_level = "national",
    spectrum_plhiv_calibration_strat = "sex_age_coarse",
    spectrum_artnum_calibration_level = "subnational",
    spectrum_artnum_calibration_strat = "age_coarse",
    spectrum_aware_calibration_level = "subnational",
    spectrum_aware_calibration_strat = "age_coarse",
    spectrum_infections_calibration_level = "none",
    spectrum_infections_calibration_strat = "age_coarse",
    calibrate_method = "logistic"
  )
  calibrated_output_2 <- hintr_calibrate(calibrated_output,
                                         calibration_options)

  expect_s3_class(calibrated_output_2, "hintr_output")

  ## Output has been calibrated
  expect_file_different(calibrated_output_2$output_path,
                        a_hintr_output$output_path)
  expect_file_different(calibrated_output_2$output_path,
                        calibrated_output$output_path)
  indicators_output <- readRDS(calibrated_output_2$output_path)
  ## Check there is some data
  expect_equal(nrow(indicators_output), 31 * 3 * 3 * 22 * 12 + 3 * 9 * 22 * 11)

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
  expect_true(any(grepl("DEMO2016PHIA, DEMO2015DHS", brio::readLines(summary_report_2))))
  expect_true(any(grepl("demo_mwi2019.PJNZ", brio::readLines(summary_report_2))))

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

test_that("progress can report on model fit", {
  ## Mock some times for consistent testing of elapsed time, return
  ## now, in 30s time, in 2 mins time, in 1h 2 mins time and in 1h 5m 8s time
  now <- round.POSIXt(Sys.time(), units = "mins")
  mock_sys_time <- mockery::mock(now, now + 30, now + (2 * 60), now + (62 * 60),
                                 now + (65 * 60) + 8)
  with_mock("Sys.time" = mock_sys_time, {
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
  })
  expect_equal(progress$start_time, now)
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
  file.copy(a_hintr_data$pjnz, temp_pjnz)
  utils::zip(temp_pjnz, "malawi.zip.shiny90", flags="-d", extras = "-q")
  expect_false(assert_pjnz_shiny90(temp_pjnz))

  data <- a_hintr_data
  data$pjnz <- temp_pjnz
  data <- format_data_input(data)

  opts <- a_hintr_options
  opts$output_aware_plhiv <- "false"
  expect_true(validate_model_options(data, opts))

  ## Fit model without .shiny90 in PJNZ
  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  coarse_output_path <- tempfile(fileext = ".zip")
  summary_report_path = tempfile(fileext = ".html")
  calibration_path <- tempfile(fileext = ".rds")

  model_run <- hintr_run_model(data,
                               opts,
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
                 "sex", "age_group", "age_group_label",
                 "calendar_quarter", "quarter_label",
                 "indicator", "indicator_label",
                 "mean", "se", "median", "mode", "lower", "upper"))

  ## Total population outputs:
  ## * 31 age groups
  ## * 3 sexes
  ## * 3 output times
  ## * 22 areas
  ## * 9 indicators [9 vs. 11 OMITTED 3 aware of status]
  ##
  ## ANC indicators outputs
  ## 3 = number or output times
  ## 9 = number of ANC indicators
  ## 22 = number of areas
  ## 11 = number of ANC age groups
  expect_equal(nrow(output), 31 * 3 * 3 * 22 * 9 + 3 * 9 * 22 * 11)
  expect_equal(model_run$spectrum_path, output_spectrum)
  file_list <- unzip(model_run$spectrum_path, list = TRUE)
  ## Note that this test is likely quite platform specific
  info <- naomi_info(format_data_input(a_hintr_data), a_hintr_options)
  info_names <- paste0("info/", names(info))
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "pepfar_datapack_indicators_2021.csv",
      "info/", info_names,
      "fit/", "fit/spectrum_calibration.csv", "fit/calibration_options.csv")
  )

  expect_equal(model_run$coarse_output_path, coarse_output_path)
  file_list <- unzip(model_run$coarse_output_path, list = TRUE)
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "pepfar_datapack_indicators_2021.csv",
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

  ## Check coarse age outputs saved in coarse_output_path
  coarse_ages <- c("Y015_049", "Y015_064", "Y015_999", "Y050_999", "Y000_999", "Y000_064",
                   "Y000_014", "Y015_024", "Y025_034", "Y035_049", "Y050_064", "Y065_999")

  coarse_age_outputs <- read_output_package(model_run$coarse_output_path)
  expect_setequal(coarse_age_outputs$meta_age_group$age_group, coarse_ages)
  expect_setequal(coarse_age_outputs$indicators$age_group, coarse_ages)

  ## Metadata has been saved
  expect_equal(model_run$metadata$areas, "MWI_1_2_demo")

  ## Summary report has been generated
  expect_true(file.size(summary_report_path) > 2000)
  expect_true(any(grepl("DEMO2016PHIA, DEMO2015DHS", brio::readLines(summary_report_path))))

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


  ## ## Calibrate model

  ## Calibration modifies files in place.
  calibrated_output <- hintr_calibrate(model_run, a_hintr_calibration_options)

  expect_s3_class(calibrated_output, "hintr_output")

  indicators_output <- readRDS(calibrated_output$output_path)
  ## Check there is some data
  expect_equal(nrow(indicators_output), 31 * 3 * 3 * 22 * 9 + 3 * 9 * 22 * 11)
})

test_that("hintr_run_model can skip validation", {
  options <- a_hintr_options
  options$area_scope <- "MWI"
  options$area_level <- 0

  mock_validate_model_options <- mockery::mock(TRUE)

  with_mock("naomi:::validate_model_options" = mock_validate_model_options, {
    ## Don't really care about result here, just using some test that will
    ## complete relatively quickly so we can test model validation is skipped
    expect_error(hintr_run_model(format_data_input(a_hintr_data), options,
                                 validate = FALSE))
  })
  mockery::expect_called(mock_validate_model_options, 0)

  with_mock("naomi:::validate_model_options" = mock_validate_model_options, {
    ## Don't really care about result here, just using some test that will
    ## complete relatively quickly so we can test model validation is skipped
    expect_error(hintr_run_model(format_data_input(a_hintr_data), options))
  })
  mockery::expect_called(mock_validate_model_options, 1)
})

test_that("simple progress", {
  now <- round.POSIXt(Sys.time(), units = "mins")
  mock_sys_time <- mockery::mock(now, now + 30, now + (2 * 60), now + (62 * 60))
  with_mock("Sys.time" = mock_sys_time, {
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
  })
  expect_equal(progress$start_time, now)
  expect_equal(messages1$progress[[1]]$message,
               "Calibrating outputs - 30s elapsed")
  expect_equal(messages2$progress[[1]]$message,
               "Calibrating outputs - 2m elapsed")
  expect_equal(messages3$progress[[1]]$message,
               "Calibrage des sorties - 1h 2m écoulées")
})

test_that("calibration reports simple progress", {
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  with_mock("naomi:::new_simple_progress" = mock_new_simple_progress, {
    messages <- naomi_evaluate_promise(
      hintr_calibrate(a_hintr_output, a_hintr_calibration_options))
  })
  expect_length(messages$progress, 3)
  progress <- messages$progress
  expect_match(progress[[1]]$message,
               "Calibrating outputs - [\\d.m\\s]+s elapsed", perl = TRUE)
  expect_match(progress[[2]]$message,
               "Saving outputs - [\\d.m\\s]+s elapsed", perl = TRUE)
  expect_match(progress[[3]]$message,
               "Generating report - [\\d.m\\s]+s elapsed", perl = TRUE)
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

test_that("calibrating adds output descriptions when missing", {
  output_hash <- tools::md5sum(a_hintr_output$output_path)
  spectrum_hash <- tools::md5sum(a_hintr_output$spectrum_path)
  coarse_output_hash <- tools::md5sum(a_hintr_output$coarse_output_path)
  summary_report_hash <- tools::md5sum(a_hintr_output$summary_report_path)
  calibration_hash <- tools::md5sum(a_hintr_output$calibration_path)

  output_path <- tempfile()
  spectrum_path <- tempfile(fileext = ".zip")
  coarse_output_path <- tempfile(fileext = ".zip")
  summary_report_path = tempfile(fileext = ".html")
  calibration_path <- tempfile(fileext = ".rds")
  output <- a_hintr_output
  output$metadata$output_description <- NULL
  output$metadata$summary_report_description <- NULL
  calibrated_output <- hintr_calibrate(output,
                                       a_hintr_calibration_options,
                                       output_path,
                                       spectrum_path,
                                       coarse_output_path,
                                       summary_report_path,
                                       calibration_path)

  expect_s3_class(calibrated_output, "hintr_output")

  expect_type(calibrated_output$metadata$output_description, "character")
  expect_length(calibrated_output$metadata$output_description, 1)
  expect_type(calibrated_output$metadata$summary_report_description,
              "character")
  expect_length(calibrated_output$metadata$summary_report_description, 1)
})
