context("run-model")

test_that("model can be run", {

  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  summary_path <- tempfile(fileext = ".zip")
  model_run <- hintr_run_model(a_hintr_data,
                               a_hintr_options,
                               output_path,
                               output_spectrum,
                               summary_path)
  expect_equal(names(model_run),
               c("output_path", "spectrum_path", "summary_path", "metadata"))

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
  info <- naomi_info(a_hintr_data, a_hintr_options)
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
  expect_equal(model_run$summary_path, summary_path)
  file_list <- unzip(model_run$summary_path, list = TRUE)
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
          "spectrum_region_code", "area_sort_order", "name", "geometry") %in%
        names(outputs$meta_area))
  )

  expect_equal(model_run$metadata$areas, "MWI_1_2")
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
  summary_path <- tempfile(fileext = ".zip")
  model_run <- hintr_run_model(data, options, output_path, output_spectrum,
                         summary_path)
  expect_equal(names(model_run),
               c("output_path", "spectrum_path", "summary_path", "metadata"))

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

  info <- naomi_info(data, options)
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
  expect_equal(model_run$summary_path, summary_path)
  file_list <- unzip(model_run$summary_path, list = TRUE)
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "info/", info_names,
      "fit/", "fit/spectrum_calibration.csv", "fit/calibration_options.csv")
  )

})

test_that("model fit without survey ART and survey recency data", {

  ## !!! TODO: need to get this working or validation flags
  skip("Need to return to either get working or set validation flags")

  options <- a_hintr_options
  options$survey_art_coverage <- NULL
  expect_error(
    hintr_run_model(a_hintr_data, options, tempfile(), tempfile(), tempfile()),
    NA)

  options <- a_hintr_options
  options$survey_recently_infected <- NULL
  expect_error(
    hintr_run_model(a_hintr_data, options, tempfile(), tempfile(), tempfile()),
    NA)

  ## No survey ART coverage or ART programme data
  ## !!! TODO: This is a situation that **should** be supported, needs attention
  options <- a_hintr_options
  options$survey_art_coverage <- NULL
  options$include_art_t1 = "false"
  options$include_art_t2 = "false"
  expect_error(
    hintr_run_model(a_hintr_data, options, tempfile(), tempfile(), tempfile()),
    "false convergence \\(8\\)")

}
)

test_that("progress messages are printed", {
  skip_on_covr()
  mock_new_progress <- mockery::mock(MockProgress$new())

  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  summary_path <- tempfile(fileext = ".zip")
  with_mock("naomi:::new_progress" = mock_new_progress,
            "naomi::fit_tmb" = fit, "naomi::sample_tmb" = sample, {
    model_run <- naomi_evaluate_promise(
      hintr_run_model(a_hintr_data, a_hintr_options,
                      output_path, output_spectrum, summary_path))
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
  summary_path <- tempfile(fileext = ".zip")
  expect_error(
    hintr_run_model(data, a_hintr_options_bad,
                    output_path, output_spectrum, summary_path)

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
  summary_path <- tempfile(fileext = ".zip")

  model_run <- hintr_run_model(data, options,
                               output_path, output_spectrum,
                               summary_path)

  options2 <- options
  options2$rng_seed <- 17

  output_path2 <- tempfile()
  output_spectrum2 <- tempfile(fileext = ".zip")
  summary_path2 <- tempfile(fileext = ".zip")

  model_run2 <- hintr_run_model(data, options2, output_path2,
                                output_spectrum2, summary_path2)

  options3 <- options
  options3$rng_seed <- NULL

  output_path3 <- tempfile()
  output_spectrum3 <- tempfile(fileext = ".zip")
  summary_path3 <- tempfile(fileext = ".zip")

  model_run3 <- hintr_run_model(data, options3, output_path3,
                                output_spectrum3, summary_path3)

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
  summary_path <- tempfile(fileext = ".zip")

  expect_error(hintr_run_model(data, options,
                               output_path, output_spectrum,
                               summary_path))

  options$permissive <- "true"
  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  summary_path <- tempfile(fileext = ".zip")
  expect_warning(hintr_run_model(data, options,
                                 output_path, output_spectrum,
                                 summary_path))
})


test_that("naomi_info_input(data) handles NULL string", {

  data <- list(file1 = "file1.ext",
               file2 = "file2.ext",
               file3 = NULL)

  expect_equal(nrow(naomi_info_input(data)), 3)
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
               c("output_path", "spectrum_path", "summary_path", "metadata"))
})
