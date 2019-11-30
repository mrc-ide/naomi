context("run-model")

## A single set of valid model options and data, update once instead of copying
## for every test.

a_hintr_data <- list(
  pjnz = system_file("extdata/mwi2019.PJNZ"),
  population = system_file("extdata/population/population_agesex.csv"),
  shape = system_file("extdata/areas/area_merged.geojson"),
  survey = system_file("extdata/survey/survey_hiv_indicators.csv"),
  art_number = system_file("extdata/programme/art_number.csv"),
  anc_testing = system_file("extdata/programme/anc_testing.csv")
)

a_hintr_options <- list(
  area_scope = "MWI_1_1",
  area_level = 4,
  calendar_quarter_t1 = "CY2016Q1",
  calendar_quarter_t2 = "CY2018Q3",
  survey_prevalence = c("MWI2016PHIA", "MWI2015DHS"),
  survey_art_coverage = "MWI2016PHIA",
  survey_recently_infected = "MWI2016PHIA",
  include_art_t1 = "true",
  include_art_t2 = "true",
  anc_prevalence_year1 = 2016,
  anc_prevalence_year2 = 2018,
  anc_art_coverage_year1 = 2016,
  anc_art_coverage_year2 = 2018,
  artattend = FALSE,
  rng_seed = 17,
  no_of_samples = 20,
  max_iter = 250,
  permissive = FALSE
)

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
               c("output_path", "spectrum_path", "summary_path"))

  output <- readRDS(model_run$output_path)
  expect_equal(colnames(output),
               c("area_level", "area_level_label", "area_id", "area_name",
                 "sex", "age_group", "age_group_id", "age_group_label",
                 "calendar_quarter", "quarter_id", "quarter_label",
                 "indicator", "indicator_id", "indicator_label",
                 "mean", "se", "median", "mode", "lower", "upper"))
  expect_true(nrow(output) == 22320)
  expect_equal(model_run$spectrum_path, output_spectrum)
  file_list <- unzip(model_run$spectrum_path, list = TRUE)
  ## Note that this test is likely quite platform specific
  info <- naomi_info(a_hintr_data, a_hintr_options)
  info_names <- paste0("info/", names(info))
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
      "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "info/", info_names))

  ## TODO: replace with checks for spectrum digest once function to create
  ## that has been added mrc-636
  expect_equal(model_run$summary_path, summary_path)
  file_list <- unzip(model_run$summary_path, list = TRUE)
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
      "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "info/", info_names))

  tmp <- tempfile()
  unzip(model_run$spectrum_path, exdir = tmp, files = info_names)
  expect_equal(dir(tmp), "info")
  expect_equal(dir(file.path(tmp, "info")), names(info))
})

test_that("model can be run without programme data", {

  data <- a_hintr_data
  data$art_number <- NULL
  data$anc_testing <- NULL

  options <- a_hintr_options
  options$include_art_t1 <- NULL
  options$include_art_t2 <- NULL
  options$anc_prevalence_year1 <- NULL
  options$anc_prevalence_year2 <- NULL
  options$anc_art_coverage_year1 <- NULL
  options$anc_art_coverage_year2 <- NULL

  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  summary_path <- tempfile(fileext = ".zip")
  model_run <- hintr_run_model(data, options, output_path, output_spectrum,
                         summary_path)
  expect_equal(names(model_run),
               c("output_path", "spectrum_path", "summary_path"))

  output <- readRDS(model_run$output_path)
  expect_equal(colnames(output),
               c("area_level", "area_level_label", "area_id", "area_name",
                 "sex", "age_group", "age_group_id", "age_group_label",
                 "calendar_quarter", "quarter_id", "quarter_label",
                 "indicator", "indicator_id", "indicator_label",
                 "mean", "se", "median", "mode", "lower", "upper"))
  expect_true(nrow(output) == 22320)

  expect_equal(model_run$spectrum_path, output_spectrum)
  file_list <- unzip(model_run$spectrum_path, list = TRUE)

  info <- naomi_info(data, options)
  info_names <- paste0("info/", names(info))
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
      "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "info/", info_names))

  ## TODO: replace with checks for spectrum digest once function to create
  ## that has been added mrc-636
  expect_equal(model_run$summary_path, summary_path)
  file_list <- unzip(model_run$summary_path, list = TRUE)
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
      "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "info/", info_names))

})

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
  options_bad <- list(
    area_scope = "MWI",
    calendar_quarter_t1 = "CY2016Q1",
    survey_prevalence = c("MWI2016PHIA", "MWI2015DHS"),
    survey_art_coverage = "MWI2016PHIA",
    survey_recently_infected = "MWI2016PHIA",
    include_art_t1 = "true",
    include_art_t2 = "true",
    anc_prevalence_year1 = 2016,
    anc_prevalence_year2 = 2018,
    anc_art_coverage_year1 = 2016,
    anc_art_coverage_year2 = 2018,
    no_of_samples = 20
  )
  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  summary_path <- tempfile(fileext = ".zip")
  expect_error(
    hintr_run_model(data, options_bad, output_path, output_spectrum,
                    summary_path)
  )
})


test_that("setting rng_seed returns same output", {

  data <- a_hintr_data
  
  options <- a_hintr_options
  options$survey_prevalence = "MWI2016PHIA"
  options$survey_art_coverage <- NULL
  options$survey_recently_infected <- NULL
  options$include_art_t1 = "false"
  options$include_art_t2 = "false"

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
