context("run-model")

test_that("model can be run", {
  data <- list(
    pjnz = system_file("extdata/mwi2019.PJNZ"),
    population = system_file("extdata/population/population_agesex.csv"),
    shape = file.path("testdata/malawi.geojson"),
    survey = system_file("extdata/survey/survey_hiv_indicators.csv"),
    programme = system_file("extdata/programme/art_number.csv"),
    anc = system_file("extdata/programme/anc_testing.csv")
  )
  options <- list(
    area_scope = "MWI",
    area_level = 4,
    t1 = 465,
    t2 = 475,
    survey_prevalence = c("MWI2016PHIA", "MWI2015DHS"),
    survey_art_coverage = "MWI2016PHIA",
    survey_vls = NULL,
    survey_recently_infected = "MWI2016PHIA",
    survey_art_or_vls = "art_coverage",
    art_t1 = 465,
    art_t2 = 475,
    anc_prevalence_t1 = 464,
    anc_prevalence_t2 = 475,
    anc_art_coverage_t1 = 464,
    anc_art_coverage_t2 = 475,
    no_of_samples = 20
  )
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
                 "sex", "age_group_id", "age_group_label", "quarter_id",
                 "quarter_label", "indicator_id", "indicator_label", "mode",
                 "mean", "se", "median", "lower", "upper"))
  expect_true(nrow(output) == 42021)

  expect_equal(model_run$spectrum_path, output_spectrum)
  file_list <- unzip(model_run$spectrum_path, list = TRUE)
  expect_equal(file_list$Name,
               c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
                 "meta_area.csv", "meta_indicator.csv", "meta_period.csv"))

  ## TODO: replace with checks for spectrum digest once function to create
  ## that has been added mrc-636
  expect_equal(model_run$summary_path, summary_path)
  file_list <- unzip(model_run$summary_path, list = TRUE)
  expect_equal(file_list$Name,
               c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
                 "meta_area.csv", "meta_indicator.csv", "meta_period.csv"))

})

test_that("model can be run without programme data", {
  testthat::skip("Skipping test as running without programme data not supported see mrc-638")
  data <- list(
    pjnz = system_file("extdata/mwi2019.PJNZ"),
    population = system_file("extdata/population/population_agesex.csv"),
    shape = file.path("testdata/malawi.geojson"),
    survey = system_file("extdata/survey/survey_hiv_indicators.csv")
  )
  options <- list(
    area_scope = "MWI",
    area_level = 4,
    t1 = 465,
    t2 = 475,
    survey_prevalence = "MWI2016PHIA",
    survey_art_coverage = "MWI2016PHIA",
    survey_vls = NULL,
    survey_recently_infected = "MWI2016PHIA",
    survey_art_or_vls = "art_coverage",
    no_of_samples = 20
  )
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
                 "sex", "age_group_id", "age_group_label", "quarter_id",
                 "quarter_label", "indicator_id", "indicator_label", "mode",
                 "mean", "se", "median", "lower", "upper"))
  expect_true(nrow(output) == 42021)

  expect_equal(model_run$spectrum_path, output_spectrum)
  file_list <- unzip(model_run$spectrum_path, list = TRUE)
  expect_equal(file_list$Name,
               c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
                 "meta_area.csv", "meta_indicator.csv", "meta_period.csv"))

  ## TODO: replace with checks for spectrum digest once function to create
  ## that has been added mrc-636
  expect_equal(model_run$summary_path, summary_path)
  file_list <- unzip(model_run$summary_path, list = TRUE)
  expect_equal(file_list$Name,
               c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
                 "meta_area.csv", "meta_indicator.csv", "meta_period.csv"))

})

test_that("progress messages are printed", {
  skip_on_covr()
  mock_new_progress <- mockery::mock(MockProgress$new())

  data <- list(
    pjnz = system_file("extdata/mwi2019.PJNZ"),
    population = system_file("extdata/population/population_agesex.csv"),
    shape = file.path("testdata/malawi.geojson"),
    survey = system_file("extdata/survey/survey_hiv_indicators.csv"),
    programme = system_file("extdata/programme/art_number.csv"),
    anc = system_file("extdata/programme/anc_testing.csv")
  )
  options <- list(
    area_scope = "MWI",
    area_level = 4,
    t1 = 465,
    t2 = 475,
    survey_prevalence = c("MWI2016PHIA", "MWI2015DHS"),
    survey_art_coverage = "MWI2016PHIA",
    survey_vls = NULL,
    survey_recently_infected = "MWI2016PHIA",
    survey_art_or_vls = "art_coverage",
    art_t1 = 465,
    art_t2 = 475,
    anc_prevalence_t1 = 464,
    anc_prevalence_t2 = 475,
    anc_art_coverage_t1 = 464,
    anc_art_coverage_t2 = 475,
    no_of_samples = 20
  )
  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  summary_path <- tempfile(fileext = ".zip")
  with_mock("naomi:::new_progress" = mock_new_progress,
            "naomi::fit_tmb" = fit, "naomi::sample_tmb" = sample, {
    model_run <- naomi_evaluate_promise(
      hintr_run_model(data, options, output_path, output_spectrum, summary_path))
  })
  expect_equal(length(model_run$progress), 5)
  for (step in model_run$progress) {
    expect_equal(step[[1]]$name, "Preparing input data")
    expect_equal(step[[2]]$name, "Fitting the model")
    expect_equal(step[[3]]$name, "Generating uncertainty ranges")
    expect_equal(step[[4]]$name, "Preparing outputs")
  }
  first_message <- model_run$progress[[1]]
  ## 4 different states
  expect_equal(length(first_message), 4)
  expect_true(first_message[[1]]$started)
  expect_false(first_message[[1]]$completed)
  expect_false(first_message[[2]]$started)
  expect_false(first_message[[2]]$completed)

  second_message <- model_run$progress[[2]]
  expect_equal(length(second_message), 4)
  expect_true(second_message[[1]]$started)
  expect_true(second_message[[1]]$completed)
  expect_true(second_message[[2]]$started)
  expect_false(second_message[[2]]$completed)
})
