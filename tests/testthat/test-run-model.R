context("run-model")

test_that("model can be run", {
  data <- list(
    pjnz = system_file("extdata/mwi2019.PJNZ"),
    population = system_file("extdata/population/population_agesex.csv"),
    shape = file.path("testdata/malawi.geojson"),
    survey = system_file("extdata/survey/survey_hiv_indicators.csv"),
    art = system_file("extdata/programme/art_number.csv"),
    anc = system_file("extdata/programme/anc_testing.csv")
  )
  options <- list(
    area_scope = "MWI",
    area_level = 4,
    t1 = 465,
    t2 = 475,
    ## TODO: UI only gives 1 option for this not a multi select
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
    anc_art_coverage_t2 = 475
  )
  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  output_indicators <- tempfile(fileext = ".zip")
  model_run <- run_model(data, options, output_path, output_spectrum,
                         output_indicators)
  expect_equal(names(model_run),
               c("output_path", "spectrum_path", "indicators_path"))

  output <- readRDS(model_run$output_path)
  expect_equal(colnames(output),
               c("area_level", "area_level_label", "area_id", "area_name",
                 "sex", "age_group_id", "age_group_label", "quarter_id",
                 "quarter_label", "indicator_id", "indicator_label", "mode",
                 "mean", "se", "median", "lower", "upper"))
  expect_true(nrow(output) == 42021)

  expect_equal(model_run$spectrum_path, output_spectrum)
  ## TODO: replace with checks for spectrum digest once function to create
  ## that has been added
  file_list <- unzip(model_run$spectrum_path, list = TRUE)
  expect_equal(file_list$Name,
               c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
                 "meta_area.csv", "meta_indicator.csv", "meta_period.csv"))

  expect_equal(model_run$indicators_path, output_indicators)
  file_list <- unzip(model_run$indicators_path, list = TRUE)
  expect_equal(file_list$Name,
               c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
                 "meta_area.csv", "meta_indicator.csv", "meta_period.csv"))

})

test_that("model can be run without programme data", {
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
    survey_art_or_vls = "art_coverage"
  )
  output_path <- tempfile()
  output_spectrum <- tempfile(fileext = ".zip")
  output_indicators <- tempfile(fileext = ".zip")
  ## TODO: Make model run with optional inputs work

  ## model_run <- run_model(data, options, output_path, output_spectrum,
  ##                        output_indicators)
  ## expect_equal(names(model_run),
  ##              c("output_path", "spectrum_path", "indicators_path"))
  ##
  ## output <- readRDS(model_run$output_path)
  ## expect_equal(colnames(output),
  ##              c("area_level", "area_level_label", "area_id", "area_name",
  ##                "sex", "age_group_id", "age_group_label", "quarter_id",
  ##                "quarter_label", "indicator_id", "Indicator_label", "mode",
  ##                "mean", "se", "median", "lower", "upper"))
  ## expect_true(nrow(output) == 42022)
  ##
  ## expect_equal(model_run$spectrum_path, output_spectrum)
  ## ## TODO: replace with checks for spectrum digest once function to create
  ## ## that has been added
  ## file_list <- unzip(model_run$spectrum_path, list = TRUE)
  ## expect_equal(file_list$Name,
  ##              c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
  ##                "meta_area.csv", "meta_indicator.csv", "meta_period.csv"))
  ##
  ## expect_equal(model_run$indicators_path, output_indicators)
  ## file_list <- unzip(model_run$indicators_path, list = TRUE)
  ## expect_equal(file_list$Name,
  ##              c("boundaries.geojson", "indicators.csv", "meta_age_group.csv",
  ##                "meta_area.csv", "meta_indicator.csv", "meta_period.csv"))
})
