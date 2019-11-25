context("model-options")

test_that("can get valid model run options template", {
  options <- get_model_options_template(TRUE, TRUE)
  expect_length(options, 4)
  expect_equal(names(options), c("general", "survey", "art", "anc"))
  expect_true(any(grepl("General", options$general)))
  expect_true(any(grepl("<area_scope_options>", options$general)))
  expect_true(any(grepl("<area_level_options>", options$general)))
  expect_true(any(grepl("<calendar_quarter_t1_options>", options$general)))
  expect_true(any(grepl("<calendar_quarter_t2_options>", options$general)))

  expect_true(any(grepl("Survey", options$survey)))
  expect_true(any(grepl("<survey_prevalence_options>", options$survey)))
  expect_true(any(grepl("<survey_art_coverage_options>", options$survey)))
  expect_true(any(grepl("<survey_vls_options>", options$survey)))
  expect_true(any(grepl("<survey_recently_infected_options>", options$survey)))
  expect_true(any(grepl("<survey_art_or_vls_options>", options$survey)))

  expect_true(any(grepl("ART", options$art)))

  expect_true(any(grepl("ANC", options$anc)))
  expect_true(any(grepl("<anc_prevalence_year1_options>", options$anc)))
  expect_true(any(grepl("<anc_prevalence_year2_options>", options$anc)))
  expect_true(any(grepl("<anc_art_coverage_year1_options>", options$anc)))
  expect_true(any(grepl("<anc_art_coverage_year2_options>", options$anc)))
})

test_that("art and anc data can be omitted from model run options", {
  options <- get_model_options_template(FALSE, FALSE)
  expect_length(options, 2)
  expect_equal(names(options), c("general", "survey"))
  expect_true(any(grepl("General", options$general)))
  expect_true(any(grepl("<area_scope_options>", options$general)))
  expect_true(any(grepl("<area_level_options>", options$general)))
  expect_true(any(grepl("<calendar_quarter_t1_options>", options$general)))
  expect_true(any(grepl("<calendar_quarter_t2_options>", options$general)))

  expect_true(any(grepl("Survey", options$survey)))
  expect_true(any(grepl("<survey_prevalence_options>", options$survey)))
  expect_true(any(grepl("<survey_art_coverage_options>", options$survey)))
  expect_true(any(grepl("<survey_vls_options>", options$survey)))
  expect_true(any(grepl("<survey_recently_infected_options>", options$survey)))
  expect_true(any(grepl("<survey_art_or_vls_options>", options$survey)))

  expect_false(any(grepl("ART", options$art)))
  expect_false(any(grepl("ANC", options$anc)))
})

test_that("validate model options returns true", {
  data <- list(
    pjnz = system_file("extdata/mwi2019.PJNZ"),
    population = system_file("extdata/population/population_agesex.csv"),
    shape = file.path("testdata/malawi.geojson"),
    survey = system_file("extdata/survey/survey_hiv_indicators.csv"),
    programme = system_file("extdata/programme/art_number.csv"),
    anc = system_file("extdata/programme/anc_testing.csv")
  )
  options <- list(
    option1 = "one"
  )
  expect_true(validate_model_options(data, options))
})

