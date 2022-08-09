context("test-select-naomi-data")

test_that("select ANC programme data returns expected rows", {

  anc_testing <- read_anc_testing(system_file("extdata/demo_anc_testing.csv"))

  anc_testing_prev_mf <- anc_testing_prev_mf(2017, anc_testing, a_naomi_mf)
  expect_equal(nrow(anc_testing_prev_mf$model_input), nrow(a_naomi_mf$mf_areas))

  anc_testing_artcov_mf <- anc_testing_artcov_mf(2017, anc_testing, a_naomi_mf)
  expect_equal(nrow(anc_testing_artcov_mf$model_input),nrow(a_naomi_mf$mf_areas))

  ## Year not in data
  expect_error(nrow(anc_testing_prev_mf(2000, anc_testing, a_naomi_mf)),
               "ANC testing data not found for year 2000")
  expect_error(nrow(anc_testing_artcov_mf(2000, anc_testing, a_naomi_mf)),
               "ANC testing data not found for year 2000")

  ## NULL data provided
  anc_testing_prev_mf <- anc_testing_prev_mf(2017, NULL, a_naomi_mf)
  expect_equal(nrow(anc_testing_prev_mf$model_input), 0)
  anc_testing_artcov_mf <- anc_testing_artcov_mf(2017, NULL, a_naomi_mf)
  expect_equal(nrow(anc_testing_artcov_mf$model_input), 0)

  ## NULL year provided
  anc_testing_prev_mf <- anc_testing_prev_mf(NULL, anc_testing, a_naomi_mf)
  expect_equal(nrow(anc_testing_prev_mf$model_input), 0)
  anc_testing_artcov_mf <- anc_testing_artcov_mf(NULL, anc_testing, a_naomi_mf)
  expect_equal(nrow(anc_testing_artcov_mf$model_input), 0)

})

test_that("Data inputs aggregated and tagged correctly", {

  quiet_semi_join <- function(x,y){suppressMessages(dplyr::semi_join(x,y))}

  # Model inputs do not intersect with excluded data
  # Survey data
  survey_tagged <- a_naomi_data$model_inputs$survey_full_mf
  survey_excluded <- dplyr::filter(survey_tagged, naomi_input == FALSE)
  survey_included <- dplyr::filter(survey_tagged, naomi_input == TRUE)
  expect_equal(nrow(quiet_semi_join(survey_included, survey_excluded)), 0)

  # ART data
  artnum_tagged <- a_naomi_data$model_inputs$artnum_full_mf
  artnum_excluded <- dplyr::filter(artnum_tagged, naomi_input == FALSE)
  artnum_included <- dplyr::filter(artnum_tagged, naomi_input == TRUE)
  expect_equal(nrow(quiet_semi_join(artnum_included, artnum_excluded)), 0)

  # ANC data
  anc_tagged <- a_naomi_data$model_inputs$artnum_full_
  anc_excluded <- dplyr::filter(anc_tagged, naomi_input == FALSE)
  anc_included <- dplyr::filter(anc_tagged, naomi_input == TRUE)
  expect_equal(nrow(quiet_semi_join(anc_included, anc_excluded)), 0)

  # Excluded data contains aggregated area_id/age/sex strata and included data
  # contains strata specified in model options

  aggregated_ids <- a_area_merged[a_area_merged$area_level < 4,]$area_id
  model_input_ids <- a_area_merged[a_area_merged$area_level == 4,]$area_id

  expect_true(!any(aggregated_ids %in% unique(survey_included$area_id)))
  expect_true(!any(aggregated_ids %in% unique(artnum_included$area_id)))
  expect_true(!any(aggregated_ids %in% unique(anc_included$area_id)))

})
