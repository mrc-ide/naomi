test_that("select ANC programme data returns expected rows", {

  # Test output created by anc_testing_prev_mf()
  expect_equal(nrow(a_naomi_data$anc_prev_t1_dat), nrow(a_naomi_mf$mf_areas))
  # Test output created by anc_testing_artcov_mf()
  expect_equal(nrow(a_naomi_data$anc_artcov_t1_dat),nrow(a_naomi_mf$mf_areas))
  # Test output created by anc_testing_clients_mf()
  expect_equal(nrow(a_naomi_data$anc_artcov_t1_dat),nrow(a_naomi_mf$mf_areas))

  ## Year not in data
  # Test output created by anc_testing_prev_mf()
  expect_error(
    select_naomi_data(a_naomi_mf,
                      demo_survey_hiv_indicators,
                      anc_testing = demo_anc_testing,
                      demo_art_number,
                      prev_survey_ids = c("DEMO2016PHIA", "DEMO2015DHS"),
                      artcov_survey_ids = "DEMO2016PHIA",
                      recent_survey_ids = "DEMO2016PHIA",
                      anc_prev_year_t1 = 2000),
    "ANC testing data not found for year 2000")

  # Test output created by anc_testing_artcov_mf()
  expect_error(
    select_naomi_data(a_naomi_mf,
                      demo_survey_hiv_indicators,
                      anc_testing = demo_anc_testing,
                      demo_art_number,
                      prev_survey_ids = c("DEMO2016PHIA", "DEMO2015DHS"),
                      artcov_survey_ids = "DEMO2016PHIA",
                      recent_survey_ids = "DEMO2016PHIA",
                      anc_artcov_year_t1 = 1999),
    "ANC testing data not found for year 1999")

  # Test output created by anc_testing_clients_mf()
  expect_error(
    select_naomi_data(a_naomi_mf,
                      demo_survey_hiv_indicators,
                      anc_testing = demo_anc_testing,
                      demo_art_number,
                      prev_survey_ids = c("DEMO2016PHIA", "DEMO2015DHS"),
                      artcov_survey_ids = "DEMO2016PHIA",
                      recent_survey_ids = "DEMO2016PHIA",
                      anc_clients_year_t2 = 1992),
    "ANC testing data not found for year 1992")


  ## NULL data provided
  naomi_data_null_anc <- select_naomi_data(a_naomi_mf,
                                    demo_survey_hiv_indicators,
                                    anc_testing = NULL,
                                    demo_art_number,
                                    prev_survey_ids = c("DEMO2016PHIA", "DEMO2015DHS"),
                                    artcov_survey_ids = "DEMO2016PHIA",
                                    recent_survey_ids = "DEMO2016PHIA")

  # Test output created by anc_testing_prev_mf()
  expect_equal(nrow(naomi_data_null_anc$anc_prev_t1_dat),0)
  # Test output created by anc_testing_artcov_mf()
  expect_equal(nrow(naomi_data_null_anc$anc_artcov_t1_dat),0)
  # Test output created by anc_testing_clients_mf()
  expect_equal(nrow(naomi_data_null_anc$anc_artcov_t1_dat),0)

  ## NULL year provided

  naomi_data_null_anc_year <- select_naomi_data(a_naomi_mf,
                                           demo_survey_hiv_indicators,
                                           anc_testing = demo_anc_testing,
                                           demo_art_number,
                                           prev_survey_ids = c("DEMO2016PHIA", "DEMO2015DHS"),
                                           artcov_survey_ids = "DEMO2016PHIA",
                                           recent_survey_ids = "DEMO2016PHIA",
                                           anc_prev_year_t1 = NULL,
                                           anc_prev_year_t2 = NULL,
                                           anc_artcov_year_t1 = NULL,
                                           anc_artcov_year_t2 = NULL,
                                           anc_clients_year_t2 = NULL)

  # Test output created by anc_testing_prev_mf()
  expect_equal(nrow(naomi_data_null_anc_year$anc_prev_t1_dat),0)
  # Test output created by anc_testing_artcov_mf()
  expect_equal(nrow(naomi_data_null_anc_year$anc_artcov_t1_dat),0)
  # Test output created by anc_testing_clients_mf()
  expect_equal(nrow(naomi_data_null_anc_year$anc_artcov_t1_dat),0)

})

test_that("Data inputs aggregated and tagged correctly", {

  quiet_semi_join <- function(x,y){suppressMessages(dplyr::semi_join(x,y))}

  # Model inputs do not intersect with excluded data
  # Survey data
  survey_tagged <- a_naomi_data$full_data$survey_full_mf
  survey_excluded <- dplyr::filter(survey_tagged, naomi_input == FALSE)
  survey_included <- dplyr::filter(survey_tagged, naomi_input == TRUE)
  expect_equal(nrow(quiet_semi_join(survey_included, survey_excluded)), 0)

  # ART data
  artnum_tagged <- a_naomi_data$full_data$artnum_full_mf
  artnum_excluded <- dplyr::filter(artnum_tagged, naomi_input == FALSE)
  artnum_included <- dplyr::filter(artnum_tagged, naomi_input == TRUE)
  expect_equal(nrow(quiet_semi_join(artnum_included, artnum_excluded)), 0)

  # ANC data
  anc_tagged <- a_naomi_data$full_data$artnum_full_
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

