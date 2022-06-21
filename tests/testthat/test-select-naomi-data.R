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

test_that("Full data inputs and filtered data inputs do not intersect", {

  quiet_semi_join <- function(x,y){suppressMessages(dplyr::semi_join(x,y))}

  check_filtered_inputs <- function(raw_input, model_input){
    # Filter raw input for excluded observations
    excluded <- raw_input[raw_input$data_type == "raw_excluded",]
    included <- model_input
    # Check that there are no overlaps between model input data and excluded data
    expect_equal(nrow(quiet_semi_join(included, excluded)), 0)
    }

  # Survey data
  check_filtered_inputs(a_naomi_data$prev_dat$raw_input,
                        a_naomi_data$prev_dat$model_input)

  # ANC data
  check_filtered_inputs(a_naomi_data$anc_prev_t1_dat$raw_input,
                        a_naomi_data$anc_prev_t1_dat$model_input)


  check_filtered_inputs(a_naomi_data$anc_artcov_t1_dat$raw_input,
                        a_naomi_data$anc_artcov_t1_dat$model_input)

  check_filtered_inputs(a_naomi_data$anc_clients_t2_dat$raw_input,
                        a_naomi_data$anc_clients_t2_dat$model_input)

  # ART data
  check_filtered_inputs(a_naomi_data$artnum_t1_dat$raw_input,
                        a_naomi_data$artnum_t1_dat$model_input)



})


