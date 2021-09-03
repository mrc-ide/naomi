test_that("data can be formatted for ART input time series", {
  data <- prepare_input_time_series_art(a_hintr_data$art_number,
                                        a_hintr_data$shape)

  expect_true(nrow(data) > 100) ## Check that we have read out some data
  expect_setequal(colnames(data),
                  c("area_id", "area_name", "area_level_label","area_level",
                    "time_step", "time_period", "plot", "value"))

  # Time period has correct format
  expect_setequal(data$time_step, "annual")
  expect_match(as.character(data$time_period), "\\d{4}")
})

test_that("data can be formatted for ANC input time series", {
  data <- prepare_input_time_series_anc(a_hintr_data$anc_testing,
                                        a_hintr_data$shape)
  expect_true(nrow(data) > 100) ## Check that we have read out some data
  expect_setequal(colnames(data),
                  c("area_id", "area_name", "area_level", "area_level_label",
                    "age_group", "time_period", "time_step", "plot", "value"))

  # Time period has correct format
  expect_setequal(data$time_step, "annual")
  expect_match(as.character(data$time_period), "\\d{4}")
})

test_that("ART data can be aggregated by time and space", {

  art_number <- readr::read_csv(a_hintr_data$art_number, show_col_types = FALSE)

  # Make dummy quarterly data
  art_q <- art_number %>%
    dplyr::mutate(art_current = art_current/4,
           calendar_quarter = paste0("CY",year, "Q1")) %>%
    rbind(art_number %>%
            dplyr::mutate(art_current = art_current/4,
                   calendar_quarter = paste0("CY",year, "Q2"))) %>%
    rbind(art_number %>%
            dplyr::mutate(art_current = art_current/4,
                   calendar_quarter = paste0("CY",year, "Q3"))) %>%
    rbind(art_number %>%
            dplyr::mutate(art_current = art_current/4)) %>%
    dplyr::select(-c(art_new))

  dir <- tempdir()
  art_q_file <- paste0(dir, "art_q.csv")
  readr::write_csv(art_q, art_q_file)

  data <- prepare_input_time_series_art(art_q_file,
                                        a_hintr_data$shape)

  ## Check that data has been aggregated to all levels in hierarchy from baseline
  ## determined by highest area_level in program data
  shape <- sf::read_sf(a_hintr_data$shape)
  art_number <- art_number %>%
    dplyr::left_join(shape %>% dplyr::select(area_id, area_level), by = "area_id")

  # Check data has been aggregated from correct baseline
  expect_equal(unique(art_number$area_level), max(data$area_level))
  # Check data has been aggregated from baseline to lowest level in hierarchy
  shape_level <- unique(shape$area_level)[unique(shape$area_level) <= unique(art_number$area_level)]
  expect_equal(shape_level, unique(data$area_level))

  # Time period has correct format
  expect_setequal(data$time_step, "quarterly")
  expect_match(data$time_period, "\\d{4} Q\\d{1}")
})

test_that("plots are filtered according to avalible disaggregates", {

  dir <- tempdir()
  art_number <- readr::read_csv(a_hintr_data$art_number, show_col_types = FALSE) %>%
    dplyr::select(-c(art_new))

  adult_f <- art_number %>% dplyr::filter(age_group == "Y015_999") %>% dplyr::mutate(sex = "female")
  adult_m <- art_number %>% dplyr::filter(age_group == "Y015_999") %>% dplyr::mutate(sex ="male")
  peads <- art_number %>% dplyr::filter(age_group == "Y000_014")

  # Check data with sex aggregated, age disaggregated
  data <- prepare_input_time_series_art(a_hintr_data$art_number,
                                        a_hintr_data$shape)
  expect_setequal(unique(data$plot),
                  c("art_adult" , "art_adult_child_ratio", "art_child" , "art_prop_u15",
                    "art_total"))

  # Check data with sex disaggregated, age disaggregated
  test1 <- rbind(adult_f, adult_m, peads)
  test1_file<- paste0(dir, "test1.csv")
  readr::write_csv(test1, test1_file)

  data1 <- prepare_input_time_series_art(test1_file,
                                             a_hintr_data$shape)
  expect_setequal(unique(data1$plot),
                  c("art_adult" , "art_adult_child_ratio", "art_child" ,
                    "art_prop_u15", "art_total", "art_adult_f","art_adult_m",
                    "art_adult_sex_ratio"))

  # Check data with sex disaggregated, age aggregated
  test2 <- rbind(adult_f, adult_m)
  test2_file<- paste0(dir, "test2.csv")
  readr::write_csv(test2, test2_file)

  data2 <- prepare_input_time_series_art(test2_file,
                                         a_hintr_data$shape)
  expect_setequal(unique(data2$plot),
                  c("art_adult" ,"art_total", "art_adult_f","art_adult_m",
                    "art_adult_sex_ratio"))
})

test_that("ANC data can be aggregated by space", {

  data <- prepare_input_time_series_anc(a_hintr_data$anc_testing,
                                        a_hintr_data$shape)

  ## Check that data has been aggregated to all levels in hierarchy from baseline
  ## determined by highest area_level in program data
  shape <- sf::read_sf(a_hintr_data$shape)
  anc_testing <- readr::read_csv(a_hintr_data$anc_testing) %>%
    dplyr::left_join(shape %>% dplyr::select(area_id, area_level), by = "area_id")

  # Check data has been aggregated from correct baseline
  expect_equal(max(data$area_level), unique(anc_testing$area_level))
  # Check data has been aggregated from baseline to lowest level in hierarchy
  shape_level <- unique(shape$area_level)[unique(shape$area_level) <= unique(anc_testing$area_level)]
  expect_equal(unique(data$area_level), shape_level)

  # Time period has correct format
  expect_setequal(data$time_step, "annual")
  expect_match(as.character(data$time_period), "\\d{4}")
})

test_that("ART data throws error if dupe annual data or incomplete quarterly", {

  art_number <- readr::read_csv(a_hintr_data$art_number, show_col_types = FALSE)

  # Make dummy quarterly data
  art_q <-art_number %>%
    dplyr::mutate(art_current = art_current/4,
                  calendar_quarter = paste0("CY",year, "Q1")) %>%
    rbind(art_number %>%
            dplyr::mutate(art_current = art_current/4,
                          calendar_quarter = paste0("CY",year, "Q2"))) %>%
    dplyr::select(-c(art_new))

  dir <- tempdir()
  art_q_file <- paste0(dir, "art_q.csv")
  readr::write_csv(art_q, art_q_file)

  data <- expect_error(
    prepare_input_time_series_art(art_q_file, a_hintr_data$shape),
    paste0("Quarterly data not provided for all disaggregates or duplicate ",
           "annual data provided for all disaggregates."))
})
