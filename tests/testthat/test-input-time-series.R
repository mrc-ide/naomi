test_that("ART data can be aggregated", {
  data <- aggregate_art(a_hintr_data$art_number,
                        a_hintr_data$shape)

  expect_true(nrow(data) > 100) ## Check that we have read out some data
  expect_setequal(colnames(data),
                  c("area_id", "area_name",  "area_level","area_level_label",
                    "parent_area_id", "area_sort_order", "sex", "age_group",
                    "time_period", "year", "quarter", "calendar_quarter", "area_hierarchy",
                    "art_current", "art_new", "vl_tested_12mos", "vl_suppressed_12mos"))


  # Time period has correct format
  expect_match(as.character(data$time_period), "\\d{4}")

  # Check that data has been aggregated to all levels in hierarchy from baseline
  # determined by highest area_level in program data
  shape <- sf::read_sf(a_hintr_data$shape)
  art_number <- readr::read_csv(a_hintr_data$art_number, show_col_types = FALSE) %>%
    dplyr::left_join(shape %>% dplyr::select(area_id, area_level), by = "area_id")

  # Check data has been aggregated from correct baseline
  expect_equal(max(data$area_level), unique(art_number$area_level))
  # Check data has been aggregated from baseline to lowest level in hierarchy
  shape_level <- unique(shape$area_level)[unique(shape$area_level) <= unique(art_number$area_level)]
  expect_equal(unique(data$area_level), shape_level)

  ## Area hierarchy is formatted correctly
  check_hierarchy_format <- function(hierarchy) {
    grepl("[\\w\\-/ ]+", hierarchy, perl = TRUE)
  }
  expect_true(all(check_hierarchy_format(data$area_hierarchy)))

  ## Check no of / in hierarchy is correct i.e. the label is for the correct
  ## level. For country level values should be the area name
  ## For below country level values should have no of / equal to 1 less
  ## than their area level
  country_level <- data[data$area_level == 0, ]
  expect_true(all(country_level$area_hierarchy == country_level$area_name))
  hierarchy <-  data[data$area_level > 0, ]
  slash_count <- lengths(regmatches(hierarchy$area_hierarchy,
                                    gregexpr("/", hierarchy$area_hierarchy)))
  expect_equal(slash_count, hierarchy$area_level - 1)

  ## Check that there is only a single age_group, time_period + quater value for
  ## each area_id

  dup_strata <- data %>%
    dplyr::group_by(area_id,sex,age_group, time_period, calendar_quarter) %>%
    dplyr::filter(dplyr::n() > 1)

  expect_true(nrow(dup_strata) == 0)


})

test_that("ART data can be aggregated when avalible at different admin levels", {

  # Create dummy data simillar to MOZ edge case:
  # ART data at admin1 2014-2016, admin2 2015-2016
  data <- aggregate_art(a_hintr_data$art_number,
                        a_hintr_data$shape)

  admin1_data <- dplyr::filter(data, area_level == 1,
                               calendar_quarter %in% c("CY2014Q4","CY2015Q4"))

  admin2_data <- dplyr::filter(data, area_level == 2,
                               calendar_quarter %in% c("CY2016Q4","CY2017Q4","CY2018Q4"))

  test_data <- rbind(admin1_data, admin2_data)
  test_data <- test_data[names(data)]

  # Aggregate data
  art_agg <- aggregate_art(test_data, a_hintr_data$shape)

  # Aggregated data has data for all years provided
  expect_equal(unique(art_agg$calendar_quarter),
               unique(test_data$calendar_quarter))

  # Data has been aggregated correctly
  df1 <- dplyr::filter(art_agg, year %in% c("2014","2015"))
  expect_equal(unique(df1$area_level), c(0, 1))

  df2 <- dplyr::filter(art_agg, year %in% c("2016", "2017", "2018"))
  expect_equal(unique(df2$area_level), c(0, 1, 2))

  ## Check that aggregated values are equal
  data_long <- data  %>%
    tidyr::pivot_longer(c(art_current, art_new, vl_tested_12mos, vl_suppressed_12mos)) %>%
    dplyr::select(area_id, sex, age_group, calendar_quarter, name, value_raw = value)

  art_agg_long <- art_agg  %>%
    tidyr::pivot_longer(c(art_current, art_new, vl_tested_12mos, vl_suppressed_12mos)) %>%
    dplyr::select(area_id, sex, age_group, calendar_quarter, name, value_check = value)

  data_check <- art_agg_long %>%
    dplyr::inner_join(data_long, by = c("area_id", "sex", "age_group", "calendar_quarter", "name"))

  expect_equal(nrow(data_check), nrow(art_agg_long))
  expect_equal(data_check$value_check, data_check$value_raw)

})


test_that("data can be formatted for ART input time series", {
  data <- prepare_input_time_series_art(a_hintr_data$art_number,
                                        a_hintr_data$shape)

  expect_true(nrow(data) > 50) ## Check that we have read out some data
  expect_setequal(colnames(data),
                  c("area_id", "area_name",  "area_level", "area_level_label",
                    "parent_area_id", "area_sort_order",  "time_period", "year",
                    "quarter","calendar_quarter", "area_hierarchy", "plot", "value"))

  # Time period has correct format
  expect_match(as.character(data$time_period), "\\d{4}")

  ## Check that there is only a single age_group, time_period + quater value for
  ## each area_id

  dup_strata <- data %>%
    dplyr::group_by(area_id, time_period, calendar_quarter, plot) %>%
    dplyr::filter(dplyr::n() > 1)

  expect_true(nrow(dup_strata) == 0)
})


test_that("ANC data can be aggregated", {
  data <- aggregate_anc(a_hintr_data$anc_testing,
                        a_hintr_data$shape)

  expect_true(nrow(data) > 50) ## Check that we have read out some data
  expect_setequal(
    colnames(data),
    c(
      "area_id", "area_name", "area_level", "area_level_label",
      "parent_area_id", "area_sort_order", "sex", "age_group",
      "time_period", "year", "quarter", "calendar_quarter",
      "anc_clients", "anc_known_pos", "anc_already_art", "anc_tested",
      "anc_tested_pos", "anc_known_neg", "births_facility",
      "area_hierarchy"
    )
  )


  # Time period has correct format
  expect_match(as.character(data$time_period), "\\d{4}")

  # Check that data has been aggregated to all levels in hierarchy from baseline
  # determined by highest area_level in program data
  shape <- sf::read_sf(a_hintr_data$shape)
  anc_testing <- readr::read_csv(a_hintr_data$anc_testing, show_col_types = FALSE) %>%
    dplyr::left_join(shape %>% dplyr::select(area_id, area_level), by = "area_id")

  # Check data has been aggregated from correct baseline
  expect_equal(max(data$area_level), unique(anc_testing$area_level))
  # Check data has been aggregated from baseline to lowest level in hierarchy
  shape_level <- unique(shape$area_level)[unique(shape$area_level) <= unique(anc_testing$area_level)]
  expect_equal(unique(data$area_level), shape_level)

  ## Area hierarchy is formatted correctly
  check_hierarchy_format <- function(hierarchy) {
    grepl("[\\w\\-/ ]+", hierarchy, perl = TRUE)
  }
  expect_true(all(check_hierarchy_format(data$area_hierarchy)))

  ## Check no of / in hierarchy is correct i.e. the label is for the correct
  ## level. For country level values should be the area name
  ## For below country level values should have no of / equal to 1 less
  ## than their area level
  country_level <- data[data$area_level == 0, ]
  expect_true(all(country_level$area_hierarchy == country_level$area_name))
  hierarchy <-  data[data$area_level > 0, ]
  slash_count <- lengths(regmatches(hierarchy$area_hierarchy,
                                    gregexpr("/", hierarchy$area_hierarchy)))
  expect_equal(slash_count, hierarchy$area_level - 1)

  ## Check that there is only a single age_group, time_period + quater value for
  ## each area_id

  dup_strata <- data %>%
    dplyr::group_by(area_id, sex, age_group, time_period, calendar_quarter) %>%
    dplyr::filter(dplyr::n() > 1)

  expect_true(nrow(dup_strata) == 0)
})


test_that("data can be formatted for ANC input time series", {
  data <- prepare_input_time_series_anc(a_hintr_data$anc_testing,
                                        a_hintr_data$shape)
  expect_true(nrow(data) > 100) ## Check that we have read out some data
  expect_setequal(colnames(data),
                  c("area_id", "area_name", "area_level", "area_level_label",
                    "parent_area_id", "area_sort_order", "age_group",
                    "time_period", "year", "quarter", "calendar_quarter",
                    "plot", "value", "area_hierarchy"))

  expect_setequal(unique(data$plot),
                  c("anc_clients", "anc_tested", "anc_tested_pos",
                    "anc_prevalence", "anc_known_pos", "anc_known_neg",
                    "anc_art_coverage", "births_clients_ratio", "births_facility"))

  # Time period has correct format
  expect_match(as.character(data$time_period), "\\d{4}")

  ## Check that there is only a single age_group, time_period + quater value for
  ## each area_id

  dup_strata <- data %>%
    dplyr::group_by(area_id, time_period, calendar_quarter, plot) %>%
    dplyr::filter(dplyr::n() > 1)

  expect_true(nrow(dup_strata) == 0)
})



test_that("plots are filtered according to avalible disaggregates", {

  dir <- tempdir()
  art_number <- readr::read_csv(a_hintr_data$art_number, show_col_types = FALSE)

  adult_f <- art_number %>% dplyr::filter(age_group == "Y015_999") %>% dplyr::mutate(sex = "female")
  adult_m <- art_number %>% dplyr::filter(age_group == "Y015_999") %>% dplyr::mutate(sex ="male")
  peads <- art_number %>% dplyr::filter(age_group == "Y000_014")

  # Check data with sex aggregated, age disaggregated
  data <- prepare_input_time_series_art(a_hintr_data$art_number,
                                        a_hintr_data$shape)
  expect_setequal(unique(data$plot),
                  c( "art_total","art_adult","art_child",
                     "art_adult_child_ratio","art_prop_u15","art_new_total",
                     "art_new_adult","art_new_child","vl_tested_12mos_total",
                     "vl_tested_12mos_adult","vl_tested_12mos_child","vl_suppressed_12mos_total",
                     "vl_suppressed_12mos_adult","vl_suppressed_12mos_child" , "vl_coverage_total",
                     "vl_coverage_adult", "vl_coverage_child","vl_prop_suppressed_total",
                     "vl_prop_suppressed_adult", "vl_prop_suppressed_child"))

  # Check data with sex disaggregated, age disaggregated
  test1 <- rbind(adult_f, adult_m, peads)
  test1_file<- paste0(dir, "test1.csv")
  readr::write_csv(test1, test1_file)

  data1 <- prepare_input_time_series_art(test1_file,
                                         a_hintr_data$shape)
  expect_setequal(unique(data1$plot),
                  c("art_adult" , "art_adult_child_ratio", "art_child" ,
                    "art_prop_u15", "art_total", "art_adult_f","art_adult_m",
                    "art_adult_sex_ratio", "art_new_total","art_new_adult",
                    "art_new_adult_f", "art_new_adult_m", "art_new_child",
                    "vl_tested_12mos_total","vl_tested_12mos_adult", "vl_tested_12mos_adult_f",
                    "vl_tested_12mos_adult_m", "vl_tested_12mos_child","vl_suppressed_12mos_total",
                    "vl_suppressed_12mos_adult","vl_suppressed_12mos_adult_f","vl_suppressed_12mos_adult_m",
                    "vl_suppressed_12mos_child","vl_coverage_total","vl_coverage_adult",
                    "vl_coverage_adult_f","vl_coverage_adult_m","vl_coverage_child",
                    "vl_prop_suppressed_total", "vl_prop_suppressed_adult","vl_prop_suppressed_adult_f",
                    "vl_prop_suppressed_adult_m","vl_prop_suppressed_child"))

  # Check data with sex disaggregated, age aggregated
  test2 <- rbind(adult_f, adult_m)
  test2_file<- paste0(dir, "test2.csv")
  readr::write_csv(test2, test2_file)

  data2 <- prepare_input_time_series_art(test2_file,
                                         a_hintr_data$shape)
  expect_setequal(unique(data2$plot),
                  c("art_adult" , "art_total","art_adult_f","art_adult_m",
                    "art_adult_sex_ratio","art_new_total","art_new_adult","art_new_adult_f",
                    "art_new_adult_m",  "vl_tested_12mos_total","vl_tested_12mos_adult",
                    "vl_tested_12mos_adult_f", "vl_tested_12mos_adult_m", "vl_suppressed_12mos_total",
                    "vl_suppressed_12mos_adult","vl_suppressed_12mos_adult_f","vl_suppressed_12mos_adult_m",
                    "vl_coverage_total","vl_coverage_adult", "vl_coverage_adult_f",
                    "vl_coverage_adult_m","vl_prop_suppressed_total",
                    "vl_prop_suppressed_adult", "vl_prop_suppressed_adult_f",
                    "vl_prop_suppressed_adult_m"))
})


test_that("can get plot type descriptions from key", {
  ret <- get_plot_type_column_metadata(c("art_total", "art_child"))
  expect_equivalent(ret, list(
    list(
      id = "art_total",
      label = "ART count",
      description = "Number on ART at the end of calendar year",
      format = "0,0",
      accuracy = NA_integer_
    ),
    list(
      id = "art_child",
      label = "ART paediatric",
      description = "Number of children (<15) on ART at the end of calendar year",
      format = "0,0",
      accuracy = NA_integer_
    )
  ))
})

test_that("data can be aggregated without all indicators", {

  art <- readr::read_csv(a_hintr_data$art_number)

  # data with no art_new
  no_art_new <- art
  no_art_new$art_new <- NULL

  data <- prepare_input_time_series_art(no_art_new,
                                        a_hintr_data$shape)
  expect_setequal(unique(data$plot),
                  c( "art_total" ,"art_adult","art_child",
                     "art_adult_child_ratio","art_prop_u15","vl_tested_12mos_total",
                     "vl_tested_12mos_adult","vl_tested_12mos_child","vl_suppressed_12mos_total",
                     "vl_suppressed_12mos_adult","vl_suppressed_12mos_child","vl_coverage_total",
                     "vl_coverage_adult" ,"vl_coverage_child","vl_prop_suppressed_total",
                     "vl_prop_suppressed_adult","vl_prop_suppressed_child"))

  # data with no vls indicators
  no_vls <- art
  no_vls$vl_tested_12mos <- NULL
  no_vls$vl_suppressed_12mos <- NULL

  data <- prepare_input_time_series_art(no_vls,
                                        a_hintr_data$shape)
  expect_setequal(unique(data$plot),
                  c("art_total", "art_adult","art_child","art_adult_child_ratio",
                    "art_prop_u15","art_new_total","art_new_adult","art_new_child"))

  # data with no art_new or vls indicators
  no_vls_art_new <- no_vls
  no_vls_art_new$art_new <- NULL

  data <- prepare_input_time_series_art(no_vls_art_new,
                                        a_hintr_data$shape)
  expect_setequal(unique(data$plot),
                  c("art_total", "art_adult","art_child","art_adult_child_ratio",
                    "art_prop_u15"))

})


test_that("anc input time series can handle data with NA rows", {
  ## This is a regression test for issue #41 Mozambique
  data <- read.csv(a_hintr_data$anc_testing)
  t <- tempfile(fileext = ".csv")
  data <- rbind(data, c("", "", "", NA, NA, NA, NA, NA, NA, NA, NA))
  write.csv(data, t, row.names = FALSE)
  data <- prepare_input_time_series_anc(t, a_hintr_data$shape)
  ## Check that NA entry has been removed
  expect_true(!any(is.na(unique(data$age_group))))
})


test_that("ANC data without births_facility can be aggregated", {
  anc <- read_anc_testing(a_hintr_data$anc_testing)
  anc$births_facility <- NULL
  t <- tempfile(fileext = ".csv")
  readr::write_csv(anc, t, na = "")

  data <- aggregate_anc(t, a_hintr_data$shape)

  expect_true(nrow(data) > 50) ## Check that we have read out some data
  expect_setequal(colnames(data),
                  c("area_id", "area_name", "area_level", "area_level_label",
                    "parent_area_id", "area_sort_order", "sex", "age_group",
                    "time_period", "year", "quarter", "calendar_quarter",
                    "anc_clients", "anc_known_pos", "anc_already_art",
                    "anc_tested", "anc_tested_pos", "anc_known_neg",
                    "births_facility", "area_hierarchy"))

  expect_equal(data$births_facility, rep(0, nrow(data)))
})

test_that("aggregate_anc() and aggregate_art() discard additional columns", {

  anc <- read_anc_testing(a_hintr_data$anc_testing)
  anc$area_level <- 4

  data <- aggregate_anc(anc, a_hintr_data$shape)

  expect_true(nrow(data) > 50) ## Check that we have read out some data
  expect_setequal(colnames(data),
                  c("area_id", "area_name", "area_level", "area_level_label",
                    "parent_area_id", "area_sort_order", "sex", "age_group",
                    "time_period", "year", "quarter", "calendar_quarter",
                    "anc_clients", "anc_known_pos", "anc_already_art",
                    "anc_tested", "anc_tested_pos", "anc_known_neg",
                    "births_facility", "area_hierarchy"))

  art <- readr::read_csv(a_hintr_data$art_number)
  art$area_level <- 4
  data <- aggregate_art(art, a_hintr_data$shape)

  expect_true(nrow(data) > 100) ## Check that we have read out some data
  expect_setequal(colnames(data),
                  c("area_id", "area_name",  "area_level", "area_level_label",
                    "parent_area_id", "area_sort_order", "sex", "age_group",
                    "time_period", "year", "quarter", "calendar_quarter",
                    "area_hierarchy", "art_current", "art_new",
                    "vl_tested_12mos", "vl_suppressed_12mos"))
})

test_that("there is metadata for every indicator", {
  anc <- prepare_input_time_series_anc(a_hintr_data$anc_testing,
                                       a_hintr_data$shape)

  art_number <- readr::read_csv(a_hintr_data$art_number, show_col_types = FALSE)
  adult_f <- art_number %>%
    dplyr::filter(age_group == "Y015_999") %>%
    dplyr::mutate(sex = "female")
  adult_m <- art_number %>%
    dplyr::filter(age_group == "Y015_999") %>%
    dplyr::mutate(sex = "male")
  peads <- art_number %>% dplyr::filter(age_group == "Y000_014")


  # Check data with sex disaggregated, age aggregated
  art <- rbind(adult_f, adult_m, peads)
  art_data <- tempfile(fileext = ".csv")
  readr::write_csv(art, art_data)

  art <- prepare_input_time_series_art(art, a_hintr_data$shape)

  plot_types <- unique(c(anc$plot, art$plot))
  metadata <- naomi_read_csv(
    system_file("metadata", "time_series_plot_metadata.csv"),
    col_types = readr::cols(.default = "c"))
  expect_setequal(plot_types, metadata$id)
})
