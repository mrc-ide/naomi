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


  # (1) Data provided at different levels for different years
  # Create dummy data similar to MOZ edge case:
  # ART data at admin1 2014-2016, admin2 2015-2016
  data <- aggregate_art(a_hintr_data$art_number,
                        a_hintr_data$shape)

  admin1_data <- dplyr::filter(data, area_level == 1,
                               calendar_quarter %in% c("CY2014Q4","CY2015Q4"))

  admin2_data <- dplyr::filter(data, area_level == 2,
                               calendar_quarter %in% c("CY2016Q4","CY2017Q4","CY2018Q4"))

  test_data1 <- rbind(admin1_data, admin2_data)
  test_data1 <- test_data1[names(data)]

  # Aggregate data
  art_agg1 <- aggregate_art(test_data1, a_hintr_data$shape)

  # Check for different of records at each area aggregation
  check1 <- art_agg1 %>%
    dplyr::group_by(area_level_label, year, age_group, sex) %>%
    dplyr::summarise(.groups = "drop") %>%
    dplyr::group_by(area_level_label) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  expect_equal(check1$n, c(10, 10, 6))

  # Aggregated data has data for all years provided
  expect_equal(unique(art_agg1$calendar_quarter),
               unique(test_data1$calendar_quarter))

  # Data has been aggregated correctly
  df1 <- dplyr::filter(art_agg1, year %in% c("2014","2015"))
  expect_equal(unique(df1$area_level), c(0, 1))

  df2 <- dplyr::filter(art_agg1, year %in% c("2016", "2017", "2018"))
  expect_equal(unique(df2$area_level), c(0, 1, 2))

  ## Check that aggregated values are equal
  data_long <- data  %>%
    tidyr::pivot_longer(c(art_current, art_new, vl_tested_12mos, vl_suppressed_12mos)) %>%
    dplyr::select(area_id, sex, age_group, calendar_quarter, name, value_raw = value)

  art_agg_long <- art_agg1  %>%
    tidyr::pivot_longer(c(art_current, art_new, vl_tested_12mos, vl_suppressed_12mos)) %>%
    dplyr::select(area_id, sex, age_group, calendar_quarter, name, value_check = value)

  data_check <- art_agg_long %>%
    dplyr::inner_join(data_long, by = c("area_id", "sex", "age_group", "calendar_quarter", "name"))

  expect_equal(nrow(data_check), nrow(art_agg_long))
  expect_equal(data_check$value_check, data_check$value_raw)

  # Check for edge cases

  # (2) Data provided at multiple levels for the same years
  # Expected behavior - aggregate up from lowest  level available at each year
  # discard additional aggregated data
  # TO DO: improve this to retain all data present and add in missing aggregates
  test_data2 <- aggregate_art(a_hintr_data$art_number, a_hintr_data$shape) %>%
    dplyr::filter(area_level %in% c(0,2))

  test_data2 <- test_data2[names(data)]
  art_agg2 <- aggregate_art(test_data2, a_hintr_data$shape)

  # Check for same number of records at each area aggregation
  check2 <- art_agg2 %>%
           dplyr::group_by(area_level_label, year, age_group, sex) %>%
           dplyr::summarise(.groups = "drop") %>%
           dplyr::group_by(area_level_label) %>%
           dplyr::summarise(n = dplyr::n(), .groups = "drop")


  expect_equal(unique(check2$n), 16)

  # (3) Data provided at more than one level for different years
  # Expected behavior - aggregate up from lowestnlevel available at each year
  # discard additional aggregated data
  # TO DO: improve this to retain all data present and add in missing aggregates

  admin01_data <- dplyr::filter(data, area_level %in% c(0,1),
                               calendar_quarter %in% c("CY2014Q4","CY2015Q4"))

  test_data3 <- rbind(admin01_data, admin2_data)
  test_data3 <- test_data3[names(data)]

  art_agg3 <- aggregate_art(test_data3, a_hintr_data$shape)

  # Check for different of records at each area aggregation
  check3 <- art_agg3 %>%
    dplyr::group_by(area_level_label, year, age_group, sex) %>%
    dplyr::summarise(.groups = "drop") %>%
    dplyr::group_by(area_level_label) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  expect_equal(check3$n, c(10, 10, 6))


  # (4) Test that ART data can be aggregated with missing records
  # Expected behavior - retain NAs at lowest admin levels, aggregate with
  # missing data at higher admin levels
  art <- system.file("extdata/demo_art_number.csv", package = "naomi")
  shape <- system.file("extdata/demo_areas.geojson", package = "naomi")

  art_agg4 <- aggregate_art(art, shape)
  missing <- dplyr::filter(art_agg4, is.na(art_current))

  # Likoma + parent areas ART data missing for 2012 in aggregated data
  #  Check for missing data at Likoma
  expect_equal(unique(missing$area_name), c("Likoma"))
  expect_equal(unique(missing$year), 2012)

  # Missing records filled in for correct age/sex stratifications
  expect_equal(nrow(missing), 2)
  expect_equal(unique(missing$sex), "both")
  expect_equal(unique(missing$age_group), c("Y000_014","Y015_999"))

  # Aggregated data present at higher admin levels
  dat <- art_agg4 %>% dplyr::filter(calendar_quarter == "CY2012Q4",
                                    area_id %in% c("MWI","MWI_2_1_demo"))

  expect_equal(dat$art_current, c(24045, 376706, 2476, 38788))


  # Test that ART data is aggregated correctly when provided with different age/sex
  #  stratification

  # (4) Test that ART data can be aggregated with missing records
  # Expected behavior - create NAs when missing data is summed up area hierarchy
  art <- readr::read_csv(a_hintr_data$art_number, show_col_types = FALSE)
  art_over_15 <- art %>% dplyr::filter(age_group == "Y015_999")
  art_under_15 <- art %>% dplyr::filter(age_group == "Y000_014")

  males <- art_over_15 %>% dplyr::mutate(sex = "male", art_current = art_current * 0.33)
  females <- art_over_15 %>% dplyr::mutate(sex = "female", art_current = art_current * 0.67)

  test_data5 <- dplyr::bind_rows(males, females, art_under_15)

  art_agg5 <- aggregate_art(test_data5, a_hintr_data$shape)

  ## Check that aggregated values are equal
  data_long <- test_data5  %>%
    tidyr::pivot_longer(c(art_current, art_new, vl_tested_12mos, vl_suppressed_12mos)) %>%
    dplyr::select(area_id, sex, age_group, calendar_quarter, name, value_raw = value)

  art_agg_long <- art_agg5  %>%
    tidyr::pivot_longer(c(art_current, art_new, vl_tested_12mos, vl_suppressed_12mos)) %>%
    dplyr::select(area_id, sex, age_group, calendar_quarter, name, value_check = value)

  data_check <- art_agg_long %>%
    dplyr::inner_join(data_long, by = c("area_id", "sex", "age_group", "calendar_quarter", "name"))

  expect_equal(data_check$value_check, data_check$value_raw)

  # Check that correct age/sex combinations have been aggregated
  expect_true(identical(art_agg5 %>%
                          dplyr::group_by(age_group, sex) %>%
                          dplyr::summarise(.groups = "drop"),
                        tibble::tribble(~age_group, ~sex,
                                        "Y000_014", "both",
                                        "Y015_999", "female",
                                        "Y015_999",  "male")))

})


test_that("data can be formatted for ART input time series", {

  data <- prepare_input_time_series_art(a_hintr_data$art_number,
                                        a_hintr_data$shape)

  expect_true(nrow(data) > 50) ## Check that we have read out some data
  expect_setequal(colnames(data),
                  c("area_id", "area_name",  "area_level", "area_level_label",
                    "parent_area_id", "area_sort_order",  "time_period", "year",
                    "quarter","calendar_quarter", "area_hierarchy", "plot",
                    "value",  "missing_ids"))

  # Time period has correct format
  expect_match(as.character(data$time_period), "\\d{4}")

  ## Check that there is only a single age_group, time_period + quarter value for
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

test_that("ANC data can be aggregated when avalible at different admin levels", {

  # (1) Data provided at different levels for different years
  # Create dummy data similar to MOZ edge case:
  # ART data at admin1 2014-2016, admin2 2015-2016
  data <- aggregate_anc(a_hintr_data$anc_testing,
                        a_hintr_data$shape)

  admin1_data <- dplyr::filter(data, area_level == 1,
                               calendar_quarter %in% c("CY2014Q4","CY2015Q4"))

  admin2_data <- dplyr::filter(data, area_level == 2,
                               calendar_quarter %in% c("CY2016Q4","CY2017Q4","CY2018Q4"))

  test_data1 <- rbind(admin1_data, admin2_data)
  test_data1 <- test_data1[names(data)]

  # Aggregate data
  anc_agg1 <- aggregate_anc(test_data1, a_hintr_data$shape)

  # Check for different of records at each area aggregation
  check1 <- anc_agg1 %>%
    dplyr::group_by(area_level_label, year, age_group, sex) %>%
    dplyr::summarise(.groups = "drop") %>%
    dplyr::group_by(area_level_label) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  expect_equal(check1$n, c(5, 5, 3))

  # Aggregated data has data for all years provided
  expect_equal(unique(anc_agg1$calendar_quarter),
               unique(test_data1$calendar_quarter))

  # Data has been aggregated correctly
  df1 <- dplyr::filter(anc_agg1, year %in% c("2014","2015"))
  expect_equal(unique(df1$area_level), c(0, 1))

  df2 <- dplyr::filter(anc_agg1, year %in% c("2016", "2017", "2018"))
  expect_equal(unique(df2$area_level), c(0, 1, 2))

  ## Check that aggregated values are equal
  data_long <- data  %>%
    tidyr::pivot_longer(c(anc_clients, anc_known_pos, anc_already_art, anc_tested,
                          anc_tested_pos, anc_known_neg, births_facility)) %>%
    dplyr::select(area_id, sex, age_group, calendar_quarter, name, value_raw = value)

  anc_agg_long <- anc_agg1  %>%
    tidyr::pivot_longer(c(anc_clients, anc_known_pos, anc_already_art, anc_tested,
                          anc_tested_pos, anc_known_neg, births_facility)) %>%
    dplyr::select(area_id, sex, age_group, calendar_quarter, name, value_check = value)

  data_check <- anc_agg_long %>%
    dplyr::inner_join(data_long, by = c("area_id", "sex", "age_group", "calendar_quarter", "name"))

  expect_equal(nrow(data_check), nrow(anc_agg_long))
  expect_equal(data_check$value_check, data_check$value_raw)


  # (2) Data provided at multiple levels for the same years
  # Expected behavior - aggregate up from lowest  level available at each year
  # discard additional aggregated data
  # TO DO: improve this to retain all data present and add in missing aggregates
  test_data2 <- aggregate_anc(a_hintr_data$anc_testing, a_hintr_data$shape) %>%
    dplyr::filter(area_level %in% c(0, 1))

  test_data2 <- test_data2[names(data)]
  anc_agg2 <- aggregate_anc(test_data2, a_hintr_data$shape)

  # Check for same number of records at each area aggregation
  check2 <- anc_agg2 %>%
    dplyr::group_by(area_level_label, year, age_group, sex) %>%
    dplyr::summarise(.groups = "drop") %>%
    dplyr::group_by(area_level_label) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")


  expect_equal(unique(check2$n), 8)

  # (3) Data provided at more than one level for different years
  # Expected behavior - aggregate up from lowest  level available at each year
  # discard additional aggregated data
  # TO DO: improve this to retain all data present and add in missing aggregates

  admin01_data <- dplyr::filter(data, area_level %in% c(0,1),
                                calendar_quarter %in% c("CY2014Q4","CY2015Q4"))

  test_data3 <- rbind(admin01_data, admin2_data)
  test_data3 <- test_data3[names(data)]

  anc_agg3 <- aggregate_anc(test_data3, a_hintr_data$shape)

  # Check for different of records at each area aggregation
  check3 <- anc_agg3 %>%
    dplyr::group_by(area_level_label, year, age_group, sex) %>%
    dplyr::summarise(.groups = "drop") %>%
    dplyr::group_by(area_level_label) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  expect_equal(check1$n, c(5, 5, 3))

  # (4) Test that ANC data can be aggregated with missing records
  # Expected behavior - retain NAs at lowest admin levels, aggregate with
  # missing data at higher admin levels
  anc <- system.file("extdata/demo_anc_testing.csv", package = "naomi")
  shape <- system.file("extdata/demo_areas.geojson", package = "naomi")

  test_data4 <- read_anc_testing(anc) %>%
    dplyr::filter(!(area_id == "MWI_4_7_demo" & year == "2012"))

  anc_agg4 <- aggregate_anc(test_data4, a_hintr_data$shape)

  missing <- dplyr::filter(anc_agg4, is.na(anc_clients))

  # Likoma + parents areas ANC data missing for 2012 in aggregated data
  expect_equal(unique(missing$area_name), c("Likoma"))
  expect_equal(unique(missing$year), 2012)

  # Missing records filled in for correct age/sex stratifications
  expect_equal(nrow(missing), 1)
  expect_equal(unique(missing$age_group), c("Y015_049"))

  # Aggregated data present at higher admin levels
  dat <- anc_agg4 %>% dplyr::filter(calendar_quarter == "CY2012Q4",
                                    area_id %in% c("MWI","MWI_2_1_demo"))

  expect_equal(dat$anc_clients, c( 638233,74262))
})


test_that("data can be formatted for ANC input time series", {

  data <- prepare_input_time_series_anc(a_hintr_data$anc_testing,
                                        a_hintr_data$shape)
  expect_true(nrow(data) > 100) ## Check that we have read out some data
  expect_setequal(colnames(data),
                  c("area_id", "area_name", "area_level", "area_level_label",
                    "parent_area_id", "area_sort_order", "age_group",
                    "time_period", "year", "quarter", "calendar_quarter",
                    "plot", "value", "area_hierarchy", "missing_ids"))

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
                  c( "art_total","art_adult", "art_child",
                     "art_child_adult_ratio", "art_new_total",
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
                  c("art_adult" , "art_child_adult_ratio", "art_child" ,
                    "art_total", "art_adult_f","art_adult_m",
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
  expect_equal(ret, list(
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
  ), ignore_attr = TRUE)
})

test_that("data can be aggregated without all indicators", {

  art <- readr::read_csv(a_hintr_data$art_number, show_col_types = FALSE)

  # data with no art_new
  no_art_new <- art
  no_art_new$art_new <- NULL

  data <- prepare_input_time_series_art(no_art_new,
                                        a_hintr_data$shape)
  expect_setequal(unique(data$plot),
                  c( "art_total" ,"art_adult","art_child",
                     "art_child_adult_ratio", "vl_tested_12mos_total",
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
                  c("art_total", "art_adult","art_child","art_child_adult_ratio",
                    "art_new_total","art_new_adult","art_new_child"))

  # data with no art_new or vls indicators
  no_vls_art_new <- no_vls
  no_vls_art_new$art_new <- NULL

  data <- prepare_input_time_series_art(no_vls_art_new,
                                        a_hintr_data$shape)
  expect_setequal(
    unique(data$plot),
    c("art_total", "art_adult", "art_child", "art_child_adult_ratio"))

})


test_that("anc input time series can handle data with NA rows", {
  ## This is a regression test for issue #41 Mozambique
  data <- utils::read.csv(a_hintr_data$anc_testing)
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

  admin01 <- dplyr::filter(data, area_level < 2)
  admin2 <- dplyr::filter(data, area_level == 2)

  expect_equal(admin01$births_facility, rep(0, nrow(admin01)))
  expect_equal(admin2$births_facility, rep(NA_integer_, nrow(admin2)))
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

  art <- readr::read_csv(a_hintr_data$art_number, show_col_types = FALSE)
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

test_that("missing data is tagged correctly in aggregated plot data", {

  # Missing ART data
  art <- system.file("extdata/demo_art_number.csv", package = "naomi")
  shape <- system.file("extdata/demo_areas.geojson", package = "naomi")

  art_plot <- prepare_input_time_series_art(art, shape)

  # Likoma ART data missing for 2012 in test data
  # Check that Likoma + parent areas have missing data labels corresponding to
  # Likoma
  missing <- dplyr::filter(art_plot, missing_ids != "NULL", grepl("art", plot),
                           year == 2012)

  expect_equal(unique(missing$area_id), c("MWI","MWI_1_1_demo","MWI_2_1_demo",
                                          "MWI_3_6_demo","MWI_4_7_demo"))
  expect_equal(unique(missing$missing_ids), list("MWI_4_7_demo"))


  # Viral load data missing for all districts in test data
  # Check higher admin levels missing data labels contain all districts and
  # district level missing data labels contain individual districts
  missing_vl <- dplyr::filter(art_plot, missing_ids != "NULL", grepl("vl", plot))

  summary <- missing_vl %>%
    dplyr::group_by(area_id, missing_ids) %>%
    dplyr::summarise()

  expect_equal(lengths(summary[summary$area_id == "MWI", ]$missing_ids), 32)
  expect_equal(lengths(summary[summary$area_id == "MWI_1_1_demo", ]$missing_ids), 7)
  expect_equal(lengths(summary[summary$area_id == "MWI_2_1_demo", ]$missing_ids), 7)
  expect_equal(lengths(summary[summary$area_id == "MWI_3_1_demo", ]$missing_ids), 1)

  expect_equal(summary[summary$area_id == "MWI_3_1_demo", ]$missing_ids, list("MWI_4_1_demo"))
  expect_equal(summary[summary$area_id == "MWI_4_1_demo", ]$missing_ids, list("MWI_4_1_demo"))


  # Create ANC test data with missing values
  # * All data missing for  Likoma for 2012 in test data
  # * Facility births missing for all districts

  anc <- system.file("extdata/demo_anc_testing.csv", package = "naomi")
  shape <- system.file("extdata/demo_areas.geojson", package = "naomi")

  test_data <- read_anc_testing(anc) %>%
    dplyr::filter(!(area_id == "MWI_4_7_demo" & year == "2012")) %>%
    dplyr::mutate(births_facility = NA_real_)

  anc_plot <- prepare_input_time_series_anc(test_data, a_hintr_data$shape)

  # Likoma ANC data missing for 2012 in test data
  # Check that Likoma + parent areas have missing data labels corresponding to
  # Likoma
  missing <- dplyr::filter(anc_plot, missing_ids != "NULL", year == 2012,
                           grepl("anc", plot))

  expect_equal(unique(missing$area_id), c("MWI","MWI_1_1_demo","MWI_2_1_demo",
                                          "MWI_3_6_demo","MWI_4_7_demo"))

  expect_equal(unique(missing$missing_ids), list("MWI_4_7_demo"))


  # Facility births missing for all districts
  # Check higher admin levels missing data labels contain all districts and
  # district level missing data labels contain individual districts

  missing_births <- dplyr::filter(anc_plot, missing_ids != "NULL", grepl("births", plot))

  summary <- missing_births %>%
    dplyr::group_by(area_id, missing_ids) %>%
    dplyr::summarise()

  expect_equal(lengths(summary[summary$area_id == "MWI", ]$missing_ids), 32)
  expect_equal(lengths(summary[summary$area_id == "MWI_1_1_demo", ]$missing_ids), 7)
  expect_equal(lengths(summary[summary$area_id == "MWI_2_1_demo", ]$missing_ids), 7)
  expect_equal(lengths(summary[summary$area_id == "MWI_3_1_demo", ]$missing_ids), 1)

  expect_equal(summary[summary$area_id == "MWI_3_1_demo", ]$missing_ids, list("MWI_4_1_demo"))
  expect_equal(summary[summary$area_id == "MWI_4_1_demo", ]$missing_ids, list("MWI_4_1_demo"))


})


test_that("ART data is properly aggreagted for Spectrum comparison table", {


  # Create test data with sex disaggreagted adults on ART
  art <- a_hintr_data$art_number
  art_dat <- naomi::read_art_number(art)
  art_adult_female <- art_dat |> dplyr::filter(age_group == "Y015_999") |>
    dplyr::mutate(sex = "female", art_current = 0.60 * art_current)
  art_adult_male <- art_dat |> dplyr::filter(age_group == "Y015_999") |>
    dplyr::mutate(sex = "male", art_current = 0.40* art_current)
  art_pead <- art_dat |> dplyr::filter(age_group == "Y000_014")
  art_sexdiff <- dplyr::bind_rows(art_adult_female, art_adult_male, art_pead)

  # Test that aggregation works with subnational pjnz and sex disaggreagted adults on ART
  shape <- a_hintr_data$shape
  pjnz <- a_hintr_data$pjnz

  x <- prepare_art_spectrum_comparison(art_sexdiff, shape, pjnz)

  expect_equal(unique(x$group), c("art_children", "art_adult_female",
                                  "art_adult_male"))
  expect_equal(unique(x$area_name), c("Northern", "Central", "Southern"))

  # Test that aggregation works with national pjnz and sex aggregated adults on ART
  shape <- system.file("extdata/demo_areas.geojson", package = "naomi")
  pjnz <- system.file("extdata/demo_mwi2019.pjnz", package = "naomi")

  x <- prepare_art_spectrum_comparison(art, shape, pjnz)

  expect_equal(unique(x$group), c("art_children", "art_adult_both"))
  expect_equal(unique(x$area_name), c("Malawi - Demo"))

})

test_that("ANC data is properly aggreagted for Spectrum comparison table", {

  # Test that aggregation works with subnational pjnz
  shape <- a_hintr_data$shape
  pjnz <- a_hintr_data$pjnz
  anc <- a_hintr_data$anc_testing

  x <- prepare_anc_spectrum_comparison(anc, shape, pjnz)

  expect_equal(unique(x$indicator), c("anc_already_art", "anc_clients",
                                      "anc_known_pos", "anc_tested" , "anc_tested_pos"))
  expect_equal(unique(x$group), c("anc_adult_female"))
  expect_equal(unique(x$area_name), c("Northern", "Central", "Southern"))

  # Test that aggregation works with national pjnz
  shape <- system.file("extdata/demo_areas.geojson", package = "naomi")
  pjnz <- system.file("extdata/demo_mwi2019.pjnz", package = "naomi")

  x <- prepare_anc_spectrum_comparison(anc, shape, pjnz)


  expect_equal(unique(x$indicator), c("anc_already_art", "anc_clients",
                                  "anc_known_pos", "anc_tested" , "anc_tested_pos"))
  expect_equal(unique(x$group), c("anc_adult_female"))
  expect_equal(unique(x$area_name), c("Malawi - Demo"))

})
