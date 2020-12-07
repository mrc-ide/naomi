context("test-example-data")

test_that("example areas datasets are valid", {
  expect_is(
    create_areas(demo_area_levels,
                 demo_area_hierarchy,
                 demo_area_boundaries),
    "naomi_areas")
})

test_that("example population dataset match area hierarchy", {
  expect_true(all(demo_population_agesex$area_id %in%
                  subset(demo_area_hierarchy, area_level == 4)$area_id))
})

test_that("example survey indicators dataset match area hierarchy", {
  expect_true(all(demo_survey_hiv_indicators$area_id %in% demo_area_hierarchy$area_id))
})

test_that("example programme datasets match area hierarchy", {
  expect_true(all(demo_anc_testing$area_id %in% 
                  subset(demo_area_hierarchy, area_level == 4)$area_id))
  expect_true(all(demo_art_number$area_id %in% 
                  subset(demo_area_hierarchy, area_level == 4)$area_id))
})
