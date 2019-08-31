
areas <- readRDS(system.file("extdata/areas/areas.rds", package = "naomi"))

test_that("spread_areas() works", {
  expect_error(spread_areas(areas, -4))
  expect_error(spread_areas(areas, 7))
  expect_equal(nrow(spread_areas(areas)), 32L)
  expect_equal(nrow(spread_areas(areas, min_level = 1, max_level = 3)), 28L)
})
