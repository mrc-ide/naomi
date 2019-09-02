
test_that("spread_areas() works", {
  expect_error(spread_areas(mwi_areas, -4))
  expect_error(spread_areas(mwi_areas, 7))
  expect_equal(nrow(spread_areas(mwi_areas)), 32L)
  expect_equal(nrow(spread_areas(mwi_areas, min_level = 1, max_level = 3)), 28L)
})
