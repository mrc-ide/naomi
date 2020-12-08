context("test-areas")

## test_that("get_area_collection() defaults to all areas at lowest level", {
##   val <- get_area_collection(demo_areas)
##   expect_equal(nrow(val), 32)
##   expect_equal(val$area_level, rep(4, 32))
## })

## test_that("get_area_collection() returns correct collection at each level", {
##   val1 <- get_area_collection(demo_areas, level = 1)
##   val3 <- get_area_collection(demo_areas, level = 3)
##   expect_equal(nrow(val1), 3)
##   expect_equal(val1$area_level, rep(1, 3))
##   expect_equal(nrow(val3), 28)
##   expect_equal(val3$area_level, rep(3, 28))
## })

## test_that("get_area_collection() returns collection in scope", {
##   val <- get_area_collection(demo_areas,
##                              level = 3,
##                              area_scope = c("MWI.1", "MWI.3.5"))
##   expect_equal(nrow(val), 13)
## })

## test_that("get_area_collection() returns a single value if scope matches level", {
##   val <- get_area_collection(demo_areas,
##                              level = 2,
##                              area_scope = "MWI.3.5")
##   expect_equal(nrow(val), 1)
## })

## test_that("get_area_collection() throws error if area_level outside of hierarchy range", {
##   expect_error(get_area_collection(demo_areas, level = -1))
##   expect_error(get_area_collection(demo_areas, level = 6))
##   expect_error(get_area_collection(demo_areas, level = 3.3))
##   expect_error(get_area_collection(demo_areas, level = "foo"))
## })

## test_that("get_area_collection() throws error if area_scope is not in area_id", {
##   expect_error(get_area_collection(demo_areas, area_scope = "MWI.5.3"))
##   expect_error(get_area_collection(demo_areas, area_scope = "foo"))
##   expect_error(get_area_collection(demo_areas, area_scope = 3))
## })

## test_that("get_area_collection() returns nothing if area_scope is lower level than area_level", {
##   val <- get_area_collection(demo_areas, level = 2, area_scope = "MWI.3.5.24")
##   expect_equal(nrow(val), 0)
## })
