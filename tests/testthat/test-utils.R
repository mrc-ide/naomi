test_that("suppress_one_warning behaves", {
  expect_equal(suppress_one_warning(log(-1), "NaNs produced"), NaN)
  expect_warning(suppress_one_warning(log(-1), "NaNs produced"), NA)
  expect_warning(suppress_one_warning(log(-1), "unmatched"), "^NaNs produced$")
})

test_that("read csv can read semicolon delimited files", {
  path <- tempfile()
  data <- data.frame(a = 1:4, b = 1:4)
  dir.create(path)
  path1 <- file.path(path, "input1.csv")
  path2 <- file.path(path, "input2.csv")
  write.csv(data, path1)
  write.csv2(data, path2)
  expect_equal(naomi_read_csv(path1), naomi_read_csv(path2))
  expect_equal(readr_read_csv(path1), readr_read_csv(path2))
})

test_that("is_empty", {
  expect_true(is_empty(NA))
  expect_true(is_empty(NULL))
  expect_true(is_empty(""))
  expect_false(is_empty("text"))
  expect_false(is_empty(2))
  expect_true(is_empty(list()))
  expect_true(is_empty(c()))
  expect_false(is_empty(c("things")))
})

test_that("can get area level from area id", {
  expect_equal(area_level_from_id("MWI_2_3"), 2)
  expect_equal(area_level_from_id("MWI"), 0)
  expect_equal(area_level_from_id(c("MWI_4_3", "MWI_23_25")), c(4, 23))
})
