context("utils")

test_that("suppress_one_warning behaves", {
  expect_equal(suppress_one_warning(log(-1), "NaNs produced"), NaN)
  expect_warning(suppress_one_warning(log(-1), "NaNs produced"), NA)
  expect_warning(suppress_one_warning(log(-1), "unmatched"), "^NaNs produced$")
})
