context("test-data-survey")

test_that("CMC from date conversion", {
  expect_equal(cmc_date(as.Date("1987-02-11", format = "%Y-%m-%d")), 1046)
  expect_error(cmc_date("foo"))
  expect_error(cmc_date(1046))
  expect_error(cmc_date("1987-02-11"))
})
