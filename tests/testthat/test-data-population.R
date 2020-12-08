context("data-population")

test_that("test can convert between time ids", {
  expect_equal(convert_quarter_id(2012, 1), 449)
  expect_equal(convert_quarter_id(2012, 4), 452)
  expect_equal(convert_calendar_quarter(2012, 1), "CY2012Q1")
  expect_equal(convert_calendar_quarter(2012, 4), "CY2012Q4")
  expect_equal(calendar_quarter_to_quarter_id("CY2012Q1"), 449)
  expect_equal(calendar_quarter_to_quarter_id("CY2012Q4"), 452)
  expect_equal(quarter_id_to_calendar_quarter(449), "CY2012Q1")
  expect_equal(quarter_id_to_calendar_quarter(452), "CY2012Q4")
  expect_equal(calendar_quarter_to_year("CY2012Q4"), 2012)
  expect_equal(calendar_quarter_to_year("CY2012Q1"), 2012)
})
