context("test-data-survey")

test_that("cmc_date() returns correct value", {
  expect_equal(cmc_date(as.Date("1987-02-11", format = "%Y-%m-%d")), 1046)
})

test_that("cmc_date() returns an error if provided non-Date argument", {
  expect_error(cmc_date("foo"))
  expect_error(cmc_date(1046))
  expect_error(cmc_date("1987-02-11"))
})

test_that("get_mid_calendar_quarter() returns correct value", {
  start <- c("2005-04-01", "2010-12-15", "2016-01-01")
  end <-c("2005-08-01", "2011-05-15", "2016-06-01")
  expect_equal(get_mid_calendar_quarter(start, end),
               c("CY2005Q2", "CY2011Q1", "CY2016Q1"))
})

test_that("get_mid_calendar_quarter() returns error if arguments not Date", {
  expect_error(get_mid_calendar_quarter("2016-01-01", NA),
               "!is.na\\(end_date\\) is not TRUE")
  expect_error(get_mid_calendar_quarter("2016-01-01", "jibberish"),
               "character string is not in a standard unambiguous format")
  expect_error(get_mid_calendar_quarter(NA, "2016-01-01"),
               "!is.na\\(start_date\\) is not TRUE")
  expect_error(get_mid_calendar_quarter("2016-01-01", "2015-12-01"),
               "start_date <= end_date is not TRUE")
})
