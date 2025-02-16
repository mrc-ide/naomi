test_that("suppress_conditions works as expected", {
  expect_silent(suppress_conditions(log(-1), warning_regexp = "NaNs produced"))
  expect_warning(suppress_conditions(log(-1), warning_regexp = "unmatched"),
                 "^NaNs produced$")

  f_warn <- function(x) {
    warning("my first warning")
    2 + 2
    warning("my second warning")
  }
  expect_silent(suppress_conditions(
    f_warn(),
    warning_regexp = c("first warning", "second warning")))

  f_msg <- function(n) {
    for (i in seq_len(n)) {
      message(paste("msg", i))
    }
  }
  expect_silent(suppress_conditions(f_msg(1), message_regexp = "msg 1"))
  expect_message(suppress_conditions(f_msg(1), message_regexp = "unmatched"),
                 "^msg 1\n$")
  expect_silent(suppress_conditions(f_msg(2), message_regexp = c("1", "2")))

  f_both <- function(n) {
    warning(paste("Raising", n))
    for (i in seq_len(n)) {
      message(paste("msg", i))
    }
  }
  expect_silent(suppress_conditions(f_both(1),
                                    message_regexp = "msg 1",
                                    warning_regexp = "Raising 1"))
  expect_warning(suppress_conditions(f_both(1), message_regexp = "msg 1"),
                 "^Raising 1$")
  expect_message(suppress_conditions(f_both(1), warning_regexp = "Raising 1"),
                 "^msg 1\n$")
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

test_that("can assert optional package is installed", {
  expect_true(assert_package_installed("testthat"))

  expect_error(
    assert_package_installed("my.fake.pkg"),
    "Package 'my.fake.pkg' must be installed to use this function.",
    fixed = TRUE)
})
