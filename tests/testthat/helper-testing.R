expect_no_error <- function(object) {
  expect_error(object, NA)
}

with_mock <- function(..., .parent = parent.frame()) {
  mockr::with_mock(..., .parent = .parent, .env = "naomi")
}

MockProgress <- R6::R6Class(
  "MockProgress",
  inherit = Progress,
  cloneable = FALSE,
  public = list(
    now = NULL,
    mock_index = 0,
    mock_times = list(),

    initialize = function() {
      self$now <- round.POSIXt(Sys.time(), units = "mins")
      self$mock_times = list(
        self$now, self$now + 30, self$now + (2 * 60), self$now + (62 * 60),
        self$now + (65 * 60) + 8)
      super$initialize()
    },
    ## Wrap print message in a with restarts so we can capture messages for
    ## testing
    print = function() {
      withRestarts({
        super$print()
      }, muffleProgress = function(...) NULL)
    },

    ## Mocking doesn't work in R6 classes so we have to do this gross hack
    ## Mock some times for consistent testing of elapsed time, return
    ## now, in 30s time, in 2 mins time, in 1h 2 mins time and in 1h 5m 8s time
    time_now = function() {
      self$mock_index <- self$mock_index + 1
      self$mock_times[[self$mock_index]]
    }
  )
)

MockSimpleProgress <- R6::R6Class(
  "MockSimpleProgress",
  inherit = SimpleProgress,
  cloneable = FALSE,
  public = list(
    now = NULL,
    mock_index = 0,
    mock_times = list(),

    initialize = function() {
      self$now <- round.POSIXt(Sys.time(), units = "mins")
      self$mock_times = list(
        self$now, self$now + 30, self$now + (2 * 60), self$now + (62 * 60))
      super$initialize()
    },

    ## Wrap print message in a with restarts so we can capture messages for
    ## testing
    print = function() {
      withRestarts({
        super$print()
      }, muffleProgress = function(...) NULL)
    },

    ## Mocking doesn't work in R6 classes so we have to do this gross hack
    ## Mock some times for consistent testing of elapsed time, return
    ## now, in 30s time, in 2 mins time, in 1h 2 mins time and in 1h 5m 8s time
    time_now = function() {
      self$mock_index <- self$mock_index + 1
      self$mock_times[[self$mock_index]]
    }
  )
)

naomi_evaluate_promise <- function (code, print = FALSE) {
  warnings <- testthat:::Stack$new()
  handle_warning <- function(condition) {
    warnings$push(condition)
    invokeRestart("muffleWarning")
    if (!is.null(findRestart("muffleWarning"))) {
      invokeRestart("muffleWarning")
    }
  }
  messages <- testthat:::Stack$new()
  handle_message <- function(condition) {
    messages$push(condition)
    if (!is.null(findRestart("muffleMessage"))) {
      invokeRestart("muffleMessage")
    }
  }
  progress <- testthat:::Stack$new()
  handle_progress <- function(condition) {
    progress$push(condition)
    if (!is.null(findRestart("muffleProgress"))) {
      invokeRestart("muffleProgress")
    }
  }
  temp <- tempfile()
  result <- withr::with_output_sink(
    temp,
    withCallingHandlers(withVisible(code),
                        warning = handle_warning,
                        message = handle_message,
                        progress = handle_progress))
  if (result$visible && print) {
    withr::with_output_sink(temp, print(result$value))
  }
  output <- brio::read_file(temp)
  list(result = result$value,
       output = output,
       warnings = testthat:::get_messages(warnings$as_list()),
       messages = testthat:::get_messages(messages$as_list()),
       progress = progress$as_list())
}

expect_file_equivalent <- function(path_object, path_expected) {
  object_md5 <- tools::md5sum(path_object)
  expected_md5 <- tools::md5sum(path_expected)
  expect_equal(object_md5, expected_md5, check.attributes = FALSE)
}

expect_file_different <- function(path_object, path_expected) {
  object_md5 <- tools::md5sum(path_object)
  expected_md5 <- tools::md5sum(path_expected)
  expect_false(isTRUE(all.equal(object_md5, expected_md5,
                                check.attributes = FALSE)))
}
