expect_no_error <- function(object) {
  expect_error(object, NA)
}

with_mock <- function(..., .parent = parent.frame()) {
  ## Don't use this, this should be removed, now testthat have
  ## added with_mocked_bindings that should be preferred
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

create_mock_art_data <- function(){

  shape <- sf::read_sf(system_file("extdata/demo_areas.geojson"))
  shape_wide <- spread_areas(shape)

  art_mock <- data.frame(
    area_id = shape_wide$area_id,
    sex = rep(c("male", "female", "both"), each = 3, times = 32),
    age_group = rep(c("Y015_999","Y015_999", "Y000_014"), each = 3, times = 32),
    calendar_quarter = rep(c("CY2021Q4", "CY2022Q4", "CY2023Q4"), times = 96),
    art_current = rep(c(500, 750, 300), each = 3, times = 32))

  spec_comparison_mock <- data.frame(
    year = rep(c(2021, 2022, 2023), times = 3),
    group = rep(c("art_children", "art_adult_female","art_adult_male"),each = 3),
    spectrum_region_code = 0L,
    value_spectrum_reported = rep(c(9600, 24000, 16000), each = 3),
    spec_adjustment_ratio = c(1, 1, 1,
                              1, 0.88, 0.86,
                              1, 0.88, 0.86)) |>
    dplyr::mutate(value_spectrum_adjusted = value_spectrum_reported * spec_adjustment_ratio)

  list(art = art_mock,
       spec_comparison = spec_comparison_mock,
       shape = shape)

}

