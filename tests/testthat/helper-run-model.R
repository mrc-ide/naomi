## Use fit.RDS if it exists locally, otherwise just use the actual functions
## fit.RDS not on git because it is pretty massive ~ 220MB
if (file.exists("testdata/fit.RDS")) {
  model_output <- readRDS("testdata/fit.RDS")
  fit <- mockery::mock(model_output, cycle = TRUE)
  sample <- mockery::mock(model_output, cycle = TRUE)
} else {
  fit <- fit_tmb
  sample <- sample_tmb
}

naomi_evaluate_promise <- function (code, print = FALSE) {
  warnings <- testthat:::Stack$new()
  handle_warning <- function(condition) {
    warnings$push(condition)
    invokeRestart("muffleWarning")
  }
  messages <- testthat:::Stack$new()
  handle_message <- function(condition) {
    messages$push(condition)
    invokeRestart("muffleMessage")
  }
  progress <- testthat:::Stack$new()
  handle_progress <- function(condition) {
    progress$push(condition)
    invokeRestart("muffleProgress")
  }
  temp <- file()
  on.exit(close(temp))
  result <- withr::with_output_sink(
    temp,
    withCallingHandlers(withVisible(code),
                        warning = handle_warning,
                        message = handle_message,
                        progress = handle_progress))
  if (result$visible && print) {
    withr::with_output_sink(temp, print(result$value))
  }
  output <- paste0(readLines(temp, encoding = "UTF-8", warn = FALSE),
                   collapse = "\n")
  list(result = result$value,
       output = output,
       warnings = testthat:::get_messages(warnings$as_list()),
       messages = testthat:::get_messages(messages$as_list()),
       progress = testthat:::get_messages(progress$as_list()))
}
