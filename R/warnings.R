naomi_warning <- function(text, locations) {
  ## If you want to add a new location here then let one of the developers know
  ## we will need to make sure the front end knows where to display the
  ## new location
  match_values(locations, c("model_options", "model_fit", "model_calibrate",
                           "review_output", "download_results"))
  warn <- list(
    text = text,
    locations = locations
  )
  signalCondition(structure(
    warn,
    class = c("naomi_warning", "condition")))
}

handle_naomi_warnings <- function(expr) {
  naomi_warnings <- new.env(parent = emptyenv())
  naomi_warnings$warnings <- list()
  out <- withCallingHandlers(
    force(expr),
    naomi_warning = function(e) {
      naomi_warnings$warnings <- c(naomi_warnings$warnings, list(e))
    }
  )
  out$warnings <- naomi_warnings$warnings
  out
}
