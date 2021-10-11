naomi_warning <- function(text, locations) {
  ## If you want to add a new location here then let one of the developers know
  ## we will need to make sure the front end knows where to display the
  ## new location
  match_values(locations, c("model_options", "fit_model", "calibrate_model",
                           "review_output"))
  ## TODO: Should we add the call to this?
  warn <- list(
    type = "warning",
    message = text,
    location = locations
  )
  warning(structure(
    warn,
    class = c("naomi_warning", "warning", "condition")))
}
