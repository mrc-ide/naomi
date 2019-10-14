#' Get JSON representing model run options
#'
#' This reads JSON file of model run options which describe how the Naomi
#' front end should display input options. It validates these against a schema
#' to ensure it can create a UI.
#'
#' @return Validated model run options JSON.
#' @export
#'
#' @examples
#' get_model_run_options()
get_model_run_options <- function() {
  options <- read_schema()
  schema <- system_file("extdata", "meta", "schema",
                        "model_run_options.schema.json")
  jsonvalidate::json_validate(options, schema, engine = "ajv", error = TRUE)
  options
}

## As a separate function so we can mock out for testing
read_schema <- function() {
  readLines(
    system_file("extdata", "meta", "model_run_options.json"),
    encoding = "UTF-8")
}
