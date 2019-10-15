#' Get JSON template representing model run options
#'
#' This reads JSON file of model run options which describe how the Naomi
#' front end should display input options. Returns template for the API to
#' enrich with model options.
#'
#' @return Model run options template.
#' @export
#'
#' @examples
#' get_model_options_template()
get_model_options_template <- function() {
  readLines(
    system_file("extdata", "meta", "model_run_options.json"),
    encoding = "UTF-8")
}
