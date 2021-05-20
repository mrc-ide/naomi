#' Prepare spectrum download
#'
#' @param hintr_output object
#' @param path Path to save output file
#'
#' @return Path to output file and metadata for file
#' @export
hintr_prepare_spectrum_download <- function(output,
                                            path = tempfile(fileext = ".zip")) {
  assert_model_output_version(output)
  model_output <- readRDS(output$model_output_path)
  options <- yaml::read_yaml(text = model_output$info$options.yml)
  list(
    path = save_output_spectrum(path, model_output$output_package),
    metadata = list(
      description = build_output_description(options),
      areas = options$area_scope,
      type = "spectrum"
    )
  )
}

#' Prepare coarse age group download
#'
#' @param hintr_output object
#' @param path Path to save output file
#'
#' @return Path to output file and metadata for file
#' @export
hintr_prepare_coarse_age_group_download <- function(
  output, path = tempfile(fileext = ".zip")) {
  assert_model_output_version(output)
  model_output <- readRDS(output$model_output_path)
  options <- yaml::read_yaml(text = model_output$info$options.yml)
  list(
    path = save_output_coarse_age_groups(path, model_output$output_package),
    metadata = list(
      areas = options$area_scope,
      type = "coarse_output"
    )
  )
}

#' Prepare summary report download
#'
#' @param hintr_output object
#' @param path Path to save output file
#'
#' @return Path to output file and metadata for file
#' @export
hintr_prepare_summary_report_download <- function(
  output, path = tempfile(fileext = ".html")) {
  assert_model_output_version(output)
  model_output <- readRDS(output$model_output_path)
  options <- yaml::read_yaml(text = model_output$info$options.yml)
  ## TODO: Update to not need to generate spectrum file first - all data
  ## for report already exists in RDS
  t <- tempfile()
  spectrum_path <- save_output_spectrum(t, model_output$output_package)
  list(
    path = generate_output_summary_report(path, spectrum_path, quiet = TRUE),
    metadata = list(
      description = build_summary_report_description(options),
      areas = options$area_scope,
      type = "summary"
    )
  )
}
