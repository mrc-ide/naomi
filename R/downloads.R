#' Prepare spectrum download
#'
#' @param output hintr output object
#' @param path Path to save output file
#' @param notes User added notes from front end of app as a string
#'
#' @return Path to output file and metadata for file
#' @export
hintr_prepare_spectrum_download <- function(output,
                                            path = tempfile(fileext = ".zip"),
                                            notes = NULL) {
  assert_model_output_version(output)
  progress <- new_simple_progress()
  progress$update_progress("PROGRESS_DOWNLOAD_SPECTRUM")
  model_output <- readRDS(output$model_output_path)
  options <- yaml::read_yaml(text = model_output$info$options.yml)
  list(
    path = save_output_spectrum(path, model_output$output_package, notes),
    metadata = list(
      description = build_output_description(options),
      areas = options$area_scope,
      type = "spectrum"
    )
  )
}

#' Prepare coarse age group download
#'
#' @param output hintr output object
#' @param path Path to save output file
#'
#' @return Path to output file and metadata for file
#' @export
hintr_prepare_coarse_age_group_download <- function(output,
                                                    path = tempfile(fileext = ".zip")) {
  assert_model_output_version(output)
  progress <- new_simple_progress()
  progress$update_progress("PROGRESS_DOWNLOAD_COARSE")
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
#' @param output hintr output object
#' @param path Path to save output file
#'
#' @return Path to output file and metadata for file
#' @export
hintr_prepare_summary_report_download <- function(output,
                                                  path = tempfile(fileext = ".html")) {
  assert_model_output_version(output)
  progress <- new_simple_progress()
  progress$update_progress("PROGRESS_DOWNLOAD_SUMMARY")
  model_output <- readRDS(output$model_output_path)
  options <- yaml::read_yaml(text = model_output$info$options.yml)
  list(
    path = generate_output_summary_report(path, output$model_output_path,
      quiet = TRUE),
    metadata = list(
      description = build_summary_report_description(options),
      areas = options$area_scope,
      type = "summary"
    )
  )
}

#' Prepare comparison report download
#'
#' @param hintr_output object
#' @param path Path to save output file
#'
#' @return Path to output file and metadata for file
#' @export
hintr_prepare_comparison_report_download <- function(output,
                                                     path = tempfile(fileext = ".html")) {
  assert_model_output_version(output, "2.7.16")
  progress <- new_simple_progress()
  progress$update_progress("PROGRESS_DOWNLOAD_COMPARISON")
  model_output <- readRDS(output$model_output_path)
  options <- yaml::read_yaml(text = model_output$info$options.yml)
  list(
    ## Just using the summary report as a placeholder - eventually
    ## this will call a separate method to generate comparison report
    path = generate_comparison_report(path, output$model_output_path,
      quiet = TRUE),
    metadata = list(
      description = build_comparison_report_description(options),
      areas = options$area_scope,
      type = "comparison"
    )
  )
}
