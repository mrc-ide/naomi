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
  model_output <- read_hintr_output(output$model_output_path)
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
  model_output <- read_hintr_output(output$model_output_path)
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
  model_output <- read_hintr_output(output$model_output_path)
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
  model_output <- read_hintr_output(output$model_output_path)
  options <- yaml::read_yaml(text = model_output$info$options.yml)
  list(
    path = generate_comparison_report(path, output$model_output_path,
      quiet = TRUE),
    metadata = list(
      description = build_comparison_report_description(options),
      areas = options$area_scope,
      type = "comparison"
    )
  )
}

#' Prepare AGYW tool download
#'
#' @param hintr_output object
#' @param path Path to save output file
#' @param pjnz Path to input PJNZ file
#'
#' @return Path to output file and metadata for file
#' @export
hintr_prepare_agyw_download <- function(output, pjnz,
                                        path = tempfile(fileext = ".xlsx")) {
  ## TODO: Do we need a version restriction on this?
  assert_model_output_version(output, "2.7.16")
  progress <- new_simple_progress()
  progress$update_progress("PROGRESS_DOWNLOAD_AGYW")
  dummy_data <- data.frame(x = c(1, 2, 3), y = c(3, 4, 5))
  writexl::write_xlsx(list(sheet = dummy_data), path = path)

  model_output <- read_hintr_output(output$model_output_path)
  options <- yaml::read_yaml(text = model_output$info$options.yml)
  list(
    path = path,
    metadata = list(
      description = build_agyw_tool_description(options),
      areas = options$area_scope,
      type = "agyw"
    )
  )
}

build_output_description <- function(options) {
  build_description(t_("DOWNLOAD_OUTPUT_DESCRIPTION"), options)
}

build_summary_report_description <- function(options) {
  build_description(t_("DOWNLOAD_SUMMARY_DESCRIPTION"), options)
}

build_comparison_report_description <- function(options) {
  build_description(t_("DOWNLOAD_COMPARISON_DESCRIPTION"), options)
}

build_agyw_tool_description <- function(options) {
  build_description(t_("DOWNLOAD_AGYW_DESCRIPTION"), options)
}

build_description <- function(type_text, options) {
  write_options <- function(name, value) {
    sprintf("%s - %s", name, value)
  }
  lang <- traduire::translator(package = "naomi")$language()
  labels <- c("OPTIONS_GENERAL_AREA_SCOPE_LABEL",
            "OPTIONS_GENERAL_AREA_LEVEL_LABEL",
            "OPTIONS_GENERAL_CALENDAR_QUARTER_T2_LABEL",
            "OPTIONS_OUTPUT_PROJECTION_QUARTER_LABEL")
  translated_labels <- naomi.options::translate_labels(labels, lang = lang)
  opt_text <- Map(write_options,
                  translated_labels,
                  c(options[["area_scope"]],
                    options[["area_level"]],
                    options[["calendar_quarter_t2"]],
                    options[["calendar_quarter_t3"]]))
  paste0(c(type_text, "", opt_text), collapse = "\n")
}
