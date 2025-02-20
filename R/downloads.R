#' Prepare spectrum download
#'
#' @param output hintr output object
#' @param path Path to save output file
#' @param notes Optional, user added notes from front end of app as a string
#' @param vmmc_file Optional file object, with path, filename and hash for
#'   VMMC input
#'
#' @return Path to output file and metadata for file
#' @export
hintr_prepare_spectrum_download <- function(output,
                                            path = tempfile(fileext = ".zip"),
                                            notes = NULL,
                                            vmmc_file = NULL) {
  assert_model_output_version(output)
  progress <- new_simple_progress()
  progress$update_progress("PROGRESS_DOWNLOAD_SPECTRUM")
  model_output <- read_hintr_output(output$model_output_path)
  options <- yaml::read_yaml(text = model_output$info$options.yml)
  list(
    path = save_output_spectrum(path, model_output$output_package, notes,
                                vmmc_file$path),
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

#' Prepare SHIPP tool download
#'
#' @param hintr_output object
#' @param path Path to save output file
#' @param pjnz Path to input PJNZ file
#'
#' @return Path to output file and metadata for file
#' @export
hintr_prepare_shipp_download <- function(output, pjnz,
                                        path = tempfile(fileext = ".xlsx")) {
  ## TODO: Do we need a version restriction on this?
  assert_model_output_version(output, "2.7.16")
  progress <- new_simple_progress()
  progress$update_progress("PROGRESS_DOWNLOAD_SHIPP")

  risk_populations <- shipp_generate_risk_populations(output$model_output_path,
                                                     pjnz)

  sheets <- list(
    "All outputs - F" = risk_populations$female_incidence,
    "All outputs - M" = risk_populations$male_incidence,
    "NAOMI outputs" = risk_populations$naomi_output
  )
  write_shipp_workbook(sheets, dest = path)

  model_output <- read_hintr_output(output$model_output_path)
  options <- yaml::read_yaml(text = model_output$info$options.yml)
  list(
    path = path,
    metadata = list(
      description = build_shipp_tool_description(options),
      areas = options$area_scope,
      type = "shipp"
    )
  )
}

#' Prepare Datapack download
#'
#' @param output hintr output object
#' @param path Path to save output file
#' @param vmmc_file Optional file object, with path, filename and hash for
#'   VMMC input
#' @param ids List of naomi web app queue ids for putting into metadata
#'
#' @return Path to output file and metadata for file
#' @export
hintr_prepare_datapack_download <- function(output,
                                            path = tempfile(fileext = ".xlsx"),
                                            vmmc_file = NULL,
                                            ids = NULL) {
  assert_model_output_version(output)
  progress <- new_simple_progress()
  progress$update_progress("PROGRESS_DOWNLOAD_SPECTRUM")

  if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    path <- paste0(path, ".xlsx")
  }

  model_output <- read_hintr_output(output$model_output_path)
  options <- yaml::read_yaml(text = model_output$info$options.yml)
  vmmc_datapack <- datapack_read_vmmc(vmmc_file$path)
  datapack_output <- build_datapack_output(
    model_output$output_package,
    model_output$output_package$fit$model_options$psnu_level,
    vmmc_datapack)
  datapack_metadata <- build_datapack_metadata(model_output$output_package, ids)
  writexl::write_xlsx(list(data = datapack_output, metadata = datapack_metadata),
                      path = path)
  list(
    path = path,
    metadata = list(
      description = build_datapack_description(options),
      areas = options$area_scope,
      type = "datapack"
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

build_shipp_tool_description <- function(options) {
  build_description(t_("DOWNLOAD_SHIPP_DESCRIPTION"), options)
}

build_datapack_description <- function(options) {
  build_description(t_("DOWNLOAD_DATAPACK_DESCRIPTION"), options)
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
