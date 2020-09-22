#' Run the model and save output to file
#'
#' This prepares the model inputs from data and options and saves output as
#' an rds, the spectrum digst and indicators at specified paths.
#'
#' @param data List of paths to input data files.
#' @param options List of model run options (see details).
#' @param output_path Path to store output indicators as an RDS at.
#' @param spectrum_path Path to store spectrum digest file at.
#' @param summary_path Path to store summary download zip file at.
#'
#' @details
#'
#' The `data` argument must be a list specifying paths to the following:
#'
#' * `pjnz`
#' * `shape`
#' * `population`
#' * `survey data`
#' * `anc_testing` (optional)
#' * `art_number` (optional)
#'
#' Each item in list can either be a character containing the path to the file
#' or another list of the format:
#'
#' list(
#'   path = "path/to/file",
#'   hash = "file_hash",
#'   filename = "file"
#' )
#'
#' The `options` argument must be a list specifying minimally:
#'
#' * area_scope
#' * area_level
#' * calendar_quarter_t1
#' * calendar_quarter_t2
#' * calendar_quarter_t3
#' * survey_prevalence
#' * survey_art_coverage
#' * survey_recently_infected
#' * include_art_t1
#' * include_art_t2
#' * anc_prevalence_year1
#' * anc_prevalence_year2
#' * anc_art_coverage_year1
#' * anc_art_coverage_year2
#'
#' @return Paths to 3 output files.
#' @export
#'
hintr_run_model <- function(data, options, output_path = tempfile(),
                            spectrum_path = tempfile(fileext = ".zip"),
                            summary_path = tempfile(fileext = ".zip")) {

  progress <- new_progress()

  progress$start("validate_options")
  progress$print()

  data <- format_data_input(data)

  if(is.null(options$permissive))
    permissive <- FALSE
  else
    permissive <- as.logical(options$permissive)

  validate_model_options(data, options)
  progress$complete("validate_options")

  progress$start("prepare_inputs")
  progress$print()


  naomi_data <- naomi_prepare_data(data, options)

  tmb_inputs <- prepare_tmb_inputs(naomi_data)

  progress$complete("prepare_inputs")
  progress$start("fit_model")
  progress$print()

  fit <- fit_tmb(tmb_inputs,
                 outer_verbose = ifelse(is.null(options$outer_verbose), FALSE, options$outer_verbose),
                 inner_verbose = ifelse(is.null(options$inner_verbose), FALSE, options$inner_verbose),
                 max_iter = ifelse(is.null(options$max_iterations), 250, options$max_iterations))

  if(fit$convergence != 0 && !permissive)
    stop(paste("convergence error:", fit$message))

  progress$complete("fit_model")
  progress$start("uncertainty")
  progress$print()
  fit <- sample_tmb(fit,
                    nsample = options$no_of_samples,
                    rng_seed = options$rng_seed)

  progress$complete("uncertainty")
  progress$start("prepare_outputs")
  progress$print()

  ## TODO: Include input data in output package based on model options
  ## input download_input
  outputs <- output_package(fit, naomi_data)

  outputs <- calibrate_outputs(outputs, naomi_data,
                               options$spectrum_plhiv_calibration_level,
                               options$spectrum_plhiv_calibration_strat,
                               options$spectrum_artnum_calibration_level,
                               options$spectrum_artnum_calibration_strat,
                               options$spectrum_infections_calibration_level,
                               options$spectrum_infections_calibration_strat)

  outputs <- disaggregate_0to4_outputs(outputs, naomi_data)

  attr(outputs, "info") <- naomi_info(data, options)


  indicators <- add_output_labels(outputs)
  saveRDS(indicators, file = output_path)
  save_result_summary(summary_path, outputs)
  save_output_spectrum(spectrum_path, outputs)

  progress$complete("prepare_outputs")
  progress$print()
  list(output_path = output_path,
       spectrum_path = spectrum_path,
       summary_path = summary_path,
       metadata = list(
         areas = options$area_scope
       ))
}

naomi_prepare_data <- function(data, options) {

  area_merged <- read_area_merged(data$shape$path)
  population <- read_population(data$population$path)
  survey <- read_survey_indicators(data$survey$path)

  spec <- extract_pjnz_naomi(data$pjnz$path)

  if (!is.null(data$art_number)) {
    art_number <- read_art_number(data$art_number$path)
  } else {
    art_number <- NULL
  }
  if (!is.null(data$anc_testing)) {
    anc_testing <- read_anc_testing(data$anc_testing$path)
  } else {
    anc_testing <- NULL
  }

  if (is.null(options$artattend)) {
    options$artattend <- FALSE
  }
  if (is.null(options$artattend_t2)) {
    options$artattend_t2 <- FALSE
  }
  if (is.null(options$artattend_log_gamma_offset)) {
    options$artattend_log_gamma_offset <- -4
  }

  if(is.null(options$deff_prev))
    options$deff_prev <- 1.0

  if(is.null(options$deff_artcov))
    options$deff_artcov <- 1.0

  if(is.null(options$deff_recent))
    options$deff_recent <- 1.0

  if(is.null(options$deff_vls))
    options$deff_vls <- 1.0


  ## Get from the options
  scope <- options$area_scope
  level <- as.integer(options$area_level)
  calendar_quarter_t1 <- options$calendar_quarter_t1
  calendar_quarter_t2 <- options$calendar_quarter_t2
  calendar_quarter_t3 <- options$calendar_quarter_t3
  prev_survey_ids  <- options$survey_prevalence
  recent_survey_ids <- options$survey_recently_infected
  artcov_survey_ids <- options$survey_art_coverage

  ## VLS survey data not supported by model options
  vls_survey_ids <- NULL

  if(!is.null(options$include_art_t1) &&
     as.logical(options$include_art_t1))
    artnum_calendar_quarter1 <- calendar_quarter_t1
  else
    artnum_calendar_quarter1 <- NULL

  if(!is.null(options$include_art_t2) &&
     as.logical(options$include_art_t2))
    artnum_calendar_quarter2 <- calendar_quarter_t2
  else
    artnum_calendar_quarter2 <- NULL

  ## Recode anc_*_year* from "" to NULL
  if(!is.null(options$anc_prevalence_year1) && options$anc_prevalence_year1 == "")
    options["anc_prevalence_year1"] <- list(NULL)
  if(!is.null(options$anc_prevalence_year2) && options$anc_prevalence_year2 == "")
    options["anc_prevalence_year2"] <- list(NULL)
  if(!is.null(options$anc_art_coverage_year1) && options$anc_art_coverage_year1 == "")
    options["anc_art_coverage_year1"] <- list(NULL)
  if(!is.null(options$anc_art_coverage_year2) && options$anc_art_coverage_year2 == "")
    options["anc_art_coverage_year2"] <- list(NULL)

  naomi_mf <- naomi_model_frame(
    area_merged,
    population,
    spec,
    scope = scope,
    level = level,
    calendar_quarter_t1,
    calendar_quarter_t2,
    calendar_quarter_t3,
    spectrum_population_calibration = options$spectrum_population_calibration,
    artattend = as.logical(options$artattend),
    artattend_t2 = as.logical(options$artattend_t2),
    artattend_log_gamma_offset = as.numeric(options$artattend_log_gamma_offset)
  )

  naomi_data <- select_naomi_data(naomi_mf,
                                  survey,
                                  anc_testing,
                                  art_number,
                                  prev_survey_ids,
                                  artcov_survey_ids,
                                  recent_survey_ids,
                                  vls_survey_ids,
                                  artnum_calendar_quarter1,
                                  artnum_calendar_quarter2,
                                  options$anc_prevalence_year1,
                                  options$anc_prevalence_year2,
                                  options$anc_art_coverage_year1,
                                  options$anc_art_coverage_year2,
                                  deff_prev = options$deff_prev,
                                  deff_artcov = options$deff_artcov,
                                  deff_recent = options$deff_recent,
                                  deff_vls = options$deff_vls)

  return(naomi_data)
}

new_progress <- function() {
  Progress$new()
}

Progress <- R6::R6Class("Progress", list(
  progress = NULL,
  cloneable = FALSE,
  initialize = function() {
    self$progress <-
      list(
        validate_options = list(
          started = FALSE,
          complete = FALSE,
          name = t_("PROGRESS_VALIDATE_OPTIONS")
        ),
        prepare_inputs = list(
          started = FALSE,
          complete = FALSE,
          name = t_("PROGRESS_PREPARE_INPUTS")
        ),
        fit_model = list(
          started = FALSE,
          complete = FALSE,
          name = t_("PROGRESS_FIT_MODEL")
        ),
        uncertainty = list(
          started = FALSE,
          complete = FALSE,
          name = t_("PROGRESS_UNCERTAINTY")
        ),
        prepare_outputs = list(
          started = FALSE,
          complete = FALSE,
          name = t_("PROGRESS_PREPARE_OUTPUTS")
        )
      )
  },
  start = function(step_name) {
    self$step_exists(step_name)
    self$progress[[step_name]]$started <- TRUE
  },
  complete = function(step_name) {
    self$step_exists(step_name)
    self$progress[[step_name]]$complete <- TRUE
  },
  step_exists = function(step_name) {
    self$progress[[step_name]] %||% stop(sprintf("Invalid step '%s'", step_name))
  },
  print = function() {
    signalCondition(structure(self$progress,
                              class = c("progress", "condition")))
  }
))

naomi_info_input <- function(data) {
  get_col_from_list <- function(data, what) {
    value <- data[[what]]
    if (is.null(value)) {
      value <- NA_character_
    }
    value
  }
  filenames <- vapply(data, get_col_from_list, character(1), "filename")
  hash <- vapply(data, get_col_from_list, character(1), "hash")
  data.frame(
    role = names(data),
    filename = filenames,
    md5sum = hash,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

naomi_info_packages <- function() {
  info <- sessionInfo()

  versions <- function(x, type) {
    ret <- t(vapply(x, function(x) c(x$Package, x$Version, type), character(3),
                    USE.NAMES = FALSE))
    ret[order(ret[, 1]), , drop = FALSE]
  }

  as.data.frame(rbind(
    cbind(name = "R", version = as.character(getRversion()), type = "base"),
    versions(info$otherPkgs, "other"),
    versions(info$loadedOnly, "loaded")),
    stringsAsFactors = FALSE)
}

naomi_info <- function(data, options) {
  list("inputs.csv" = write_csv_string(naomi_info_input(data)),
       "options.yml" = yaml::as.yaml(options),
       "packages.csv" = write_csv_string(naomi_info_packages()))
}

## Ensures data is of format
## list(
##   input = list(
##     path = "path/to/file",
##     hash = "file_hash",
##     filename = "filename"
##   ),
##   input2 = list(
##     ...
##   ),
##   ...
## )
## From either a list of file paths or returns passed in data unchanged
format_data_input <- function(data) {
  if (all(vapply(data, is.character, logical(1)))) {
    data <- convert_format(data)
  } else if (!all(vapply(data, is.list, logical(1)))) {
    stop("Unsupported input data type, must be a list of file paths or list of file metadata.")
  }
  data
}

convert_format <- function(data) {
  lapply(data, function(input) {
    list(
      path = input,
      hash = tools::md5sum(input),
      filename = basename(input)
    )
  })
}
