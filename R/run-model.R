#' Run the model and save output to file
#'
#' This prepares the model inputs from data and options and saves output as
#' an rds, the spectrum digst and indicators at specified paths.
#'
#' @param data List of paths to input data files.
#' @param options List of model run options (see details).
#' @param output_path Path to store output indicators as an RDS at.
#' @param spectrum_path Path to store spectrum digest file at.
#' @param coarse_output_path Path to store coarse age group output zip file at.
#' @param summary_report_path Path to store summary report at.
#' @param calibration_path Path to store data required for calibrating model.
#' @param validate If FALSE validation of inputs & data will be skipped.
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
#' @return Paths to 4 output files.
#' @export
#'
hintr_run_model <- function(data, options, output_path = tempfile(),
                            spectrum_path = tempfile(fileext = ".zip"),
                            coarse_output_path = tempfile(fileext = ".zip"),
                            summary_report_path = tempfile(fileext = ".html"),
                            calibration_path = tempfile(fileext = ".rds"),
                            validate = TRUE) {
  progress <- new_progress()
  progress$start("prepare_inputs")
  progress$print()

  data <- format_data_input(data)
  options <- format_options(options)

  if (validate) {
    validate_model_options(data, options)
  }

  naomi_data <- naomi_prepare_data(data, options)
  tmb_inputs <- prepare_tmb_inputs(naomi_data)

  progress$complete("prepare_inputs")
  progress$start("fit_model")
  progress$print()

  fit <- fit_tmb(tmb_inputs,
                 outer_verbose = options$outer_verbose %||% FALSE,
                 inner_verbose = options$inner_verbose %||%  FALSE,
                 max_iter = options$max_iterations %||% 250,
                 progress = progress)

  if(fit$convergence != 0 && !options$permissive) {
    stop(paste("convergence error:", fit$message))
  }

  progress$finalise_fit()
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

  ## TODO: Remove calibration after separate options are integrated
  ## into app
  calibration_data <- list(
    output_package = outputs,
    naomi_data = naomi_data
  )
  ## TODO: Remove this call to calibrate_outputs once front end has been
  ## updated to support separate calibration
  ## Should hintr not return any data either?
  outputs <- calibrate_outputs(outputs, naomi_data,
                               options$spectrum_plhiv_calibration_level,
                               options$spectrum_plhiv_calibration_strat,
                               options$spectrum_artnum_calibration_level,
                               options$spectrum_artnum_calibration_strat,
                               options$spectrum_aware_calibration_level,
                               options$spectrum_aware_calibration_strat,
                               options$spectrum_infections_calibration_level,
                               options$spectrum_infections_calibration_strat,
                               options$calibrate_method)

  outputs <- disaggregate_0to4_outputs(outputs, naomi_data)

  info <- naomi_info(data, options)
  attr(outputs, "info") <- info
  calibration_data$info <- info
  saveRDS(calibration_data, calibration_path)

  indicators <- add_output_labels(outputs)
  saveRDS(indicators, file = output_path)
  save_output_coarse_age_groups(coarse_output_path, outputs)
  save_output_spectrum(spectrum_path, outputs)
  generate_output_summary_report(summary_report_path,
                                 spectrum_path,
                                 quiet = TRUE)

  progress$complete("prepare_outputs")
  progress$print()
  build_hintr_output(
    output_path,
    spectrum_path,
    coarse_output_path,
    summary_report_path,
    calibration_path,
    metadata = list(
      areas = options$area_scope,
      output_description = build_output_description(options),
      summary_report_description = build_summary_report_description(options)
  ))
}

build_hintr_output <- function(output_path, spectrum_path, coarse_output_path,
                               summary_report_path, calibration_path,
                               metadata) {
  out <- list(output_path = output_path,
              spectrum_path = spectrum_path,
              coarse_output_path = coarse_output_path,
              summary_report_path = summary_report_path,
              calibration_path = calibration_path,
              metadata = metadata)
  class(out) <- "hintr_output"
  out
}

is_hintr_output <- function(object) {
  inherits(object, "hintr_output")
}

#' Calibrate hintr_output
#'
#' Take a previously generated hintr_output object and calibrate. Format
#' response as another hintr_output object.
#'
#' @param output A hintr_output object
#' @param calibration_options A set of calibration options
#' @param output_path Path to store calibrated output indicators as an RDS at.
#' @param spectrum_path Path to store calibrated spectrum digest file at.
#' @param coarse_output_path Path to store calibrated coarse age group output
#'   zip file at.
#' @param summary_report_path Path to store calibrated summary report at.
#' @param calibration_path Path to store data required for calibrating model.
#'
#' @return Calibrated hintr_output object
#' @export
hintr_calibrate <- function(output, calibration_options,
                            output_path = tempfile(),
                            spectrum_path = tempfile(fileext = ".zip"),
                            coarse_output_path = tempfile(fileext = ".zip"),
                            summary_report_path = tempfile(fileext = ".html"),
                            calibration_path = tempfile(fileext = ".rds")) {
  prev_calibration_path <- output$calibration_path
  if (!is_hintr_output(output) || is.null(prev_calibration_path)) {
    stop(t_("INVALID_CALIBRATE_OBJECT"))
  }
  validate_calibrate_options(calibration_options)
  progress <- new_simple_progress()
  progress$update_progress("PROGRESS_CALIBRATE")
  calibration_data <- readRDS(prev_calibration_path)
  calibrated_output <- calibrate_outputs(
    output = calibration_data$output_package,
    naomi_mf = calibration_data$naomi_data,
    spectrum_plhiv_calibration_level = calibration_options$spectrum_plhiv_calibration_level,
    spectrum_plhiv_calibration_strat = calibration_options$spectrum_plhiv_calibration_strat,
    spectrum_artnum_calibration_level = calibration_options$spectrum_artnum_calibration_level,
    spectrum_artnum_calibration_strat = calibration_options$spectrum_artnum_calibration_strat,
    spectrum_aware_calibration_level = calibration_options$spectrum_aware_calibration_level,
    spectrum_aware_calibration_strat = calibration_options$spectrum_aware_calibration_strat,
    spectrum_infections_calibration_level = calibration_options$spectrum_infections_calibration_level,
    spectrum_infections_calibration_strat = calibration_options$spectrum_infections_calibration_strat,
    calibrate_method = calibration_options$calibrate_method
  )

  calibrated_output <- disaggregate_0to4_outputs(output = calibrated_output,
                                                 naomi_mf = calibration_data$naomi_data)

  calibration_data$info$calibration_options.yml <-
    yaml::as.yaml(calibration_options)
  progress$update_progress("PROGRESS_CALIBRATE_SAVE_OUTPUT")
  saveRDS(calibration_data, calibration_path)
  attr(calibrated_output, "info") <- calibration_data$info

  indicators <- add_output_labels(calibrated_output)
  saveRDS(indicators, file = output_path)
  save_output_coarse_age_groups(coarse_output_path, calibrated_output,
                                overwrite = TRUE)
  save_output_spectrum(spectrum_path, calibrated_output,
                       overwrite = TRUE)
  progress$update_progress("PROGRESS_CALIBRATE_GENERATE_REPORT")
  generate_output_summary_report(summary_report_path,
                                 spectrum_path,
                                 quiet = TRUE)

  metadata <- output$metadata
  if (is.null(metadata$output_description)) {
    metadata$output_description <- build_output_description(
      yaml::yaml.load(calibration_data$info$options.yml))
  }
  if (is.null(metadata$summary_report_description)) {
    metadata$summary_report_description <- build_summary_report_description(
      yaml::yaml.load(calibration_data$info$options.yml))
  }
  build_hintr_output(output_path, spectrum_path,
                     coarse_output_path, summary_report_path,
                     calibration_path, metadata)
}

validate_calibrate_options <- function(calibration_options) {

  expected_options <- c("spectrum_plhiv_calibration_level",
                        "spectrum_plhiv_calibration_strat",
                        "spectrum_artnum_calibration_level",
                        "spectrum_artnum_calibration_strat",
                        "spectrum_aware_calibration_level",
                        "spectrum_aware_calibration_strat",
                        "spectrum_infections_calibration_level",
                        "spectrum_infections_calibration_strat",
                        "calibrate_method")
  missing_options <- expected_options[
    !(expected_options %in% names(calibration_options))]
  if (length(missing_options) > 0) {
    stop(t_("Calibration cannot be run, missing options for {{missing}}.",
                 list(missing = paste(missing_options, collapse = ", "))))
  }

  if (!all(calibration_options[["calibrate_method"]] %in% c("logistic", "proportional"))) {
    stop(t_("calibrate_method must be either \"logistic\" or \"proportional\""))
  }

  invisible(TRUE)
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

  naomi_mf <- naomi_model_frame(
    area_merged = area_merged,
    population_agesex = population,
    spec = spec,
    scope = scope,
    level = level,
    calendar_quarter1 = calendar_quarter_t1,
    calendar_quarter2 = calendar_quarter_t2,
    calendar_quarter3 = calendar_quarter_t3,
    spectrum_population_calibration = options$spectrum_population_calibration,
    output_aware_plhiv = as.logical(options$output_aware_plhiv),
    artattend = as.logical(options$artattend),
    artattend_t2 = as.logical(options$artattend_t2),
    artattend_log_gamma_offset = as.numeric(options$artattend_log_gamma_offset)
  )

  naomi_data <- select_naomi_data(
    naomi_mf = naomi_mf,
    survey_hiv_indicators = survey,
    anc_testing = anc_testing,
    art_number = art_number,
    prev_survey_ids = prev_survey_ids,
    artcov_survey_ids = artcov_survey_ids,
    recent_survey_ids = recent_survey_ids,
    vls_survey_ids = vls_survey_ids,
    artnum_calendar_quarter_t1 = artnum_calendar_quarter1,
    artnum_calendar_quarter_t2 = artnum_calendar_quarter2,
    anc_clients_year_t2 = options[["anc_clients_year2"]],
    anc_clients_year_t2_num_months = as.numeric(options[["anc_clients_year2_num_months"]]),
    anc_prev_year_t1 = options[["anc_prevalence_year1"]],
    anc_prev_year_t2 = options[["anc_prevalence_year2"]],
    anc_artcov_year_t1 = options[["anc_art_coverage_year1"]],
    anc_artcov_year_t2 = options[["anc_art_coverage_year2"]],
    use_kish_prev = options[["use_kish_prev"]],
    deff_prev = options[["deff_prev"]],
    use_kish_artcov = options[["use_kish_artcov"]],
    deff_artcov = options[["deff_artcov"]],
    use_kish_recent = options[["use_kish_recent"]],
    deff_recent = options[["deff_recent"]],
    use_kish_vls = options[["use_kish_vls"]],
    deff_vls = options[["deff_vls"]],
    use_survey_aggregate = as.logical(options[["use_survey_aggregate"]])
  )

  return(naomi_data)
}

new_progress <- function() {
  Progress$new()
}

Progress <- R6::R6Class("Progress", list(
  cloneable = FALSE,
  progress = NULL,
  iteration = 0,
  start_time = NULL,
  elapsed = NULL,

  initialize = function() {
    self$progress <-
      list(
        prepare_inputs = list(
          started = FALSE,
          complete = FALSE,
          name = t_("PROGRESS_PREPARE_INPUTS")
        ),
        fit_model = list(
          started = FALSE,
          complete = FALSE,
          name = t_("PROGRESS_FIT_MODEL"),
          helpText = NULL
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
  },

  set_start_time = function() {
    self$start_time <- Sys.time()
  },

  iterate_fit = function() {
    if (is.null(self$start_time)) {
      self$set_start_time()
    }
    self$iteration <- self$iteration + 1
    self$elapsed <- Sys.time() - self$start_time
    self$progress$fit_model$helpText <- t_("PROGRESS_FIT_MODEL_HELP_TEXT",
      list(iteration = self$iteration,
           elapsed = prettyunits::pretty_dt(self$elapsed)))
    self$print()
  },

  finalise_fit = function() {
    self$progress$fit_model$helpText <- t_(
      "PROGRESS_FIT_MODEL_HELP_TEXT_COMPLETE",
      list(iteration = self$iteration,
           elapsed = prettyunits::pretty_dt(self$elapsed)))
  }
))

new_simple_progress <- function() {
  SimpleProgress$new()
}

SimpleProgress <- R6::R6Class("SimpleProgress", list(
  cloneable = FALSE,
  progress = "",
  start_time = NULL,
  elapsed = NULL,

  initialize = function() {
    self$set_start_time()
  },

  print = function() {
    signalCondition(structure(list(message = self$progress),
                              class = c("progress", "condition")))
  },

  set_start_time = function() {
    self$start_time <- Sys.time()
  },

  update_progress = function(message_key) {
    if (is.null(self$start_time)) {
      self$set_start_time()
    }
    self$elapsed <- Sys.time() - self$start_time
    self$progress <- t_(message_key,
                        list(elapsed = prettyunits::pretty_dt(self$elapsed)))
    self$print()
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


## Move several ad hoc options formatting from hintr_run_model() into a separate
## function.
##
## In future, refactor this to systmatically cast options based on type and set
## defaults if missing from metadata.
format_options <- function(options) {

  if (is.null(options$permissive)) {
    options$permissive <- FALSE
  } else {
    options$permissive <- as.logical(options$permissive)
  }

  ## Set default "none" calibration options if missing from options list

  if (is.null(options$spectrum_plhiv_calibration_level)) {
    options$spectrum_plhiv_calibration_level  <-  "none"
  }
  if (is.null(options$spectrum_plhiv_calibration_strat)) {
    options$spectrum_plhiv_calibration_strat <- "sex_age_group"
  }
  if (is.null(options$spectrum_artnum_calibration_level)) {
    options$spectrum_artnum_calibration_level <- "none"
  }
  if (is.null(options$spectrum_artnum_calibration_strat)) {
    options$spectrum_artnum_calibration_strat <- "sex_age_coarse"
  }

  if (is.null(options$spectrum_aware_calibration_level)) {
    options$spectrum_aware_calibration_level <- "none"
  }

  if (is.null(options$spectrum_aware_calibration_strat)) {
    options$spectrum_aware_calibration_strat <- "sex_age_coarse"
  }

  if (is.null(options$spectrum_infections_calibration_level)) {
    options$spectrum_infections_calibration_level <- "none"
  }
  if (is.null(options$spectrum_infections_calibration_strat)) {
    options$spectrum_infections_calibration_strat <- "sex_age_coarse"
  }
  if (is.null(options$calibrate_method)) {
    options$calibrate_method <- "logistic"
  }



  if (is.null(options[["artattend"]])) {
    options$artattend <- FALSE
  }
  if (is.null(options[["artattend_t2"]])) {
    options$artattend_t2 <- FALSE
  }
  if (is.null(options[["artattend_log_gamma_offset"]])) {
    options$artattend_log_gamma_offset <- -4
  }

  if(is.null(options[["deff_prev"]]))
    options$deff_prev <- 1.0

  if(is.null(options[["deff_artcov"]]))
    options$deff_artcov <- 1.0

  if(is.null(options[["deff_recent"]]))
    options$deff_recent <- 1.0

  if(is.null(options[["deff_vls"]]))
    options$deff_vls <- 1.0

  if(is.null(options[["use_kish_prev"]]))
    options$use_kish_prev <- "true"

  if(is.null(options[["use_kish_artcov"]]))
    options$use_kish_artcov <- "true"

  if(is.null(options[["use_kish_recent"]]))
    options$use_kish_recent <- "true"

  if(is.null(options[["use_kish_vls"]]))
    options$use_kish_vls <- "true"

  if(is.null(options[["use_survey_aggregate"]]))
    options$use_survey_aggregate <- "false"



  ## Recode anc_*_year* from "" to NULL
  if(!is.null(options[["anc_clients_year2"]]) && options$anc_clients_year2 == "")
    options["anc_clients_year2"] <- list(NULL)
  if(!is.null(options[["anc_prevalence_year1"]]) && options$anc_prevalence_year1 == "")
    options["anc_prevalence_year1"] <- list(NULL)
  if(!is.null(options[["anc_prevalence_year2"]]) && options$anc_prevalence_year2 == "")
    options["anc_prevalence_year2"] <- list(NULL)
  if(!is.null(options[["anc_art_coverage_year1"]]) && options$anc_art_coverage_year1 == "")
    options["anc_art_coverage_year1"] <- list(NULL)
  if(!is.null(options[["anc_art_coverage_year2"]]) && options$anc_art_coverage_year2 == "")
    options["anc_art_coverage_year2"] <- list(NULL)

  options
}

build_output_description <- function(options) {
  build_description("Naomi output uploaded from Naomi web app", options)
}

build_summary_report_description <- function(options) {
  build_description("Naomi summary report uploaded from Naomi web app",
                    options)
}

build_description <- function(type_text, options) {
  write_options <- function(name, value) {
    sprintf("%s - %s", name, value)
  }
  opt_text <- Map(write_options,
      c(t_("OPTIONS_GENERAL_AREA_SCOPE_LABEL"),
        t_("OPTIONS_GENERAL_AREA_LEVEL_LABEL"),
        t_("OPTIONS_GENERAL_CALENDAR_QUARTER_T2_LABEL"),
        t_("OPTIONS_OUTPUT_PROJECTION_QUARTER_LABEL")),
      c(options[["area_scope"]],
        options[["area_level"]],
        options[["calendar_quarter_t2"]],
        options[["calendar_quarter_t3"]]))
  paste0(c(type_text, "", opt_text), collapse = "\n")
}
