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
#' * `anc` (optional)
#' * `art` (optional)
#'
#' The `options` argument must be a list specifying minimally:
#'
#' * area_scope
#' * area_level
#' * calendar_quarter_t1
#' * calendar_quarter_t2
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

  INLA:::inla.dynload.workaround()
  progress <- new_progress()

  progress$start("Validating inputs and options")
  progress$print()
  validate_model_options(data, options)
  progress$complete("Validating inputs and options")

  progress$start("Preparing input data")
  progress$print()
  area_merged <- sf::read_sf(data$shape)
  areas <- create_areas(area_merged = area_merged)
  population <- readr::read_csv(data$population)
  survey <- readr::read_csv(data$survey)

  if(!is.null(data$art_number))
    art_number <- readr::read_csv(data$art_number)
  else
    art_number <- NULL

  if(!is.null(data$anc_testing)) 
    anc <- readr::read_csv(data$anc_testing)
  else
    anc <- NULL

  spec <- extract_pjnz_naomi(data$pjnz)

  ## Get from the options
  scope <- options$area_scope
  level <- options$area_level
  calendar_quarter_t1 <- options$calendar_quarter_t1
  calendar_quarter_t2 <- options$calendar_quarter_t2
  prev_survey_ids  <- options$survey_prevalence
  recent_survey_ids <- options$survey_recently_infected
  artcov_survey_ids <- options$survey_art_coverage

  ## VLS survey data not supported by model options
  vls_survey_ids <- NULL

  if(!is.null(options$include_art_t1) &&
     options$include_art_t1 == "true")
    artnum_calendar_quarter1 <- calendar_quarter_t1
  else
    artnum_calendar_quarter1 <- NULL

  if(!is.null(options$include_art_t2) &&
     options$include_art_t2 == "true")
    artnum_calendar_quarter2 <- calendar_quarter_t2
  else
    artnum_calendar_quarter2 <- NULL

  anc_prevalence_year1 <- options$anc_prevalence_year1
  anc_prevalence_year2 <- options$anc_prevalence_year2
  anc_art_coverage_year1 <- options$anc_art_coverage_year1
  anc_art_coverage_year2 <- options$anc_art_coverage_year2

  naomi_mf <- naomi_model_frame(areas,
                                population,
                                spec,
                                scope = scope,
                                level = level,
                                calendar_quarter_t1,
                                calendar_quarter_t2)

  naomi_data <- select_naomi_data(naomi_mf,
                                  survey,
                                  anc,
                                  art_number,
                                  prev_survey_ids,
                                  artcov_survey_ids,
                                  recent_survey_ids,
                                  vls_survey_ids,
                                  artnum_calendar_quarter1,
                                  artnum_calendar_quarter2,
                                  anc_prevalence_year1,
                                  anc_prevalence_year2,
                                  anc_art_coverage_year1,
                                  anc_art_coverage_year2)

  tmb_inputs <- prepare_tmb_inputs(naomi_data)

  progress$complete("Preparing input data")
  progress$start("Fitting the model")
  progress$print()
  fit <- fit_tmb(tmb_inputs)

  progress$complete("Fitting the model")
  progress$start("Generating uncertainty ranges")
  progress$print()
  fit <- sample_tmb(fit, nsample = options$no_of_samples)

  progress$complete("Generating uncertainty ranges")
  progress$start("Preparing outputs")
  progress$print()

  # TODO: it would be nicer if output_package could get this too, but
  # that requires that it can see the same inputs.
  ## TODO: Include input data in output package based on model options
  ## input download_input
  outputs <- output_package(fit, naomi_mf, areas)
  attr(outputs, "info") <- naomi_info(data, options)

  indicators <- add_output_labels(outputs)
  saveRDS(indicators, file = output_path)
  save_result_summary(summary_path, outputs)
  save_output_spectrum(spectrum_path, outputs)

  progress$complete("Preparing outputs")
  progress$print()
  list(output_path = output_path,
       spectrum_path = spectrum_path,
       summary_path = summary_path)
}

new_progress <- function() {
  Progress$new()
}

Progress <- R6::R6Class("Progress", list(
  progress = NULL,
  initialize = function() {
    self$progress <-
      list(
        list(
          started = FALSE,
          complete = FALSE,
          name = "Validating inputs and options"
        ),
        list(
          started = FALSE,
          complete = FALSE,
          name = "Preparing input data"
        ),
        list(
          started = FALSE,
          complete = FALSE,
          name = "Fitting the model"
        ),
        list(
          started = FALSE,
          complete = FALSE,
          name = "Generating uncertainty ranges"
        ),
        list(
          started = FALSE,
          complete = FALSE,
          name = "Preparing outputs"
        )
      )
  },
  start = function(message) {
    index <- self$find_step(message)
    self$progress[[index]]$started <- TRUE
  },
  complete = function(message) {
    index <- self$find_step(message)
    self$progress[[index]]$complete <- TRUE
  },
  find_step = function(message) {
    steps <- vapply(self$progress, function(step) {
      step$name == message
    }, logical(1))
    if (sum(steps) != 1) {
      stop(sprintf("Found %s steps matching message %s.", sum(steps), message))
    }
    step <- which(steps)
  },
  print = function() {
    signalCondition(structure(list(message = self$progress),
                              class = c("progress", "condition")))
  }
))

naomi_info_input <- function(data) {
  files <- vapply(data, identity, character(1))
  hash <- unname(tools::md5sum(vapply(data, identity, character(1))))
  data.frame(
    role = names(data),
    filename = basename(files),
    md5sum = unname(tools::md5sum(files)),
    stringsAsFactors = FALSE)
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
