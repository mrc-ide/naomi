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
#' * t1
#' * t2
#' * survey_prevalence
#' * survey_art_coverage
#' * survey_vls
#' * survey_recently_infected
#' * survey_art_or_vls
#' * art_t1
#' * art_t2
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
  progress$start("Preparing input data")
  progress$print()
  area_merged <- sf::read_sf(data$shape)
  areas <- create_areas(area_merged = area_merged)
  population <- readr::read_csv(data$population)
  survey <- readr::read_csv(data$survey)
  art_number <- readr::read_csv(data$programme)
  anc <- readr::read_csv(data$anc)

  spec <- extract_pjnz_naomi(data$pjnz)

  ## TODO: Remove this filter - it is in temporarily as model does not run
  ## without it mrc-640
  art_number <- art_number %>%
    dplyr::filter(age_group == "15+")

  ## Get from the options
  scope <- options$area_scope
  level <- options$area_level
  quarter_id_t1 <- options$t1
  quarter_id_t2 <- options$t2
  prev_survey_ids  <- options$survey_prevalence
  artcov_survey_ids  <- options$survey_art_coverage
  vls_survey_ids <- NULL
  recent_survey_ids <- options$survey_recently_infected
  art_or_vls <- options$survey_art_or_vls

  artnum_quarter_id_t1 <- options$art_t1
  artnum_quarter_id_t2 <- options$art_t2

  anc_prevalence_year1 <- options$anc_prevalence_year1
  anc_prevalence_year2 <- options$anc_prevalence_year2
  anc_art_coverage_year1 <- options$anc_art_coverage_year1
  anc_art_coverage_year2 <- options$anc_art_coverage_year2

  naomi_mf <- naomi_model_frame(areas,
                                population,
                                spec,
                                scope = scope,
                                level = level,
                                quarter_id_t1,
                                quarter_id_t2)

  naomi_data <- select_naomi_data(naomi_mf,
                                  survey,
                                  anc,
                                  art_number,
                                  prev_survey_ids,
                                  artcov_survey_ids,
                                  recent_survey_ids,
                                  vls_survey_ids,
                                  artnum_quarter_id_t1,
                                  artnum_quarter_id_t2,
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
  outputs <- output_package(fit, naomi_mf, areas)
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
