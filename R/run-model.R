#' Run the model and save output to file
#'
#' This prepares the model inputs from data and options and saves output as
#' an rds, the spectrum digst and indicators at specified paths.
#'
#' @param data List of paths to input data files. Must provide pjnz, shape,
#' population and survey data. Optionally include anc and art data too.
#' @param options List of model run options, must specify area_scope,
#' area_level, t1, t2, survey_prevalence, survey_art_coverage, survey_vls,
#' survey_recently_infected, survey_art_or_vls, art_t1, art_t2,
#' anc_prevalence_t1, anc_prevalence_t2, anc_art_coverage_t1,
#' anc_art_coverage_t2.
#' @param output_path Path to store output indicators as an RDS at.
#' @param spectrum_path Path to store spectrum digest file at.
#' @param summary_path Path to store summary download zip file at.
#'
#' @return Paths to 3 output files.
#' @export
#'
run_model <- function(data, options, output_path, spectrum_path,
                      summary_path) {

  ## Options will have previously been validated
  ## TODO: What format do the progress messages have to adhere to?
  progress("Preparing input data")
  area_merged <- sf::read_sf(data$shape)
  areas <- create_areas(area_merged = area_merged)
  population <- readr::read_csv(data$population)
  survey <- readr::read_csv(data$survey)
  art_number <- readr::read_csv(data$art)
  anc <- readr::read_csv(data$anc)

  spec <- extract_pjnz_naomi(data$pjnz)

  ## TODO: Remove this filter - it is in temporarily as model does not run
  ## without it mrc-640
  art_number <- art_number %>%
    dplyr::filter(age_group_id == 20)

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

  ## TODO: make these single values once we have updated to using years instead
  ## of quarter mrc-577
  ## TODO: make anc prevalence and anc_art_coverage separate controls of quarter
  ## in the naomi_model_frame code
  anc_quarter_id_t1 <- convert_quarter_id(c(4, 1, 2, 3), c(2015, 2016, 2016, 2016))
  anc_quarter_id_t2 <- convert_quarter_id(1:4, 2018)
  anc_prevalence_t1 <- options$anc_prevalence_t1
  anc_prevalence_t2 <- options$anc_prevalence_t2
  anc_art_coverage_t1 <- options$anc_art_coverage_t1
  anc_art_coverage_t2 <- options$anc_art_coverage_t2

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
                                  anc_quarter_id_t1,
                                  anc_quarter_id_t2)

  tmb_inputs <- prepare_tmb_inputs(naomi_data)

  progress("Fitting the model")
  fit <- fit_tmb(tmb_inputs)

  progress("Generating uncertainty")
  fit <- sample_tmb(fit)

  progress("Preparing outputs")
  outputs <- output_package(fit, naomi_mf, areas)
  indicators <- add_output_labels(outputs)
  saveRDS(indicators, file = output_path)
  save_result_summary(summary_path, outputs)
  save_output_spectrum(spectrum_path, outputs)

  list(output_path = output_path,
       spectrum_path = spectrum_path,
       summary_path = summary_path)
}

progress <- function(message) {
  withRestarts({
    signalCondition(structure(list(message = message),
                            class = c("progress", "condition")))
  }, muffleProgress = function(...) NULL)
}
