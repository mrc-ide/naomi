run_model <- function(data, options, output_path, spectrum_path,
                      indicators_path) {

  ## Options will have previously been validated

  ## Prepare input data
  ## Question - do we want to pass in already read data instead of paths to data?
  ## That way we can leverage caching but perhaps that isn't a big win?
  area_merged <- read_sf(data$pjnz)

  pop_agesex <- read_csv(data$population)

  survey_hiv_indicators <- read_csv(data$survey)

  art_number <- read_csv(data$art)
  anc_testing <- read_csv(data$anc)

  art_number <- art_number %>%
    filter(age_group_id == 20)

  spec <- extract_pjnz_naomi(data$pjnz)

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

  ## TODO: How are the options for these being used? They are slightly different
  ## to what is in the vignette
  ## TODO: What about when these are NULL?
  anc_quarter_id_t1 <- convert_quarter_id(c(4, 1, 2, 3), c(2015, 2016, 2016, 2016))
  anc_quarter_id_t2 <- convert_quarter_id(1:4, 2018)
  anc_prevalence_t1 <- options$anc_prevalence_t1
  anc_prevalence_t2 <- options$anc_prevalence_t2
  anc_art_coverage_t1 <- options$anc_art_coverage_t1
  anc_art_coverage_t2 <- options$anc_art_coverage_t2

  naomi_mf <- naomi_model_frame(areas,
                                pop_agesex,
                                spec,
                                scope = scope,
                                level = level,
                                quarter_id_t1,
                                quarter_id_t2)


  naomi_data <- select_naomi_data(naomi_mf,
                                  survey_hiv_indicators,
                                  anc_testing,
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

  ## Run the model
  fit <- fit_tmb(tmb_inputs)

  ## Add uncertainty
  fit <- sample_tmb(fit)

  ## Prepare output package
  outputs <- output_package(fit, naomi_mf, areas)
  saveRDS(outputs, file = output_path)
  save_output_indicators(indicators_path, outputs)
  save_output_spectrum(spectrum_path, outputs)

  ## Return output path spectrum path and indicators path
  list(output_path = output_path,
       spectrum_path = spectrum_path,
       indicators_path = indicators_path)
}
