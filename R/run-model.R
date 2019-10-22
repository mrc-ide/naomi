run_model <- function(data, options, output_path, spectrum_path,
                      indicators_path) {

  ## Options will have previously been validated

  ## Prepare input data
  ## Question - do we want to pass in already read data instead of paths to data?
  ## That way we can leverage caching but perhaps that isn't a big win?
  area_merged <- read_sf(data$pjnz)

  pop_agesex <- read_csv(system.file("extdata/population/population_agesex.csv", package = "naomi"))

  survey_hiv_indicators <- read_csv(system.file("extdata/survey/survey_hiv_indicators.csv", package = "naomi"))

  art_number <- read_csv(system.file("extdata/programme/art_number.csv", package = "naomi"))
  anc_testing <- read_csv(system.file("extdata/programme/anc_testing.csv", package = "naomi"))

  art_number <- art_number %>%
    filter(age_group_id == 20)

  pjnz <- system.file("extdata/mwi2019.PJNZ", package = "naomi")
  spec <- extract_pjnz_naomi(pjnz)


  ## Get from the options
  scope <- "MWI"
  level <- 4
  quarter_id_t1 <- convert_quarter_id(1, 2016)
  quarter_id_t2 <- convert_quarter_id(3, 2018)
  prev_survey_ids  <- c("MWI2016PHIA", "MWI2015DHS")
  artcov_survey_ids  <- "MWI2016PHIA"
  vls_survey_ids <- NULL
  recent_survey_ids <- "MWI2016PHIA"

  artnum_quarter_id_t1 <- convert_quarter_id(1, 2016)
  artnum_quarter_id_t2 <- convert_quarter_id(3, 2018)

  anc_quarter_id_t1 <- convert_quarter_id(c(4, 1, 2, 3), c(2015, 2016, 2016, 2016))
  anc_quarter_id_t2 <- convert_quarter_id(1:4, 2018)


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
  outputs$indicators %>%
    dplyr::filter(
      indicator_id == 2L,  # HIV prevalence
      age_group_id == 18   # Age group 15-49
    ) %>%
    head()

  save_output_package(outputs, "mwi_outputs_with_labels", "outputs", with_labels = TRUE)

  ## Return output path spectrum path and indicators path
  list(output_path = output_path,
       spectrum_path = spectrum_path,
       indicators_path = indicators_path)
}
