
#' Test fit model to Northern Malawi only for efficiency

spec <- extract_pjnz_naomi(system.file("extdata/mwi2019.PJNZ", package = "naomi"))

mf <- naomi_model_frame(create_areas(mwi_area_levels, mwi_area_hierarchy, mwi_area_boundaries),
                        mwi_population_agesex,
                        spec,
                        scope = "MWI.1",
                        level = 4,
                        quarter_id1 = convert_quarter_id(1, 2016),
                        quarter_id2 = convert_quarter_id(3, 2018))

data <- select_naomi_data(mf,
                          mwi_survey_hiv_indicators,
                          anc_testing = mwi_anc_testing,
                          art_number = mwi_art_number,
                          prev_survey_ids = c("MWI2016PHIA", "MWI2015DHS"),
                          artcov_survey_ids = "MWI2016PHIA",
                          recent_survey_ids = "MWI2016PHIA")

tmb_inputs <- prepare_tmb_inputs(data)

fit <- fit_tmb(tmb_inputs, outer_verbose = FALSE)

#' Fit model with no ART attendance allocation

mf <- naomi_model_frame(create_areas(mwi_area_levels, mwi_area_hierarchy, mwi_area_boundaries),
                        mwi_population_agesex,
                        spec,
                        scope = "MWI.1",
                        level = 4,
                        quarter_id1 = convert_quarter_id(1, 2016),
                        quarter_id2 = convert_quarter_id(3, 2018),
                        artattend = FALSE)

data <- select_naomi_data(mf,
                          mwi_survey_hiv_indicators,
                          anc_testing = mwi_anc_testing,
                          art_number = mwi_art_number,
                          prev_survey_ids = c("MWI2016PHIA", "MWI2015DHS"),
                          artcov_survey_ids = "MWI2016PHIA",
                          recent_survey_ids = "MWI2016PHIA")

tmb_input <- prepare_tmb_inputs(data)

fit_noartattend <- fit_tmb(tmb_inputs, outer_verbose = FALSE)
