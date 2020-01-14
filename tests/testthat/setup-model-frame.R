INLA:::inla.dynload.workaround()

a_area_merged <- sf::read_sf(system.file("extdata/areas/area_merged.geojson", package = "naomi"))
a_spec <- extract_pjnz_naomi(system.file("extdata/mwi2019.PJNZ", package = "naomi"))


a_naomi_mf <- naomi_model_frame(a_area_merged,
                                mwi_population_agesex,
                                a_spec,
                                scope = "MWI_1_1",
                                level = 4,
                                calendar_quarter1 = "CY2016Q1",
                                calendar_quarter2 = "CY2018Q3",
                                artattend = FALSE,
                                spectrum_population_calibration = "none")

a_naomi_data <- select_naomi_data(a_naomi_mf,
                                  mwi_survey_hiv_indicators,
                                  mwi_anc_testing,
                                  mwi_art_number,
                                  prev_survey_ids = c("MWI2016PHIA", "MWI2015DHS"),
                                  artcov_survey_ids = "MWI2016PHIA",
                                  recent_survey_ids = "MWI2016PHIA")

                                                      
a_tmb_inputs <- prepare_tmb_inputs(a_naomi_data)
a_fit <- fit_tmb(a_tmb_inputs, outer_verbose = FALSE)
a_fit_sample <- sample_tmb(a_fit, nsample = 30, rng_seed = 28)
a_output <- output_package(a_fit_sample, a_naomi_mf)
