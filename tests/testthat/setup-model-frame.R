
a_area_merged <- sf::read_sf(system.file("extdata/demo_areas.geojson", package = "naomi"))
a_spec <- extract_pjnz_naomi(system.file("extdata/demo_mwi2019.PJNZ", package = "naomi"))


a_naomi_mf <- naomi_model_frame(a_area_merged,
                                demo_population_agesex,
                                a_spec,
                                scope = "MWI_1_1_demo",
                                level = 4,
                                calendar_quarter1 = "CY2016Q1",
                                calendar_quarter2 = "CY2018Q3",
                                calendar_quarter3 = "CY2019Q2",
                                artattend = FALSE,
                                spectrum_population_calibration = "none")

a_naomi_data <- select_naomi_data(a_naomi_mf,
                                  demo_survey_hiv_indicators,
                                  demo_anc_testing,
                                  demo_art_number,
                                  prev_survey_ids = c("DEMO2016PHIA", "DEMO2015DHS"),
                                  artcov_survey_ids = "DEMO2016PHIA",
                                  recent_survey_ids = "DEMO2016PHIA")

                                                      
a_tmb_inputs <- prepare_tmb_inputs(a_naomi_data)
a_fit <- fit_tmb(a_tmb_inputs, outer_verbose = FALSE)
a_fit_sample <- sample_tmb(a_fit, nsample = 30, rng_seed = 28)
a_output <- output_package(a_fit_sample, a_naomi_mf)

a_output_calib <- calibrate_outputs(a_output, a_naomi_data,
                                    "none", "sex_age_coarse",
                                    "none", "sex_age_coarse",
                                    "none", "sex_age_coarse",
                                    "none", "sex_age_coarse",
                                    calibrate_method = "logistic")

a_output_full  <- disaggregate_0to4_outputs(a_output_calib, a_naomi_data)
