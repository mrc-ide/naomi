test_that("Lproj returns", {

  ## Zone demo data
  
  zone_data <- list(
    pjnz = system.file("extdata/demo-subnational-pjnz/demo_mwi2019_region-pjnz.zip", package = "naomi"),
    population = system.file("extdata/demo-subnational-pjnz/demo_population_zone.csv", package = "naomi"),
    shape = system.file("extdata/demo-subnational-pjnz/demo_areas_region-pjnz.geojson", package = "naomi"),
    survey = system.file("extdata/demo_survey_hiv_indicators.csv", package = "naomi"),
    art_number = system.file("extdata/demo-subnational-pjnz/demo_art_number_zone.csv", package = "naomi"),
    anc_testing = system.file("extdata/demo-subnational-pjnz/demo_anc_testing_zone.csv", package = "naomi")
  )

  area_merged <- read_area_merged(zone_data$shape)
  pop_agesex <- read_population(zone_data$population)
  spec <- extract_pjnz_naomi(zone_data$pjnz)

  spec$infections[spec$age == 15] <- 0.0
  
  pop_zone <- pop_agesex %>%
    dplyr::left_join(
      sf::st_drop_geometry(area_merged) %>%
        dplyr::select(, area_id, parent_area_id),
      by = "area_id") %>%
    dplyr::count(area_id = parent_area_id, source, calendar_quarter, sex, age_group,
                 wt = population, name = "population")
    

  ## Mid-year to mid-year, 3 year projection
  naomi_mf <- naomi_model_frame(area_merged,
                                pop_zone,
                                spec,
                                scope = "MWI_1_3_demo",
                                level = 1,
                                calendar_quarter1 = "CY2016Q2",
                                calendar_quarter2 = "CY2019Q2",
                                calendar_quarter3 = "CY2023Q4",
                                spectrum_population_calibration = "subnational")

  mf <- naomi_mf$mf_model
  
  population_t1 <- mf$population_t1
  plhiv_t1 <- mf$population_t1 * mf$spec_prev_t1

  lambda_adult_t1 <- mf$spec_incid_t1
  lambda_adult_t1[mf$age_group == "Y000_004"] <- 0.0
  infections_adult_t1t2 <- lambda_adult_t1 * (population_t1 - plhiv_t1)
  
  plhiv_t2_input <- mf$population_t2 * mf$spec_prev_t2
  
  plhiv_t2_proj <- naomi_mf$Lproj_t1t2$Lproj_hivpop %*% plhiv_t1 +
    naomi_mf$Lproj_t1t2$Lproj_paed %*% plhiv_t1 +
    naomi_mf$Lproj_t1t2$Lproj_incid %*% infections_adult_t1t2

  plhiv_t2_proj <- as.vector(plhiv_t2_proj)


  expect_equal(plhiv_t2_proj, plhiv_t2_input)

  
})
