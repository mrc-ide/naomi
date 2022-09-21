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

  ## spec$infections[spec$age == 15] <- 0.0
  
  pop_zone <- pop_agesex %>%
    dplyr::left_join(
      sf::st_drop_geometry(area_merged) %>%
        dplyr::select(, area_id, parent_area_id),
      by = "area_id") %>%
    dplyr::count(area_id = parent_area_id, source, calendar_quarter, sex, age_group,
                 wt = population, name = "population")
    

  ## Mid-year to mid-year, 3 year projection, region level
  ## * With mid-year to mid-year projection at same level
  ##   as the PJNZ, expect to exactly return Spectrum
  ##   projection.
  naomi_mfA <- naomi_model_frame(area_merged,
                                 pop_zone,
                                 spec,
                                 scope = "MWI",
                                 level = 1,
                                 calendar_quarter1 = "CY2016Q2",
                                 calendar_quarter2 = "CY2019Q2",
                                 calendar_quarter3 = "CY2024Q4",
                                 spectrum_population_calibration = "subnational")
  
  mfA <- naomi_mfA$mf_model
  
  mfA$plhiv_t1 <- mfA$population_t1 * mfA$spec_prev_t1
  mfA$plhiv_t2_input <- mfA$population_t2 * mfA$spec_prev_t2
  
  mfA$lambda_adult_t1 <- mfA$spec_incid_t1
  mfA$lambda_adult_t1[mfA$age_group == "Y000_004"] <- 0.0
  mfA$infections_adult_t1t2 <- mfA$lambda_adult_t1 * (mfA$population_t1 - mfA$plhiv_t1)
  
  mfA$plhiv_t2_proj <- as.vector(
    naomi_mfA$Lproj_t1t2$Lproj_hivpop %*% mfA$plhiv_t1 +
      naomi_mfA$Lproj_t1t2$Lproj_paed %*% mfA$plhiv_t1 +
      naomi_mfA$Lproj_t1t2$Lproj_incid %*% mfA$infections_adult_t1t2
  )
  
  expect_equal(mfA$plhiv_t2_proj, mfA$plhiv_t2_input)

  ## Mid-year to mid-year, **7 year projection**, region level
  ## * Checking the same, but with projection spanning longer
  ##   period
  naomi_mfB <- naomi_model_frame(area_merged,
                                 pop_zone,
                                 spec,
                                 scope = "MWI",
                                 level = 1,
                                 calendar_quarter1 = "CY2016Q2",
                                 calendar_quarter2 = "CY2023Q2",
                                 calendar_quarter3 = "CY2024Q4",
                                 spectrum_population_calibration = "subnational")
  
  mfB <- naomi_mfB$mf_model
  
  mfB$plhiv_t1 <- mfB$population_t1 * mfB$spec_prev_t1
  mfB$plhiv_t2_input <- mfB$population_t2 * mfB$spec_prev_t2
  
  mfB$lambda_adult_t1 <- mfB$spec_incid_t1
  mfB$lambda_adult_t1[mfB$age_group == "Y000_004"] <- 0.0
  mfB$infections_adult_t1t2 <- mfB$lambda_adult_t1 * (mfB$population_t1 - mfB$plhiv_t1)
  
  mfB$plhiv_t2_proj <- as.vector(
    naomi_mfB$Lproj_t1t2$Lproj_hivpop %*% mfB$plhiv_t1 +
      naomi_mfB$Lproj_t1t2$Lproj_paed %*% mfB$plhiv_t1 +
      naomi_mfB$Lproj_t1t2$Lproj_incid %*% mfB$infections_adult_t1t2
  )
  
  expect_equal(mfB$plhiv_t2_proj, mfB$plhiv_t2_input)

  ## Mid-year to mid-year, 3 year projection, **ZONE** level
  ## * When model level (zone) is below the PJNZ level (region),
  ##   don't expect the projection to align at the model level,
  ##   but aggregation to PJNZ level should match

  naomi_mfC <- naomi_model_frame(area_merged,
                                 pop_agesex,     ## Zone population
                                 spec,
                                 scope = "MWI",
                                 level = 2,      ## Zones
                                 calendar_quarter1 = "CY2016Q2",
                                 calendar_quarter2 = "CY2019Q2",
                                 calendar_quarter3 = "CY2024Q4",
                                 spectrum_population_calibration = "subnational")
  
  mfC <- naomi_mfC$mf_model
  
  mfC$plhiv_t1 <- mfC$population_t1 * mfC$spec_prev_t1
  mfC$plhiv_t2_input <- mfC$population_t2 * mfC$spec_prev_t2
  
  mfC$lambda_adult_t1 <- mfC$spec_incid_t1
  mfC$lambda_adult_t1[mfC$age_group == "Y000_004"] <- 0.0
  mfC$infections_adult_t1t2 <- mfC$lambda_adult_t1 * (mfC$population_t1 - mfC$plhiv_t1)
  
  mfC$plhiv_t2_proj <- as.vector(
    naomi_mfC$Lproj_t1t2$Lproj_hivpop %*% mfC$plhiv_t1 +
      naomi_mfC$Lproj_t1t2$Lproj_paed %*% mfC$plhiv_t1 +
      naomi_mfC$Lproj_t1t2$Lproj_incid %*% mfC$infections_adult_t1t2
  )

  mfCregion <- mfC %>%
    dplyr::group_by(spectrum_region_code, sex, age_group) %>%
    dplyr::summarise(dplyr::across(c(plhiv_t2_proj, plhiv_t2_input), sum),
                     .groups = "drop")
  
  expect_equal(mfCregion$plhiv_t2_proj, mfCregion$plhiv_t2_input)

  ## **NOT** mid-year to mid-year (3.5 years projection)
  ## * Expect some small difference due to interpolation,
  ##   but should be relatively close.
  ##  _This is a soft test. May require adjustment to the threshold
  ##   if data change._

  naomi_mfD <- naomi_model_frame(area_merged,
                                 pop_zone,
                                 spec,
                                 scope = "MWI",
                                 level = 1,
                                 calendar_quarter1 = "CY2016Q2",
                                 calendar_quarter2 = "CY2019Q4",
                                 calendar_quarter3 = "CY2024Q4",
                                 spectrum_population_calibration = "subnational")
  
  mfD <- naomi_mfD$mf_model
  
  mfD$plhiv_t1 <- mfD$population_t1 * mfD$spec_prev_t1
  mfD$plhiv_t2_input <- mfD$population_t2 * mfD$spec_prev_t2
  
  mfD$lambda_adult_t1 <- mfD$spec_incid_t1
  mfD$lambda_adult_t1[mfD$age_group == "Y000_004"] <- 0.0
  mfD$infections_adult_t1t2 <- mfD$lambda_adult_t1 * (mfD$population_t1 - mfD$plhiv_t1)
  
  mfD$plhiv_t2_proj <- as.vector(
    naomi_mfD$Lproj_t1t2$Lproj_hivpop %*% mfD$plhiv_t1 +
      naomi_mfD$Lproj_t1t2$Lproj_paed %*% mfD$plhiv_t1 +
      naomi_mfD$Lproj_t1t2$Lproj_incid %*% mfD$infections_adult_t1t2
  )

  ## Not exactly equal
  expect_match(all.equal(mfD$plhiv_t2_proj, mfD$plhiv_t2_input),
               "Mean relative difference")

  ## Less than 0.1% relative difference
  expect_equal(mfD$plhiv_t2_proj, mfD$plhiv_t2_input, tolerance = 0.001)

})
