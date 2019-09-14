context("test-example-data")

test_that("example areas datasets are valid", {
  expect_is(
    create_areas(mwi_area_layer_meta,
                 mwi_area_hierarchy,
                 mwi_area_layers)
   ,
    "naomi_areas")
})

test_that("example population dataset match area hierarchy", {
  expect_true(all(mwi_population_agesex$area_id %in%
                  subset(mwi_area_layers, layer_id == "MWI_4")$area_id))
})

test_that("example survey indicators dataset match area hierarchy", {
  expect_true(all(mwi_survey_hiv_indicators$area_id %in% mwi_area_layers$area_id))
})

test_that("example programme datasets match area hierarchy", {
  expect_true(all(mwi_anc_testing$area_id %in% 
                  subset(mwi_area_layers, layer_id == "MWI_4")$area_id))
  expect_true(all(mwi_art_number$area_id %in% 
                  subset(mwi_area_layers, layer_id == "MWI_4")$area_id))
})
