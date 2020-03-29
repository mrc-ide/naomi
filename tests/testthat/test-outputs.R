context("model outputs")

test_that("traidure hooks work in model outputs", {

  out_en <- output_package(a_fit_sample, a_naomi_mf)
  expect_setequal(out_en$meta_period$quarter_label, c("March 2016", "September 2018", "June 2019"))
  expect_setequal(out_en$meta_indicator$indicator_label[out_en$meta_indicator$indicator %in% c("art_coverage", "prevalence")],
                  c("ART coverage", "HIV prevalence"))
  expect_setequal(out_en$meta_indicator$description[out_en$meta_indicator$indicator %in% c("art_coverage", "prevalence")],
                  c("Proportion of PLHIV on ART (residents)", "Proportion of total population HIV positive"))
   
  reset <- naomi_set_language("fr")
  on.exit(reset())

  ## !!! TODO test need updating with French strings
  out_fr <- output_package(a_fit_sample, a_naomi_mf)
  ## expect_setequal(out_fr$meta_period$quarter_label, c("<...> 2016", "<...> 2018", "<...> 2020"))
  expect_setequal(out_fr$meta_indicator$indicator_label[out_fr$meta_indicator$indicator %in% c("art_coverage", "prevalence")],
                  c("Prévalence du VIH", "Couverture ART"))
  ## expect_setequal(out_fr$meta_indicator$description[out_fr$meta_indicator$indicator %in% c("art_coverage", "prevalence")],
  ##                 c("<...>", "<...>"))
})


test_that("all output stratifications are included in metatdata", {

  expect_setequal(a_output_full$indicators$age_group, a_output_full$meta_age_group$age_group)
  expect_setequal(a_output_full$indicators$indicator, a_output_full$meta_indicator$indicator)
  expect_setequal(a_output_full$indicators$area_id, a_output_full$meta_area$area_id)
  expect_setequal(a_output_full$indicators$calendar_quarter, a_output_full$meta_period$calendar_quarter)

})

test_that("datapack export writes a csv", {

  tmpf <- tempfile(fileext = ".csv")
  res <- export_datapack(a_output_full, tmpf)
  datapack <- readr_read_csv(res)

  expect_equal(tmpf, res)
  expect_true(!any(is.na(datapack)))
})

test_that("write and read hintr outputs returns same thing", {

  read1 <- read_output_package(a_hintr_output$spectrum_path)

  tmpf <- tempfile(fileext = ".zip")
  save_output_spectrum(tmpf, read1)
  read2 <- read_output_package(tmpf)

  ## The sfc column creates an error for expect_equal(). Check the type
  ## then drop the geometry column.
  expect_is(read2$meta_area, "sf")
  read1$meta_area <- sf::st_drop_geometry(read1$meta_area)
  read2$meta_area <- sf::st_drop_geometry(read2$meta_area)
  
  expect_equal(read1, read2)
})

test_that("write and read naomi outputs returns same thing", {

  tmpf <- tempfile(fileext = ".zip")
  save_output_package(a_output_full, basename(tmpf), dirname(tmpf))
  
  read1 <- read_output_package(tmpf)

  ## Note: expect_equal(a_output_full, read1) doesn't work due to 
  ## rounding errors in CSV write/read of numerical outputs.
  
  expect_equal(lapply(a_output_full, names), lapply(read1, names))
  expect_equal(lapply(a_output_full, dim), lapply(read1, dim))

  expect_equal(lapply(a_output_full$fit, names), lapply(read1$fit, names))
  expect_equal(lapply(a_output_full$fit, dim), lapply(read1$fit, dim))
    
  expect_equal(attributes(a_output_full), attributes(read1))
})

test_that("subset output returns output packages", {

  outf <- tempfile(fileext = ".zip")
  save_output_spectrum(outf, a_output_full)

  output_full <- read_output_package(outf)
  
})
