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

  expect_setequal(a_output$indicators$age_group,
                  setdiff(a_output$meta_age_group$age_group, c("00-00", "01-04")))
  expect_setequal(a_output$indicators$indicator, a_output$meta_indicator$indicator)
  expect_setequal(a_output$indicators$area_id, a_output$meta_area$area_id)
  expect_setequal(a_output$indicators$calendar_quarter, a_output$meta_period$calendar_quarter)

})
