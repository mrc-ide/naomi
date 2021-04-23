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

  out_fr <- output_package(a_fit_sample, a_naomi_mf)
  expect_setequal(out_fr$meta_period$quarter_label, c("Mars 2016", "Septembre 2018", "Juin 2019"))
  expect_setequal(out_fr$meta_indicator$indicator_label[out_fr$meta_indicator$indicator %in% c("art_coverage", "prevalence")],
                  c("Prévalence du VIH", "Couverture ART"))
  expect_setequal(out_fr$meta_indicator$description[out_fr$meta_indicator$indicator %in% c("art_coverage", "prevalence")],
                  c("Proportion de la population totale séropositive",
                    "Proportion de PLHIV sur ART (résidents)"))
})


test_that("all output stratifications are included in metatdata", {

  expect_setequal(a_output_full$indicators$age_group, a_output_full$meta_age_group$age_group)
  expect_setequal(a_output_full$indicators$indicator, a_output_full$meta_indicator$indicator)
  expect_setequal(a_output_full$indicators$area_id, a_output_full$meta_area$area_id)
  expect_setequal(a_output_full$indicators$calendar_quarter, a_output_full$meta_period$calendar_quarter)

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

test_that("subset output returns expected subset", {


  area_id_sub <- c("MWI_1_1_demo", "MWI_2_1_demo")
  sex_sub <- "both"
  age_group_sub <- c("Y000_014", "Y015_024", "Y050_999")
  calendar_quarter_sub <- c("CY2018Q3", "CY2019Q2")
  indicator_sub <- c("prevalence", "plhiv")

  sub_keep <- subset_naomi_output(a_output,
                                  area_id = area_id_sub,
                                  sex = sex_sub,
                                  age_group = age_group_sub,
                                  calendar_quarter = calendar_quarter_sub,
                                  indicator = indicator_sub)

  expect_setequal(area_id_sub, sub_keep$indicators$area_id)
  expect_setequal(sex_sub, sub_keep$indicators$sex)
  expect_setequal(age_group_sub, sub_keep$indicators$age_group)
  expect_setequal(calendar_quarter_sub, sub_keep$indicators$calendar_quarter)
  expect_setequal(indicator_sub, sub_keep$indicators$indicator)


  area_level_sub <- 1
  sub_keep_level <- subset_naomi_output(a_output,
                                        area_level = area_level_sub,
                                        sex = sex_sub,
                                        age_group = age_group_sub,
                                        calendar_quarter = calendar_quarter_sub,
                                        indicator = indicator_sub)
  expect_setequal("MWI_1_1_demo", sub_keep_level$indicators$area_id)

  sub_drop <- subset_naomi_output(a_output,
                                  area_id = area_id_sub,
                                  sex = sex_sub,
                                  age_group = age_group_sub,
                                  calendar_quarter = calendar_quarter_sub,
                                  indicator = indicator_sub,
                                  drop = TRUE)

  expect_setequal(setdiff(a_output$meta_area$area_id, area_id_sub),
                  sub_drop$indicators$area_id)
  expect_setequal(c("male", "female"), sub_drop$indicators$sex)
  expect_setequal(setdiff(a_output$meta_age_group$age_group, c(age_group_sub, "Y000_000", "Y001_004")),
                  sub_drop$indicators$age_group)
  expect_setequal(setdiff(a_output$meta_period$calendar_quarter, calendar_quarter_sub),
                  sub_drop$indicators$calendar_quarter)
  expect_setequal(setdiff(a_output$meta_indicator$indicator, indicator_sub),
                  sub_drop$indicators$indicator)


  sub_drop_level <- subset_naomi_output(a_output, area_level = 2:4, drop = TRUE)
  expect_setequal("MWI_1_1_demo", sub_drop_level$indicators$area_id)


  expect_error(subset_naomi_output(a_output, area_id = c("MWI_2_1_demo", "jibberish")),
               "area_ids not found in naomi_output: jibberish")
  expect_error(subset_naomi_output(a_output, area_id = c("MWI_2_1_demo", "jibberish"), check_list = FALSE), NA)

})

test_that("subset_output_package() saves expected output package", {

  area_id_sub <- c("MWI_1_2_demo", "MWI_2_2_demo")
  sex_sub <- "both"
  age_group_sub <- c("Y000_014", "Y015_024", "Y050_999")
  calendar_quarter_sub <- c("CY2018Q3", "CY2019Q2")
  indicator_sub <- c("prevalence", "plhiv")

  sub_keep_file <- tempfile(fileext = ".zip")

  sub_keep_return <- expect_warning(
    subset_output_package(a_hintr_output$spectrum_path,
                          sub_keep_file,
                          area_id = area_id_sub,
                          sex = sex_sub,
                          age_group = age_group_sub,
                          calendar_quarter = calendar_quarter_sub,
                          indicator = indicator_sub),
    "PSNU level 3 not included in model outputs"
  )

  sub_keep_out <- read_output_package(sub_keep_file)

  expect_equal(normalizePath(sub_keep_return),
               normalizePath(sub_keep_file))
  expect_is(sub_keep_out, "naomi_output")
  expect_setequal(area_id_sub, sub_keep_out$indicators$area_id)
  expect_setequal(sex_sub, sub_keep_out$indicators$sex)
  expect_setequal(age_group_sub, sub_keep_out$indicators$age_group)
  expect_setequal(calendar_quarter_sub, sub_keep_out$indicators$calendar_quarter)
  expect_setequal(indicator_sub, sub_keep_out$indicators$indicator)


  sub_drop_file <- tempfile(fileext = ".zip")

  sub_drop_return <- subset_output_package(a_hintr_output$spectrum_path,
                                           sub_drop_file,
                                           area_id = area_id_sub,
                                           sex = sex_sub,
                                           age_group = age_group_sub,
                                           calendar_quarter = calendar_quarter_sub,
                                           indicator = indicator_sub,
                                           drop = TRUE)

  sub_drop_out <- read_output_package(sub_drop_file)

  expect_true(!any(area_id_sub %in% sub_drop_out$indicators$area_id))
  expect_true(!any(sex_sub %in% sub_drop_out$indicators$sex))
  expect_true(!any(age_group_sub %in% sub_drop_out$indicators$age_group))
  expect_true(!any(calendar_quarter_sub %in% sub_drop_out$indicators$calendar_quarter))
  expect_true(!any(indicator_sub %in% sub_drop_out$indicators$indicator))

})

test_that("can generate summary report", {
  t <- tempfile(fileext = ".html")
  generate_output_summary_report(t, a_hintr_output$spectrum_path, quiet = TRUE)
  expect_true(file.size(t) > 2000)
  content <- brio::readLines(t)
  expect_true(any(grepl("DEMO2016PHIA, DEMO2015DHS", content)))
  expect_true(any(grepl("demo_mwi2019.PJNZ", content)))
  expect_true(any(grepl("Central", content)))
  expect_true(any(grepl("class=\"logo-naomi\"", content)))
})

test_that("output_package() catches error if NA in simulated sample.", {

  bad_sample <- a_fit_sample
  bad_sample$sample$alpha_t1_out[50] <- NA
  expect_error(output_package(bad_sample, a_naomi_mf),
               "Error simulating output for indicator: alpha_t1_out. Please contact support for troubleshooting.")
})

test_that("summary report can be translated", {
  t_en <- tempfile(fileext = ".html")
  generate_output_summary_report(t_en, a_hintr_output$spectrum_path,
                                 quiet = TRUE)
  expect_true(file.size(t_en) > 2000)
  content <- brio::readLines(t_en)
  ## Contains both sets of content
  expect_true(any(grepl("Methods", content)))
  expect_true(any(grepl("Méthodes", content)))
  ## Styling correct - all non English sections are hidden
  expect_true(any(grepl('#translate[lang="en"]', content, fixed = TRUE)))
  style_line <- which(grepl('#translate[lang="en"]', content,
                      fixed = TRUE))
  expect_equal(content[style_line + 1], "display: block;")


  reset <- naomi_set_language("fr")
  on.exit(reset())
  t_fr <- tempfile(fileext = ".html")
  generate_output_summary_report(t_fr, a_hintr_output$spectrum_path,
                                 quiet = TRUE)
  expect_true(file.size(t_fr) > 2000)
  content <- brio::readLines(t_fr)
  ## Contains both sets of content
  expect_true(any(grepl("Methods", content)))
  expect_true(any(grepl("Méthodes", content)))
  ## Styling correct - all non French sections are hidden
  expect_true(any(grepl('#translate[lang="fr"]', content,
                        fixed = TRUE)))
  style_line <- which(grepl('#translate[lang="fr"]', content,
                            fixed = TRUE))
  expect_equal(content[style_line + 1], "display: block;")
})
