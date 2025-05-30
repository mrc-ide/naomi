test_that("traidure hooks work in model outputs", {
  out_en <- output_package(a_fit_sample, a_naomi_data)
  expect_setequal(out_en$meta_period$quarter_label,
                  c("March 2016", "December 2018", "June 2019", "September 2022", "September 2023"))
  expect_setequal(out_en$meta_indicator$indicator_label[out_en$meta_indicator$indicator %in% c("art_coverage", "prevalence")],
                  c("ART coverage", "HIV prevalence"))
  expect_setequal(out_en$meta_indicator$description[out_en$meta_indicator$indicator %in% c("art_coverage", "prevalence")],
                  c("Proportion of PLHIV on ART (residents)", "Proportion of total population HIV positive"))

  reset <- naomi_set_language("fr")
  on.exit(reset())

  out_fr <- output_package(a_fit_sample, a_naomi_data)
  expect_setequal(out_fr$meta_period$quarter_label,
                  c("Mars 2016", "Décembre 2018", "Juin 2019", "Septembre 2022", "Septembre 2023"))
  expect_setequal(out_fr$meta_indicator$indicator_label[out_fr$meta_indicator$indicator %in% c("art_coverage", "prevalence")],
                  c("Prévalence du VIH", "Couverture TARV"))
  expect_setequal(out_fr$meta_indicator$description[out_fr$meta_indicator$indicator %in% c("art_coverage", "prevalence")],
                  c("Proportion de la population totale séropositive",
                    "Proportion de PLHIV sur TARV (résidents)"))
})


test_that("all output stratifications are included in meta data", {

  expect_setequal(a_output_full$indicators$age_group, a_output_full$meta_age_group$age_group)
  expect_true(all(a_output_full$indicators$indicator %in% a_output_full$meta_indicator$indicator))
  expect_setequal(a_output_full$indicators$area_id, a_output_full$meta_area$area_id)
  expect_setequal(a_output_full$indicators$calendar_quarter, a_output_full$meta_period$calendar_quarter)

})

test_that("write and read hintr outputs returns same thing", {
  out <- hintr_prepare_spectrum_download(a_hintr_output_calibrated)

  read1 <- read_output_package(out$path)

  tmpf <- tempfile(fileext = ".zip")
  save_output_spectrum(tmpf, read1)
  read2 <- read_output_package(tmpf)

  ## The sfc column creates an error for expect_equal(). Check the type
  ## then drop the geometry column.
  expect_s3_class(read2$meta_area, "sf")
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
  calendar_quarter_sub <- c("CY2018Q4", "CY2019Q2")
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
  expect_setequal(setdiff(a_output$indicators$indicator, indicator_sub),
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
  calendar_quarter_sub <- c("CY2023Q4", "CY2024Q3")
  indicator_sub <- c("prevalence", "plhiv")

  sub_keep_file <- tempfile(fileext = ".zip")
  out <- hintr_prepare_spectrum_download(a_hintr_output_calibrated)

  expect_warning(
    expect_warning(
      sub_keep_return <- subset_output_package(out$path,
                                               sub_keep_file,
                                               area_id = area_id_sub,
                                               sex = sex_sub,
                                               age_group = age_group_sub,
                                               calendar_quarter = calendar_quarter_sub,
                                               indicator = indicator_sub),
      "PSNU level 3 not included in model outputs"
    ),
    "Required indicators not in output: plhiv, population, art_current_residents, infections"
  )

  sub_keep_out <- read_output_package(sub_keep_file)

  expect_equal(normalizePath(sub_keep_return),
               normalizePath(sub_keep_file))
  expect_s3_class(sub_keep_out, "naomi_output")
  expect_setequal(area_id_sub, sub_keep_out$indicators$area_id)
  expect_setequal(sex_sub, sub_keep_out$indicators$sex)
  expect_setequal(age_group_sub, sub_keep_out$indicators$age_group)
  expect_setequal(calendar_quarter_sub, sub_keep_out$indicators$calendar_quarter)
  expect_setequal(indicator_sub, sub_keep_out$indicators$indicator)


  ## Test that can **drop** selected indicators (rather than keep)

  sub_drop_file <- tempfile(fileext = ".zip")

  expect_warning(
    sub_drop_return <- subset_output_package(out$path,
                                             sub_drop_file,
                                             area_id = area_id_sub,
                                             sex = sex_sub,
                                             age_group = age_group_sub,
                                             calendar_quarter = calendar_quarter_sub,
                                             indicator = indicator_sub,
                                             drop = TRUE),
    "Required indicators not in output: plhiv"
  )

  sub_drop_out <- read_output_package(sub_drop_file)

  expect_true(!any(area_id_sub %in% sub_drop_out$indicators$area_id))
  expect_true(!any(sex_sub %in% sub_drop_out$indicators$sex))
  expect_true(!any(age_group_sub %in% sub_drop_out$indicators$age_group))
  expect_true(!any(calendar_quarter_sub %in% sub_drop_out$indicators$calendar_quarter))
  expect_true(!any(indicator_sub %in% sub_drop_out$indicators$indicator))

})

test_that("can generate summary report from a qs file", {
  t <- tempfile(fileext = ".html")
  generate_output_summary_report(t, a_hintr_output_calibrated$model_output_path,
                                 quiet = TRUE)
  expect_true(file.size(t) > 2000)
  content <- brio::readLines(t)
  expect_true(any(grepl("DEMO2020PHIA", content)))
  expect_true(any(grepl("demo_mwi2024_region-pjnz.zip", content)))
  expect_true(any(grepl("Central", content)))
  expect_true(any(grepl("class=\"logo-naomi\"", content)))
})

test_that("can generate summary report from zip file", {
  zip <- hintr_prepare_spectrum_download(a_hintr_output_calibrated)
  t <- tempfile(fileext = ".html")
  generate_output_summary_report(t, zip$path, quiet = TRUE)
  expect_true(file.size(t) > 2000)
  content <- brio::readLines(t)
  expect_true(any(grepl("DEMO2020PHIA", content)))
  expect_true(any(grepl("demo_mwi2024_region-pjnz.zip", content)))
  expect_true(any(grepl("Central", content)))
  expect_true(any(grepl("class=\"logo-naomi\"", content)))
})

test_that("calibration options used in summary report if present", {
  zip <- hintr_prepare_spectrum_download(a_hintr_output_calibrated)
  t <- tempfile(fileext = ".html")
  generate_output_summary_report(t, zip$path, quiet = TRUE)
  content <- brio::readLines(t)
  expect_true(any(grepl("Sex and 5-year age group",
                        paste0(content, collapse = " "))))
})

test_that("output_package() catches error if NA in simulated sample.", {

  bad_sample <- a_fit_sample
  bad_sample$sample$alpha_t1_out[50] <- NA
  expect_error(output_package(bad_sample, a_naomi_data),
               "Error simulating output for indicator: alpha_t1_out. Please contact support for troubleshooting.")
})

test_that("summary report can be translated", {
  out <- hintr_prepare_spectrum_download(a_hintr_output_calibrated)
  t_en <- tempfile(fileext = ".html")
  generate_output_summary_report(t_en, out$path, quiet = TRUE)
  expect_true(file.size(t_en) > 2000)
  content <- brio::readLines(t_en)
  ## Contains both sets of content
  expect_true(any(grepl("Methods", content)))
  expect_true(any(grepl("Méthodes", content)))

  ## Styling correct - all non English sections are hidden
  ## Depending on where this is generated the CSS might be included in plain
  ## text (buildkite) or as encoded JSON (locally). We check for 1 or the other
  ## On windows the style is generated on a single line, on linux and max with
  ## newlines
  if (any(grepl('#translate[lang="en"]', content, fixed = TRUE))) {
    style_line <- which(grepl('#translate[lang="en"]', content,
                              fixed = TRUE))
    expect_equal(length(style_line), 1)
    has_block_style <- grepl("display: block\\;", content[style_line]) ||
      content[style_line + 1] == "display: block;"
    expect_true(has_block_style)
  } else {
    en <- "%23translate%5Blang%3D%22en%22%5D%20%7B%0Adisplay%3A%20block%3B%0A%7D"
    expect_true(any(grepl(en, content, fixed = TRUE)))
  }

  reset <- naomi_set_language("fr")
  on.exit(reset())
  t_fr <- tempfile(fileext = ".html")
  generate_output_summary_report(t_fr, out$path, quiet = TRUE)
  expect_true(file.size(t_fr) > 2000)
  content <- brio::readLines(t_fr)
  ## Contains both sets of content
  expect_true(any(grepl("Methods", content)))
  expect_true(any(grepl("Méthodes", content)))

  ## Styling correct - all non English sections are hidden
  ## Depending on where this is generated the CSS might be included in plain
  ## text (buildkite) or as encoded JSON (locally). We check for 1 or the other
  ## On windows the style is generated on a single line, on linux and max with
  ## newlines
  if (any(grepl('#translate[lang="fr"]', content, fixed = TRUE))) {
    style_line <- which(grepl('#translate[lang="fr"]', content,
                              fixed = TRUE))
    expect_equal(length(style_line), 1)
    has_block_style <- grepl("display: block\\;", content[style_line]) ||
      content[style_line + 1] == "display: block;"
    expect_true(has_block_style)
  } else {
    fr <- "%23translate%5Blang%3D%22fr%22%5D%20%7B%0Adisplay%3A%20block%3B%0A%7D"
    expect_true(any(grepl(fr, content, fixed = TRUE)))
  }
})

test_that("navigator checklist returns expected results", {

  model_output <- read_hintr_output(a_hintr_output_calibrated$model_output_path)

  expected_checklist <- c("ART_is_Spectrum"            = NA,
                          "ANC_is_Spectrum"            = NA,
                          "Package_created"            = TRUE,
                          "Package_has_all_data"       = TRUE,
                          "Opt_recent_qtr"             = FALSE,
                          "Opt_future_proj_qtr"        = FALSE,
                          "Opt_area_ID_selected"       = TRUE,
                          "Opt_calendar_survey_match"  = TRUE,
                          "Opt_recent_survey_only"     = TRUE,
                          "Opt_ART_coverage"           = TRUE,
                          "Opt_ANC_data"               = TRUE,
                          "Opt_ART_data"               = TRUE,
                          "Opt_ART_attendance_yes"     = FALSE,
                          "Model_fit"                  = TRUE,
                          "Cal_Population"             = FALSE,
                          "Cal_PLHIV"                  = FALSE,
                          "Cal_ART"                    = TRUE,
                          "Cal_KOS"                    = FALSE,
                          "Cal_new_infections"         = FALSE,
                          "Cal_method"                 = TRUE)

  tmp_checklist <- tempfile(fileext = ".csv")
  write_navigator_checklist(model_output$output_package, tmp_checklist)
  checklist <- utils::read.csv(tmp_checklist)

  expect_equal(unname(expected_checklist[checklist$NaomiCheckPermPrimKey]),
               checklist$TrueFalse)

  ## Construct a checklist that will return all TRUE

  adj_output <- model_output$output_package

  adj_output$fit$model_options$calendar_quarter_t2 <- "CY2024Q4"
  adj_output$fit$model_options$calendar_quarter_t3 <- "CY2025Q3"
  adj_output$fit$model_options$artattend_t2 <- TRUE

  adj_output$fit$data_options$prev_survey_ids <- "DEMO2020PHIA"
  adj_output$fit$data_options$prev_survey_quarters <- "CY2020Q3"

  adj_output$fit$calibration_options$spectrum_population_calibration <- "subnational"
  adj_output$fit$calibration_options$spectrum_artnum_calibration_level <- "subnational"
  adj_output$fit$calibration_options$spectrum_aware_calibration_level <- "subnational"
  adj_output$fit$calibration_options$spectrum_infections_calibration_level <- "subnational"

  adj_output$fit$calibration_options$spectrum_plhiv_calibration_strat <- "sex_age_coarse"
  adj_output$fit$calibration_options$spectrum_aware_calibration_strat <- "sex_age_coarse"
  adj_output$fit$calibration_options$spectrum_infections_calibration_strat <- "sex_age_coarse"

  tmp_checklist_adj <- tempfile(fileext = ".csv")
  write_navigator_checklist(adj_output, tmp_checklist_adj)
  checklist_adj <- utils::read.csv(tmp_checklist_adj)

  expect_true(all(checklist_adj$TrueFalse))
})


test_that("navigator checklist returns results if options lists missing", {

  model_output <- read_hintr_output(a_hintr_output_calibrated$model_output_path)
  no_data_opts_output <- model_output$output_package

  no_data_opts_output$fit$data_options <- NULL

  expect_chklst_no_data_opts<- c("Package_created"            = TRUE,
                                 "Package_has_all_data"       = TRUE,
                                 "Opt_recent_qtr"             = FALSE,
                                 "Opt_future_proj_qtr"        = FALSE,
                                 "Opt_area_ID_selected"       = TRUE,
                                 "Opt_calendar_survey_match"  = NA,
                                 "Opt_recent_survey_only"     = NA,
                                 "Opt_ART_coverage"           = NA,
                                 "Opt_ANC_data"               = NA,
                                 "Opt_ART_data"               = NA,
                                 "Opt_ART_attendance_yes"     = NA,
                                 "Model_fit"                  = TRUE,
                                 "Cal_Population"             = FALSE,
                                 "Cal_PLHIV"                  = FALSE,
                                 "Cal_ART"                    = TRUE,
                                 "Cal_KOS"                    = FALSE,
                                 "Cal_new_infections"         = FALSE,
                                 "Cal_method"                 = TRUE)

  tmp_checklist_no_data_opts<- tempfile(fileext = ".csv")
  write_navigator_checklist(no_data_opts_output, tmp_checklist_no_data_opts)
  checklist_no_data_opts <- utils::read.csv(tmp_checklist_no_data_opts)

  expect_equal(unname(expect_chklst_no_data_opts[checklist_no_data_opts$NaomiCheckPermPrimKey]),
               checklist_no_data_opts$TrueFalse)


  ## No model_options
  no_model_opts_output <- model_output$output_package

  no_model_opts_output$fit$model_options <- NULL

  expect_chklst_no_model_opts <- c("Package_created"            = TRUE,
                                   "Package_has_all_data"       = TRUE,
                                   "Opt_recent_qtr"             = NA,
                                   "Opt_future_proj_qtr"        = NA,
                                   "Opt_area_ID_selected"       = NA,
                                   "Opt_calendar_survey_match"  = NA,
                                   "Opt_recent_survey_only"     = TRUE,
                                   "Opt_ART_coverage"           = TRUE,
                                   "Opt_ANC_data"               = TRUE,
                                   "Opt_ART_data"               = TRUE,
                                   "Opt_ART_attendance_yes"     = NA,
                                   "Model_fit"                  = TRUE,
                                   "Cal_Population"             = FALSE,
                                   "Cal_PLHIV"                  = FALSE,
                                   "Cal_ART"                    = TRUE,
                                   "Cal_KOS"                    = FALSE,
                                   "Cal_new_infections"         = FALSE,
                                   "Cal_method"                 = TRUE)

  tmp_checklist_no_model_opts<- tempfile(fileext = ".csv")
  write_navigator_checklist(no_model_opts_output, tmp_checklist_no_model_opts)
  checklist_no_model_opts <- utils::read.csv(tmp_checklist_no_model_opts)

  expect_equal(unname(expect_chklst_no_model_opts[checklist_no_model_opts$NaomiCheckPermPrimKey]),
               checklist_no_model_opts$TrueFalse)

  ## No calibration_options
  ## No model_options
  no_calib_opts_output <- model_output$output_package

  no_calib_opts_output$fit$calibration_options <- NULL

  expect_chklst_no_calib_opts <- c( "Package_created"            = TRUE,
                                   "Package_has_all_data"       = TRUE,
                                   "Opt_recent_qtr"             = FALSE,
                                   "Opt_future_proj_qtr"        = FALSE,
                                   "Opt_area_ID_selected"       = TRUE,
                                   "Opt_calendar_survey_match"  = TRUE,
                                   "Opt_recent_survey_only"     = TRUE,
                                   "Opt_ART_coverage"           = TRUE,
                                   "Opt_ANC_data"               = TRUE,
                                   "Opt_ART_data"               = TRUE,
                                   "Opt_ART_attendance_yes"     = FALSE,
                                   "Model_fit"                  = TRUE,
                                   "Cal_Population"             = NA,
                                   "Cal_PLHIV"                  = NA,
                                   "Cal_ART"                    = NA,
                                   "Cal_KOS"                    = NA,
                                   "Cal_new_infections"         = NA,
                                   "Cal_method"                 = NA)


  tmp_checklist_no_calib_opts<- tempfile(fileext = ".csv")
  write_navigator_checklist(no_calib_opts_output, tmp_checklist_no_calib_opts)
  checklist_no_calib_opts <- utils::read.csv(tmp_checklist_no_calib_opts)

  expect_equal(unname(expect_chklst_no_calib_opts[checklist_no_calib_opts$NaomiCheckPermPrimKey]),
               checklist_no_calib_opts$TrueFalse)
})

test_that("navigator checklist results change with different calibration options", {

  model_output <- read_hintr_output(a_hintr_output_calibrated$model_output_path)

  ## Changing from "sex_age_group" to "sex_age_coarse" -> TRUE
  adj_output <- model_output$output_package
  adj_output$fit$calibration_options$spectrum_plhiv_calibration_strat <- "sex_age_coarse"

  tmp_checklist_adj <- tempfile(fileext = ".csv")
  write_navigator_checklist(adj_output, tmp_checklist_adj)
  checklist_adj <- utils::read.csv(tmp_checklist_adj)

  expect_true(checklist_adj$TrueFalse[checklist_adj$NaomiCheckPermPrimKey == "Cal_PLHIV"])
  expect_true(checklist_adj$TrueFalse[checklist_adj$NaomiCheckPermPrimKey == "Cal_ART"])  ## Remains TRUE

  ## Changing from "subnational" to "national" -> FALSE
  adj_output <- model_output$output_package
  adj_output$fit$calibration_options$spectrum_artnum_calibration_level <- "national"

  tmp_checklist_adj <- tempfile(fileext = ".csv")
  write_navigator_checklist(adj_output, tmp_checklist_adj)
  checklist_adj <- utils::read.csv(tmp_checklist_adj)

  expect_false(checklist_adj$TrueFalse[checklist_adj$NaomiCheckPermPrimKey == "Cal_PLHIV"]) ## Remains FALSE
  expect_false(checklist_adj$TrueFalse[checklist_adj$NaomiCheckPermPrimKey == "Cal_ART"])

})

test_that("navigator checklist returns results for uncalibrated model output", {

  out_uncalibrated <- read_hintr_output(a_hintr_output$model_output_path)
  tmp_checklist <- tempfile(fileext = ".csv")
  write_navigator_checklist(out_uncalibrated$output_package, tmp_checklist)
  checklist <- utils::read.csv(tmp_checklist)

  expected_checklist <- c("ART_is_Spectrum"            = FALSE,
                          "ANC_is_Spectrum"            = FALSE,
                          "Package_created"            = TRUE,
                          "Package_has_all_data"       = TRUE,
                          "Opt_recent_qtr"             = FALSE,
                          "Opt_future_proj_qtr"        = FALSE,
                          "Opt_area_ID_selected"       = TRUE,
                          "Opt_calendar_survey_match"  = TRUE,
                          "Opt_recent_survey_only"     = TRUE,
                          "Opt_ART_coverage"           = TRUE,
                          "Opt_ANC_data"               = TRUE,
                          "Opt_ART_data"               = TRUE,
                          "Opt_ART_attendance_yes"     = FALSE,
                          "Model_fit"                  = TRUE,
                          "Cal_Population"             = FALSE,
                          "Cal_PLHIV"                  = FALSE,
                          "Cal_ART"                    = FALSE,
                          "Cal_KOS"                    = FALSE,
                          "Cal_new_infections"         = FALSE,
                          "Cal_method"                 = FALSE)

  expect_equal(unname(expected_checklist[checklist$NaomiCheckPermPrimKey]),
               checklist$TrueFalse)


  ## Change population_calibration to "subnational" -> TRUE
  adj_output <- out_uncalibrated$output_package
  adj_output$fit$calibration_options$spectrum_population_calibration <- "subnational"

  tmp_checklist_adj <- tempfile(fileext = ".csv")
  write_navigator_checklist(adj_output, tmp_checklist_adj)
  checklist_adj <- utils::read.csv(tmp_checklist_adj)

  expect_true(checklist_adj$TrueFalse[checklist_adj$NaomiCheckPermPrimKey == "Cal_Population"])
})

test_that("navigator checklist runs with multiple artcov surveys", {
  # This test checks issue-122 has been resolved
  out <- read_hintr_output(a_hintr_output$model_output_path)
  out$output_package$fit$data_options$artcov_survey_ids <-
    c(out$output_package$fit$data_options$artcov_survey_ids, "other")
  checklist <- tempfile(fileext = ".csv")
  write_navigator_checklist(out$output_package, checklist)
  checklist <- utils::read.csv(checklist)

  expected_checklist <- c("ART_is_Spectrum"            = FALSE,
                          "ANC_is_Spectrum"            = FALSE,
                          "Package_created"            = TRUE,
                          "Package_has_all_data"       = TRUE,
                          "Opt_recent_qtr"             = FALSE,
                          "Opt_future_proj_qtr"        = FALSE,
                          "Opt_area_ID_selected"       = TRUE,
                          "Opt_calendar_survey_match"  = TRUE,
                          "Opt_recent_survey_only"     = FALSE,
                          "Opt_ART_coverage"           = TRUE,
                          "Opt_ANC_data"               = TRUE,
                          "Opt_ART_data"               = TRUE,
                          "Opt_ART_attendance_yes"     = FALSE,
                          "Model_fit"                  = TRUE,
                          "Cal_Population"             = FALSE,
                          "Cal_PLHIV"                  = FALSE,
                          "Cal_ART"                    = FALSE,
                          "Cal_KOS"                    = FALSE,
                          "Cal_new_infections"         = FALSE,
                          "Cal_method"                 = FALSE)
})

test_that("meta_indicator table contains same indicators as outputs", {
  expect_setequal(a_output_full$meta_indicator$indicator,
                  a_output_full$indicators$indicator)
})

test_that("one input and output for each area_id/age/sex/indicator/period combination", {

  inputs_outputs <- a_output_calib$inputs_outputs

  # For all indicators excluding prevalence (where there are multiple surveys),
  # test for a single input/output comparison per area_id/age/sex/indicator/period combination

  dups <- inputs_outputs %>%
    dplyr::group_by(area_id, sex, age_group, calendar_quarter, indicator) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(indicator != "prevalence", n != 2)

  expect_equal(nrow(dups), 0)
})


test_that("writing output package translates labels", {
  reset <- naomi_set_language("fr")
  on.exit(reset())
  out <- hintr_prepare_spectrum_download(a_hintr_output_calibrated)
  read <- read_output_package(out$path)
  ## area_level_label comes from input data (not translated)
  expect_true("Prévalence du VIH" %in% read$indicators$indicator_label)
  expect_setequal(read$indicators$quarter_label,
                  c("Septembre 2020", "Décembre 2023", "Septembre 2024", "Septembre 2025", "Septembre 2026"))
  ## age group label currently doesn't have translations
  expect_true("all ages" %in% read$indicators$age_group_label)

  expect_setequal(read$art_attendance$quarter_label,
                  c("Septembre 2020", "Décembre 2023"))
  expect_true("all ages" %in% read$art_attendance$age_group_label)
})

test_that("output file README generated in output zip", {

  out <- hintr_prepare_spectrum_download(a_hintr_output_calibrated)

  # README saved to output zip
  tmpd <- tempdir()
  unzip(out$path, exdir = tmpd)
  expect_true("README.md" %in% list.files(tmpd))

  # READE contains text
  t <- file.path(tmpd, "README.md")
  expect_true(file.size(t) > 1500)
  content <- brio::readLines(t)
  expect_true(any(grepl("art_attendance.csv", content)))
  expect_true(any(grepl("The following files have been generated as part of a Naomi model fit:" , content)))
})

test_that("can generate comparison report from a qs file", {
  t <- tempfile(fileext = ".html")
  generate_comparison_report(t, a_hintr_output_calibrated$model_output_path,
                             quiet = TRUE)
  expect_true(file.size(t) > 2000)
  content <- brio::readLines(t)
  expect_true(any(grepl("DEMO2020PHIA", content)))
  expect_true(any(grepl("Naomi estimate CY2020Q3", content)))
  expect_true(any(grepl("class=\"logo-naomi\"", content)))
})

test_that("can generate summary report from zip file", {
  zip <- hintr_prepare_spectrum_download(a_hintr_output_calibrated)
  t <- tempfile(fileext = ".html")
  generate_comparison_report(t, zip$path, quiet = TRUE)
  expect_true(file.size(t) > 2000)
  content <- brio::readLines(t)
  expect_true(any(grepl("DEMO2020PHIA", content)))
  expect_true(any(grepl("Naomi estimate CY2020Q3", content)))
  expect_true(any(grepl("class=\"logo-naomi\"", content)))
})

test_that("can generate comparison report with only 1 survey chosen", {
  ## Create a model output with only 1 option chosen for survey_prevalence
  model_output <- a_hintr_output_calibrated$model_output_path
  output <- read_hintr_output(model_output)
  options <- yaml::read_yaml(text = output$info$options.yml)
  options$survey_prevalence <- options$survey_prevalence[1]
  output$info$options.yml <- yaml::as.yaml(options)
  out <- tempfile(fileext = ".qs")
  model_output <- qs::qsave(output, preset = "fast", out)

  t <- tempfile(fileext = ".html")
  generate_comparison_report(t, out, quiet = TRUE)
  expect_true(file.size(t) > 2000)
  content <- brio::readLines(t)
  expect_true(any(grepl("DEMO2020PHIA", content)))
  expect_true(any(grepl("Naomi estimate CY2020Q3", content)))
  expect_true(any(grepl("class=\"logo-naomi\"", content)))
})

test_that("can generate comparison report without survey ART coverage", {
  ## Create a model output with no option chosen for survey_art_coverage
  output <- read_hintr_output(a_hintr_output_calibrated$model_output_path)
  options <- yaml::read_yaml(text = output$info$options.yml)
  options$survey_art_coverage <- NULL
  output$info$options.yml <- yaml::as.yaml(options)
  out <- tempfile(fileext = ".qs")
  model_output <- qs::qsave(output, preset = "fast", out)

  t <- tempfile(fileext = ".html")
  generate_comparison_report(t, out, quiet = TRUE)
  expect_true(file.size(t) > 2000)
  html <- rvest::read_html(t, encoding = "UTF-8")
  expect_length(rvest::html_element(html, ".prevalence-barchart"), 2)
  expect_length(rvest::html_element(html, ".prevalence-scatter1"), 2)
  expect_length(rvest::html_element(html, ".prevalence-scatter1B"), 0) ## Only 1 survey now; this plot is not shown
  expect_length(rvest::html_element(html, ".prevalence-plotly"), 2)
  expect_length(rvest::html_element(html, ".art-barchart"), 0)
  expect_length(rvest::html_element(html, ".art-scatter"), 0)
  expect_length(rvest::html_element(html, ".art-plotly"), 0)
})

test_that("prevalence survey plots not drawn when using aggregate survey", {
  ## This is to address Guinea-Bissau 2022/2023 issue #36
  ## Create a model output with only 1 option chosen for survey_prevalence
  output <- read_hintr_output(a_hintr_output_calibrated$model_output_path)
  options <- yaml::read_yaml(text = output$info$options.yml)
  options$use_survey_aggregate <- "true"
  output$info$options.yml <- yaml::as.yaml(options)
  out <- tempfile(fileext = ".qs")
  model_output <- qs::qsave(output, preset = "fast", out)

  t <- tempfile(fileext = ".html")
  generate_comparison_report(t, out, quiet = TRUE)
  expect_true(file.size(t) > 2000)
  html <- rvest::read_html(t, encoding = "UTF-8")
  expect_length(rvest::html_element(html, ".prevalence-barchart"), 0)
  expect_length(rvest::html_element(html, ".prevalence-scatter1"), 0)
  expect_length(rvest::html_element(html, ".prevalence-scatter1B"), 0)
  expect_length(rvest::html_element(html, ".prevalence-plotly"), 0)
  expect_length(rvest::html_element(html, ".art-barchart"), 2)
  expect_length(rvest::html_element(html, ".art-scatter"), 2)
  expect_length(rvest::html_element(html, ".art-plotly"), 2)
})

test_that("prevalence survey plots not drawn when using aggregate survey", {
  ## This is to address Cameroon 2022/2023 issue #41
  ## Create a model output with different ART time to prevalence
  output <- read_hintr_output(a_hintr_output_calibrated$model_output_path)
  output$output_package$inputs_outputs <-
    output$output_package$inputs_outputs %>%
    dplyr::mutate(calendar_quarter = dplyr::case_when(
      indicator == "art_coverage" & calendar_quarter == "CY2020Q3" ~ "CY2019Q3",
      TRUE ~ calendar_quarter))

  out <- tempfile(fileext = ".qs")
  model_output <- qs::qsave(output, preset = "fast", out)

  t <- tempfile(fileext = ".html")
  generate_comparison_report(t, out, quiet = TRUE)
  expect_true(file.size(t) > 2000)

  html <- rvest::read_html(t, encoding = "UTF-8")
  expect_length(rvest::html_element(html, ".prevalence-barchart"), 2)
  expect_length(rvest::html_element(html, ".prevalence-scatter1"), 2)
  expect_length(rvest::html_element(html, ".prevalence-scatter1B"), 0)
  expect_length(rvest::html_element(html, ".prevalence-plotly"), 2)
  expect_length(rvest::html_element(html, ".art-barchart"), 0)
  expect_length(rvest::html_element(html, ".art-scatter"), 0)
  expect_length(rvest::html_element(html, ".art-plotly"), 0)
})

test_that("can generate comparison report with ANC data at T1 not macthed to model T1", {
  ## Create a model output with only 1 option chosen for survey_prevalence
  model_output <- a_hintr_output_calibrated$model_output_path
  output <- read_hintr_output(model_output)
  options <- yaml::read_yaml(text = output$info$options.yml)
  options$anc_prevalence_year1 <- "2017"
  options$anc_art_coverage_year1 <- "2017"
  output$info$options.yml <- yaml::as.yaml(options)
  out <- tempfile(fileext = ".qs")
  model_output <- qs::qsave(output, preset = "fast", out)

  t <- tempfile(fileext = ".html")
  generate_comparison_report(t, out, quiet = TRUE)
  expect_true(file.size(t) > 2000)
})
