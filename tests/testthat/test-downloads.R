test_that("spectrum download can be created", {
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  notes <- "these are my\nmultiline notes"
  with_mock(new_simple_progress = mock_new_simple_progress, {
    messages <- naomi_evaluate_promise(
      out <- hintr_prepare_spectrum_download(a_hintr_output_calibrated,
                                             notes = notes))
  })
  expect_true(file.exists(out$path))

  expect_type(out$metadata$description, "character")
  expect_length(out$metadata$description, 1)
  expect_equal(out$metadata$areas, "MWI")

  tmp <- tempfile()
  info <- naomi_info(format_data_input(a_hintr_data), a_hintr_options)
  info_names <- paste0("info/", names(info))
  unzip(out$path, exdir = tmp, files = info_names)
  expect_equal(dir(tmp), "info")
  expect_equal(dir(file.path(tmp, "info")), names(info))


  ## # UNAIDS Navigator Checklist checks
  navigator_checklist <- utils::read.csv(unz(out$path, "info/unaids_navigator_checklist.csv"))


  expect_equal(names(navigator_checklist),
               c("NaomiCheckPermPrimKey", "NaomiCheckDes", "TrueFalse"))

  checklist_primkeys <- c( "ART_is_Spectrum","ANC_is_Spectrum","Package_created",
                           "Package_has_all_data","Opt_recent_qtr","Opt_future_proj_qtr",
                           "Opt_area_ID_selected","Opt_calendar_survey_match","Opt_recent_survey_only",
                           "Opt_ART_coverage","Opt_ANC_data","Opt_ART_data",
                           "Opt_ART_attendance_yes","Model_fit","Cal_Population",
                           "Cal_PLHIV","Cal_ART","Cal_KOS",
                           "Cal_new_infections","Cal_method" )
  expect_equal(navigator_checklist$NaomiCheckPermPrimKey, checklist_primkeys)
  expect_true(all(navigator_checklist$TrueFalse %in% c(TRUE, FALSE)))
  ## Check tradiure translation hooks worked
  expect_true("Calibration - method is logistic" %in% navigator_checklist$NaomiCheckDes)


  outputs <- read_output_package(out$path)
  expect_true(
    all(c("area_level", "area_level_label", "area_id", "area_name", "parent_area_id",
          "spectrum_region_code", "area_sort_order", "geometry") %in%
          names(outputs$meta_area))
  )

  tmpf <- tempfile()
  unzip(out$path, "boundaries.geojson", exdir = tmpf)
  output_boundaries <- sf::read_sf(file.path(tmpf, "boundaries.geojson"))

  ## Column 'name' added in boundaries.geojson during save_output() for Spectrum
  expect_true(
    all(c("area_level", "area_level_label", "area_id", "area_name", "parent_area_id",
          "spectrum_region_code", "area_sort_order", "name", "geometry") %in%
          names(output_boundaries))
  )

  ## Progress messages printed
  expect_length(messages$progress, 1)
  expect_equal(messages$progress[[1]]$message,
               "Generating output zip download")

  ## Notes are saved
  t <- tempfile()
  unzip(out$path, "notes.txt", exdir = t)
  saved_notes <- readLines(file.path(t, "notes.txt"))
  expect_equal(saved_notes, c("these are my", "multiline notes"))
})

test_that("spectrum download can include vmmc data", {
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  notes <- "these are my\nmultiline notes"
  vmmc_file <- list(path = file.path("testdata", "vmmc.xlsx"),
                    hash = "123",
                    filename = "vmmc.xlsx")
  testthat::with_mocked_bindings(
    messages <- naomi_evaluate_promise(
      out <- hintr_prepare_spectrum_download(a_hintr_output_calibrated,
                                             notes = notes,
                                             vmmc_file = vmmc_file)
    ),
    new_simple_progress = mock_new_simple_progress
  )
  expect_true(file.exists(out$path))

  t <- tempfile()
  unzip(out$path, PEPFAR_DATAPACK_FILENAME, exdir = t)
  datapack <- utils::read.csv(file.path(t, PEPFAR_DATAPACK_FILENAME))

  expect_true("psnu_uid" %in% colnames(datapack))
  expect_true(!any(is.na(datapack)))
  expect_true(all(c("VMMC_CIRC_SUBNAT.T_1", "VMMC_TOTALCIRC_SUBNAT.T_1") %in%
                    datapack$indicator_code))

  unzip(out$path, "notes.txt", exdir = t)
  saved_notes <- readLines(file.path(t, "notes.txt"))
  expect_equal(saved_notes, c("these are my", "multiline notes"))
})

test_that("coarse age group download can be created", {
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  with_mock(new_simple_progress = mock_new_simple_progress, {
    messages <- naomi_evaluate_promise(
      out <- hintr_prepare_coarse_age_group_download(a_hintr_output_calibrated))
  })
  expect_true(file.exists(out$path))

  expect_null(out$metadata$description)
  expect_equal(out$metadata$areas, "MWI")

  info <- naomi_info(format_data_input(a_hintr_data), a_hintr_options)
  info_names <- paste0("info/", names(info))
  file_list <- unzip(out$path, list = TRUE)
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "info/", info_names, "info/unaids_navigator_checklist.csv",
      "fit/", "fit/spectrum_calibration.csv",
      "fit/model_options.yml",
      "fit/data_options.yml",
      "fit/calibration_options.yml", "inputs_outputs.csv", "README.md")
  )

  ## Check coarse age outputs saved in coarse_output_path
  coarse_ages <- c("Y015_049", "Y015_064", "Y015_999", "Y050_999", "Y000_999",
                   "Y000_064", "Y000_014", "Y015_024", "Y025_034", "Y035_049",
                   "Y050_064", "Y065_999")

  coarse_age_outputs <- read_output_package(out$path)
  expect_setequal(coarse_age_outputs$meta_age_group$age_group, coarse_ages)
  expect_setequal(coarse_age_outputs$indicators$age_group, coarse_ages)

  ## Progress messages printed
  expect_length(messages$progress, 1)
  expect_equal(messages$progress[[1]]$message,
               "Generating coarse-output download")
})

test_that("summary report download can be created", {
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  with_mock(new_simple_progress = mock_new_simple_progress, {
    messages <- naomi_evaluate_promise(
      out <- hintr_prepare_summary_report_download(a_hintr_output_calibrated))
  })
  expect_true(file.exists(out$path))

  expect_type(out$metadata$description, "character")
  expect_length(out$metadata$description, 1)
  expect_equal(out$metadata$areas, "MWI")

  expect_true(file.size(out$path) > 2000)
  expect_true(any(grepl("DEMO2016PHIA, DEMO2015DHS",
                        brio::readLines(out$path))))
  expect_true(any(grepl(basename(a_hintr_data$pjnz),
                        brio::readLines(out$path))))
  expect_true(any(grepl("Central", brio::readLines(out$path))))

  ## Progress messages printed
  expect_length(messages$progress, 1)
  expect_equal(messages$progress[[1]]$message,
               "Generating summary report")
})


test_that("comparison report download can be created", {
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  with_mock(new_simple_progress = mock_new_simple_progress, {
    messages <- naomi_evaluate_promise(
      out <- hintr_prepare_comparison_report_download(
        a_hintr_output_calibrated))
  })
  expect_true(file.exists(out$path))

  expect_type(out$metadata$description, "character")
  expect_length(out$metadata$description, 1)
  expect_equal(out$metadata$areas, "MWI")

  expect_true(file.size(out$path) > 2000)
  content <- brio::readLines(out$path)
  expect_true(any(grepl("DEMO2016PHIA, DEMO2015DHS", content)))
  expect_true(any(grepl("Naomi estimate CY2016Q1", content)))
  expect_true(any(grepl("class=\"logo-naomi\"", content)))
  expect_true(any(grepl("Central", content)))

  ## Progress messages printed
  expect_length(messages$progress, 1)
  expect_equal(messages$progress[[1]]$message,
               "Generating comparison report")
})


test_that("AGYW download can be created", {

  agyw_output_demo <- make_agyw_testfiles(a_hintr_output_calibrated)

  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())

  with_mocked_bindings(
    messages <- naomi_evaluate_promise(
      out <- hintr_prepare_agyw_download(agyw_output_demo,
                                         a_hintr_data$pjnz)),
    new_simple_progress = mock_new_simple_progress)

  expect_true(file.exists(out$path))

  expect_type(out$metadata$description, "character")
  expect_length(out$metadata$description, 1)
  expect_equal(out$metadata$areas, "MWI")

  outputs_female <- openxlsx::readWorkbook(out$path, sheet = "All outputs - F")
  expect_true(nrow(outputs_female) > 10)
  outputs_male <- openxlsx::readWorkbook(out$path, sheet = "All outputs - M")
  expect_true(nrow(outputs_male) > 10)
  naomi_outputs <- openxlsx::readWorkbook(out$path, sheet = "NAOMI outputs")
  expect_true(nrow(naomi_outputs) > 4)

  ## Progress messages printed
  expect_length(messages$progress, 1)
  expect_equal(messages$progress[[1]]$message, "Generating AGYW tool")

  # Test agyw workbook with no kp workbook saved into spectrum
  risk_prop <- agyw_generate_risk_populations(agyw_output_demo$model_output_path,
                                              a_hintr_data$pjnz)

  expect_equal(risk_prop$meta_consensus,
               data.frame(kp = c("FSW", "MSM", "PWID"),
                          consensus_estimate = NA))

  # Test agyw workbook with mock workbook saved into spectrum
  kp_consensus <- readRDS(file.path("testdata/kp_workbook_spectrum.rds"))
  mock_extract_kp_workbook <- mockery::mock(kp_consensus)
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())

  with_mocked_bindings(
    risk_prop_scaled <- agyw_generate_risk_populations(
      agyw_output_demo$model_output_path, a_hintr_data$pjnz),
    new_simple_progress = mock_new_simple_progress,
    extract_kp_workbook = mock_extract_kp_workbook
  )

  expect_equal(risk_prop_scaled$meta_consensus,
               data.frame(kp = c("FSW", "MSM", "PWID"),
                          consensus_estimate = c(40000, 35500, 5000)))

  # Test for warning when KP workbook has PSE consensus estimates that
  # are >5% of the age matched population
  kp_consensus_bad <- readRDS(file.path("testdata/kp_workbook_spectrum_bad.rds"))
  mock_extract_kp_workbook <- mockery::mock(kp_consensus_bad)
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())


  expect_equal(risk_prop_scaled$meta_consensus,
               data.frame(kp = c("FSW", "MSM", "PWID"),
                          consensus_estimate = c(260000, 260000, 260000)))

})

test_that("Error thrown when AGYW resources are out of date", {

  kp_error <- paste0("Available KP PSE estimates for: \n",
                     "MWI_1_1; MWI_1_2; MWI_1_3",
                     "\n\n Do not match Naomi estimates for: \n",
                     "MWI_2_1_demo; MWI_2_2_demo; MWI_2_3_demo; MWI_2_4_demo; MWI_2_5_demo",
                     "\n\nTo update estimates, please contact Naomi support.")

 expect_error(hintr_prepare_agyw_download(a_hintr_output_calibrated,
                                          a_hintr_data$pjnz), kp_error)

})


test_that("output description is translated", {
  text <- build_output_description(a_hintr_options)
  expect_match(
    text,
    "Naomi output uploaded from Naomi web app\\n\\nArea scope - MWI\\n.+")

  reset <- naomi_set_language("fr")
  on.exit(reset())
  text <- build_output_description(a_hintr_options)
  expect_match(text, paste0("Paquet Naomi téléchargée depuis l'application ",
                            "web Naomi\\n\\nPérimètre de zone - MWI\\n.+"))
})
