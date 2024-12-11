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

  checklist_primkeys <- c( "Package_created", "Package_has_all_data","Opt_recent_qtr","Opt_future_proj_qtr",
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
  expect_true(any(grepl("DEMO2020PHIA",
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
  expect_true(any(grepl("DEMO2020PHIA", content)))
  expect_true(any(grepl("Naomi estimate CY2020Q3", content)))
  expect_true(any(grepl("class=\"logo-naomi\"", content)))
  expect_true(any(grepl("Central", content)))

  ## Progress messages printed
  expect_length(messages$progress, 1)
  expect_equal(messages$progress[[1]]$message,
               "Generating comparison report")
})

test_that("AGYW download can be created", {
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  with_mock(new_simple_progress = mock_new_simple_progress, {
    messages <- naomi_evaluate_promise(
      out <- hintr_prepare_agyw_download(a_hintr_output_calibrated,
                                         a_hintr_data$pjnz))
  })
  expect_true(file.exists(out$path))

  expect_type(out$metadata$description, "character")
  expect_length(out$metadata$description, 1)
  expect_equal(out$metadata$areas, "MWI")

  read <- readxl::read_xlsx(out$path)
  expect_equal(read,
               data.frame(x = c(1, 2, 3), y = c(3, 4, 5)),
               ignore_attr = TRUE)

  ## Progress messages printed
  expect_length(messages$progress, 1)
  expect_equal(messages$progress[[1]]$message, "Generating AGYW tool")
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

test_that("datapack download can be created", {
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  with_mock(new_simple_progress = mock_new_simple_progress, {
    messages <- naomi_evaluate_promise(
      out <- hintr_prepare_datapack_download(a_hintr_output_calibrated))
  })
  expect_true(file.exists(out$path))

  expect_type(out$metadata$description, "character")
  expect_length(out$metadata$description, 1)
  expect_equal(out$metadata$areas, "MWI")

  datapack <- readxl::read_xlsx(out$path, "data")

  expect_true("psnu_uid" %in% colnames(datapack))
  expect_true(!any(is.na(datapack)))
  ## Simple smoke test that we have some indicator code
  expect_true("HIV_PREV.T_1" %in% datapack$indicator_code)

  metadata <- readxl::read_xlsx(out$path, "metadata")

  expect_true(nrow(metadata) > 0)
  expect_equal(as.character(metadata[1, 1]), "Naomi Version")
})

test_that("datapack download can include vmmc data", {
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  vmmc_file <- list(path = file.path("testdata", "vmmc.xlsx"),
                    hash = "123",
                    filename = "vmmc.xlsx")
  testthat::with_mocked_bindings(
    messages <- naomi_evaluate_promise(
      out <- hintr_prepare_datapack_download(a_hintr_output_calibrated,
                                             vmmc_file = vmmc_file)
    ),
    new_simple_progress = mock_new_simple_progress
  )
  expect_true(file.exists(out$path))

  datapack <- readxl::read_xlsx(out$path, "data")

  expect_true("psnu_uid" %in% colnames(datapack))
  expect_true(!any(is.na(datapack)))
  expect_true(all(c("VMMC_CIRC_SUBNAT.T_1", "VMMC_TOTALCIRC_SUBNAT.T_1") %in%
                    datapack$indicator_code))

  metadata <- readxl::read_xlsx(out$path, "metadata")

  expect_true(nrow(metadata) > 0)
  expect_equal(as.character(metadata[1, 1]), "Naomi Version")
})
