context("downloads")

test_that("spectrum download can be created", {
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  with_mock("naomi:::new_simple_progress" = mock_new_simple_progress, {
    messages <- naomi_evaluate_promise(
      out <- hintr_prepare_spectrum_download(a_hintr_output_calibrated))
  })
  expect_true(file.exists(out$path))

  expect_type(out$metadata$description, "character")
  expect_length(out$metadata$description, 1)
  expect_equal(out$metadata$areas, "MWI_1_2_demo")

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
})

test_that("coarse age group download can be created", {
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  with_mock("naomi:::new_simple_progress" = mock_new_simple_progress, {
    messages <- naomi_evaluate_promise(
      out <- hintr_prepare_coarse_age_group_download(a_hintr_output_calibrated))
  })
  expect_true(file.exists(out$path))

  expect_null(out$metadata$description)
  expect_equal(out$metadata$areas, "MWI_1_2_demo")

  info <- naomi_info(format_data_input(a_hintr_data), a_hintr_options)
  info_names <- paste0("info/", names(info))
  file_list <- unzip(out$path, list = TRUE)
  expect_setequal(
    file_list$Name,
    c("boundaries.geojson", "indicators.csv", "art_attendance.csv",
      "meta_age_group.csv", "meta_area.csv", "meta_indicator.csv", "meta_period.csv",
      "pepfar_datapack_indicators_2021.csv",
      "info/", info_names,
      "fit/", "fit/spectrum_calibration.csv", "fit/calibration_options.csv")
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
  with_mock("naomi:::new_simple_progress" = mock_new_simple_progress, {
    messages <- naomi_evaluate_promise(
      out <- hintr_prepare_summary_report_download(a_hintr_output_calibrated))
  })
  expect_true(file.exists(out$path))

  expect_type(out$metadata$description, "character")
  expect_length(out$metadata$description, 1)
  expect_equal(out$metadata$areas, "MWI_1_2_demo")

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
