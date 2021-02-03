test_that("datapack_psnu_area_id_map is well formed", {

  psnu_map <- naomi_read_csv(system_file("datapack/datapack_psnu_area_id_map.csv"))

  expect_true(all(c("iso3", "area_id", "map_id", "map_name", "map_source") %in%
                  names(psnu_map)))
  expect_equal(anyDuplicated(psnu_map$area_id), 0)

  ## Remove _demo area_id. map_id included twice for these cases
  expect_equal(anyDuplicated(psnu_map$map_id[!grepl("_demo$", psnu_map$area_id)]), 0)

  expect_true(all(psnu_map$map_source == "Datim"))
  expect_true(all(!is.na(psnu_map$map_name)))
  expect_true(all(grepl("^[A-Z]{3}$", psnu_map$iso3)))
})

test_that("datapack_psnu_area_level is well formed", {

  psnu_level <- read_datapack_psnu()

  expect_equal(names(psnu_level), c("iso3", "psnu_area_level"))
  expect_equal(anyDuplicated(psnu_level$iso3), 0)
  expect_true(all(!is.na(psnu_level$psnu_area_level)))
  expect_is(psnu_level$psnu_area_level, "integer")
})

test_that("datapack_indicator_map is well formed", {

  datapack_indicator_map <- naomi_read_csv(system_file("datapack", "datapack_indicator_mapping.csv"))
  expect_true(all(c("indicator", "datapack_indicator_code", "is_integer") %in%
                  names(datapack_indicator_map)))
  expect_true(all(datapack_indicator_map$indicator %in% get_meta_indicator()$indicator))
  expect_equal(anyDuplicated(datapack_indicator_map$indicator), 0)
  expect_equal(anyDuplicated(datapack_indicator_map$datapack_indicator_code), 0)
})

test_that("datapack_age_group_map is well formed", {

  datapack_age_group_map <- naomi_read_csv(system_file("datapack", "datapack_age_group_mapping.csv"))
  expect_true(all(c("age_group", "datapack_age_group_label", "datapack_age_group_id") %in%
                  names(datapack_age_group_map)))
  expect_true(all(datapack_age_group_map$age_group %in% get_age_groups()$age_group))
  expect_equal(anyDuplicated(datapack_age_group_map$age_group), 0)
  expect_equal(anyDuplicated(datapack_age_group_map$datapack_age_group_label), 0)
})


test_that("datapack export writes correct psnu_level", {

  tmpf <- tempfile(fileext = ".csv")
  tmpf3 <- tempfile(fileext = ".csv")
  res <- write_datapack_csv(a_output_full, tmpf)
  res3 <- write_datapack_csv(a_output_full, tmpf3, psnu_level = 3)
  expect_equal(tmpf, res)

  datapack <- readr_read_csv(res)
  datapack3 <- readr_read_csv(res3)

  expect_true(!any(is.na(datapack)))
  expect_equal(datapack, datapack3)
})
