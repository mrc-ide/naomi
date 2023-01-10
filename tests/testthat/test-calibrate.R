test_that("calibrated model matches end-year Spectrum ART", {

  out <- read_hintr_output(a_hintr_output_calibrated$model_output_path)
  ind <- out$output_package$indicators

  tmpf <- tempfile()
  unzip(a_hintr_data$pjnz, exdir = tmpf)
  art_dec31 <- lapply(list.files(tmpf, full.names = TRUE), function(pjnz) {
    dp <- read_dp(pjnz)
    read_dp_art_dec31(dp)
  })

  art_dec31_2018 <- lapply(art_dec31, dplyr::filter, year == 2018) %>%
    dplyr::bind_rows() %>%
    dplyr::count(sex, age_group, wt = art_dec31, name = "spectrum_art_dec31")

  check <- ind %>%
    dplyr::filter(area_id == "MWI",
                  calendar_quarter == "CY2018Q4",
                  indicator == "art_current") %>%
    dplyr::inner_join(art_dec31_2018, by = c("sex", "age_group"))

  expect_equal(check$mean, check$spectrum_art_dec31)
})
