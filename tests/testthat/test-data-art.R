test_that("Apply Spectrum adjustment when no subnational ART adjustment specified", {
  x <- create_mock_art_data()
  art_adjust <- apply_art_adjustment(x$art, x$shape, x$spec_comparison)

  expect_equal(art_adjust$art_current_adjusted, rep(c(300,300,300,
                                                      750,660,645,
                                                      500,440,430),
                                                    times = 32))

  expect_equal(art_adjust$art_adjustment_factor, rep(c(1, 1, 1,
                                                       1, 0.88, 0.86,
                                                       1, 0.88, 0.86),
                                                     times = 32))
  subnational_summed <- art_adjust |>
    dplyr::group_by(age_group, sex, calendar_quarter) |>
    dplyr::summarise(naomi_art_adjusted = sum(art_current_adjusted), .groups = "drop")

  expect_equal(subnational_summed$naomi_art_adjusted, x$spec_comparison$value_spectrum_adjusted)

})

test_that("Scale subnational ART adjustments to match national Spectrum adjustments", {

  # National + subnational adjustments do not match + subnational adjustments
  # have been entered for *all* districts:
  #  -> scale all subnational adjustments to match national

  x <- create_mock_art_data()
  art <- x$art
  art$art_current_adjusted <- rep(c(500,440,430,
                                    750,660,645,
                                    300,300,300),times = 32)

  art$art_adjustment_factor <- rep(c(1, 0.9, 0.9,
                                     1, 0.9, 0.9,
                                     1, 1, 1), times = 32)

  art_adjust <- handle_naomi_warnings(apply_art_adjustment(art, x$shape, x$spec_comparison))

  expect_equal(art_adjust$art_current_adjusted, rep(c(300,300,300,
                                                      750,660,645,
                                                      500,440,430),
                                                    times = 32))

  expect_equal(art_adjust$art_adjustment_factor, rep(c(1, 1, 1,
                                                       1, 0.88, 0.86,
                                                       1, 0.88, 0.86),
                                                     times = 32))

  subnational_summed <- art_adjust |>
    dplyr::group_by(age_group, sex, calendar_quarter) |>
    dplyr::summarise(naomi_art_adjusted = sum(art_current_adjusted), .groups = "drop")

  expect_equal(subnational_summed$naomi_art_adjusted, x$spec_comparison$value_spectrum_adjusted)

  expect_equal(art_adjust$warnings[[1]]$text,
               paste("Subnational adjustment factors have been recalculated to",
                     "match reported vs. adjusted numbers on ART."))

  # National + subnational adjustments do not match + subnational adjustments
  # have been entered for *some* districts:
  # -> scale districts with no subnational adjustments so that sum of subnational
  #    adjustments matches national adjustments

  art$art_current_adjusted <- NA_real_

  art$art_adjustment_factor <- rep(c(1, 0.82, 0.80,
                                     1, 0.82, 0.80,
                                     1, 1, 1), times = 32)

  ids <- unique(art$area_id)[1:20]
  art[art$area_id %in% ids, ]$art_adjustment_factor <- NA_real_

  art_adjust <- handle_naomi_warnings(apply_art_adjustment(art, x$shape, x$spec_comparison))

  # Check subnational adjustments that have been entered have been retained
  expect_equal(round(art_adjust[!(art_adjust$area_id %in% ids),]$art_current_adjusted),
               rep(c(300, 300, 300,
                     750,615,600,
                     500,410,400), times = 12))

  expect_equal(round((art_adjust[!(art_adjust$area_id %in% ids),]$art_adjustment_factor), 2),
               rep(c(1, 1, 1,
                     1, 0.82, 0.80,
                     1, 0.82, 0.80), times = 12))

  # Check districts with no subnational adjustments have been adjusted to match
  # national adjusted number in Spectrum
  expect_equal(round(art_adjust[art_adjust$area_id %in% ids,]$art_current_adjusted),
               rep(c(300, 300, 300,
                     750,687,672,
                     500,458,448), times = 20))

  expect_equal(round(art_adjust[art_adjust$area_id %in% ids,]$art_adjustment_factor, 2),
               rep(c(1, 1, 1,
                     1, 0.92, 0.90,
                     1, 0.92, 0.90), times = 20))


  subnational_summed <- art_adjust |>
    dplyr::group_by(age_group, sex, calendar_quarter) |>
    dplyr::summarise(naomi_art_adjusted = sum(art_current_adjusted), .groups = "drop")

  expect_equal(subnational_summed$naomi_art_adjusted, x$spec_comparison$value_spectrum_adjusted)

  expect_equal(art_adjust$warnings[[1]]$text,
               paste("Districts with no adjustments factors have been adjusted",
               "to match the national adjusted number on ART in Spectrum."))

})

test_that("Subnational ratios recalculated if they do not match reported vs. adjusted
          number on ART entered into Naomi ART data", {

  # Check that warning thrown when subnational adjusted subnational counts do
  # match subnational proportions

  x <- create_mock_art_data()
  art <- x$art
  art$art_adjustment_factor <- rep(c(1, 0.9, 0.9,
                                     1, 0.9, 0.9,
                                     1, 1, 1), times = 32)

  art_adjust <- handle_naomi_warnings(apply_art_adjustment(art, x$shape, x$spec_comparison))

  expect_equal(art_adjust$art_current_adjusted, rep(c(300,300,300,
                                                      750,660,645,
                                                      500,440,430),
                                                    times = 32))



})


