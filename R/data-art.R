##' Adjusted reported number on ART based on national/regional adjustment factor in
##' Spectrum and/or subnational adjustment factor specified ART data
##'
##' @param art Subnational ART data read by `read_art_number()`
##' @param shape Shapefile read by `read_area_merged()`
##' @param art_spectrum_comparison Created by `prepare_art_spectrum_comparison()`
##' @keywords internal
##'
##'
##'
##'


apply_art_adjustment <- function(art, shape, art_spectrum_comparison) {


  # Format subnational ART and Spectrum for comparison
  if(!("art_current_adjusted" %in% names(art))) {art$art_current_adjusted <- NA}
  if(!("art_adjustment_factor" %in% names(art))) {art$art_adjustment_factor <- NA}

  art_number <- art |>
    dplyr::mutate(year = calendar_quarter_to_year(calendar_quarter)) |>
    dplyr::left_join(shape |> sf::st_drop_geometry() |>
                       dplyr::select(area_id, spectrum_region_code),
                     by = dplyr::join_by(area_id))

  spec_adjustment <- art_spectrum_comparison |>
    dplyr::mutate(
      sex = dplyr::case_when(
        group == "art_children" ~ "both",
        group == "art_adult_female" ~ "female",
        group == "art_adult_male" ~ "male",
        group == "art_adult_both" ~ "both",
        TRUE ~ NA_character_),
      spec_adjustment_ratio = round(spec_adjustment_ratio, digits = 2),
      age_group = dplyr::if_else(group == "art_children", "Y000_014", "Y015_999")) |>
    dplyr::select(spectrum_region_code, year, sex, age_group, value_spectrum_adjusted,
                  spec_adjustment_ratio)

  # 1. If no subnational adjustments -> apply national adjustments in Spectrum
  if(all(is.na(art_number$art_current_adjusted) & is.na(art_number$art_adjustment_factor))) {

    art_adjusted <- dplyr::left_join(art_number, spec_adjustment,
                                     by = dplyr::join_by(age_group, sex, year,
                                                         spectrum_region_code)) |>
      dplyr::mutate(
        art_adjustment_factor = spec_adjustment_ratio,
        art_current_adjusted = art_current * spec_adjustment_ratio
      )

  } else {

    # Users may enter adjusted subnational number on ART and/or
    # ART adjustment ratio for some or all districts.

    # Calculate adjustment ratio where only adjusted on ART is entered
    # and vice-versa
    art_number <- art_number |>
      dplyr::mutate(
        art_current_adjusted = dplyr::if_else(is.na(art_current_adjusted),
                                              art_current * art_adjustment_factor,
                                              art_current_adjusted),
        art_adjustment_factor = dplyr::if_else(is.na(art_adjustment_factor),
                                               art_current_adjusted/ art_current,
                                               art_adjustment_factor))

    # # Check 1: If adjustment ratio entered not equal to reported vs.
    # adjusted on ART, throw warning and recalculate ratio from numbers on ART

    check <- dplyr::filter(art_number,
                             !is.na(art_current_adjusted) &
                             !is.na(art_adjustment_factor)) |>
    dplyr::mutate(ratio = art_current_adjusted / art_current,
                  dplyr::across(where(is.numeric), ~ signif(.x, 2)))

    x <- check[check$art_adjustment_factor != check$ratio, ]

    if(nrow(x) > 0) {
      naomi_warning(t_("SUBNAT_ADJ_RECALCULATE_RATIO"), "review_inputs")
      art_number$art_adjustment_factor <- art_number$art_current_adjusted / art_number$art_current
    }

    # #  Check 2: Compare national and subnational adjustments
    # # If national + subnational adjustments match -> do nothing
    # # If national + subnational adjustments do not match + subnational adjustments
    #   have been entered for *all* districts:
    #     -> scale all subnational adjustments to match national
    # # If national + subnational adjustments do not match + subnational adjustments
    #   have been entered for *some* districts:
    #     -> scale districts with no subnational adjustments so that sum of subnational
    #        adjustments matches national adjustments

    art_adj_national <- art_number |>
      dplyr::group_by(age_group, sex, year, spectrum_region_code) |>
      dplyr::summarise(
        art_current = sum(art_current, na.rm = TRUE),
        art_current_adjusted = sum(art_current_adjusted, na.rm = TRUE), .groups = "drop") |>
      dplyr::left_join(spec_adjustment,
                       by = dplyr::join_by(age_group, sex, year,
                                           spectrum_region_code)) |>
      dplyr::mutate(ratio_naomi_raw_spec_adj = value_spectrum_adjusted/ art_current )

    x <- art_adj_national[art_adj_national$art_current_adjusted != art_adj_national$value_spectrum_adjusted, ]

    if(nrow(x) == 0) {
      art_adjusted <- art_number
    } else {

      art_adjusted <- art_number |>
        dplyr::left_join(art_adj_national |>
                           dplyr::select(-c("art_current", "art_current_adjusted")),
                         by = dplyr::join_by(age_group, sex, year, spectrum_region_code))

      # Scale to match Spectrum if subnational adjustments added for all districts
      if(all(!is.na(art_adjusted$art_adjustment_factor))) {

        naomi_warning(t_("SUBNAT_ADJ_SCALE_ALL_DISTRICTS_TO_SPECTRUM"), "review_inputs")

        art_adjusted <- art_adjusted |>
          dplyr::mutate(
            art_current_adjusted = art_current * ratio_naomi_raw_spec_adj,
            art_adjustment_factor = ratio_naomi_raw_spec_adj)

      } else {
      # Scale districts with no subnational adjustments so that sum of subnational
      # adjustments matches national adjustments

        naomi_warning(t_("SUBNAT_ADJ_SCALE_SOME_DISTRICTS_TO_SPECTRUM"), "review_inputs")

        x <- art_number |>
          dplyr::mutate(adjust = dplyr::if_else(is.na(art_current_adjusted), "to_adjust", "adjusted"),
                        art_adjusted = dplyr::if_else(is.na(art_current_adjusted),
                                                      art_current, art_current_adjusted)) |>
          dplyr::group_by(adjust, age_group, sex, year) |>
          dplyr::summarise(
            art_adjusted = sum(art_adjusted, na.rm = TRUE), .groups = "drop") |>
          tidyr::pivot_wider(names_from = adjust, values_from = art_adjusted) |>
          dplyr::mutate(to_adjust = tidyr::replace_na(to_adjust, 0)) |>
          dplyr::left_join(art_adj_national |>
                             dplyr::select(age_group, sex, year, value_spectrum_adjusted),
                           by = dplyr::join_by(age_group, sex, year)) |>
          dplyr::mutate(remainder = dplyr::if_else(to_adjust == 0, 0, value_spectrum_adjusted - adjusted),
                        remainder_adj_factor = dplyr::if_else(to_adjust == 0, 1, remainder/ to_adjust))


        art_adjusted <- art_number |>
          dplyr::left_join(x, by = dplyr::join_by(age_group, sex, year)) |>
          dplyr::mutate(art_adjustment_factor =
                          dplyr::if_else(is.na(art_adjustment_factor),
                                         remainder_adj_factor, art_adjustment_factor),
                        art_current_adjusted = art_current * art_adjustment_factor) |>
          dplyr::select(names(art_number))

      }

    }

  }

  art_adjusted |> dplyr::arrange(area_id, sex, calendar_quarter)

}







