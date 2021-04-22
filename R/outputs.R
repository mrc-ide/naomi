
#' Get indicator metadata
#'
#' @return data.frame of indicator ids, labels, descriptions, and parameter mapping.
#'
#'
#' @export
#'
#' @examples
#' get_meta_indicator()
get_meta_indicator <- function() {

  ## TODO: refactor this to be harmonised with inst/metadata/metadata.csv.
  val <- naomi_read_csv(system_file("metadata", "meta_indicator.csv"))
  val$indicator_label <- traduire::translator()$replace(val$indicator_label)
  val$description <- traduire::translator()$replace(val$description)

  val
}



add_stats <- function(df, mode = NULL, sample = NULL, prefix = ""){

  v <- df

  if(!is.null(mode)) {
    v[[paste0(prefix, "mode")]] <- mode
  } else {
    v[[paste0(prefix, "mode")]] <- NA_real_
  }

  if(!is.null(sample)) {
    qtl <- apply(sample, 1, stats::quantile, c(0.5, 0.025, 0.975), names = FALSE)
    v[[paste0(prefix, "mean")]] <- rowMeans(sample)
    v[[paste0(prefix, "se")]] <- sqrt(rowSums((sample - v[[paste0(prefix, "mean")]])^2) / (max(ncol(sample), 2) - 1))
    v[[paste0(prefix, "median")]] <- qtl[1,]
    v[[paste0(prefix, "lower")]] <- qtl[2,]
    v[[paste0(prefix, "upper")]] <- qtl[3,]
  } else {
    v[paste0(prefix, c("mean", "se", "median", "lower", "upper"))] <- NA_real_
  }

  v[c(names(df), paste0(prefix, c("mean", "se", "median", "mode", "lower", "upper")))]
}


extract_indicators <- function(naomi_fit, naomi_mf) {

  get_est <- function(varname,
                      indicator,
                      calendar_quarter,
                      mf = naomi_mf$mf_out) {
    v <- dplyr::mutate(
                  mf,
                  calendar_quarter = calendar_quarter,
                  indicator = indicator)

    tryCatch(
      if(!is.null(naomi_fit$sample)) {
        v <- add_stats(v, naomi_fit$mode[[varname]], naomi_fit$sample[[varname]])
      } else {
        v <- add_stats(v, naomi_fit$mode[[varname]])
      },
      "error" = function(e) {
        stop(t_("EXTRACT_INDICATORS_SIMULATE_ERROR",
                list(varname = varname)))
      })

    v
  }

  indicators_t1 <- c("population_t1_out" = "population",
                     "rho_t1_out" = "prevalence",
                     "plhiv_t1_out" = "plhiv",
                     "alpha_t1_out" = "art_coverage",
                     "artnum_t1_out" = "art_current_residents",
                     "artattend_t1_out" = "art_current",
                     "untreated_plhiv_num_t1_out" = "untreated_plhiv_num",
                     "lambda_t1_out" = "incidence",
                     "infections_t1_out" = "infections")

  indicators_t2 <- c("population_t2_out" = "population",
                     "rho_t2_out" = "prevalence",
                     "plhiv_t2_out" = "plhiv",
                     "alpha_t2_out" = "art_coverage",
                     "artnum_t2_out" = "art_current_residents",
                     "artattend_t2_out" = "art_current",
                     "untreated_plhiv_num_t2_out" = "untreated_plhiv_num",
                     "lambda_t2_out" = "incidence",
                     "infections_t2_out" = "infections")

  indicators_t3 <- c("population_t3_out" = "population",
                     "rho_t3_out" = "prevalence",
                     "plhiv_t3_out" = "plhiv",
                     "alpha_t3_out" = "art_coverage",
                     "artnum_t3_out" = "art_current_residents",
                     "artattend_t3_out" = "art_current",
                     "untreated_plhiv_num_t3_out" = "untreated_plhiv_num",
                     "lambda_t3_out" = "incidence",
                     "infections_t3_out" = "infections")

  if (naomi_mf$output_aware_plhiv) {

    indicators_t1 <- c(indicators_t1,
                       "aware_plhiv_prop_t1_out" = "aware_plhiv_prop",
                       "aware_plhiv_num_t1_out" = "aware_plhiv_num",
                       "unaware_plhiv_num_t1_out" = "unaware_plhiv_num")
    indicators_t2 <- c(indicators_t2,
                       "aware_plhiv_prop_t2_out" = "aware_plhiv_prop",
                       "aware_plhiv_num_t2_out" = "aware_plhiv_num",
                       "unaware_plhiv_num_t2_out" = "unaware_plhiv_num")
    indicators_t3 <- c(indicators_t3,
                       "aware_plhiv_prop_t3_out" = "aware_plhiv_prop",
                       "aware_plhiv_num_t3_out" = "aware_plhiv_num",
                       "unaware_plhiv_num_t3_out" = "unaware_plhiv_num")
  }

  indicator_est_t1 <- Map(get_est, names(indicators_t1), indicators_t1, naomi_mf$calendar_quarter1)
  indicator_est_t2 <- Map(get_est, names(indicators_t2), indicators_t2, naomi_mf$calendar_quarter2)
  indicator_est_t3 <- Map(get_est, names(indicators_t3), indicators_t3, naomi_mf$calendar_quarter3)

  indicator_est_t1 <- dplyr::bind_rows(indicator_est_t1)
  indicator_est_t2 <- dplyr::bind_rows(indicator_est_t2)
  indicator_est_t3 <- dplyr::bind_rows(indicator_est_t3)

  indicators_anc_t1 <- c("anc_clients_t1_out" = "anc_clients",
                         "anc_plhiv_t1_out" = "anc_plhiv",
                         "anc_already_art_t1_out" = "anc_already_art",
                         "anc_art_new_t1_out" = "anc_art_new",
                         "anc_known_pos_t1_out" = "anc_known_pos",
                         "anc_tested_pos_t1_out" = "anc_tested_pos",
                         "anc_tested_neg_t1_out" = "anc_tested_neg",
                         "anc_rho_t1_out" = "anc_prevalence",
                         "anc_alpha_t1_out" = "anc_art_coverage")

  indicators_anc_t2 <- c("anc_clients_t2_out" = "anc_clients",
                         "anc_plhiv_t2_out" = "anc_plhiv",
                         "anc_already_art_t2_out" = "anc_already_art",
                         "anc_art_new_t2_out" = "anc_art_new",
                         "anc_known_pos_t2_out" = "anc_known_pos",
                         "anc_tested_pos_t2_out" = "anc_tested_pos",
                         "anc_tested_neg_t2_out" = "anc_tested_neg",
                         "anc_rho_t2_out" = "anc_prevalence",
                         "anc_alpha_t2_out" = "anc_art_coverage")

  indicators_anc_t3 <- c("anc_clients_t3_out" = "anc_clients",
                         "anc_plhiv_t3_out" = "anc_plhiv",
                         "anc_already_art_t3_out" = "anc_already_art",
                         "anc_art_new_t3_out" = "anc_art_new",
                         "anc_known_pos_t3_out" = "anc_known_pos",
                         "anc_tested_pos_t3_out" = "anc_tested_pos",
                         "anc_tested_neg_t3_out" = "anc_tested_neg",
                         "anc_rho_t3_out" = "anc_prevalence",
                         "anc_alpha_t3_out" = "anc_art_coverage")


  indicator_anc_est_t1 <- Map(get_est, names(indicators_anc_t1), indicators_anc_t1,
                              naomi_mf$calendar_quarter1, list(naomi_mf$mf_anc_out))
  indicator_anc_est_t2 <- Map(get_est, names(indicators_anc_t2), indicators_anc_t2,
                              naomi_mf$calendar_quarter2, list(naomi_mf$mf_anc_out))
  indicator_anc_est_t3 <- Map(get_est, names(indicators_anc_t3), indicators_anc_t3,
                              naomi_mf$calendar_quarter3, list(naomi_mf$mf_anc_out))


  indicator_anc_est_t1 <- dplyr::bind_rows(indicator_anc_est_t1)
  indicator_anc_est_t2 <- dplyr::bind_rows(indicator_anc_est_t2)
  indicator_anc_est_t3 <- dplyr::bind_rows(indicator_anc_est_t3)

  mf_anc_out <- naomi_mf$mf_areas %>%
    dplyr::transmute(area_id,
                     sex = "female",
                     age_group = "Y015_049")

  out <- dplyr::bind_rows(
                  indicator_est_t1,
                  indicator_anc_est_t1,
                  indicator_est_t2,
                  indicator_anc_est_t2,
                  indicator_est_t3,
                  indicator_anc_est_t3
                )

  dplyr::select(out, names(naomi_mf$mf_out),
                calendar_quarter, indicator, mean, se, median, mode, lower, upper)
}

extract_art_attendance <- function(naomi_fit, naomi_mf) {

  mode <- naomi_fit$mode

  mfout <- naomi_mf$mf_out %>%
    dplyr::mutate(out_idx = dplyr::row_number())

  v <- naomi_mf$mf_artattend %>%
    dplyr::select(reside_area_id, attend_area_id) %>%
    dplyr::mutate(sex = "both",
                  age_group = "Y000_999") %>%
    dplyr::left_join(
             dplyr::rename(mfout, reside_area_id = area_id, reside_out_idx = out_idx),
             by = c("reside_area_id", "sex", "age_group")
           ) %>%
    dplyr::left_join(
             dplyr::rename(mfout, attend_area_id = area_id, attend_out_idx = out_idx),
             by = c("attend_area_id", "sex", "age_group")
           )

  if(!is.null(mode)) {
    m_artattend_ij_t1 <- mode$artattend_ij_t1_out
    m_artnum_reside_t1 <- mode$artnum_t1_out[v$reside_out_idx]
    m_artnum_attend_t1 <- mode$artattend_t1_out[v$attend_out_idx]

    m_artattend_ij_t2 <- mode$artattend_ij_t2_out
    m_artnum_reside_t2 <- mode$artnum_t2_out[v$reside_out_idx]
    m_artnum_attend_t2 <- mode$artattend_t2_out[v$attend_out_idx]
  } else {
    m_artattend_ij_t1 <- NULL
    m_artattend_ij_t2 <- NULL
  }

  if(!is.null(m_artattend_ij_t1)) {
    m_prop_residents_t1 <- m_artattend_ij_t1 / m_artnum_reside_t1
    m_prop_attendees_t1 <- m_artattend_ij_t1 / m_artnum_attend_t1
  } else {
    m_prop_residents_t1 <- NULL
    m_prop_attendees_t1 <- NULL
  }

  if(!is.null(m_artattend_ij_t2)) {
    m_prop_residents_t2 <- m_artattend_ij_t2 / m_artnum_reside_t2
    m_prop_attendees_t2 <- m_artattend_ij_t2 / m_artnum_attend_t2
  } else {
    m_prop_residents_t2 <- NULL
    m_prop_attendees_t2 <- NULL
  }

  if(!is.null(naomi_fit$sample)) {

    s_artattend_ij_t1 <- naomi_fit$sample$artattend_ij_t1_out
    s_artnum_reside_t1 <- naomi_fit$sample$artnum_t1_out[v$reside_out_idx, ]
    s_artnum_attend_t1 <- naomi_fit$sample$artattend_t1_out[v$attend_out_idx, ]

    s_artattend_ij_t2 <- naomi_fit$sample$artattend_ij_t2_out
    s_artnum_reside_t2 <- naomi_fit$sample$artnum_t2_out[v$reside_out_idx, ]
    s_artnum_attend_t2 <- naomi_fit$sample$artattend_t2_out[v$attend_out_idx, ]
  } else {
    s_artattend_ij_t1 <- NULL
    s_artattend_ij_t2 <- NULL
  }

  if(!is.null(s_artattend_ij_t1)) {
    s_prop_residents_t1 <- s_artattend_ij_t1 / s_artnum_reside_t1
    s_prop_attendees_t1 <- s_artattend_ij_t1 / s_artnum_attend_t1
  } else {
    s_prop_residents_t1 <- NULL
    s_prop_attendees_t1 <- NULL
  }

  if(!is.null(s_artattend_ij_t2)) {
    s_prop_residents_t2 <- s_artattend_ij_t2 / s_artnum_reside_t2
    s_prop_attendees_t2 <- s_artattend_ij_t2 / s_artnum_attend_t2
  } else {
    s_prop_residents_t2 <- NULL
    s_prop_attendees_t2 <- NULL
  }

  v$reside_out_idx <- NULL
  v$attend_out_idx <- NULL

  v_t1 <- dplyr::mutate(v, calendar_quarter = naomi_mf$calendar_quarter1)
  v_t1 <- add_stats(v_t1, m_artattend_ij_t1, s_artattend_ij_t1, "artnum_")
  v_t1 <- add_stats(v_t1, m_prop_residents_t1, s_prop_residents_t1, "prop_residents_")
  v_t1 <- add_stats(v_t1, m_prop_attendees_t1, s_prop_attendees_t1, "prop_attendees_")

  v_t2 <- dplyr::mutate(v, calendar_quarter = naomi_mf$calendar_quarter2)
  v_t2 <- add_stats(v_t2, m_artattend_ij_t2, s_artattend_ij_t2, "artnum_")
  v_t2 <- add_stats(v_t2, m_prop_residents_t2, s_prop_residents_t2, "prop_residents_")
  v_t2 <- add_stats(v_t2, m_prop_attendees_t2, s_prop_attendees_t2, "prop_attendees_")

  dplyr::bind_rows(v_t1, v_t2)
}


#' Build output package from fit
#'
#' @param naomi_fit Fitted naomi model
#' @param naomi_mf Naomi model frame
#'
#' @return List containing output indicators and metadata.
#' @export
output_package <- function(naomi_fit, naomi_mf) {

  indicators <- extract_indicators(naomi_fit, naomi_mf)

  art_attendance <- extract_art_attendance(naomi_fit, naomi_mf)

  meta_area <- naomi_mf$areas %>%
    dplyr::filter(area_id %in% unique(naomi_mf$mf_out$area_id)) %>%
    dplyr::select(area_level, area_level_label, area_id, area_name, parent_area_id, spectrum_region_code, area_sort_order, center_x, center_y, geometry) %>%
    sf::st_as_sf()

  meta_period <- data.frame(
    calendar_quarter = c(naomi_mf$calendar_quarter1,
                         naomi_mf$calendar_quarter2,
                         naomi_mf$calendar_quarter3),
    stringsAsFactors = FALSE
  )%>%
    dplyr::mutate(
             quarter_id = calendar_quarter_to_quarter_id(calendar_quarter),
             quarter_label = naomi::quarter_year_labels(quarter_id)
           )

  meta_age_group <- get_age_groups()

  ## # Fitting outputs
  fit <- list()
  fit$spectrum_calibration <- naomi_mf$spectrum_calibration
  fit$calibration_options <- naomi_mf$calibration_options

  val <- list(
    indicators = indicators,
    art_attendance = art_attendance,
    meta_area = meta_area,
    meta_age_group = meta_age_group,
    meta_period = meta_period,
    meta_indicator = get_meta_indicator(),
    fit = fit
  )

  class(val) <- "naomi_output"

  val
}


#' Add labels to output indicators
#'
#' @param naomi_output Naomi output object.
#'
#' @return Labelled output indicators
#' @export
add_output_labels <- function(naomi_output) {

  stopifnot(inherits(naomi_output, "naomi_output"))

  indicators <- naomi_output$indicators %>%
    dplyr::select(area_id, sex, age_group, calendar_quarter, indicator,
                  mean, se, median, mode, lower, upper)

  meta_area <- naomi_output$meta_area %>%
    sf::st_drop_geometry() %>%
    dplyr::select(area_id, area_level, area_level_label, area_name, area_sort_order)
  indicators <- dplyr::left_join(indicators, meta_area, by = "area_id")

  meta_age_group <- naomi_output$meta_age_group %>%
    dplyr::select(age_group, age_group_label, age_group_sort_order)
  indicators <- dplyr::left_join(indicators, meta_age_group, by = "age_group")

  indicators <- dplyr::left_join(indicators, naomi_output$meta_period,
                                 by = "calendar_quarter")

  meta_indicators <- naomi_output$meta_indicator %>%
    dplyr::select(indicator, indicator_label, indicator_sort_order)
  indicators <- dplyr::left_join(indicators, meta_indicators, by = "indicator")

  indicators <- dplyr::arrange(indicators,
                               area_level,
                               area_sort_order,
                               calendar_quarter,
                               indicator_sort_order,
                               sex,
                               age_group_sort_order)

  indicators <- dplyr::select(indicators,
                              area_level,
                              area_level_label,
                              area_id,
                              area_name,
                              sex,
                              age_group,
                              age_group_label,
                              calendar_quarter,
                              quarter_label,
                              indicator,
                              indicator_label,
                              mean,
                              se,
                              median,
                              mode,
                              lower,
                              upper
                              )

  indicators
}

remove_output_labels <- function(naomi_output) {

  dplyr::select(naomi_output$indicators,
                area_id,
                sex,
                age_group,
                calendar_quarter,
                indicator,
                mean, se, median, mode, lower, upper)
}

add_art_attendance_labels <- function(naomi_output) {

  stopifnot(inherits(naomi_output, "naomi_output"))

  art_attendance <- naomi_output$art_attendance %>%
    dplyr::left_join(
             sf::st_drop_geometry(naomi_output$meta_area) %>%
             dplyr::select(reside_area_id = area_id,
                           reside_area_name = area_name,
                           reside_area_sort_order = area_sort_order),
             by = "reside_area_id"
           ) %>%
    dplyr::left_join(
             sf::st_drop_geometry(naomi_output$meta_area) %>%
             dplyr::select(attend_area_id = area_id,
                           attend_area_name = area_name,
                           attend_area_sort_order = area_sort_order),
             by = "attend_area_id"
           ) %>%
    dplyr::left_join(
             naomi_output$meta_age_group %>%
             dplyr::select(age_group, age_group_label, age_group_sort_order),
             by = "age_group"
           ) %>%
    dplyr::left_join(naomi_output$meta_period, by = "calendar_quarter") %>%
    dplyr::arrange(
             reside_area_sort_order,
             attend_area_sort_order,
             calendar_quarter,
             sex,
             age_group_sort_order
           ) %>%
    dplyr::select(
             reside_area_id,
             reside_area_name,
             attend_area_id,
             attend_area_name,
             sex,
             age_group,
             age_group_label,
             calendar_quarter,
             quarter_label,
             dplyr::starts_with("artnum"),
             dplyr::starts_with("prop_residents"),
             dplyr::starts_with("prop_attendees")
           )

  art_attendance
}

remove_art_attendance_labels <- function(naomi_output) {

  dplyr::select(naomi_output$art_attendance,
                reside_area_id,
                attend_area_id,
                sex,
                age_group,
                calendar_quarter,
                dplyr::starts_with("artnum"),
                dplyr::starts_with("prop_residents"),
                dplyr::starts_with("prop_attendees"))
}

#' Subset the results returned in Naomi output package
#'
#' @param naomi_output Naomi output object.
#' @param area_id vector of area_ids to include/exclude.
#' @param area_level vector of area_levels to include/exclude.
#' @param sex vector of sexes to include/exclude.
#' @param age_group vector of age_groups to include/exclude.
#' @param calendar_quarter vector of calendar_quarters to include/exclude.
#' @param indicator vector of indicators to include/exclude.
#' @param drop logical whether to drop the supplied indices instead of keep
#'   only the supplied indices (default).
#' @param check_list logical whether to check that supplied values are in the
#'   output package to be subsetted.
#'
#' @return
#' A naomi output package with a subset of results.
#'
#' @details
#' If arguemnts are `NULL` (default), no subsetting is done on that dimension.
#'
#' By default the argument `check_list = TRUE` means an error will be thrown
#' if any of the values in the vectors to subset are not found in the
#' `naomi_output` object supplied. This might be set to `FALSE` for some batch
#' processing applications, for example of the `naomi_output` could have already
#' been partially subsetted.
#'
subset_naomi_output <- function(naomi_output,
                                area_id = NULL,
                                area_level = NULL,
                                sex = NULL,
                                age_group = NULL,
                                calendar_quarter = NULL,
                                indicator = NULL,
                                drop = FALSE,
                                check_list = TRUE) {

  stopifnot(inherits(naomi_output, "naomi_output"))

  if(!is.null(area_id)) {
    if(check_list && !all(area_id %in% naomi_output$meta_area$area_id)) {
      missing_area_id <- setdiff(area_id, naomi_output$meta_area$area_id)
      stop(paste("area_ids not found in naomi_output:", paste(missing_area_id, collapse = ",")))
    }
    if(drop) {
      naomi_output$meta_area <- dplyr::filter(naomi_output$meta_area, !area_id %in% !!area_id)
    } else {
      naomi_output$meta_area <- dplyr::filter(naomi_output$meta_area, area_id %in% !!area_id)
    }
  }

  if(!is.null(area_level)) {
    if(check_list && !all(area_level %in% naomi_output$meta_area$area_level)) {
      missing_area_level <- setdiff(area_level, naomi_output$meta_area$area_level)
      stop(paste("area_levels not found in naomi_output:", paste(missing_area_level, collapse = ",")))
    }
    if(drop) {
      naomi_output$meta_area <- dplyr::filter(naomi_output$meta_area, !area_level %in% !!area_level)
    } else {
      naomi_output$meta_area <- dplyr::filter(naomi_output$meta_area, area_level %in% !!area_level)
    }
  }

  if(!is.null(age_group)) {
    if(check_list && !all(age_group %in% naomi_output$meta_age_group$age_group)) {
      missing_age_group <- setdiff(age_group, naomi_output$meta_age_group$age_group)
      stop(paste("age_groups not found in naomi_output:", paste(missing_age_group, collapse = ",")))
    }
    if(drop) {
      naomi_output$meta_age_group <- dplyr::filter(naomi_output$meta_age_group, !age_group %in% !!age_group)
    } else {
      naomi_output$meta_age_group <- dplyr::filter(naomi_output$meta_age_group, age_group %in% !!age_group)
    }
  }

  if(!is.null(calendar_quarter)) {
    if(check_list && !all(calendar_quarter %in% naomi_output$meta_period$calendar_quarter)) {
      missing_calendar_quarter <- setdiff(calendar_quarter, naomi_output$meta_period$calendar_quarter)
      stop(paste("calendar_quarters not found in naomi_output:", paste(missing_calendar_quarter, collapse = ",")))
    }
    if(drop) {
      naomi_output$meta_period <- dplyr::filter(naomi_output$meta_period, !calendar_quarter %in% !!calendar_quarter)
    } else {
      naomi_output$meta_period <- dplyr::filter(naomi_output$meta_period, calendar_quarter %in% !!calendar_quarter)
    }
  }

  if(!is.null(indicator)) {
    if(check_list && !all(indicator %in% naomi_output$meta_indicator$indicator)) {
      missing_indicator <- setdiff(indicator, naomi_output$meta_indicator$indicator)
      stop(paste("indicators not found in naomi_output:", paste(missing_indicator, collapse = ",")))
    }
    if(drop) {
      naomi_output$meta_indicator <- dplyr::filter(naomi_output$meta_indicator, !indicator %in% !!indicator)
    } else {
      naomi_output$meta_indicator <- dplyr::filter(naomi_output$meta_indicator, indicator %in% !!indicator)
    }
  }


  ## There is no meta_sex table, so sex is dropped directly from indicators
  if(!is.null(sex)) {
    sexes <- c("male", "female", "both")

    if(check_list && !all(sex %in% sexes)) {
      missing_sex <- setdiff(sex, sexes)
      stop(paste("sexes not found in naomi_output:", paste(missing_sex, collapse = ",")))
    }

    if(drop) {
      naomi_output$indicators <- dplyr::filter(naomi_output$indicators,
                                               !sex %in% !!sex)
    } else {
      naomi_output$indicators <- dplyr::filter(naomi_output$indicators,
                                               sex %in% !!sex)
    }
  }

  naomi_output$indicators <- dplyr::filter(naomi_output$indicators,
                                           area_id %in% naomi_output$meta_area$area_id)

  naomi_output$indicators <- dplyr::filter(naomi_output$indicators,
                                           age_group %in% naomi_output$meta_age_group$age_group)

  naomi_output$indicators <- dplyr::filter(naomi_output$indicators,
                                           calendar_quarter %in% naomi_output$meta_period$calendar_quarter)

  naomi_output$indicators <- dplyr::filter(naomi_output$indicators,
                                           indicator %in% naomi_output$meta_indicator$indicator)

  naomi_output
}

#' Save outputs to zip file
#'
#' @param naomi_output Naomi output object
#' @param filename Name of file to create
#' @param dir Directory to create zip in
#' @param overwrite If TRUE overwrite any existing file
#' @param with_labels If TRUE save indicator ids with labels
#' @param boundary_format Either geojson or shp for saving boundary as geojson
#' or shape format
#' @param single_csv If TRUE only output the csv of indicators, otherwise save
#' the metadata too
#' @param export_datapack If TRUE save CSV of PEPFAR datapack indicators.
#'
#' @return Path to created zip file
#' @export
save_output_package <- function(naomi_output,
                                filename,
                                dir,
                                overwrite = FALSE,
                                with_labels = FALSE,
                                boundary_format = "geojson",
                                single_csv = FALSE,
                                export_datapack = !single_csv) {

  save_output(filename, dir, naomi_output, overwrite,
              with_labels, boundary_format, single_csv, export_datapack)
}

save_output_coarse_age_groups <- function(path, naomi_output,
                                          overwrite = FALSE) {

  age_groups_keep <- c("Y015_049", "Y015_064", "Y015_999", "Y050_999", "Y000_999", "Y000_064",
                       "Y000_014", "Y015_024", "Y025_034", "Y035_049", "Y050_064", "Y065_999")
  naomi_output_sub <- subset_naomi_output(naomi_output, age_group = age_groups_keep)

  save_output(basename(path), dirname(path), naomi_output_sub,
              overwrite = overwrite, with_labels = TRUE,
              boundary_format = "geojson", single_csv = FALSE,
              export_datapack = TRUE)
}

save_output_spectrum <- function(path, naomi_output, overwrite = FALSE) {
  save_output(basename(path), dirname(path), naomi_output,
              overwrite = overwrite, with_labels = TRUE,
              boundary_format = "geojson", single_csv = FALSE,
              export_datapack = TRUE)
}

save_output <- function(filename, dir,
                        naomi_output,
                        overwrite = FALSE,
                        with_labels = FALSE,
                        boundary_format = "geojson",
                        single_csv = FALSE,
                        export_datapack = !single_csv) {

  dir <- normalizePath(dir, mustWork = TRUE)
  if(!file.access(dir, 2) == 0) {
    stop(paste("Directory", dir, "is not writable."))
  }
  if (!grepl(".zip", filename)) {
    filename <- paste0(filename, ".zip")
  }
  path <- file.path(dir, filename)
  stopifnot(inherits(naomi_output, "naomi_output"))
  if (file.access(path, 0) == 0 && !overwrite) {
    stop(paste(
      "File", path, "already exists. Set overwrite = TRUE to write output."))
  }

  naomi_output$indicators <- remove_output_labels(naomi_output)
  naomi_output$art_attendance <- remove_art_attendance_labels(naomi_output)

  if (with_labels) {
    indicators <- add_output_labels(naomi_output)
    art_attendance <- add_art_attendance_labels(naomi_output)
  } else {
    indicators <- naomi_output$indicators
    art_attendance <- naomi_output$art_attendance
  }

  tmpd <- tempfile()
  dir.create(tmpd)
  old <- setwd(tmpd)
  on.exit(setwd(old))
  naomi_write_csv(indicators, "indicators.csv")

  if(!single_csv) {
    naomi_write_csv(art_attendance, "art_attendance.csv")
    naomi_write_csv(sf::st_drop_geometry(naomi_output$meta_area),
                    "meta_area.csv")
    naomi_write_csv(naomi_output$meta_age_group, "meta_age_group.csv")
    naomi_write_csv(naomi_output$meta_period, "meta_period.csv")
    naomi_write_csv(naomi_output$meta_indicator, "meta_indicator.csv")

    naomi_output$meta_area$name <- naomi_output$meta_area$area_id

    if(!is.null(boundary_format) && !is.na(boundary_format)) {
      if(boundary_format == "geojson") {
        sf::st_write(naomi_output$meta_area, "boundaries.geojson", quiet = TRUE)
      } else if(boundary_format == "shp") {
        dir.create("shp")
        sf::st_write(naomi_output$meta_area, "shp/boundaries.shp", quiet = TRUE)
      } else {
        stop(paste("Boundary file format", boundary_format, "not recognized.",
                   "Please select 'geojson', 'shp', or NA to not save boundaries."))
      }
    }
  }

  if (export_datapack) {
    write_datapack_csv(naomi_output, "pepfar_datapack_indicators_2021.csv")
  }

  info <- attr(naomi_output, "info")
  if (length(info) > 0L) {
    dir.create("info")
    for (p in names(info)) {
      writeLines(trimws(info[[p]]), file.path("info", p))
    }
  }

  fit <- naomi_output$fit
  if(length(fit) > 0L) {
    dir.create("fit")
    naomi_write_csv(fit$spectrum_calibration, "fit/spectrum_calibration.csv")

    naomi_write_csv(
      data.frame(option = names(fit$calibration_options),
                 value  = unlist(fit$calibration_options)),
      "fit/calibration_options.csv")
  }

  zip::zipr(path, list.files())
  path
}

#' Generate and save summary report at specified path
#'
#' @param report_path Path to save summary report at
#' @param output_zip Path to model outputs zip file
#' @param quiet Suppress printing of the pandoc command line
#'
#' @return Path to summary report
#' @keywords internal
generate_output_summary_report <- function(report_path,
                                           output_zip,
                                           quiet = FALSE) {
  report_filename <- basename(report_path)
  report_path_dir <- normalizePath(dirname(report_path), mustWork = TRUE)
  output_zip_path <- normalizePath(output_zip, mustWork = TRUE)
  ## Render uses relative paths to locate the html file. The package author
  ## advises against using output_dir see:
  ## https://github.com/rstudio/rmarkdown/issues/587#issuecomment-168437646
  ## so set up a temp directory with all report sources and generate from there
  ## then copy report to destination
  tmpd <- tempfile()
  dir.create(tmpd)
  on.exit(unlink(tmpd, recursive = TRUE))
  withr::with_dir(tmpd, {
    fs::file_copy(list.files(system_file("report"), full.names = TRUE), ".")
    style <- brio::readLines("styles.css")
    style <- traduire::translator()$replace(style)
    writeLines(style, "styles.css")
    rmarkdown::render("summary_report.Rmd",
                      params = list(output_zip = output_zip_path,
                                    lang = t_("LANG")),
                      output_file = report_filename,
                      quiet = quiet
                      )

    fs::file_copy(report_filename, report_path_dir, overwrite = TRUE)
  })
  invisible(report_path)
}


#' @rdname save_output_package
#' @param path Path to output zip file.
#' @export
read_output_package <- function(path) {

  tmpd <- tempfile()
  on.exit(unlink(tmpd))

  utils::unzip(path, exdir = tmpd)

  ## Fit list
  spectrum_calibration <- readr_read_csv(file.path(tmpd, "fit/spectrum_calibration.csv"))
  calibration_options <- readr_read_csv(file.path(tmpd, "fit/calibration_options.csv"))
  calibration_options <- setNames(calibration_options$value,
                                  calibration_options$option)

  fit <- list(spectrum_calibration = spectrum_calibration,
              calibration_options = calibration_options)

  v <- list(
    indicators = readr_read_csv(file.path(tmpd, "indicators.csv")),
    art_attendance = readr_read_csv(file.path(tmpd, "art_attendance.csv")),
    meta_area = sf::read_sf(file.path(tmpd, "boundaries.geojson")),
    meta_age_group = readr_read_csv(file.path(tmpd, "meta_age_group.csv")),
    meta_period = readr_read_csv(file.path(tmpd, "meta_period.csv")),
    meta_indicator = readr_read_csv(file.path(tmpd, "meta_indicator.csv")),
    fit = fit
  )

  v$meta_area$name <- NULL

  info_files <- list.files(file.path(tmpd, "info"))
  if(length(info_files)) {
    info <- lapply(file.path(tmpd, "info", info_files), readLines)
    names(info) <- info_files
    attr(v, "info") <- info
  }

  class(v) <- "naomi_output"
  v
}


#' Resave a subsetted Naomi output package
#'
#' This function reads an output package, subsets it using [subset_naomi_output()]
#' and resaves the ouput package.
#'
#' @param path file path to naomi output package.
#' @param output_path path to resave subsetted output package.
#' @param ... arguments to [subset_naomi_output()].
#'
#' @return path to saved output package.
#'
#' @details
#' See `?subset_naomi_output()` for subsetting arguments and options.
#'
#' @seealso
#' [subset_naomi_output()]
#'
#' @export
subset_output_package <- function(path, output_path, ...) {

  naomi_output <- read_output_package(path)
  naomi_output <- subset_naomi_output(naomi_output, ...)
  save_output_spectrum(output_path, naomi_output)
}



#' Calibrate naomi model outputs
#'
#' @param output Naomi model output package produced by [`output_package()`].
#' @param naomi_mf Naomi model frame, objective of class `naomi_mf`.
#' @param spectrum_plhiv_calibration_level Level to calibrate PLHIV, see details.
#' @param spectrum_plhiv_calibration_strat Age/sex stratification to calibrate PLHIV, see details.
#' @param spectrum_artnum_calibration_level Level to calibrate number on ART, see details.
#' @param spectrum_artnum_calibration_strat Age/sex stratification to calibrate number on ART, see details.
#' @param spectrum_aware_calibration_level Level to calibrate number aware of HIV positive status, see details.
#' @param spectrum_aware_calibration_strat Age/sex stratification to calibrate number aware of HIV positive status, see details.
#' @param spectrum_infections_calibration_level Level to calibrate number infections of HIV positive status, see details.
#' @param spectrum_infections_calibration_strat Age/sex stratification to calibrate number infections of HIV positive status, see details.
#' @param calibrate_method Calibration method, either `"logistic"` (default) or `"proportional"`.
#'
#' @details
#'
#' The following indicators are calibrated:
#'
#' * `plhiv`
#' * `art_current_residents`
#' * `unaware_plhiv_num`
#' * `infections`
#' * `art_current` (attending)
#' * `aware_plhiv_num`
#' * `untreated_plhiv_num`
#' * `prevalence`
#' * `art_coverage`
#' * `aware_plhiv_prop`
#' * `incidence`
#'
#' Steps in the calibration:
#'
#' 1. Aggregate Spectrum totals to specified stratification by level/sex/age to
#'    calculate the target totals within each stratification.
#' 2. Adjust fine area/sex/age-group mean values to match targeted total using
#'    either "logistic" or "proportional" scaling.
#' 3. Aggregate revised mean count values to all stratifications of Naomi outputs.
#' 4. Calculate calibrated mean for proportion indicators.
#' 5. Adjust outputs for all statistics (mean, median, mode, se, range).
#' 6. Aggregate totals spectrum_calibration table.
#'
#'
#' The "logistic" scaling method converts fine counts to logit proportions, then
#' uses numerical optimisation to solve the logit-scale adjustment to the fine
#' district/sex/age proportions such that the adjusted proportions times the
#' denominator sums to the target value.
#'
#' Calibration proceeds sequentially through the following indicators.
#' * PLHIV
#' * Number of residents on ART
#' * Number unaware of HIV status
#' * Number of new infections
#' * Number of attending ANC by district
#'
#' Calibration of a previous indicator may affect the denominator for the next
#' indicator. This does not affect the calculation for proportional scaling,
#' but will affect logistic scaling. Inconsistent selections for calibration
#' levels or stratifications could result in a denominator smaller than a target
#' numerator for a particular value. This will throw an error for logistic
#' scaling methods.
#'
#'
#' The number of attending ARG clients is always calibrated proportionally by
#' sex and five-year age group to the number or residents attending.
#'
#' @importMethodsFrom Matrix %*%
#' @export
calibrate_outputs <- function(output,
                              naomi_mf,
                              spectrum_plhiv_calibration_level,
                              spectrum_plhiv_calibration_strat,
                              spectrum_artnum_calibration_level,
                              spectrum_artnum_calibration_strat,
                              spectrum_aware_calibration_level,
                              spectrum_aware_calibration_strat,
                              spectrum_infections_calibration_level,
                              spectrum_infections_calibration_strat,
                              calibrate_method = "logistic") {

  stopifnot(inherits(output, "naomi_output"))
  stopifnot(inherits(naomi_mf, "naomi_mf"))
  stopifnot(calibrate_method %in% c("logistic", "proportional"))

  group_vars <- c("spectrum_region_code", "calendar_quarter", "sex", "age_group")

  mf <- dplyr::select(naomi_mf$mf_model, area_id, sex, age_group) %>%
    dplyr::left_join(
             sf::st_drop_geometry(output$meta_area) %>%
             dplyr::select(area_id, spectrum_region_code),
             by = "area_id"
           )

  mfout <- dplyr::select(naomi_mf$mf_out, area_id, sex, age_group)

  indicators <- output$indicators %>%
    dplyr::select(area_id, sex, age_group, calendar_quarter, indicator,
                  mean, se, median, mode, lower, upper)

  ## Subset to most granular estimates in model frame.
  ## Add ID columns to merge to spectrum_calibration data frame.
  val <- indicators %>%
    dplyr::filter(indicator %in%
                  c("population", "plhiv", "art_current_residents", "art_current", "aware_plhiv_num", "unaware_plhiv_num", "infections")) %>%
    dplyr::inner_join(mf, by = c("area_id", "sex", "age_group")) %>%
    dplyr::select(area_id, indicator, tidyselect::all_of(group_vars), mean)


  ## Table of target values from Spectrum
  spectrum_calibration <- naomi_mf$spectrum_calibration %>%
    dplyr::mutate(age_coarse = dplyr::if_else(
                                        age_group %in% c("Y000_004", "Y005_009", "Y010_014"),
                                        "Y000_014", "Y015_999"))

  ## Join aggregates of raw Naomi mean to archive
  val_aggr <- val %>%
    dplyr::group_by_at(c("indicator", group_vars)) %>%
    dplyr::summarise(est_raw = sum(mean), .groups = "drop")

  val_aggr_wide <- val_aggr %>%
    dplyr::filter(
             indicator %in% c("plhiv", "art_current_residents",
                              "unaware_plhiv_num", "infections")
           ) %>%
    dplyr::mutate(
             indicator = indicator %>%
               dplyr::recode("plhiv" = "plhiv_raw",
                             "art_current_residents" = "art_current_raw",
                             "unaware_plhiv_num" = "unaware_raw",
                             "infections" = "infections_raw")
           ) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = est_raw)

  if (is.null(val_aggr_wide[["unaware_raw"]])) {
    val_aggr_wide[["unaware_raw"]] <- NA_real_
  }

  spectrum_calibration <- spectrum_calibration %>%
    dplyr::left_join(val_aggr_wide, by = group_vars)


  ## Calculate calibration adjustments for means PLHIV, ART number, and new infections

  valmean_wide <- val %>%
    tidyr::pivot_wider(c(area_id, tidyselect::all_of(group_vars)),
                       names_from = indicator, values_from = mean) %>%
    dplyr::mutate(
             age_coarse = dplyr::if_else(
                                   age_group %in% c("Y000_004", "Y005_009", "Y010_014"),
                                   "Y000_014", "Y015_999")
           )

  if (!naomi_mf$output_aware_plhiv) {
    valmean_wide$unaware_plhiv_num <- NA_real_
  }

  valmean_wide <- valmean_wide %>%
    dplyr::mutate(
             prevalence = plhiv / population,
             art_coverage = art_current_residents / plhiv,
             unaware_plhiv_prop = unaware_plhiv_num / (plhiv - art_current_residents),
             incidence = infections / (population - plhiv)
           )

  calibrate_logistic_one <- function(proportion_raw,
                                     denominator_new,
                                     target_val) {

    ## Adjust for small numerical discrepancy
    proportion_raw[proportion_raw >= 1 & proportion_raw < 1+1e-5] <- 0.99999
    proportion_raw[proportion_raw <= 0 & proportion_raw > -1e-5] <- 0.00001

    stopifnot(proportion_raw >= 0)
    stopifnot(proportion_raw <= 1)
    stopifnot(target_val == target_val[1])
    target_val <- target_val[1]

    ## Add a small value for numerical issues
    if (target_val > (sum(denominator_new) + 1e-2)) {
      stop("Error in logistic calibration: target numerator is larger than aggregated denominator.")
    }

    logit_p_fine <- qlogis(proportion_raw)
    adjust_numerator <- function(theta, l, d) plogis(l + theta) * d
    optfn <- function(theta){ (sum(adjust_numerator(theta, logit_p_fine, denominator_new)) - target_val)^2 }
    opt <- optimise(optfn, c(-10, 10), tol = .Machine$double.eps^0.5)

    adjust_numerator(opt$minimum, logit_p_fine, denominator_new)
  }

  calibrate_proportional_one <- function(value_raw, target_val) {

    stopifnot(target_val == target_val[1])
    target_val <- target_val[1]

    value_raw * target_val / sum(value_raw)
  }

  calibrate_one <- function(value_raw, proportion_raw, denominator_new,
                            target_val, calibrate_method) {

    if (calibrate_method == "logistic") {
      val <- calibrate_logistic_one(proportion_raw, denominator_new, target_val)
    } else if (calibrate_method == "proportional") {
      val <- calibrate_proportional_one(value_raw, target_val)
    } else {
      stop(paste0("calibrate_method \"", calibrate_method, "\" not found."))
    }

    val
  }


  ## Calibrate PLHIV
  plhiv_aggr_var <- get_spectrum_aggr_var(spectrum_plhiv_calibration_level,
                                          spectrum_plhiv_calibration_strat)

  if(length(plhiv_aggr_var) > 0L) {

    plhiv_target <- spectrum_calibration %>%
      dplyr::group_by_at(plhiv_aggr_var) %>%
      dplyr::summarise(plhiv_target = sum(plhiv_spectrum),
                       .groups = "drop")

    valmean_wide <- valmean_wide %>%
      dplyr::left_join(plhiv_target, by = plhiv_aggr_var)

    valmean_wide <- valmean_wide %>%
      dplyr::group_by_at(plhiv_aggr_var) %>%
      dplyr::mutate(
               plhiv = calibrate_one(plhiv,
                                     prevalence,
                                     population,
                                     plhiv_target,
                                     calibrate_method)
             )
  }


  ## Calibrate ART number
  artnum_aggr_var <- get_spectrum_aggr_var(spectrum_artnum_calibration_level,
                                           spectrum_artnum_calibration_strat)

  if(length(artnum_aggr_var) > 0L) {

    artnum_target <- spectrum_calibration %>%
      dplyr::group_by_at(artnum_aggr_var) %>%
      dplyr::summarise(artnum_target = sum(art_current_spectrum),
                       .groups = "drop")

    valmean_wide <- valmean_wide %>%
      dplyr::left_join(artnum_target, by = artnum_aggr_var)

    valmean_wide <- valmean_wide %>%
      dplyr::group_by_at(artnum_aggr_var) %>%
      dplyr::mutate(
               art_current_residents = calibrate_one(art_current_residents,
                                                     art_coverage,
                                                     plhiv,
                                                     artnum_target,
                                                     calibrate_method)
             )

    ## Calibrate number attending
    ## Note: aggregation based off calibrated values for valmean_wide$art_current_residents

    artattend_aggr_var <- get_spectrum_aggr_var(spectrum_artnum_calibration_level,
                                                "sex_age_group")

    artattend_target <- valmean_wide %>%
      dplyr::group_by_at(artattend_aggr_var) %>%
      dplyr::summarise(artattend_target = sum(art_current_residents),
                       .groups = "drop")

    valmean_wide <- valmean_wide %>%
      dplyr::left_join(artattend_target, by = artattend_aggr_var)

    valmean_wide <- valmean_wide %>%
      dplyr::group_by_at(artattend_aggr_var) %>%
      dplyr::mutate(
               art_current = calibrate_proportional_one(art_current,
                                                        artattend_target)
             )
  }
  valmean_wide$untreated_plhiv_num = valmean_wide$plhiv - valmean_wide$art_current_residents

  ## Calibrate infections
  infections_aggr_var <- get_spectrum_aggr_var(spectrum_infections_calibration_level,
                                               spectrum_infections_calibration_strat)

  if(length(infections_aggr_var) > 0L) {

    infections_target <- spectrum_calibration %>%
      dplyr::group_by_at(infections_aggr_var) %>%
      dplyr::summarise(infections_target = sum(infections_spectrum),
                       .groups = "drop")

    valmean_wide <- valmean_wide %>%
      dplyr::left_join(infections_target, by = infections_aggr_var)

    valmean_wide <- valmean_wide %>%
      dplyr::group_by_at(infections_aggr_var) %>%
      dplyr::mutate(
               infections = calibrate_one(infections,
                                          incidence,
                                          population - plhiv,
                                          infections_target,
                                          calibrate_method)
             )
  }

  ## Calibrate number unaware of HIV status

  unaware_aggr_var <- get_spectrum_aggr_var(spectrum_aware_calibration_level,
                                            spectrum_aware_calibration_strat)


  if (naomi_mf$output_aware_plhiv & length(unaware_aggr_var) > 0L) {

    unaware_target <- spectrum_calibration %>%
      dplyr::group_by_at(unaware_aggr_var) %>%
      dplyr::summarise(unaware_target = sum(unaware_spectrum),
                       .groups = "drop")

    valmean_wide <- valmean_wide %>%
      dplyr::left_join(unaware_target, by = unaware_aggr_var)

    valmean_wide <- valmean_wide %>%
      dplyr::group_by_at(unaware_aggr_var) %>%
      dplyr::mutate(
               unaware_plhiv_num = calibrate_one(unaware_plhiv_num,
                                                 unaware_plhiv_prop,
                                                 plhiv - art_current_residents,
                                                 unaware_target,
                                                 calibrate_method)
             )

  }

  valmean_wide <- valmean_wide %>%
    dplyr::mutate(aware_plhiv_num = plhiv - unaware_plhiv_num)



  spectrum_calibration <- spectrum_calibration %>%
    dplyr::select(spectrum_region_code, sex, age_group, calendar_quarter,
                  population_spectrum, population_raw, population_calibration, population_final = population,
                  plhiv_spectrum, plhiv_raw,
                  art_current_spectrum, art_current_raw,
                  unaware_spectrum, unaware_raw,
                  infections_spectrum, infections_raw)

  val_adj <- valmean_wide %>%
    tidyr::pivot_longer(cols = c(population, plhiv, art_current_residents, art_current,
                                 untreated_plhiv_num, unaware_plhiv_num, aware_plhiv_num,
                                 infections),
                        names_to = "indicator", values_to = "adjusted")


  ## Aggregate adjusted fine stratification values to all Naomi aggregates

  .expand <- function(cq, ind) {

    stopifnot(cq %in% val_adj$calendar_quarter)
    stopifnot(ind %in% val_adj$indicator)

    byv <- c("area_id", "sex", "spectrum_region_code", "age_group")
    m <- dplyr::filter(val_adj, calendar_quarter == cq, indicator == ind)
    m <- dplyr::left_join(mf, m, by = byv)

    val <- mfout
    val$calendar_quarter <- cq
    val$indicator <- ind
    val$adjusted = as.numeric(naomi_mf$A_out %*% m$adjusted)

    val
  }

  adj <- dplyr::bind_rows(
                  .expand(naomi_mf$calendar_quarter1, "population"),
                  .expand(naomi_mf$calendar_quarter2, "population"),
                  .expand(naomi_mf$calendar_quarter3, "population"),
                  .expand(naomi_mf$calendar_quarter1, "plhiv"),
                  .expand(naomi_mf$calendar_quarter2, "plhiv"),
                  .expand(naomi_mf$calendar_quarter3, "plhiv"),
                  .expand(naomi_mf$calendar_quarter1, "art_current_residents"),
                  .expand(naomi_mf$calendar_quarter2, "art_current_residents"),
                  .expand(naomi_mf$calendar_quarter3, "art_current_residents"),
                  .expand(naomi_mf$calendar_quarter1, "art_current"),
                  .expand(naomi_mf$calendar_quarter2, "art_current"),
                  .expand(naomi_mf$calendar_quarter3, "art_current"),
                  .expand(naomi_mf$calendar_quarter1, "untreated_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter2, "untreated_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter3, "untreated_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter1, "unaware_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter2, "unaware_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter3, "unaware_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter1, "aware_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter2, "aware_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter3, "aware_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter1, "infections"),
                  .expand(naomi_mf$calendar_quarter2, "infections"),
                  .expand(naomi_mf$calendar_quarter3, "infections")
                )

  byv <- c("indicator", "area_id", "sex", "age_group", "calendar_quarter")

  adj_counts <- dplyr::right_join(
                         dplyr::select(indicators, tidyselect::all_of(byv), raw = mean),
                         dplyr::select(adj, tidyselect::all_of(byv), adjusted),
                         by = byv
                       ) %>%
    dplyr::mutate(
             ratio = dplyr::if_else(raw == 0, 0, adjusted / raw),
             raw = NULL,
             adjusted = NULL
           )

  out <- indicators %>%
    dplyr::left_join(adj_counts, by = byv) %>%
    dplyr::mutate(
             ratio = tidyr::replace_na(ratio, 1.0),
             mean = mean * ratio,
             se = se * ratio,
             median = median * ratio,
             mode = mode * ratio,
             lower = lower * ratio,
             upper = upper * ratio,
             ratio = NULL
           )

  out <- dplyr::select(out, tidyselect::all_of(names(output$indicators)))


  adj_props <- adj %>%
    tidyr::pivot_wider(names_from = indicator, values_from = adjusted) %>%
    dplyr::mutate(
             prevalence = plhiv / population,
             art_coverage = art_current_residents / plhiv,
             aware_plhiv_prop = aware_plhiv_num / plhiv,
             incidence = infections / (population - plhiv)
           ) %>%
    tidyr::pivot_longer(c(prevalence, art_coverage, aware_plhiv_prop, incidence),
                        names_to = "indicator", values_to = "adjusted") %>%
    dplyr::select(tidyselect::all_of(byv), adjusted) %>%
    dplyr::left_join(
             dplyr::select(indicators, tidyselect::all_of(byv), raw = mean),
             by = byv
           ) %>%
    dplyr::mutate(
             log_odds = dplyr::if_else(adjusted == 0, 0, qlogis(adjusted) - qlogis(raw)),
             raw = NULL,
             adjusted = NULL
           )

  adjust_prop <- function(val, log_odds) {
    idx <- !is.na(log_odds) & val > 0
    val[idx] <- plogis(qlogis(val[idx]) + log_odds[idx])
    val
  }

  adjust_prop_se <- function(se, mean, log_odds) {
    idx <- !is.na(log_odds) & se > 0
    val <- se
    val[idx] <- se[idx] * exp(log_odds[idx]) / ((exp(log_odds[idx]) - 1) * mean[idx] + 1)^2
    val
  }

  out <- out %>%
    dplyr::left_join(adj_props, by = byv) %>%
    dplyr::mutate(
             mean = adjust_prop(mean, log_odds),
             se = adjust_prop_se(se, mean, log_odds),
             median = adjust_prop(median, log_odds),
             mode = adjust_prop(mode, log_odds),
             lower = adjust_prop(lower, log_odds),
             upper = adjust_prop(upper, log_odds),
             log_odds = NULL
           )

  out <- dplyr::select(out, tidyselect::all_of(names(output$indicators)))

  ## Save calibration options

  calib_opts <- list(spectrum_plhiv_calibration_level = spectrum_plhiv_calibration_level,
                     spectrum_plhiv_calibration_strat  = spectrum_plhiv_calibration_strat,
                     spectrum_artnum_calibration_level = spectrum_artnum_calibration_level,
                     spectrum_artnum_calibration_strat = spectrum_artnum_calibration_strat,
                     spectrum_aware_calibration_level = spectrum_aware_calibration_level,
                     spectrum_aware_calibration_strat = spectrum_aware_calibration_strat,
                     spectrum_infections_calibration_level = spectrum_infections_calibration_level,
                     spectrum_infections_calibration_strat = spectrum_infections_calibration_strat,
                     calibrate_method = calibrate_method)

  output$indicators <- out
  output$fit$spectrum_calibration <- spectrum_calibration
  output$fit$calibration_options[names(calib_opts)] <- calib_opts

  output
}

get_spectrum_aggr_var <- function(level, strat) {

  if(!level %in% c("national", "subnational", "none"))
    stop(paste0("Calibration level \"", level, "\" not found."))

  if(level == "none")
    return(character(0))


  aggr_vars <- "calendar_quarter"

  if(level == "subnational")
    aggr_vars <- c(aggr_vars, "spectrum_region_code")

  stratvar <- switch(strat,
                     "age_coarse" = "age_coarse",
                     "sex_age_coarse" = c("sex", "age_coarse"),
                     "age_group" = "age_group",
                     "sex_age_group" = c("sex", "age_group"))

  if(is.null(stratvar))
    stop(paste0("Calibration stratification \"", strat, "\" not found."))

  aggr_vars <- c(aggr_vars, stratvar)

  aggr_vars
}

#' Disaggregate age 0-4 outputs to <1 / 1-4
#'
#' Disaggregate output indicators for the 0-4 age group proportional to
#' Spectrum distribution for each age group.
#'
#' @param output a `naomi_output` object
#' @param naomi_mf a `naomi_mf` object
#'
#' @export
disaggregate_0to4_outputs <- function(output, naomi_mf) {

  age_group_0to4_id  <- "Y000_004"

  out0to4 <- output$indicators %>%
    dplyr::filter(age_group == age_group_0to4_id) %>%
    dplyr::mutate(age_group = NULL)

  strat_mean_counts_model <- out0to4 %>%
    dplyr::filter(sex %in% c("male", "female")) %>%
    dplyr::left_join(
             dplyr::select(naomi_mf$mf_areas, area_id, spectrum_region_code),
             by = "area_id"
           ) %>%
    dplyr::inner_join(
             naomi_mf$spectrum_0to4distribution,
             by = c("spectrum_region_code", "calendar_quarter", "indicator")
           ) %>%
    dplyr::mutate(mean_strat = mean * distribution) %>%
    dplyr::select(area_id, sex, age_group, calendar_quarter, indicator, mean_strat)


  mf_0to4_model <- dplyr::select(naomi_mf$mf_model, area_id, sex, age_group, idx_model = idx) %>%
    dplyr::filter(age_group == age_group_0to4_id )

  mf_0to4_out <- naomi_mf$mf_out %>%
    dplyr::mutate(idx_out = dplyr::row_number()) %>%
    dplyr::filter(age_group == age_group_0to4_id) %>%
    dplyr::select(area_id_out = area_id,
                  sex_out = sex,
                  age_group_out = age_group,
                  idx_out)

  A_0to4_long <- Matrix::summary(naomi_mf$A_out) %>%
    as.data.frame() %>%
    dplyr::rename(idx_out = 1, idx_model = 2) %>%
    dplyr::inner_join(mf_0to4_model, by = "idx_model") %>%
    dplyr::inner_join(mf_0to4_out, by = "idx_out") %>%
    dplyr::select(area_id, sex, area_id_out, sex_out)

  strat_mean_counts_out <- strat_mean_counts_model %>%
    dplyr::left_join(A_0to4_long, by = c("area_id", "sex")) %>%
    dplyr::count(area_id = area_id_out, sex = sex_out, age_group, calendar_quarter, indicator,
                 wt = mean_strat, name = "distribution") %>%
    dplyr::group_by(area_id, sex, calendar_quarter, indicator) %>%
    dplyr::mutate(distribution = tidyr::replace_na(distribution / sum(distribution), 0)) %>%
    dplyr::ungroup()

  out_0to4strat_counts <- out0to4 %>%
    dplyr::inner_join(strat_mean_counts_out,
                      by = c("area_id", "sex", "calendar_quarter", "indicator")) %>%
    tidyr::pivot_longer(c(mean, se, median, mode, lower, upper)) %>%
    dplyr::mutate(value = value * distribution,
                  distribution = NULL) %>%
    tidyr::pivot_wider()

  out_0to4strat_rates <- out_0to4strat_counts %>%
    tidyr::pivot_wider(c("area_id", "sex", "calendar_quarter", "age_group"),
                       names_from = indicator, values_from = mean)

  out_0to4strat_rates <- out_0to4strat_rates %>%
    dplyr::mutate(prevalence = plhiv / population,
                  art_coverage = art_current_residents / plhiv,
                  incidence = infections / (population - plhiv))

  if (naomi_mf$output_aware_plhiv) {
    out_0to4strat_rates$aware_plhiv_prop = out_0to4strat_rates$aware_plhiv_num / out_0to4strat_rates$plhiv
  } else {
    out_0to4strat_rates$aware_plhiv_prop <- NA_real_
  }

  out_0to4strat_rates <- out_0to4strat_rates %>%
    tidyr::pivot_longer(c(prevalence, art_coverage, aware_plhiv_prop, incidence),
                        names_to = "indicator", values_to = "ratio") %>%
    dplyr::select(area_id, sex, calendar_quarter, age_group, indicator, ratio) %>%
    dplyr::inner_join(out0to4, by = c("area_id", "sex", "calendar_quarter", "indicator")) %>%
    dplyr::mutate(ratio = dplyr::if_else(mean == 0, 0, ratio / mean)) %>%
    tidyr::pivot_longer(c(mean, se, median, mode, lower, upper)) %>%
    dplyr::mutate(value = value * ratio,
                  ratio = NULL) %>%
    tidyr::pivot_wider()

  indicators <- dplyr::bind_rows(output$indicators, out_0to4strat_counts, out_0to4strat_rates) %>%
    dplyr::arrange(forcats::fct_inorder(area_id),
                   forcats::fct_inorder(calendar_quarter),
                   forcats::fct_inorder(indicator),
                   forcats::fct_inorder(sex),
                   forcats::fct_inorder(age_group))

  output$indicators <- indicators

  output
}
