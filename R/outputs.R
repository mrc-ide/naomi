
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



add_stats <- function(df, mode = NULL, sample = NULL, prefix = "", na.rm = FALSE){

  v <- df

  if(!is.null(mode)) {
    v[[paste0(prefix, "mode")]] <- mode
  } else {
    v[[paste0(prefix, "mode")]] <- NA_real_
  }

  if(!is.null(sample)) {
    qtl <- apply(sample, 1, stats::quantile, c(0.5, 0.025, 0.975), names = FALSE, na.rm = na.rm)
    v[[paste0(prefix, "mean")]] <- rowMeans(sample, na.rm = na.rm)
    rss <- rowSums((sample - v[[paste0(prefix, "mean")]])^2, na.rm = na.rm)
    ndenom <- pmax(rowSums(!is.na(sample)), 2) - 1
    v[[paste0(prefix, "se")]] <- sqrt(rss / ndenom)
    v[[paste0(prefix, "median")]] <- qtl[1,]
    v[[paste0(prefix, "lower")]] <- qtl[2,]
    v[[paste0(prefix, "upper")]] <- qtl[3,]
  } else {
    v[paste0(prefix, c("mean", "se", "median", "lower", "upper"))] <- NA_real_
  }

  v[c(names(df), paste0(prefix, c("mean", "se", "median", "mode", "lower", "upper")))]
}


extract_indicators <- function(naomi_fit, naomi_mf, na.rm = FALSE) {

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
        v <- add_stats(v, naomi_fit$mode[[varname]], naomi_fit$sample[[varname]], na.rm = na.rm)
      } else {
        v <- add_stats(v, naomi_fit$mode[[varname]], na.rm = na.rm)
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
                     "plhiv_attend_t1_out" = "plhiv_attend",
                     "untreated_plhiv_attend_t1_out" = "untreated_plhiv_attend",
                     "lambda_t1_out" = "incidence",
                     "infections_t1_out" = "infections")

  indicators_t2 <- c("population_t2_out" = "population",
                     "rho_t2_out" = "prevalence",
                     "plhiv_t2_out" = "plhiv",
                     "alpha_t2_out" = "art_coverage",
                     "artnum_t2_out" = "art_current_residents",
                     "artattend_t2_out" = "art_current",
                     "untreated_plhiv_num_t2_out" = "untreated_plhiv_num",
                     "plhiv_attend_t2_out" = "plhiv_attend",
                     "untreated_plhiv_attend_t2_out" = "untreated_plhiv_attend",
                     "lambda_t2_out" = "incidence",
                     "infections_t2_out" = "infections")

  indicators_t3 <- c("population_t3_out" = "population",
                     "rho_t3_out" = "prevalence",
                     "plhiv_t3_out" = "plhiv",
                     "alpha_t3_out" = "art_coverage",
                     "artnum_t3_out" = "art_current_residents",
                     "artattend_t3_out" = "art_current",
                     "untreated_plhiv_num_t3_out" = "untreated_plhiv_num",
                     "plhiv_attend_t3_out" = "plhiv_attend",
                     "untreated_plhiv_attend_t3_out" = "untreated_plhiv_attend",
                     "lambda_t3_out" = "incidence",
                     "infections_t3_out" = "infections")

  indicators_t4 <- c("population_t4_out" = "population",
                     "plhiv_t4_out" = "plhiv",
                     "plhiv_attend_t4_out" = "plhiv_attend",
                     "infections_t4_out" = "infections",
                     "lambda_t4_out" = "incidence")

  indicators_t5 <- c("population_t5_out" = "population",
                     "plhiv_t5_out" = "plhiv",
                     "plhiv_attend_t5_out" = "plhiv_attend",
                     "infections_t5_out" = "infections")

  if (naomi_mf$output_aware_plhiv) {

    indicators_t1 <- c(indicators_t1,
                       "aware_plhiv_prop_t1_out" = "aware_plhiv_prop",
                       "aware_plhiv_num_t1_out" = "aware_plhiv_num",
                       "unaware_plhiv_num_t1_out" = "unaware_plhiv_num",
                       "aware_plhiv_attend_t1_out" = "aware_plhiv_attend",
                       "unaware_plhiv_attend_t1_out" = "unaware_plhiv_attend")
    indicators_t2 <- c(indicators_t2,
                       "aware_plhiv_prop_t2_out" = "aware_plhiv_prop",
                       "aware_plhiv_num_t2_out" = "aware_plhiv_num",
                       "unaware_plhiv_num_t2_out" = "unaware_plhiv_num",
                       "aware_plhiv_attend_t2_out" = "aware_plhiv_attend",
                       "unaware_plhiv_attend_t2_out" = "unaware_plhiv_attend")
    indicators_t3 <- c(indicators_t3,
                       "aware_plhiv_prop_t3_out" = "aware_plhiv_prop",
                       "aware_plhiv_num_t3_out" = "aware_plhiv_num",
                       "unaware_plhiv_num_t3_out" = "unaware_plhiv_num",
                       "aware_plhiv_attend_t3_out" = "aware_plhiv_attend",
                       "unaware_plhiv_attend_t3_out" = "unaware_plhiv_attend")
  }

  indicator_est_t1 <- Map(get_est, names(indicators_t1), indicators_t1, naomi_mf$calendar_quarter1)
  indicator_est_t2 <- Map(get_est, names(indicators_t2), indicators_t2, naomi_mf$calendar_quarter2)
  indicator_est_t3 <- Map(get_est, names(indicators_t3), indicators_t3, naomi_mf$calendar_quarter3)
  indicator_est_t4 <- Map(get_est, names(indicators_t4), indicators_t4, naomi_mf$calendar_quarter4)
  indicator_est_t5 <- Map(get_est, names(indicators_t5), indicators_t5, naomi_mf$calendar_quarter5)


  indicator_est_t1 <- dplyr::bind_rows(indicator_est_t1)
  indicator_est_t2 <- dplyr::bind_rows(indicator_est_t2)
  indicator_est_t3 <- dplyr::bind_rows(indicator_est_t3)
  indicator_est_t4 <- dplyr::bind_rows(indicator_est_t4)
  indicator_est_t5 <- dplyr::bind_rows(indicator_est_t5)

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
                  indicator_anc_est_t3,
                  indicator_est_t4,
                  indicator_est_t5
                )

  dplyr::select(out, names(naomi_mf$mf_out),
                calendar_quarter, indicator, mean, se, median, mode, lower, upper)
}

extract_art_attendance <- function(naomi_fit, naomi_mf, na.rm = FALSE) {

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
  v_t1 <- add_stats(v_t1, m_artattend_ij_t1, s_artattend_ij_t1, "artnum_", na.rm = na.rm)
  v_t1 <- add_stats(v_t1, m_prop_residents_t1, s_prop_residents_t1, "prop_residents_", na.rm = na.rm)
  v_t1 <- add_stats(v_t1, m_prop_attendees_t1, s_prop_attendees_t1, "prop_attendees_", na.rm = na.rm)

  v_t2 <- dplyr::mutate(v, calendar_quarter = naomi_mf$calendar_quarter2)
  v_t2 <- add_stats(v_t2, m_artattend_ij_t2, s_artattend_ij_t2, "artnum_", na.rm = na.rm)
  v_t2 <- add_stats(v_t2, m_prop_residents_t2, s_prop_residents_t2, "prop_residents_", na.rm = na.rm)
  v_t2 <- add_stats(v_t2, m_prop_attendees_t2, s_prop_attendees_t2, "prop_attendees_", na.rm = na.rm)

  dplyr::bind_rows(v_t1, v_t2)
}

#' Align model data inputs and model estimates
#'
#' @param naomi_data Naomi object of class "naomi_data" and "naomi_mf"
#' @param indicators Naomi indicators created by `extract_indicators()`
#' @param meta_areas
#'
#' @export
align_inputs_outputs <- function(naomi_data, indicators, meta_area){

  stopifnot(inherits(naomi_data, "naomi_data"))
  stopifnot(inherits(naomi_data, "naomi_mf"))

  options <- naomi_data$model_options

  calendar_quarter1 <- options$calendar_quarter_t1
  calendar_quarter2 <- options$calendar_quarter_t2
  year1 <- calendar_quarter_to_year(calendar_quarter1)
  year2 <- calendar_quarter_to_year(calendar_quarter2)

  # Format survey data
  inputs <- naomi_data$full_data$survey_full_mf %>%
    dplyr::filter(survey_id %in% naomi_data$data_options$prev_survey_ids) %>%
    dplyr::select(area_id, sex, age_group, calendar_quarter = survey_mid_calendar_quarter,
                  indicator, naomi_input, mean = estimate, se = std_error,
                  lower = ci_lower, upper = ci_upper, source = survey_id) %>%
    # Calendar quarter == calendar quarter T1 to line up multiple surveys
    dplyr::mutate(calendar_quarter = calendar_quarter1,
                  median = NA_real_,  mode = NA_real_,
                  year = calendar_quarter_to_year(calendar_quarter))




  # If ART data provided, format ART data and add to inputs
  if(!is.null(naomi_data$full_data$artnum_full_mf)){

    art <- naomi_data$full_data$artnum_full_mf %>%
      dplyr::mutate(indicator = "art_current", se = NA_real_ ,median = NA_real_ ,
                    mode = NA_real_ , lower = NA_real_ , upper = NA_real_,
                    year = calendar_quarter_to_year(calendar_quarter)) %>%
      dplyr::select(mean = art_current, dplyr::everything())

    inputs <- rbind(inputs, art)
  }

  # If ANC data provided, format ART data and add to inputs
  if(!is.null(naomi_data$full_data$anc_full_mf)) {

    anc <- naomi_data$full_data$anc_full_mf %>%
      dplyr::mutate(source = "programme", se = NA_real_ ,median = NA_real_ ,
                    mode = NA_real_ , lower = NA_real_ , upper = NA_real_,
                    calendar_quarter = paste0("CY", year, "Q4")) %>%
      dplyr::select(mean = value, dplyr::everything())

    inputs <- rbind(inputs, anc)

  }


  # Filter inputs for:
  #  - Years contained in model outputs
  #  - Area levels contained in model outputs
  #  - Subset of indicators that can be compared to model outputs

  if(is.null(year1)){year1 <- "NULL_t1"}
  if(is.null(year2)){year2 <- "NULL_t2"}

  level <- options$area_level

  meta_area <- meta_area %>%
    dplyr::select(area_id, area_name, area_level, area_level_label, parent_area_id) %>%
    sf::st_drop_geometry()

 indicator_keep <- c("prevalence", "art_coverage", "art_current", "anc_prevalence",
                 "anc_art_coverage")

 inputs_sub <- inputs %>%
    dplyr::left_join(meta_area, by = "area_id") %>%
    dplyr::filter(indicator %in% indicator_keep,
                  area_level <= level,
                  year %in% c(year1, year2)) %>%
    dplyr::mutate(source = paste0(source," ", calendar_quarter),
                  calendar_quarter = dplyr::case_when(
                    year == year1 ~ calendar_quarter1,
                    year == year2 ~ calendar_quarter2),
                  indicator = dplyr::case_when(
                    indicator == "anc_prevalence" ~ "anc_prevalence_age_matched",
                    indicator == "anc_art_coverage" ~ "anc_art_coverage_age_matched",
                    TRUE ~ indicator)) %>%
    dplyr::select(-c(naomi_input))

  # Filter outputs for:
  #  - Indicator/age/sex/year bands present in inputs

  # Get age and sex matched outputs for ANC comparison
  anc_compare <- indicators %>%
    dplyr::filter(indicator %in% c("prevalence","art_coverage"),
                  sex == "female", age_group == "Y015_049") %>%
    dplyr::mutate(
      indicator = dplyr::case_when(
      indicator == "prevalence" ~ "anc_prevalence_age_matched",
      indicator == "art_coverage" ~ "anc_art_coverage_age_matched"),
      source = paste0("Naomi estimate ", calendar_quarter, " females 15-49"),
      year = calendar_quarter_to_year(calendar_quarter))

  outputs <- indicators %>%
    dplyr::mutate(year = calendar_quarter_to_year(calendar_quarter),
                  source = paste0("Naomi estimate ", calendar_quarter)) %>%
    dplyr::filter(!indicator %in% c("anc_prevalence", "anc_art_coverage")) %>%
    dplyr::bind_rows(anc_compare) %>%
    dplyr::left_join(meta_area, by = "area_id") %>%
    dplyr::select(colnames(inputs_sub))


  # Join to get intersecting strata
  join_cols <- c("area_id", "sex", "age_group", "indicator", "calendar_quarter")

  meta_join <- dplyr::semi_join(outputs %>%
                                  dplyr::select(dplyr::all_of(join_cols)),
                                inputs_sub %>%
                                  dplyr::select(dplyr::all_of(join_cols)),
                                by = join_cols)


  inputs_outputs <- rbind(outputs %>% dplyr::semi_join(meta_join, by = join_cols),
                          inputs_sub %>% dplyr::semi_join(meta_join, by = join_cols))
  }

#' Build output package from fit
#'
#' @param naomi_fit Fitted naomi model
#' @param naomi_data Naomi model frame with data
#' @param na.rm Whether to remove NA values when calculating summary statistics, default FALSE
#'
#' @return List containing output indicators and metadata.
#'
#' @details
#'
#' The argument `na.rm = TRUE` allows the output package to be
#' produced when there are errors due to missing values when
#' generating outputs. This is only for debugging purposes to
#' review results when there are errors. `NA` values in
#' simulated model results typically mean poor model fit or
#' non-convergence that needs to be addressed.
#'
#' @export
output_package <- function(naomi_fit, naomi_data, na.rm = FALSE) {

  stopifnot(is(naomi_fit, "naomi_fit"))
  stopifnot(is(naomi_data, "naomi_data"))

  indicators <- extract_indicators(naomi_fit, naomi_data, na.rm = na.rm)

  art_attendance <- extract_art_attendance(naomi_fit, naomi_data, na.rm = na.rm)

  meta_area <- naomi_data$areas %>%
    dplyr::filter(area_id %in% unique(naomi_data$mf_out$area_id)) %>%
    dplyr::select(area_level, area_level_label, area_id, area_name,
                  parent_area_id, spectrum_region_code, area_sort_order,
                  center_x, center_y, geometry) %>%
    sf::st_as_sf()

  meta_period <- get_period_metadata(c(naomi_data$calendar_quarter1,
                                       naomi_data$calendar_quarter2,
                                       naomi_data$calendar_quarter3,
                                       naomi_data$calendar_quarter4,
                                       naomi_data$calendar_quarter5))
  meta_age_group <- get_age_groups()

  ## # Fitting outputs
  fit <- list()
  fit$model_options <- naomi_data$model_options
  fit$data_options <- naomi_data$data_options
  fit$calibration_options <- naomi_data$calibration_options
  fit$spectrum_calibration <- naomi_data$spectrum_calibration

  meta_indicator <- get_meta_indicator()
  meta_indicator <- dplyr::filter(meta_indicator, indicator %in% indicators$indicator)

  inputs_outputs <- align_inputs_outputs(naomi_data, indicators, meta_area)

  val <- list(
    indicators = indicators,
    art_attendance = art_attendance,
    meta_area = meta_area,
    meta_age_group = meta_age_group,
    meta_period = meta_period,
    meta_indicator = meta_indicator,
    fit = fit,
    inputs_outputs = inputs_outputs
  )

  class(val) <- "naomi_output"

  val
}

is_naomi_output <- function(object) {
  inherits(object, "naomi_output")
}

#' Add labels to output indicators
#'
#' @param naomi_output Naomi output object.
#' @param geometry logical whether to include geometry from meta_area.
#'
#' @return Labelled output indicators
#' @export
add_output_labels <- function(naomi_output, geometry = FALSE) {

  stopifnot(inherits(naomi_output, "naomi_output"))

  indicators <- naomi_output$indicators %>%
    dplyr::select(area_id, sex, age_group, calendar_quarter, indicator,
                  mean, se, median, mode, lower, upper)

  if (geometry) {
    meta_area <- naomi_output$meta_area %>%
      dplyr::select(area_level, area_level_label, area_name, area_sort_order,
                    area_id, center_x, center_y)
  } else {
    meta_area <- naomi_output$meta_area %>%
      sf::st_drop_geometry() %>%
      dplyr::select(area_id, area_level, area_level_label, area_name, area_sort_order)
  }

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

  if(geometry) {
    indicators <- dplyr::select(indicators,
                                area_level,
                                area_level_label,
                                area_sort_order,
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
                                upper,
                                center_x,
                                center_y,
                                geometry) %>%
      sf::st_as_sf()
    } else {
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
                                  upper)
    }

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
#' @param notes Notes to include in output zip
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
                                notes = NULL,
                                overwrite = FALSE,
                                with_labels = FALSE,
                                boundary_format = "geojson",
                                single_csv = FALSE,
                                export_datapack = !single_csv) {

  save_output(filename, dir, naomi_output,
              notes = notes,
              overwrite = overwrite,
              with_labels = with_labels,
              boundary_format = boundary_format,
              single_csv = single_csv,
              export_datapack = export_datapack)
}

save_output_coarse_age_groups <- function(path, naomi_output,
                                          overwrite = FALSE) {

  age_groups_keep <- c("Y015_049", "Y015_064", "Y015_999", "Y050_999", "Y000_999", "Y000_064",
                       "Y000_014", "Y015_024", "Y025_034", "Y035_049", "Y050_064", "Y065_999")
  naomi_output_sub <- subset_naomi_output(naomi_output, age_group = age_groups_keep)

  save_output(basename(path), dirname(path), naomi_output_sub,
              overwrite = overwrite, with_labels = TRUE,
              boundary_format = "geojson", single_csv = FALSE,
              export_datapack = FALSE)
}


save_output_spectrum <- function(path, naomi_output, notes = NULL,
                                 vmmc_path = NULL,
                                 overwrite = FALSE) {
  save_output(basename(path), dirname(path), naomi_output, notes,
              vmmc_path = vmmc_path,
              overwrite = overwrite, with_labels = TRUE,
              boundary_format = "geojson", single_csv = FALSE,
              export_datapack = TRUE)
}

#' Save outputs to zip file
#'
#' @param naomi_output Naomi output object
#' @param options Naomi model options
#' @param filename Name of file to create
#' @param dir Directory to create zip in
#' @param notes Notes to include in output zip
#' @param vmmc_path Path to VMMC excel workbook
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
save_output <- function(filename, dir,
                        naomi_output,
                        notes = NULL,
                        vmmc_path = NULL,
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

  ## re-fetch metadata so that it is in the users current language if they
  ## have switched between running the fit and generating output
  ## only need to do this for indicators and period, meta_age_group is not
  ## translated and meta_area comes from input data so won't change
  meta_indicator <- get_meta_indicator()
  meta_indicator <- dplyr::filter(meta_indicator, indicator %in% naomi_output$indicators$indicator)
  naomi_output$meta_indicator <- meta_indicator

  meta_period <- get_period_metadata(
    c(naomi_output$fit$model_options$calendar_quarter_t1,
      naomi_output$fit$model_options$calendar_quarter_t2,
      naomi_output$fit$model_options$calendar_quarter_t3,
      naomi_output$fit$model_options$calendar_quarter_t4,
      naomi_output$fit$model_options$calendar_quarter_t5))
  naomi_output$meta_period <- meta_period

  if (with_labels) {
    indicators <- add_output_labels(naomi_output)
    art_attendance <- add_art_attendance_labels(naomi_output)
  } else {
    indicators <- naomi_output$indicators
    art_attendance <- naomi_output$art_attendance
  }

  if (!is.null(vmmc_path)) {
    vmmc_path <- normalizePath(vmmc_path)
  }

  tmpd <- tempfile()
  dir.create(tmpd)
  old <- setwd(tmpd)
  on.exit(setwd(old))
  naomi_write_csv(indicators, "indicators.csv")

  if (!is.null(notes)) {
    assert_scalar_character(notes)
    writeLines(notes, "notes.txt")
  }

  if(!single_csv) {
    naomi_write_csv(art_attendance, "art_attendance.csv")
    naomi_write_csv(sf::st_drop_geometry(naomi_output$meta_area),
                    "meta_area.csv")
    naomi_write_csv(naomi_output$meta_age_group, "meta_age_group.csv")
    naomi_write_csv(naomi_output$meta_period, "meta_period.csv")
    naomi_write_csv(naomi_output$meta_indicator, "meta_indicator.csv")
    naomi_write_csv(naomi_output$inputs_outputs, "inputs_outputs.csv")

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

    if (!is.null(vmmc_path)) {
      assert_scalar_character(vmmc_path)
      ## Skip the first row, the file has two rows of headers
      vmmc_datapack <- openxlsx::read.xlsx(vmmc_path, sheet = "Datapack inputs",
                                           startRow = 2)
      ## TODO: Add it to relevant place in download
    }

    write_datapack_csv(naomi_output = naomi_output,
                       path = PEPFAR_DATAPACK_PATH,
                       psnu_level = naomi_output$fit$model_options$psnu_level)
  }


  dir.create("info")
  info <- attr(naomi_output, "info")

  if (length(info) > 0L) {
    for (p in names(info)) {
      writeLines(trimws(info[[p]]), file.path("info", p))
    }
  }

  write_navigator_checklist(naomi_output,
                            "info/unaids_navigator_checklist.csv")

  fit <- naomi_output$fit
  if(length(fit) > 0L) {
    dir.create("fit")
    naomi_write_csv(fit$spectrum_calibration, "fit/spectrum_calibration.csv")
    yaml::write_yaml(fit$model_options, "fit/model_options.yml")
    yaml::write_yaml(fit$data_options, "fit/data_options.yml")
    yaml::write_yaml(fit$calibration_options, "fit/calibration_options.yml")
  }

  # Generate README of output folder contents
  rmarkdown::render(system_file("report/README.Rmd"),
                    output_dir = tmpd,
                    params = list(output_dir = tmpd),
                    quiet = TRUE)

  zip::zipr(path, list.files())
  path
}

#' Generate and save summary report at specified path
#'
#' @param report_path Path to save summary report at
#' @param outputs Path to model outputs rds or zip file
#' @param quiet Suppress printing of the pandoc command line
#'
#' @return Path to summary report
#' @keywords internal
generate_output_summary_report <- function(report_path,
                                           outputs,
                                           quiet = FALSE) {
  report_filename <- basename(report_path)
  report_path_dir <- normalizePath(dirname(report_path), mustWork = TRUE)
  output_file_path <- normalizePath(outputs, mustWork = TRUE)
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
                      params = list(outputs = output_file_path,
                                    lang = t_("LANG")),
                      output_file = report_filename,
                      quiet = quiet
                      )

    fs::file_copy(report_filename, report_path_dir, overwrite = TRUE)
  })
  report_path
}


#' Generate and save comparison report at specified path
#'
#' @param report_path Path to save summary report at
#' @param outputs Path to model outputs rds or zip file
#' @param quiet Suppress printing of the pandoc command line
#'
#' @return Path to summary report
#' @keywords internal
generate_comparison_report <- function(report_path,
                                           outputs,
                                           quiet = FALSE) {
  report_filename <- basename(report_path)
  report_path_dir <- normalizePath(dirname(report_path), mustWork = TRUE)
  output_file_path <- normalizePath(outputs, mustWork = TRUE)
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
    rmarkdown::render("comparison_report.Rmd",
                      params = list(outputs = output_file_path,
                                    lang = t_("LANG")),
                      output_file = report_filename,
                      quiet = quiet
    )

    fs::file_copy(report_filename, report_path_dir, overwrite = TRUE)
  })
  report_path
}

#' @rdname save_output_package
#' @param path Path to output zip file.
#' @export
read_output_package <- function(path) {

  tmpd <- tempfile()
  on.exit(unlink(tmpd))

  utils::unzip(path, exdir = tmpd)

  ## Fit list
  fit <- list()

  if (file.exists(file.path(tmpd, "fit/model_options.yml"))) {
    # If naomi_output saved: Subset model options available in fit object
    fit$model_options <- yaml::read_yaml(file.path(tmpd, "fit/model_options.yml"))
  } else if (file.exists(file.path(tmpd,"info/options.yml"))) {
    # If hintr_output saved: Full model options available in "info"
    fit$model_options <- yaml::read_yaml(file.path(tmpd,"info/options.yml"))
  } else if (file.exists(file.path(tmpd, "info/options.yml"))) {
    # Backwards compatibility for old version of coarse output zip
    fit$model_options <- yaml::read_yaml(file.path(tmpd, "info/options.yml"))
  }

  if (file.exists(file.path(tmpd,"fit/data_options.yml"))) {
    fit$data_options <- yaml::read_yaml(file.path(tmpd,"fit/data_options.yml"))
  }

  if (file.exists(file.path(tmpd,"fit/calibration_options.yml"))) {
    fit$calibration_options <- yaml::read_yaml(file.path(tmpd,"fit/calibration_options.yml"))
  } else if (file.exists(file.path(tmpd,"fit/calibration_options.csv"))) {
    # Backwards compatibility for old version of coarse output zip
    fit$calibration_options <- readr_read_csv(file.path(tmpd, "fit/calibration_options.csv"))
  }

  if (file.exists(file.path(tmpd,"fit/spectrum_calibration.csv"))) {
    fit$spectrum_calibration <- readr_read_csv(file.path(tmpd, "fit/spectrum_calibration.csv"))
  }

  v <- list(
    indicators = readr_read_csv(file.path(tmpd, "indicators.csv")),
    art_attendance = readr_read_csv(file.path(tmpd, "art_attendance.csv")),
    meta_area = sf::read_sf(file.path(tmpd, "boundaries.geojson")),
    meta_age_group = readr_read_csv(file.path(tmpd, "meta_age_group.csv")),
    meta_period = readr_read_csv(file.path(tmpd, "meta_period.csv")),
    meta_indicator = readr_read_csv(file.path(tmpd, "meta_indicator.csv")),
    fit = fit
  )

  if(file.exists(file.path(tmpd, "inputs_outputs.csv"))) {
    v$inputs_outputs <- readr_read_csv(file.path(tmpd, "inputs_outputs.csv"))
  }

  v$meta_area$name <- NULL

  info_files <- list.files(file.path(tmpd, "info"))

  ## unaids_navigator_checklist.csv gets regenerated; don't reload
  info_files <- setdiff(info_files, "unaids_navigator_checklist.csv")

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
             by = c("spectrum_region_code", "calendar_quarter", "indicator"),
             multiple = "all",
             relationship = "many-to-many"
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
    dplyr::left_join(A_0to4_long, by = c("area_id", "sex"), multiple = "all",
                     relationship = "many-to-many") %>%
    dplyr::count(area_id = area_id_out, sex = sex_out, age_group, calendar_quarter, indicator,
                 wt = mean_strat, name = "distribution") %>%
    dplyr::group_by(area_id, sex, calendar_quarter, indicator) %>%
    dplyr::mutate(distribution = tidyr::replace_na(distribution / sum(distribution), 0)) %>%
    dplyr::ungroup()

  out_0to4strat_counts <- out0to4 %>%
    dplyr::inner_join(strat_mean_counts_out,
                      by = c("area_id", "sex", "calendar_quarter", "indicator"),
                      multiple = "all",
                      relationship = "many-to-many") %>%
    tidyr::pivot_longer(c(mean, se, median, mode, lower, upper)) %>%
    dplyr::mutate(value = value * distribution,
                  distribution = NULL) %>%
    tidyr::pivot_wider()

  out_0to4strat_rates <- out_0to4strat_counts %>%
    tidyr::pivot_wider(id_cols = c("area_id", "sex", "calendar_quarter", "age_group"),
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
    tidyr::pivot_longer(cols = c(prevalence, art_coverage, aware_plhiv_prop, incidence),
                        names_to = "indicator", values_to = "ratio") %>%
    dplyr::select(area_id, sex, calendar_quarter, age_group, indicator, ratio) %>%
    dplyr::inner_join(out0to4, by = c("area_id", "sex", "calendar_quarter", "indicator")) %>%
    dplyr::mutate(ratio = dplyr::if_else(mean == 0, 0, ratio / mean)) %>%
    tidyr::pivot_longer(c(mean, se, median, mode, lower, upper)) %>%
    dplyr::mutate(value = value * ratio,
                  ratio = NULL) %>%
    tidyr::pivot_wider()

  age_groups <- unique(out_0to4strat_counts$age_group)
  out_indicators <- output$indicators %>%
    dplyr::filter(!age_group %in% age_groups)

  indicators <- dplyr::bind_rows(out_indicators, out_0to4strat_counts, out_0to4strat_rates) %>%
    dplyr::arrange(forcats::fct_inorder(area_id),
                   forcats::fct_inorder(calendar_quarter),
                   forcats::fct_inorder(indicator),
                   forcats::fct_inorder(sex),
                   forcats::fct_inorder(age_group))

  output$indicators <- indicators

  output
}

get_period_metadata <- function(calendar_quarters) {
  data.frame(
    calendar_quarter = calendar_quarters,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      quarter_id = calendar_quarter_to_quarter_id(calendar_quarter),
      quarter_label = quarter_year_labels(quarter_id)
    )
}


#' Read hintr output
#'
#' Read the hintr model output or plot data saved as a qs or an rds file.
#' This is the data saved from hintr_run_model or hintr_calibrate
#' before the output zip is generated. This uses the file extension
#' to identify the reading function to use.
#'
#' @param path Path to the file
#'
#' @return The read data
#' @export
read_hintr_output <- function(path) {
  type <- tolower(tools::file_ext(path))
  if (type == "qs") {
    qs::qread(path)
  } else if (type == "duckdb") {
    read_duckdb(path)
  } else if (type == "rds") {
    ## Model & plot data output before v2.8.0 were saved as RDS
    readRDS(path)
  } else {
    stop(sprintf(paste("Cannot read hintr data of invalid type, got '%s',",
                       "must be one of rds, qs or duckdb."), type))
  }
}

read_duckdb <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbGetQuery(con, sprintf("SELECT * from %s", DUCKDB_OUTPUT_TABLE_NAME))
}


