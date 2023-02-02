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


  if (!is.null(output[["spectrum_calibration"]])) {
    stop("Outputs have already been calibrated. ",
         "You cannot re-calibrate outputs. ",
         "Please provide uncalibrated output package.")
  }

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
                    c("population", "plhiv", "art_current_residents", "art_current", "plhiv_attend", "untreated_plhiv_attend",
                      "aware_plhiv_num", "unaware_plhiv_num", "infections")) %>%
    dplyr::inner_join(mf, by = c("area_id", "sex", "age_group")) %>%
    dplyr::select(area_id, indicator, tidyselect::all_of(group_vars), mean)


  ## Table of target values from Spectrum
  spectrum_calibration <- naomi_mf$spectrum_calibration %>%
    dplyr::mutate(age_coarse = dplyr::if_else(
                                        age_group %in% c("Y000_004", "Y005_009", "Y010_014"),
                                        "Y000_014", "Y015_999"))

  ## Join aggregates of raw Naomi mean to save in calibration output
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
    proportion_raw[proportion_raw >= 1 & proportion_raw < 1+1e-5] <- 1.0
    proportion_raw[proportion_raw <= 0 & proportion_raw > -1e-5] <- 0.0

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
    opt <- stats::optimise(optfn, c(-10, 10), tol = .Machine$double.eps^0.5)

    adjust_numerator(opt$minimum, logit_p_fine, denominator_new)
  }

  calibrate_proportional_one <- function(value_raw, target_val) {

    stopifnot(target_val == target_val[1])
    target_val <- target_val[1]

    value_raw * target_val / sum(value_raw)
  }

  calibrate_one <- function(value_raw, proportion_raw, denominator_new,
                            target_val, calibrate_method) {

    ## Don't calibrate for T4 and T5 where some outputs not produced
    if(all(is.na(value_raw))) {
      return(value_raw)
    }

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

    ## Calibrate PLHIV attending
    ## Note: aggregate based on calibrated values for valmean_wide$plhiv

    plhivattend_aggr_var <- get_spectrum_aggr_var(spectrum_plhiv_calibration_level,
                                                  "sex_age_group")

    plhivattend_target <- valmean_wide %>%
      dplyr::group_by_at(plhivattend_aggr_var) %>%
      dplyr::summarise(plhivattend_target = sum(plhiv),
                       .groups = "drop")

    valmean_wide <- valmean_wide %>%
      dplyr::left_join(plhivattend_target, by = plhivattend_aggr_var)

    valmean_wide <- valmean_wide %>%
      dplyr::group_by_at(plhivattend_aggr_var) %>%
      dplyr::mutate(
        plhiv_attend = calibrate_one(value_raw = plhiv_attend,
                                     proportion_raw = NA,
                                     denominator_new = NA,
                                     target_val = plhivattend_target,
                                     calibrate_method = "proportional")
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
        art_current = calibrate_one(value_raw = art_current,
                                    proportion_raw = NA,
                                    denominator_new = NA,
                                    target_val = artattend_target,
                                    calibrate_method = "proportional")
      )
  }
  valmean_wide$untreated_plhiv_num = valmean_wide$plhiv - valmean_wide$art_current_residents
  valmean_wide$untreated_plhiv_attend = valmean_wide$plhiv_attend - valmean_wide$art_current

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

  val_adj <- valmean_wide %>%
    tidyr::pivot_longer(cols = c(population, plhiv, art_current_residents, art_current,
                                 untreated_plhiv_num, plhiv_attend, untreated_plhiv_attend,
                                 unaware_plhiv_num, aware_plhiv_num, infections),
                        names_to = "indicator", values_to = "adjusted")


  ## Aggregate calibrated values to Spectrum region level to save in calibration
  ## output.

  val_adj_aggr <- val_adj %>%
    dplyr::group_by_at(c("indicator", group_vars)) %>%
    dplyr::summarise(est_calibrated = sum(adjusted), .groups = "drop")

  val_adj_aggr_wide <- val_adj_aggr %>%
    dplyr::filter(
             indicator %in% c("plhiv", "art_current_residents",
                              "unaware_plhiv_num", "infections")
           ) %>%
    dplyr::mutate(
             indicator = indicator %>%
               dplyr::recode("plhiv" = "plhiv_calibrated",
                             "art_current_residents" = "art_current_calibrated",
                             "unaware_plhiv_num" = "unaware_calibrated",
                             "infections" = "infections_calibrated")
           ) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = est_calibrated)

  if (is.null(val_adj_aggr_wide[["unaware_calibrated"]])) {
    val_aggr_wide[["unaware_calibrated"]] <- NA_real_
  }

  spectrum_calibration <- spectrum_calibration %>%
    dplyr::left_join(val_adj_aggr_wide, by = group_vars)


  spectrum_calibration <- spectrum_calibration %>%
    dplyr::select(spectrum_region_code, spectrum_region_name, sex, age_group, calendar_quarter,
                  population_spectrum, population_raw, population_calibrated,
                  plhiv_spectrum, plhiv_raw, plhiv_calibrated,
                  art_current_spectrum, art_current_raw, art_current_calibrated,
                  unaware_spectrum, unaware_raw, unaware_calibrated,
                  infections_spectrum, infections_raw, infections_calibrated)


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
                  .expand(naomi_mf$calendar_quarter1, "plhiv_attend"),
                  .expand(naomi_mf$calendar_quarter2, "plhiv_attend"),
                  .expand(naomi_mf$calendar_quarter3, "plhiv_attend"),
                  .expand(naomi_mf$calendar_quarter1, "untreated_plhiv_attend"),
                  .expand(naomi_mf$calendar_quarter2, "untreated_plhiv_attend"),
                  .expand(naomi_mf$calendar_quarter3, "untreated_plhiv_attend"),
                  .expand(naomi_mf$calendar_quarter1, "unaware_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter2, "unaware_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter3, "unaware_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter1, "aware_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter2, "aware_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter3, "aware_plhiv_num"),
                  .expand(naomi_mf$calendar_quarter1, "infections"),
                  .expand(naomi_mf$calendar_quarter2, "infections"),
                  .expand(naomi_mf$calendar_quarter3, "infections"),

                  .expand(naomi_mf$calendar_quarter4, "plhiv"),
                  .expand(naomi_mf$calendar_quarter4, "plhiv_attend"),
                  .expand(naomi_mf$calendar_quarter4, "infections"),

                  .expand(naomi_mf$calendar_quarter5, "plhiv"),
                  .expand(naomi_mf$calendar_quarter5, "plhiv_attend"),
                  .expand(naomi_mf$calendar_quarter5, "infections")
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
             ratio = NULL)

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
