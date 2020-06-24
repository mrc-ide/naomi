#' Model Frame and Linear Transform for Aggregated Model Outputs
#'
#' @param mf_model Model frame
#' @param area_aggregation data.frame with columns `area_id` and `model_area_id`.
#'
#' @export
naomi_output_frame <- function(mf_model, area_aggregation) {

  age_groups <- unique(mf_model$age_group)

  model_area_ids <- unique(mf_model$area_id)

  sexes <- unique(mf_model$sex)
  sex_out <- get_sex_out(sexes)

  sex_join <- data.frame(sex_out = c("male", "female", "both", "both", "both"),
                         sex = c("male", "female", "male", "female", "both"),
                         stringsAsFactors = FALSE) %>%
    dplyr::filter(sex %in% sexes, sex_out %in% !!sex_out)

  age_group_out <- get_age_group_out(age_groups)

  age_group_join <- get_age_groups() %>%
    dplyr::filter(age_group %in% age_group_out) %>%
    stats::setNames(paste0(names(.), "_out")) %>%
    tidyr::crossing(get_age_groups() %>%
                    dplyr::filter(age_group %in% age_groups)) %>%
    dplyr::filter(age_group_start_out <= age_group_start,
                  age_group_span_out == Inf |
                  (age_group_start + age_group_span) <=
                  (age_group_start_out + age_group_span_out)) %>%
    dplyr::select(age_group_out, age_group)

  stopifnot(age_groups %in% age_group_join$age_group)

  mf_out <- tidyr::crossing(
                     area_id = area_aggregation$area_id,
                     sex = sex_out,
                     age_group = age_group_out
  )

  df_join <- tidyr::crossing(area_aggregation, sex_join, age_group_join) %>%
    dplyr::full_join(
             mf_model %>%
             dplyr::select("area_id", "sex", "age_group", "idx"),
      by = c("model_area_id" = "area_id",
             "sex", "age_group")) %>%
    dplyr::full_join(
             mf_out %>%
             dplyr::mutate(out_idx = dplyr::row_number())
            ,
             by = c("area_id" = "area_id",
                    "sex_out" = "sex",
                    "age_group_out" = "age_group")
           ) %>%
    dplyr::mutate(x = 1)

  A <- Matrix::spMatrix(nrow(mf_out),
                        nrow(mf_model),
                        df_join$out_idx,
                        df_join$idx,
                        df_join$x)

  list(mf = mf_out, A = A)
}


#' Construct Model Frames and Adjacency Structures
#'
#' @param area_merged Merged version of area hierarchy
#' @param population_agesex Population by age group and sex
#' @param spec Spec
#' @param scope The collection of area IDs to be modelled. Defaults to all area
#' ids.
#' @param level Admin level
#' @param calendar_quarter1 Calendar quarter at time 1 ("CYyyyyQq")
#' @param calendar_quarter2 Calendar quarter at time 2 ("CYyyyyQq")
#' @param calendar_quarter3 Calendar quarter at time 3 ("CYyyyyQq")
#' @param age_groups Age groups to include in model frame
#' @param sexes Sexes
#' @param omega Omega
#' @param rita_param rita_param
#' @param sigma_u_sd sigma_u_sd
#' @param artattend logical; whether to estimate neighboring district ART attendance
#' @param artattend_t2 logical; whether to allow time-varying neighboring district ART attendance
#' @param artattend_log_gamma_offset logit offset for neigboring district ART attendance
#' @param rho_paed_15to49f_ratio logical; to model paediatric prevalence as ratio of 15-49 female prevalence
#' @param rho_paed_x_term logical; to include area interaction for paediatric prevalence
#' @param logit_nu_mean mean of logit viral load suppression.
#' @param logit_nu_sd standard deviation of logit viral load suppression.
#' @param spectrum_population_calibration character string values "national", "subnational", "none"
#'
#' @return Naomi model frame
#'
#' @details
#'
#' Argument `spectrum_population_calibration` determines whether to calibrate population
#' inputs to match Spectrum population by age and sex. If the Spectrum file is a single
#' national Spectrum file, then options "national" and "subnational" return the same results.
#'
#'
#' @export
naomi_model_frame <- function(area_merged,
                              population_agesex,
                              spec,
                              scope = areas$tree$area_id,
                              level = max(areas$tree$Get("area_level")),
                              calendar_quarter1,
                              calendar_quarter2,
                              calendar_quarter3,
                              age_groups = get_five_year_age_groups(),
                              sexes = c("male", "female"),
                              omega = 0.7,
                              rita_param = list(OmegaT0      = 130 / 365,
                                                sigma_OmegaT = ((142-118) / 365) / (2*qnorm(0.975)),
                                                betaT0       = 0.0,
                                                sigma_betaT  = 0.00001,
                                                ritaT        = 1.0),
                              sigma_u_sd   = 1.0,
                              artattend = TRUE,
                              artattend_t2 = FALSE,
                              artattend_log_gamma_offset = -4,
                              rho_paed_15to49f_ratio = FALSE,
                              rho_paed_x_term = FALSE,
                              logit_nu_mean = 2.0,
                              logit_nu_sd = 0.3,
                              spectrum_population_calibration = "national") {

  ## Create area tree
  ## TODO: Get rid of reliance on data.tree
  areas <- create_areas(area_merged = area_merged)

  ## Prune areas below model level
  data.tree::Prune(areas$tree, function(x) x$area_level <= level)

  ## Get leaves that are children of scope
  area_id_leaves <- areas$tree$Get("leaves", traversal = "level")

  if (length(setdiff(scope, names(area_id_leaves)))) {
    missing_areas = setdiff(scope, names(area_id_leaves))
    stop(t_("SCOPE_AREAS_MISSING_HIERARCHY",
            list(missing_areas = paste(missing_areas, collapse = ", ")),
            count = length(missing_areas)))
  }

  area_id <- area_id_leaves[scope] %>%
    lapply(data.tree::Get, "area_id") %>%
    unlist() %>%
    unique()

  spectrum_region_code <- area_id_leaves[scope] %>%
    lapply(data.tree::Get, "spectrum_region_code") %>%
    unlist()

  ## Keep

  mf_areas <- data.frame(area_id,
                         area_idx = seq_along(area_id),
                         spectrum_region_code = spectrum_region_code,
                         stringsAsFactors = FALSE) %>%
    dplyr::mutate(area_idf = factor(area_id, area_id))

  ## Model frame
  mf_model <- tidyr::crossing(
                       mf_areas,
                       sex = sexes,
                       age_group = age_groups
                     ) %>%
    dplyr::mutate(idx = dplyr::row_number()) %>%
    dplyr::left_join(
             get_age_groups() %>%
             dplyr::filter(age_group %in% age_groups) %>%
             dplyr::mutate(
                      age_below15 = as.integer(!age_group_start >= 15),
                      age15plus = as.integer(age_group_start >= 15),
                      age15to49 = as.integer(age_group_start >= 15 &
                                             (age_group_start + age_group_span) <= 50)
                    ) %>%
             dplyr::select(age_group, age_below15, age15plus, age15to49),
             by = "age_group"
           ) %>%
    dplyr::mutate(area_idf = forcats::as_factor(area_id),
                  age_group_idf = forcats::as_factor(age_group),
                  female_15plus = as.integer((sex == "female") * age15plus))

  ## Spectrum aggregation and calibration population

  quarter_id1 <- calendar_quarter_to_quarter_id(calendar_quarter1)
  quarter_id2 <- calendar_quarter_to_quarter_id(calendar_quarter2)
  quarter_id3 <- calendar_quarter_to_quarter_id(calendar_quarter3)

  stopifnot(quarter_id2 > quarter_id1)
  stopifnot(quarter_id3 > quarter_id2)

  spec_aggr <- spec %>%
    dplyr::filter(dplyr::between(year, year_labels(quarter_id1) - 2, year_labels(quarter_id3) + 2)) %>%
    dplyr::mutate(age_group = cut_naomi_age_group(age),
                  births = dplyr::if_else(is.na(asfr), 0, asfr * totpop)) %>%
    dplyr::group_by(spectrum_region_code, sex, age_group, year) %>%
    dplyr::summarise_at(dplyr::vars(totpop, hivpop, artpop, infections, births), sum) %>%
    dplyr::ungroup()

  ## Add number susceptible in previous year for Spectrum incidence calculation
  group_vars <- c("spectrum_region_code", "year", "sex", "age_group")

  spec_aggr <- spec_aggr %>%
    dplyr::left_join(
             dplyr::mutate(spec_aggr, susc_previous_year = totpop - hivpop,
                           year = year + 1) %>%
             dplyr::select(tidyselect::all_of(group_vars), susc_previous_year),
             by = group_vars
           )

  spectrum_calibration <- dplyr::bind_rows(
                                   get_spec_aggr_interpolation(spec_aggr, calendar_quarter1) %>%
                                   dplyr::mutate(time_step = "quarter1"),
                                   get_spec_aggr_interpolation(spec_aggr, calendar_quarter2) %>%
                                   dplyr::mutate(time_step = "quarter2"),
                                   get_spec_aggr_interpolation(spec_aggr, calendar_quarter3) %>%
                                   dplyr::mutate(time_step = "quarter3")
                                 )

  ## Spectrum age <1 / 1-4 distribution

  spec_0to4strat <- spec %>%
    dplyr::filter(dplyr::between(year, year_labels(quarter_id1) - 2, year_labels(quarter_id3) + 2),
                  age %in% 0:4) %>%
    dplyr::mutate(age_group = dplyr::if_else(age == 0, "00-00", "01-04"),
                  sex = "both",
                  births = 0,
                  susc_previous_year = 0) %>%
    dplyr::group_by(spectrum_region_code, sex, age_group, year) %>%
    dplyr::summarise_at(dplyr::vars(totpop, hivpop, artpop, infections), sum) %>%
    dplyr::mutate(births = 0, susc_previous_year = 0) %>%
    dplyr::ungroup()

  spectrum_0to4distribution <- dplyr::bind_rows(
                                        get_spec_aggr_interpolation(spec_0to4strat, calendar_quarter1),
                                        get_spec_aggr_interpolation(spec_0to4strat, calendar_quarter2),
                                        get_spec_aggr_interpolation(spec_0to4strat, calendar_quarter3)
                                      ) %>%
    dplyr::group_by(spectrum_region_code, calendar_quarter) %>%
    dplyr::transmute(age_group,
                     population = population_spectrum / sum(population_spectrum),
                     plhiv = plhiv_spectrum / sum(plhiv_spectrum),
                     art_num_attend = art_num_spectrum / sum(art_num_spectrum),
                     art_num_residents = art_num_attend,
                     infections = infections_spectrum / sum(infections_spectrum)) %>%
    tidyr::gather(indicator, distribution, population, plhiv, art_num_attend, art_num_residents, infections) %>%
    dplyr::ungroup()


  ## # Add population estimates

  ## !!! TODO: There's an opportunity for real mess here if areas are subset to part
  ##           of a Spectrum file and then calibrated. Currently no way to know if areas
  ##           comparise only part of a Spectrum file, so can't address.

  pop_subset <- dplyr::filter(population_agesex, area_id %in% mf_areas$area_id)
  pop_t1 <- interpolate_population_agesex(pop_subset, calendar_quarter1)
  pop_t2 <- interpolate_population_agesex(pop_subset, calendar_quarter2)
  pop_t3 <- interpolate_population_agesex(pop_subset, calendar_quarter3)
  population_est <- dplyr::bind_rows(
                             dplyr::mutate(pop_t1, time_step = "quarter1"),
                             dplyr::mutate(pop_t2, time_step = "quarter2"),
                             dplyr::mutate(pop_t3, time_step = "quarter3")
                           )

  population_est <- population_est %>%
    dplyr::left_join(
             dplyr::select(mf_areas, area_id, spectrum_region_code),
             by = "area_id"
           )


  ## Calibrate population to Spectrum populations

  group_vars <- c("spectrum_region_code", "time_step", "calendar_quarter", "sex", "age_group")

  spectrum_calibration <- spectrum_calibration %>%
    dplyr::left_join(
             dplyr::count(population_est,
                          spectrum_region_code, time_step, calendar_quarter, sex, age_group,
                          wt = population, name = "population_raw"),
             by = group_vars
           )

  if(spectrum_population_calibration %in% c("national", "subnational")) {

    if(spectrum_population_calibration == "national") {
      aggr_vars <- setdiff(group_vars, "spectrum_region_code")
    } else {
      aggr_vars <- group_vars
    }

    spectrum_calibration <- spectrum_calibration %>%
      dplyr::group_by_at(aggr_vars) %>%
      dplyr::mutate(population_calibration = sum(population_spectrum) / sum(population_raw),
                    population = population_raw * population_calibration) %>%
      dplyr::ungroup()

    population_est <- population_est %>%
      dplyr::left_join(
               dplyr::select(spectrum_calibration,
                             tidyselect::all_of(group_vars),
                             population_calibration),
               by = group_vars
             ) %>%
      dplyr::mutate(population = population * population_calibration)

  } else if(spectrum_population_calibration == "none") {
    spectrum_calibration$population_calibration <- 1.0
    spectrum_calibration$population <- spectrum_calibration$population_raw
  } else {
    stop(paste0("spectrum_calibration_option \"", spectrum_population_calibration, "\" not found."))
  }


  mf_model <- mf_model %>%
    dplyr::left_join(
             population_est %>%
             dplyr::filter(time_step == "quarter1") %>%
             dplyr::select(area_id, sex, age_group, population_t1 = population),
             by = c("area_id", "sex", "age_group")
           ) %>%
    dplyr::left_join(
             population_est %>%
             dplyr::filter(time_step == "quarter2") %>%
             dplyr::select(area_id, sex, age_group, population_t2 = population),
             by = c("area_id", "sex", "age_group")
           ) %>%
    dplyr::left_join(
             population_est %>%
             dplyr::filter(time_step == "quarter3") %>%
             dplyr::select(area_id, sex, age_group, population_t3 = population),
             by = c("area_id", "sex", "age_group")
           )

  stopifnot(!is.na(mf_model$population_t1))
  stopifnot(!is.na(mf_model$population_t2))
  stopifnot(!is.na(mf_model$population_t3))

  zeropop1 <- mf_model$population_t1 == 0
  zeropop2 <- mf_model$population_t2 == 0
  zeropop3 <- mf_model$population_t3 == 0

  if(any(zeropop1) || any(zeropop2) || any(zeropop3)) {
    warning(paste("Zero population input for", sum(zeropop1) + sum(zeropop2) + sum(zeropop3),
                  "area/age/sex groups.",
                  "Replaced with population 0.1."))
    mf_model$population_t1[zeropop1] <- 0.1
    mf_model$population_t2[zeropop2] <- 0.1
    mf_model$population_t3[zeropop3] <- 0.1
  }


  ## Add Spectrum inputs

  ## TODO::insert flag to aggregate national or regional
  spec_indicators <- spectrum_calibration %>%
    dplyr::transmute(
             spectrum_region_code,
             sex,
             age_group,
             time_step,
             calendar_quarter,
             prevalence = plhiv_spectrum / population_spectrum,
             art_coverage = pmax(pmin(art_num_spectrum / plhiv_spectrum, 0.999), 0.001),
             incidence = infections_spectrum / susc_previous_year_spectrum,
             asfr = births_spectrum / population_spectrum
           )

  mf_model <- mf_model %>%
    dplyr::left_join(
             spec_indicators %>%
             dplyr::filter(time_step == "quarter1") %>%
             dplyr::select(
                      spectrum_region_code,
                      sex,
                      age_group,
                      spec_prev_t1 = prevalence,
                      spec_incid_t1 = incidence,
                      spec_artcov_t1 = art_coverage,
                      asfr_t1 = asfr
                    ),
             by = c("spectrum_region_code", "sex", "age_group")
           ) %>%
    dplyr::left_join(
             spec_indicators %>%
             dplyr::filter(time_step == "quarter2") %>%
             dplyr::select(
                      spectrum_region_code,
                      sex,
                      age_group,
                      spec_prev_t2 = prevalence,
                      spec_incid_t2 = incidence,
                      spec_artcov_t2 = art_coverage,
                      asfr_t2 = asfr
                    ),
             by = c("spectrum_region_code", "sex", "age_group")
           ) %>%
    dplyr::left_join(
             spec_indicators %>%
             dplyr::filter(time_step == "quarter3") %>%
             dplyr::select(
                      spectrum_region_code,
                      sex,
                      age_group,
                      spec_prev_t3 = prevalence,
                      spec_incid_t3 = incidence,
                      spec_artcov_t3 = art_coverage,
                      asfr_t3 = asfr
                    ),
             by = c("spectrum_region_code", "sex", "age_group")
           )



  ## Projection matrix
  quarter_id1 <- calendar_quarter_to_quarter_id(calendar_quarter1)
  quarter_id2 <- calendar_quarter_to_quarter_id(calendar_quarter2)
  quarter_id3 <- calendar_quarter_to_quarter_id(calendar_quarter3)

  Lproj <- create_Lproj(spec, mf_model, quarter_id1, quarter_id2, quarter_id3)
  projection_duration <- (quarter_id2 - quarter_id1) / 4
  projection_duration_t2t3 <- (quarter_id3 - quarter_id2) / 4

  ## Adjacency matrix
  mf_areas_sf <- mf_areas
  mf_areas_sf$geometry <- areas$boundaries[area_id]
  mf_areas_sf <- sf::st_as_sf(mf_areas_sf)
  M <- create_adj_matrix(mf_areas_sf)


  ## Scaled  precision matrix for 'BYM2' model.
  Q  <- scale_gmrf_precision(diag(rowSums(M)) - M)

  ## Model output

  area_aggregation <- create_area_aggregation(model_area_ids = mf_model$area_id,
                                              areas = areas,
                                              drop_partial_areas = TRUE)
  outf <- naomi_output_frame(mf_model, area_aggregation)
  


  ## ART attendance model

  artattendM <- if(artattend) M else matrix(0, nrow(M), ncol(M))

  mf_areas <- mf_areas %>%
    dplyr::left_join(
             data.frame(area_idx = seq_len(nrow(artattendM)),
                        n_neighbors = colSums(artattendM)),
             by = "area_idx"
           )

  mf_artattend <- (artattendM + diag(nrow(artattendM))) %>%
    methods::as("dgCMatrix") %>%
    Matrix::summary() %>%
    dplyr::rename(reside_area_idx = i,
                  attend_area_idx = j,) %>%
    dplyr::left_join(
             dplyr::select(mf_areas, reside_area_id = area_id, reside_area_idx = area_idx),
             by = "reside_area_idx"
           ) %>%
    dplyr::left_join(
             dplyr::select(mf_areas, attend_area_id = area_id, attend_area_idx = area_idx),
             by = "attend_area_idx"
           ) %>%
    dplyr::transmute(reside_area_id,
                     attend_area_id,
                     reside_area_idx,
                     attend_area_idx,
                     istar = as.integer(reside_area_idx == attend_area_idx),
                     jstar = as.integer(reside_area_idx == attend_area_idx)) %>%
    dplyr::arrange(reside_area_idx, istar, attend_area_idx, jstar) %>%
    dplyr::mutate(attend_idx = dplyr::row_number(),
                  attend_area_idf = forcats::as_factor(attend_area_idx),
                  log_gamma_offset = dplyr::if_else(jstar == 1, 0, as.numeric(artattend_log_gamma_offset)))


  ## Incidence model

  mf_model <- mf_model %>%
    dplyr::group_by(area_id) %>%
    dplyr::mutate(
             spec_prev15to49_t1 = sum(population_t1 * spec_prev_t1 * age15to49) / sum(population_t1 * age15to49),
             spec_artcov15to49_t1 =
               sum(population_t1 * spec_prev_t1 * spec_artcov_t1 * age15to49) /
               sum(population_t1 * spec_prev_t1 * age15to49),
             spec_prev15to49_t2 = sum(population_t2 * spec_prev_t2 * age15to49) / sum(population_t2 * age15to49),
             spec_artcov15to49_t2 =
               sum(population_t2 * spec_prev_t2 * spec_artcov_t2 * age15to49) /
               sum(population_t2 * spec_prev_t2 * age15to49),
             spec_prev15to49_t3 = sum(population_t3 * spec_prev_t3 * age15to49) / sum(population_t3 * age15to49),
             spec_artcov15to49_t3 =
               sum(population_t3 * spec_prev_t3 * spec_artcov_t3 * age15to49) /
               sum(population_t3 * spec_prev_t3 * age15to49),
             logit_rho_offset = 0,
             logit_alpha_offset = 0,
             logit_alpha_t1t2_offset = qlogis(spec_artcov_t2) - qlogis(spec_artcov_t1),
             logit_alpha_t2t3_offset = qlogis(spec_artcov_t3) - qlogis(spec_artcov_t2),
             log_lambda_t1_offset = log(spec_incid_t1) - log(spec_prev15to49_t1) - log(1 - omega * spec_artcov15to49_t1),
             log_lambda_t2_offset = log(spec_incid_t2) - log(spec_prev15to49_t2) - log(1 - omega * spec_artcov15to49_t2),
             log_lambda_t3_offset = log(spec_incid_t3) - log(spec_prev15to49_t3) - log(1 - omega * spec_artcov15to49_t3),
             log_lambda_t1_offset = dplyr::if_else(age_group == "00-04", -Inf, log_lambda_t1_offset),
             log_lambda_t2_offset = dplyr::if_else(age_group == "00-04", -Inf, log_lambda_t2_offset),
             log_lambda_t3_offset = dplyr::if_else(age_group == "00-04", -Inf, log_lambda_t3_offset)
           ) %>%
    dplyr::ungroup()

  ## Paediatric prevalence ratio model
  mf_model <- mf_model %>%
    dplyr::group_by(area_id) %>%
    dplyr::mutate(
             spec_prev15to49f_t1 = sum(population_t1 * spec_prev_t1 * age15to49 * female_15plus) / sum(population_t1 * age15to49 * female_15plus),
             paed_rho_ratio = dplyr::if_else(age_group %in% c("00-04", "05-09", "10-14"), spec_prev_t1 / spec_prev15to49f_t1, 0),
             bin_rho_model = if(rho_paed_15to49f_ratio) as.integer(!age_group %in% c("00-04", "05-09", "10-14")) else 1.0,
             spec_prev15to49f_t1 = NULL
           ) %>%
    dplyr::ungroup()

  ## Remove unneeded columns from spectrum_calibration
  spectrum_calibration$susc_previous_year_spectrum <- NULL
  spectrum_calibration$births_spectrum <- NULL

  v <- list(mf_model = mf_model,
            mf_out = outf$mf,
            mf_areas = mf_areas,
            mf_artattend = mf_artattend,
            artattend_t2 = artattend_t2,
            rho_paed_x_term = rho_paed_x_term,
            area_aggregation = area_aggregation,
            A_out = outf$A,
            Lproj_hivpop = Lproj$Lproj_hivpop,
            Lproj_incid = Lproj$Lproj_incid,
            Lproj_paed = Lproj$Lproj_paed,
            Lproj_hivpop_t1t2 = Lproj$Lproj_hivpop_t1t2,
            Lproj_incid_t1t2 = Lproj$Lproj_incid_t1t2,
            Lproj_paed_t1t2 = Lproj$Lproj_paed_t1t2,
            projection_duration = projection_duration,
            Lproj_hivpop_t2t3 = Lproj$Lproj_hivpop_t2t3,
            Lproj_incid_t2t3 = Lproj$Lproj_incid_t2t3,
            Lproj_paed_t2t3 = Lproj$Lproj_paed_t2t3,
            projection_duration_t2t3 = projection_duration_t2t3,
            areas = area_merged,
            age_groups = age_groups,
            sexes = sexes,
            calendar_quarter1 = calendar_quarter1,
            calendar_quarter2 = calendar_quarter2,
            calendar_quarter3 = calendar_quarter3,
            spectrum_calibration = spectrum_calibration,
            calibration_options = list(spectrum_population_calibration = spectrum_population_calibration),
            spectrum_0to4distribution = spectrum_0to4distribution,
            omega = omega,
            rita_param = rita_param,
            logit_nu_mean = logit_nu_mean,
            logit_nu_sd = logit_nu_sd,
            M = M,
            Q = Q)

  class(v) <- "naomi_mf"
  v
}

#' Select data for model fitting
#'
#' @param naomi_mf A Naomi model frame object.
#' @param survey_hiv_indicators Data frame of survey estimates, or NULL to exclude any survey data.
#' @param anc_testing Data frame of ANC routine testing outcomes, or NULL to exclude any ANC data.
#' @param art_number Data frame of number currently receiving ART, or NULL to exclude any ART data.
#' @param prev_survey_ids A character vector of `survey_id`s for prevalence data.
#' @param artcov_survey_ids A character vector of `survey_id`s for ART coverage data.
#' @param recent_survey_ids A character vector of `survey_id`s for recent HIV infection status.
#' @param vls_survey_ids A character vector of `survey_id`s for survey VLS among all HIV+ persons.
#' @param artnum_calendar_quarter_t1 Calendar quarter for first time point for number on ART.
#' @param artnum_calendar_quarter_t2 Calendar quarter for second time point for number on ART.
#' @param anc_prev_year_t1 Calendar year (possibly multiple) for first time point for ANC prevalence.
#' @param anc_prev_year_t2 Calendar year (possibly multiple) for second time point for ANC prevalence.
#' @param anc_artcov_year_t1 Calendar year (possibly multiple) for first time point for ANC ART coverage.
#' @param anc_artcov_year_t2 Calendar year (possibly multiple) for second time point for ANC ART coverage.
#' @param deff_prev Approximate design effect for survey prevalence.
#' @param deff_artcov Approximate design effect for survey ART coverage.
#' @param deff_recent Approximate design effect for survey proportion recently infected.
#' @param deff_vls Approximate design effect for survey viral load suppression.
#'
#' @details
#' See example datasets for examples of required template for data sets. *`_survey_ids` must be reflected
#' in `survey_hiv_indicators`.
#'
#' ART coverage and VLS survey data should not be included from the same survey. This is checked
#' by the function call and will throw an error.
#'
#' The `deff_*` arguments are approximate design effects used to scale the effective sample size for survey
#' observations. Stratified design effects are will not be the same as full survey DEFF and there is not
#' a straightforward way to approximate these.
#'
#' @seealso [mwi_survey_hiv_indicators], [mwi_anc_testing], [mwi_art_number], [convert_quarter_id]
#'
#' @export
select_naomi_data <- function(naomi_mf,
                              survey_hiv_indicators,
                              anc_testing,
                              art_number,
                              prev_survey_ids,
                              artcov_survey_ids,
                              recent_survey_ids,
                              vls_survey_ids = NULL,
                              artnum_calendar_quarter_t1 = naomi_mf$calendar_quarter1,
                              artnum_calendar_quarter_t2 = naomi_mf$calendar_quarter2,
                              anc_prev_year_t1 = year_labels(calendar_quarter_to_quarter_id(naomi_mf$calendar_quarter1)),
                              anc_prev_year_t2 = year_labels(calendar_quarter_to_quarter_id(naomi_mf$calendar_quarter2)),
                              anc_artcov_year_t1 = anc_prev_year_t1,
                              anc_artcov_year_t2 = anc_prev_year_t2,
                              deff_prev = 1.0,
                              deff_artcov = 1.0,
                              deff_recent = 1.0,
                              deff_vls = 1.0) {

  stopifnot(is(naomi_mf, "naomi_mf"))

  common_surveys <- intersect(artcov_survey_ids, vls_survey_ids)
  if (length(common_surveys)) {
    stop(t_("ART_COV_AND_VLS_SAME_SURVEY",
            list(survey_ids = paste(common_surveys, collapse = ", ")),
            count = length(common_surveys)))
  }

  naomi_mf$prev_dat <- survey_mf(prev_survey_ids, "prev", survey_hiv_indicators, naomi_mf, deff = deff_prev)
  naomi_mf$artcov_dat <- survey_mf(artcov_survey_ids, "artcov", survey_hiv_indicators, naomi_mf, deff = deff_artcov)
  naomi_mf$recent_dat <- survey_mf(recent_survey_ids, "recent", survey_hiv_indicators, naomi_mf,
                                   deff = deff_recent, min_age = 15, max_age = 80)
  naomi_mf$vls_dat <- survey_mf(vls_survey_ids, "vls", survey_hiv_indicators, naomi_mf, deff = deff_vls)

  naomi_mf$anc_prev_t1_dat <- anc_testing_prev_mf(anc_prev_year_t1, anc_testing, naomi_mf)
  naomi_mf$anc_artcov_t1_dat <- anc_testing_artcov_mf(anc_artcov_year_t1, anc_testing, naomi_mf)

  naomi_mf$anc_prev_t2_dat <- anc_testing_prev_mf(anc_prev_year_t2, anc_testing, naomi_mf)
  naomi_mf$anc_artcov_t2_dat <- anc_testing_artcov_mf(anc_artcov_year_t2, anc_testing, naomi_mf)

  naomi_mf$artnum_t1_dat <- artnum_mf(artnum_calendar_quarter_t1, art_number, naomi_mf)
  naomi_mf$artnum_t2_dat <- artnum_mf(artnum_calendar_quarter_t2, art_number, naomi_mf)

  naomi_mf <- update_mf_offsets(naomi_mf,
                                naomi_mf$prev_dat,
                                naomi_mf$artcov_dat,
                                naomi_mf$vls_dat)

  class(naomi_mf) <- c("naomi_data", class(naomi_mf))

  naomi_mf
}

update_mf_offsets <- function(naomi_mf,
                              prev_dat = NULL,
                              artcov_dat = NULL,
                              vls_dat = NULL) {

  stopifnot(is(naomi_mf, "naomi_mf"))

  ## TODO: This should handle different age_max by sex...
  get_idx <- function(mf, df) {

    age_max <- df %>%
      dplyr::left_join(get_age_groups(), by = "age_group") %>%
      dplyr::summarise(age_max = max(age_group_start + age_group_span)) %>%
      .$age_max

    age_min <- df %>%
      dplyr::left_join(get_age_groups(), by = "age_group") %>%
      dplyr::summarise(age_min = min(age_group_start)) %>%
      .$age_min
    
    mf %>%
      dplyr::left_join(get_age_groups(), by = "age_group") %>%
      dplyr::transmute(idx,
                       data_range = as.integer(age_group_start >= age_min & (age_group_start + age_group_span) <= age_max),
                       upper_offset_range = as.integer((age_group_start + age_group_span) >= age_max),
                       lower_offset_range = as.integer(age_group_start <= age_min),
                       age_fct = pmin(age_group_start, max(age_group_start * data_range)),
                       age_fct = pmax(age_fct, min(age_fct / data_range, na.rm = TRUE)),
                       age_fct = factor(age_fct))
                                             
  }


  if(is.null(prev_dat) || nrow(prev_dat) == 0) {
    ## Offset vs. age 15-49 prevalence
    ## No rho_a_fct
    naomi_mf$mf_model <- naomi_mf$mf_model %>%
      dplyr::mutate(
               rho_a_fct = NA,
               logit_rho_offset = qlogis(spec_prev_t1) - qlogis(spec_prev15to49_t1),
             )
  } else {
    d <- get_idx(naomi_mf$mf_model, prev_dat)

    naomi_mf$mf_model <- naomi_mf$mf_model %>%
      dplyr::left_join(d, by = "idx") %>%
      dplyr::group_by(area_id, sex) %>%
      dplyr::mutate(
               rho_a_fct = age_fct,
               age_fct = NULL,
               logit_rho_offset = dplyr::case_when(upper_offset_range == 1 ~ qlogis(spec_prev_t1) - qlogis(max(spec_prev_t1 * data_range * upper_offset_range)),
                                                   lower_offset_range == 1 ~ qlogis(spec_prev_t1) - qlogis(max(spec_prev_t1 * data_range * lower_offset_range)),
                                                   TRUE ~ 0),
               data_range = NULL,
               lower_offset_range = NULL,
               upper_offset_range = NULL
             ) %>%
      dplyr::ungroup()
  }


  if((is.null(artcov_dat) || nrow(artcov_dat) == 0) && (is.null(vls_dat) || nrow(vls_dat) == 0)) {
    ## Offset vs. age 15-49 ART coverage
    ## No alpha_a_fct
    naomi_mf$mf_model <- naomi_mf$mf_model %>%
      dplyr::mutate(
               alpha_a_fct = NA,
               logit_alpha_offset = qlogis(spec_artcov_t1) - qlogis(spec_artcov15to49_t1),
             )

  } else {

    artcov_vls_dat <- dplyr::bind_rows(artcov_dat, vls_dat)
    d <- get_idx(naomi_mf$mf_model, artcov_vls_dat)

    naomi_mf$mf_model <- naomi_mf$mf_model %>%
      dplyr::left_join(d, by = "idx") %>%
      dplyr::group_by(area_id, sex) %>%
      dplyr::mutate(
               alpha_a_fct = age_fct,
               age_fct = NULL,
               logit_alpha_offset = dplyr::case_when(upper_offset_range == 1 ~ qlogis(spec_artcov_t1) - qlogis(max(spec_artcov_t1 * data_range * upper_offset_range)),
                                                     lower_offset_range == 1 ~ qlogis(spec_artcov_t1) - qlogis(max(spec_artcov_t1 * data_range * lower_offset_range)),
                                                     TRUE ~ 0),
               data_range = NULL,
               lower_offset_range = NULL,
               upper_offset_range = NULL
             ) %>%
      dplyr::ungroup()
  }

  naomi_mf
}

#' Get age group ids for output
#'
#' Ensures that age groups are fully spanned by modelled
#' ages.
#'
#' @param age_groups Modelled age groups. Assumed to
#'   be non-overlapping.
#'
#' @keywords internal
get_age_group_out <- function(age_groups) {

  agegr <- get_age_groups() %>%
    dplyr::filter(age_group %in% age_groups) %>%
    dplyr::transmute(mod_agegr_start = age_group_start,
                     mod_agegr_span = age_group_span)


  ## TODO: Check agegr are non overlapping

  v <- get_age_groups() %>%
    tidyr::crossing(agegr) %>%
    dplyr::filter(age_group_start <= mod_agegr_start,
                  age_group_start + age_group_span >= mod_agegr_start + mod_agegr_span) %>%
    dplyr::group_by(age_group) %>%
    dplyr::filter(age_group_start == min(mod_agegr_start)) %>%
    dplyr::ungroup() %>%
    dplyr::count(age_group, age_group_span, wt = mod_agegr_span) %>%
    dplyr::filter(age_group_span == n)

  v$age_group
}


get_sex_out <- function(sexes) {

  if(!all(c("male", "female") %in% sexes))
    sex_out <- sexes
  else
    sex_out <- c("both", "male", "female")

  sex_out
}


#' Prepare model frames for survey datasets
#'
#' @param survey_ids Survey IDs
#' @param indicator Indicator to filter, character string
#' @param survey_hiv_indicators Survey HIV indicators
#' @param naomi_mf Naomi model frame
#' @param deff Assumed design effect for scaling effective sample size
#' @param min_age Min age for calculating recent infection
#' @param max_age Max age for calculating recent infection
#'
#' @export
survey_mf <- function(survey_ids, indicator,
                      survey_hiv_indicators, naomi_mf,
                      deff = 1.0,
                      min_age = 0, max_age = 80) {

  dat <- naomi_mf$mf_model %>%
    dplyr::left_join(get_age_groups(), by = "age_group") %>%
    dplyr::filter(age_group_start >= min_age,
                  age_group_start + age_group_span <= max_age) %>%
    dplyr::inner_join(
             survey_hiv_indicators %>%
             dplyr::filter(survey_id %in% survey_ids,
                           indicator == !!indicator),
             by = c("area_id", "sex", "age_group")
           ) %>%
    dplyr::mutate(n = n_obs,
                  n_eff = n / deff,
                  x_eff = n_eff * est) %>%
    dplyr::select(idx, area_id, age_group, sex, survey_id, n, n_eff, x_eff, est, se)

  dat
}


#' Prepare Model Frames for Programme Datasets
#'
#' @param year Calendar year
#' @param anc_testing ART data frame
#' @param art_number Number on ART
#' @param naomi_mf Naomi model frame
#' @return Calculated prevalence
#'
#' @export
anc_testing_prev_mf <- function(year, anc_testing, naomi_mf) {

  if(is.null(anc_testing) || is.null(year)) {
    ## No ANC prevalence data used
    anc_prev_dat <- data.frame(
      area_id = character(0),
      anc_prev_x = integer(0),
      anc_prev_n = integer(0),
      stringsAsFactors = FALSE
    )
  } else {

    if (!all(year %in% anc_testing$year)) {
      missing_years <- setdiff(year, anc_testing$year)
      stop(t_("ANC_DATA_MISSING_FOR_YEAR",
              list(missing_year = paste(missing_years, collapse = ", ")),
              count = length(missing_years)))
    }

    ## Drop any observations with NA in required columns
    anc_prev_dat <- anc_testing %>%
      dplyr::filter(
               year %in% !!year,
               area_id %in% naomi_mf$mf_model$area_id,
               !is.na(ancrt_known_pos),
               !is.na(ancrt_test_pos),
               !is.na(ancrt_tested)
             ) %>%
      dplyr::group_by(area_id) %>%
      dplyr::summarise_at(dplyr::vars(ancrt_known_pos, ancrt_test_pos, ancrt_tested), sum) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
               area_id,
               anc_prev_x = ancrt_known_pos + ancrt_test_pos,
               anc_prev_n = ancrt_known_pos + ancrt_tested
             )

    if(any(anc_prev_dat$anc_prev_x > anc_prev_dat$anc_prev_n))
      stop(t_("ANC_POSITIVE_GREATER_TOTAL_KNOWN"))

  }

  ## Add area index
  anc_prev_dat <- anc_prev_dat %>%
    dplyr::left_join(
             dplyr::select(naomi_mf$mf_areas, area_id, area_idx),
             by = "area_id"
           ) %>%
    dplyr::select(area_id, area_idx, anc_prev_x, anc_prev_n)

  anc_prev_dat
}


#' @rdname anc_testing_prev_mf
#' @export
anc_testing_artcov_mf <- function(year, anc_testing, naomi_mf) {

  if(is.null(anc_testing) || is.null(year)) {
    ## No ANC ART coverage data used
    anc_artcov_dat <- data.frame(
      area_id = character(0),
      anc_artcov_x = integer(0),
      anc_artcov_n = integer(0),
      stringsAsFactors = FALSE
    )
  } else {

    if (!all(year %in% anc_testing$year)) {
      missing_years <- setdiff(year, anc_testing$year)
      stop(t_("ANC_DATA_MISSING_FOR_YEAR",
              list(missing_year = paste(missing_years, collapse = ", ")),
              count = length(missing_years)))
    }

    ## Drop any observations with NA in required columns
    anc_artcov_dat <- anc_testing %>%
      dplyr::filter(
               year %in% !!year,
               area_id %in% naomi_mf$mf_model$area_id,
               !is.na(ancrt_known_pos),
               !is.na(ancrt_test_pos),
               !is.na(ancrt_already_art)
             )  %>%
      dplyr::group_by(area_id) %>%
      dplyr::summarise_at(dplyr::vars(ancrt_known_pos, ancrt_test_pos, ancrt_already_art), sum) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ancrt_totpos = ancrt_known_pos + ancrt_test_pos) %>%
      dplyr::transmute(
               area_id,
               anc_artcov_x = ancrt_already_art,
               anc_artcov_n = ancrt_totpos
             )

    if(any(anc_artcov_dat$anc_artcov_x > anc_artcov_dat$anc_artcov_n)) {
      stop(t_("ANC_ON_ART_GREATER_THAN_TOTAL_POSITIVE"))
    }

  }

  ## Add area index
  anc_artcov_dat <- anc_artcov_dat %>%
    dplyr::left_join(
             dplyr::select(naomi_mf$mf_areas, area_id, area_idx),
             by = "area_id"
           ) %>%
    dplyr::select(area_id, area_idx, anc_artcov_x, anc_artcov_n)


  anc_artcov_dat
}


#' @rdname anc_testing_prev_mf
#'
#' @param calendar_quarter Calendar quarter
#'
#' @details
#'
#' Number on ART at desired quarter are linearly interpolated within the dataset.
#' If the desired quarter is before the earliest data, the first value may
#' be carried back by up to one year (four quarters). Data are never carried forward
#'
#' @export
artnum_mf <- function(calendar_quarter, art_number, naomi_mf) {

  stopifnot(length(calendar_quarter) <= 1)
  stopifnot(is(naomi_mf, "naomi_mf"))

  if(is.null(calendar_quarter) || is.null(art_number)) {
    ## No number on ART data or no year specified

    artnum_dat <- data.frame(
      area_id = character(0),
      sex = character(0),
      age_group = character(0),
      artnum_idx = integer(0),
      current_art = integer(0),
      stringsAsFactors = FALSE
    )
  } else {

    out_quarter_id <- calendar_quarter_to_quarter_id(calendar_quarter)

    dat <- art_number %>%
      dplyr::semi_join(
        naomi_mf$area_aggregation,
        by = "area_id"
      ) %>%
      dplyr::mutate(
               quarter_id = calendar_quarter_to_quarter_id(calendar_quarter)
      )

    ## Check no areas with duplicated reporting
    art_duplicated_check <- dat %>%
      dplyr::left_join(
        naomi_mf$area_aggregation,
        by = "area_id"
      ) %>%
      dplyr::count(model_area_id, age_group, sex, calendar_quarter) %>%
      dplyr::filter(n > 1)
    
    if (nrow(art_duplicated_check)) {
      stop(paste("ART data multiply reported for some age/sex strata in areas:",
                 paste(unique(art_duplicated_check$model_area_id), collapse = ", ")))
    }
    
    dat <- dat %>%
      dplyr::group_by(area_id, sex, age_group) %>%
      dplyr::summarise(min_data_quarter = min(quarter_id),
                       max_data_quarter = max(quarter_id),
                       current_art = approx(quarter_id, current_art, out_quarter_id, rule =2)$y) %>%
      dplyr::ungroup() %>%
      dplyr::filter(out_quarter_id > min_data_quarter - 4L,
                    out_quarter_id <= max_data_quarter)

    if (nrow(dat) == 0) {
      stop(t_("NO_ART_DATA_FOR_QUARTER",
              list(calendar_quarter = calendar_quarter)))
    }

    artnum_dat <- dat %>%
      dplyr::transmute(
               area_id,
               sex,
               age_group,
               artnum_idx = dplyr::row_number(),
               current_art
             )
  }

  artnum_dat
}
