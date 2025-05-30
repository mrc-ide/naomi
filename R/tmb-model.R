#' Prepare inputs for TMB model.
#'
#' @param naomi_data  Naomi data object
#' @param report_likelihood Option to report likelihood in fit object (default true).
#' @param anchor_home_district Option to include random effect home district attractiveness to retain residents on ART within home districts (default true).
#'
#' @return Inputs ready for TMB model
#'
#' @seealso [select_naomi_data]
#' @export
prepare_tmb_inputs <- function(naomi_data,
                               report_likelihood = 1L) {

  stopifnot(methods::is(naomi_data, "naomi_data"))
  stopifnot(methods::is(naomi_data, "naomi_mf"))

  ## ANC observation aggregation matrices
  ##
  ## TODO: Refactor code to make the function create_artattend_Amat() more generic.
  ##       Should not refer to 'ART' specific; also useful for ANC attendance,
  ##       fertility, etc.

  create_anc_Amat <- function(anc_obs_dat) {

    df_attend_anc <- naomi_data$mf_model %>%
    dplyr::select(reside_area_id = area_id,
                  attend_area_id = area_id,
                  sex,
                  age_group,
                  idx)

    dat <- dplyr::rename(anc_obs_dat, attend_area_id = area_id)

    Amat <- create_artattend_Amat(
      dat,
      age_groups = naomi_data$age_groups,
      sexes = naomi_data$sexes,
      area_aggregation = naomi_data$area_aggregation,
      df_art_attend = df_attend_anc,
      by_residence = FALSE
    )

    Amat
  }

  create_survey_Amat <- function(survey_dat) {

    df_attend_survey <- naomi_data$mf_model %>%
      dplyr::select(reside_area_id = area_id,
                    attend_area_id = area_id,
                    sex,
                    age_group,
                    idx)

    survey_dat$attend_area_id <- survey_dat$area_id

    Amat <- create_artattend_Amat(
      survey_dat,
      age_groups = naomi_data$age_groups,
      sexes = naomi_data$sexes,
      area_aggregation = naomi_data$area_aggregation,
      df_art_attend = df_attend_survey,
      by_residence = FALSE,
      by_survey = TRUE
    )

    Amat
  }

  A_anc_clients_t2 <- create_anc_Amat(naomi_data$anc_clients_t2_dat)
  A_anc_prev_t1 <- create_anc_Amat(naomi_data$anc_prev_t1_dat)
  A_anc_prev_t2 <- create_anc_Amat(naomi_data$anc_prev_t2_dat)
  A_anc_artcov_t1 <- create_anc_Amat(naomi_data$anc_artcov_t1_dat)
  A_anc_artcov_t2 <- create_anc_Amat(naomi_data$anc_artcov_t2_dat)

  A_prev <- create_survey_Amat(naomi_data$prev_dat)
  A_artcov <- create_survey_Amat(naomi_data$artcov_dat)
  A_vls <- create_survey_Amat(naomi_data$vls_dat)
  A_recent <- create_survey_Amat(naomi_data$recent_dat)

  ## ART attendance aggregation
  # Default model for ART attending: Anchor home district = add random effect for home district

  if(naomi_data$model_options$anchor_home_district) {
    Xgamma <- sparse_model_matrix(~0 + attend_area_idf, naomi_data$mf_artattend)
  } else {
    Xgamma <- sparse_model_matrix(~0 + attend_area_idf:as.integer(jstar != 1),
                                  naomi_data$mf_artattend)
  }

  if(naomi_data$artattend_t2) {
    Xgamma_t2 <- Xgamma
  } else {
    Xgamma_t2 <- sparse_model_matrix(~0, naomi_data$mf_artattend)
  }

  df_art_attend <- naomi_data$mf_model %>%
    dplyr::rename(reside_area_id = area_id) %>%
    dplyr::left_join(naomi_data$mf_artattend, by = "reside_area_id",
                     multiple = "all",
                     relationship = "many-to-many") %>%
    dplyr::mutate(attend_idf = forcats::as_factor(attend_idx),
                  idf = forcats::as_factor(idx))

  Xart_gamma <- sparse_model_matrix(~0 + attend_idf, df_art_attend)
  Xart_idx <- sparse_model_matrix(~0 + idf, df_art_attend)

  A_artattend_t1 <- create_artattend_Amat(artnum_df = dplyr::rename(naomi_data$artnum_t1_dat, attend_area_id = area_id),
                                          age_groups = naomi_data$age_groups,
                                          sexes = naomi_data$sexes,
                                          area_aggregation = naomi_data$area_aggregation,
                                          df_art_attend = df_art_attend,
                                          by_residence = FALSE)

  A_artattend_t2 <- create_artattend_Amat(artnum_df = dplyr::rename(naomi_data$artnum_t2_dat, attend_area_id = area_id),
                                          age_groups = naomi_data$age_groups,
                                          sexes = naomi_data$sexes,
                                          area_aggregation = naomi_data$area_aggregation,
                                          df_art_attend = df_art_attend,
                                          by_residence = FALSE)

  A_artattend_mf <- create_artattend_Amat(artnum_df = dplyr::select(naomi_data$mf_model, attend_area_id = area_id, sex, age_group, artnum_idx = idx),
                                          age_groups = naomi_data$age_groups,
                                          sexes = naomi_data$sexes,
                                          area_aggregation = naomi_data$area_aggregation,
                                          df_art_attend = df_art_attend,
                                          by_residence = FALSE)

  A_art_reside_attend <- naomi_data$mf_artattend %>%
    dplyr::transmute(
             reside_area_id,
             attend_area_id,
             sex = "both",
             age_group = "Y000_999"
           ) %>%
    create_artattend_Amat(age_groups = naomi_data$age_groups,
                          sexes = naomi_data$sexes,
                          area_aggregation = naomi_data$area_aggregation,
                          df_art_attend = df_art_attend,
                          by_residence = TRUE)


  ## Construct TMB data and initial parameter vectors

  df <- naomi_data$mf_model


  X_15to49 <- Matrix::t(sparse_model_matrix(~-1 + area_idf:age15to49, naomi_data$mf_model))

  ## Paediatric prevalence from 15-49 female ratio
  X_15to49f <- Matrix::t(Matrix::sparse.model.matrix(~0 + area_idf:age15to49:as.integer(sex == "female"), df))

  df$bin_paed_rho_model <- 1 - df$bin_rho_model
  X_paed_rho_ratio <- sparse_model_matrix(~-1 + area_idf:paed_rho_ratio:bin_paed_rho_model, df)
  paed_rho_ratio_offset <- 0.5 * df$bin_rho_model

  X_paed_lambda_ratio_t1 <- sparse_model_matrix(~-1 + area_idf:paed_lambda_ratio_t1, df)
  X_paed_lambda_ratio_t2 <- sparse_model_matrix(~-1 + area_idf:paed_lambda_ratio_t2, df)
  X_paed_lambda_ratio_t3 <- sparse_model_matrix(~-1 + area_idf:paed_lambda_ratio_t3, df)
  X_paed_lambda_ratio_t4 <- sparse_model_matrix(~-1 + area_idf:paed_lambda_ratio_t4, df)
  X_paed_lambda_ratio_t5 <- sparse_model_matrix(~-1 + area_idf:paed_lambda_ratio_t5, df)

  f_rho_a <- if(all(is.na(df$rho_a_fct))) ~0 else ~0 + rho_a_fct
  f_alpha_a <- if(all(is.na(df$alpha_a_fct))) ~0 else ~0 + alpha_a_fct

  if (naomi_data$rho_paed_x_term) {
    f_rho_xa <- ~0 + area_idf
  } else {
    f_rho_xa <- ~0
  }

  ## Ratio of paediatric incidence rate to 15-49 female prevalence

  ## If no sex stratified prevalence data, don't estimate spatial variation in
  ## sex odds ratio
  if ( ! all(c("male", "female") %in% naomi_data$prev_dat$sex)) {
    f_rho_xs <- ~0
  } else {
    f_rho_xs <- ~0 + area_idf
  }

  ## If no sex stratified ART coverage data, don't estimate spatial variation in
  ## sex odds ratio
  if ( ! all(c("male", "female") %in% naomi_data$artcov_dat$sex) &&
       ! all(c("male", "female") %in% naomi_data$artnum_t1_dat$sex) &&
       ! all(c("male", "female") %in% naomi_data$artnum_t2_dat$sex) ) {
    f_alpha_xs <- ~0
  } else {
    f_alpha_xs <- ~0 + area_idf
  }


  ## If flag **and** has ART by sex data at both times, estimate time x district x
  ## sex ART odds ratio.
  if (naomi_data$alpha_xst_term) {
    if (!all(c("male", "female") %in% naomi_data$artnum_t1_dat$sex) &&
          !all(c("male", "female") %in% naomi_data$artnum_t2_dat$sex)) {
      stop(paste("Sex-stratified ART data are required at both Time 1 and Time 2",
                 "to estimate district x sex x time interaction for ART coverage"))
    }

    f_alpha_xst <- ~0 + area_idf
  } else {
    f_alpha_xst <- ~0
  }


  ## If no ART data at both time points, do not fit a change in ART coverage. Use
  ## logit difference in ART coverage from Spectrum.
  ## T1 ART data may be either survey or programme
  ##

  has_t1_art <- nrow(naomi_data$artcov_dat) > 0 | nrow(naomi_data$artnum_t1_dat) > 0
  has_t2_art <- nrow(naomi_data$artnum_t2_dat) > 0

  if( !has_t1_art | !has_t2_art ) {
    f_alpha_t2 <- ~0
    f_alpha_xt <- ~0
    logit_alpha_t1t2_offset <- naomi_data$mf_model$logit_alpha_t1t2_offset
  } else {
    f_alpha_t2 <- ~1
    f_alpha_xt <- ~0 + area_idf
    logit_alpha_t1t2_offset <- numeric(nrow(naomi_data$mf_model))
  }

  ## Paediatric ART coverage random effects
  artnum_t1_dat <- naomi_data$artnum_t1_dat %>%
    dplyr::left_join(get_age_groups(), by = "age_group") %>%
    dplyr::mutate(age_group_end = age_group_start + age_group_span - 1)

  artnum_t2_dat <- naomi_data$artnum_t2_dat %>%
    dplyr::left_join(get_age_groups(), by = "age_group") %>%
    dplyr::mutate(age_group_end = age_group_start + age_group_span - 1)

  has_t1_paed_art <- any(artnum_t1_dat$age_group_end < 15)
  has_t2_paed_art <- any(artnum_t2_dat$age_group_end < 15)

  if(has_t1_paed_art | has_t2_paed_art) {
    f_alpha_xa <- ~0 + area_idf
  } else {
    f_alpha_xa <- ~0
  }

  if(has_t1_paed_art & has_t2_paed_art) {
    f_alpha_t2 <- ~1 + age_below15
    f_alpha_xat <- ~0 + area_idf
  } else {
    f_alpha_xat <- ~0
  }

  ## If no recent infection data, do not estimate incidence sex ratio or
  ## district random effects
  if(nrow(naomi_data$recent_dat) == 0) {
    f_lambda <- ~0
    f_lambda_x <- ~0
  } else {
    f_lambda <- ~ 1 + female_15plus
    f_lambda_x <- ~0 + area_idf
  }


  dtmb <- list(
    population_t1 = df$population_t1,
    population_t2 = df$population_t2,
    Lproj_hivpop_t1t2 = naomi_data$Lproj_t1t2$Lproj_hivpop,
    Lproj_incid_t1t2 = naomi_data$Lproj_t1t2$Lproj_incid,
    Lproj_paed_t1t2 = naomi_data$Lproj_t1t2$Lproj_paed,
    X_rho = as.matrix(sparse_model_matrix(~female_15plus, df, "bin_rho_model", TRUE)),
    X_alpha = stats::model.matrix(~female_15plus, df),
    X_alpha_t2 = stats::model.matrix(f_alpha_t2, df),
    X_lambda = stats::model.matrix(f_lambda, df),
    X_asfr = stats::model.matrix(~1, df),
    X_ancrho = stats::model.matrix(~1, df),
    X_ancalpha = stats::model.matrix(~1, df),
    Z_x = sparse_model_matrix(~0 + area_idf, df),
    Z_rho_x = sparse_model_matrix(~0 + area_idf, df, "bin_rho_model", TRUE),
    Z_rho_xs = sparse_model_matrix(f_rho_xs, df, "female_15plus", TRUE),
    Z_rho_a = sparse_model_matrix(f_rho_a, df, "bin_rho_model", TRUE),
    Z_rho_as = sparse_model_matrix(f_rho_a, df, "female_15plus", TRUE),
    Z_rho_xa = sparse_model_matrix(f_rho_xa, df, "age_below15"),
    Z_alpha_x = sparse_model_matrix(~0 + area_idf, df),
    Z_alpha_xs = sparse_model_matrix(f_alpha_xs, df, "female_15plus", TRUE),
    Z_alpha_a = sparse_model_matrix(f_alpha_a, df),
    Z_alpha_as = sparse_model_matrix(f_alpha_a, df, "female_15plus", TRUE),
    Z_alpha_xt = sparse_model_matrix(f_alpha_xt, df),
    Z_alpha_xa = sparse_model_matrix(f_alpha_xa, df, "age_below15"),
    Z_alpha_xat = sparse_model_matrix(f_alpha_xat, df, "age_below15"),
    Z_alpha_xst = sparse_model_matrix(f_alpha_xst, df, "female_15plus", TRUE),
    Z_lambda_x = sparse_model_matrix(f_lambda_x, df),
    ## Z_xa = Matrix::sparse.model.matrix(~0 + area_idf:age_group_idf, df),
    Z_asfr_x = sparse_model_matrix(~0 + area_idf, df),
    Z_ancrho_x = sparse_model_matrix(~0 + area_idf, df),
    Z_ancalpha_x = sparse_model_matrix(~0 + area_idf, df),
    log_asfr_t1_offset = log(df$asfr_t1),
    log_asfr_t2_offset = log(df$asfr_t2),
    log_asfr_t3_offset = log(df$asfr_t3),
    log_asfr_t4_offset = log(df$asfr_t4),
    logit_anc_rho_t1_offset = log(df$frr_plhiv_t1),
    logit_anc_rho_t2_offset = log(df$frr_plhiv_t2),
    logit_anc_rho_t3_offset = log(df$frr_plhiv_t3),
    logit_anc_rho_t4_offset = log(df$frr_plhiv_t4),
    logit_anc_alpha_t1_offset = log(df$frr_already_art_t1),
    logit_anc_alpha_t2_offset = log(df$frr_already_art_t2),
    logit_anc_alpha_t3_offset = log(df$frr_already_art_t3),
    logit_anc_alpha_t4_offset = log(df$frr_already_art_t4),
    ##
    logit_rho_offset = naomi_data$mf_model$logit_rho_offset * naomi_data$mf_model$bin_rho_model,
    logit_alpha_offset = naomi_data$mf_model$logit_alpha_offset,
    logit_alpha_t1t2_offset = logit_alpha_t1t2_offset,
    ##
    unaware_untreated_prop_t1 = df$spec_unaware_untreated_prop_t1,
    unaware_untreated_prop_t2 = df$spec_unaware_untreated_prop_t2,
    unaware_untreated_prop_t3 = df$spec_unaware_untreated_prop_t3,
    ##
    Q_x = methods::as(naomi_data$Q, "dgCMatrix"),
    Q_x_rankdef = ncol(naomi_data$Q) - as.integer(Matrix::rankMatrix(naomi_data$Q)),
    n_nb = naomi_data$mf_areas$n_neighbors,
    adj_i = naomi_data$mf_artattend$reside_area_idx - 1L,
    adj_j = naomi_data$mf_artattend$attend_area_idx - 1L,
    Xgamma = Xgamma,
    Xgamma_t2 = Xgamma_t2,
    log_gamma_offset = naomi_data$mf_artattend$log_gamma_offset,
    Xart_idx = Xart_idx,
    Xart_gamma = Xart_gamma,
    ##
    omega = naomi_data$omega,
    OmegaT0 = naomi_data$rita_param$OmegaT0,
    sigma_OmegaT = naomi_data$rita_param$sigma_OmegaT,
    betaT0 = naomi_data$rita_param$betaT0,
    sigma_betaT = naomi_data$rita_param$sigma_betaT,
    ritaT = naomi_data$rita_param$ritaT,
    ##
    logit_nu_mean = naomi_data$logit_nu_mean,
    logit_nu_sd = naomi_data$logit_nu_sd,
    ##
    X_15to49 = X_15to49,
    log_lambda_t1_offset = naomi_data$mf_model$log_lambda_t1_offset,
    log_lambda_t2_offset = naomi_data$mf_model$log_lambda_t2_offset,
    ##
    X_15to49f = X_15to49f,
    X_paed_rho_ratio = X_paed_rho_ratio,
    paed_rho_ratio_offset = paed_rho_ratio_offset,
    ##
    X_paed_lambda_ratio_t1 = X_paed_lambda_ratio_t1,
    X_paed_lambda_ratio_t2 = X_paed_lambda_ratio_t2,
    X_paed_lambda_ratio_t3 = X_paed_lambda_ratio_t3,
    X_paed_lambda_ratio_t4 = X_paed_lambda_ratio_t4,
    X_paed_lambda_ratio_t5 = X_paed_lambda_ratio_t5,
    ##
    ## Household survey input data
    x_prev = naomi_data$prev_dat$x_eff,
    n_prev = naomi_data$prev_dat$n_eff,
    A_prev = A_prev,
    x_artcov = naomi_data$artcov_dat$x_eff,
    n_artcov = naomi_data$artcov_dat$n_eff,
    A_artcov = A_artcov,
    x_vls = naomi_data$vls_dat$x_eff,
    n_vls = naomi_data$vls_dat$n_eff,
    A_vls = A_vls,
    x_recent = naomi_data$recent_dat$x_eff,
    n_recent = naomi_data$recent_dat$n_eff,
    A_recent = A_recent,
    ##
    ## ANC testing input data
    x_anc_clients_t2 = naomi_data$anc_clients_t2_dat$anc_clients_x,
    offset_anc_clients_t2 = naomi_data$anc_clients_t2_dat$anc_clients_pys_offset,
    A_anc_clients_t2 = A_anc_clients_t2,
    x_anc_prev_t1 = naomi_data$anc_prev_t1_dat$anc_prev_x,
    n_anc_prev_t1 = naomi_data$anc_prev_t1_dat$anc_prev_n,
    A_anc_prev_t1 = A_anc_prev_t1,
    x_anc_artcov_t1 = naomi_data$anc_artcov_t1_dat$anc_artcov_x,
    n_anc_artcov_t1 = naomi_data$anc_artcov_t1_dat$anc_artcov_n,
    A_anc_artcov_t1 = A_anc_artcov_t1,
    x_anc_prev_t2 = naomi_data$anc_prev_t2_dat$anc_prev_x,
    n_anc_prev_t2 = naomi_data$anc_prev_t2_dat$anc_prev_n,
    A_anc_prev_t2 = A_anc_prev_t2,
    x_anc_artcov_t2 = naomi_data$anc_artcov_t2_dat$anc_artcov_x,
    n_anc_artcov_t2 = naomi_data$anc_artcov_t2_dat$anc_artcov_n,
    A_anc_artcov_t2 = A_anc_artcov_t2,
    ##
    ## Number on ART input data
    A_artattend_t1 = A_artattend_t1,
    x_artnum_t1 = naomi_data$artnum_t1_dat$art_current,
    A_artattend_t2 = A_artattend_t2,
    x_artnum_t2 = naomi_data$artnum_t2_dat$art_current,
    A_artattend_mf = A_artattend_mf,
    A_art_reside_attend = A_art_reside_attend,
    ##
    ## Time 3 projection inputs
    population_t3 = df$population_t3,
    Lproj_hivpop_t2t3 = naomi_data$Lproj_t2t3$Lproj_hivpop,
    Lproj_incid_t2t3 = naomi_data$Lproj_t2t3$Lproj_incid,
    Lproj_paed_t2t3 = naomi_data$Lproj_t2t3$Lproj_paed,
    logit_alpha_t2t3_offset = df$logit_alpha_t2t3_offset,
    log_lambda_t3_offset = df$log_lambda_t3_offset,
    ##
    ## Time 4 projection inputs
    population_t4 = df$population_t4,
    Lproj_hivpop_t3t4 = naomi_data$Lproj_t3t4$Lproj_hivpop,
    Lproj_incid_t3t4 = naomi_data$Lproj_t3t4$Lproj_incid,
    Lproj_paed_t3t4 = naomi_data$Lproj_t3t4$Lproj_paed,
    logit_alpha_t3t4_offset = df$logit_alpha_t3t4_offset,
    log_lambda_t4_offset = df$log_lambda_t4_offset,
    ##
    ## Time 5 projection inputs
    population_t5 = df$population_t5,
    Lproj_hivpop_t4t5 = naomi_data$Lproj_t4t5$Lproj_hivpop,
    Lproj_incid_t4t5 = naomi_data$Lproj_t4t5$Lproj_incid,
    Lproj_paed_t4t5 = naomi_data$Lproj_t4t5$Lproj_paed,
    logit_alpha_t4t5_offset = df$logit_alpha_t4t5_offset,
    log_lambda_t5_offset = df$log_lambda_t5_offset,
    ##
    A_out = naomi_data$A_out,
    A_anc_out = naomi_data$A_anc_out,
    calc_outputs = 1L,
    report_likelihood = report_likelihood
  )


  ptmb <- list(
    beta_rho = numeric(ncol(dtmb$X_rho)),
    beta_alpha = numeric(ncol(dtmb$X_alpha)),
    beta_alpha_t2 = numeric(ncol(dtmb$X_alpha_t2)),
    beta_lambda = numeric(ncol(dtmb$X_lambda)),
    beta_asfr = numeric(1),
    beta_anc_rho = numeric(1),
    beta_anc_alpha = numeric(1),
    beta_anc_rho_t2 = numeric(1),
    beta_anc_alpha_t2 = numeric(1),
    u_rho_x = numeric(ncol(dtmb$Z_rho_x)),
    us_rho_x = numeric(ncol(dtmb$Z_rho_x)),
    u_rho_xs = numeric(ncol(dtmb$Z_rho_xs)),
    us_rho_xs = numeric(ncol(dtmb$Z_rho_xs)),
    u_rho_a = numeric(ncol(dtmb$Z_rho_a)),
    u_rho_as = numeric(ncol(dtmb$Z_rho_as)),
    u_rho_xa = numeric(ncol(dtmb$Z_rho_xa)),
    ui_asfr_x = numeric(ncol(dtmb$Z_asfr_x)),
    ui_anc_rho_x = numeric(ncol(dtmb$Z_ancrho_x)),
    ui_anc_alpha_x = numeric(ncol(dtmb$Z_ancalpha_x)),
    ui_anc_rho_xt = numeric(ncol(dtmb$Z_ancrho_x)),
    ui_anc_alpha_xt = numeric(ncol(dtmb$Z_ancalpha_x)),
    ##
    u_alpha_x = numeric(ncol(dtmb$Z_alpha_x)),
    us_alpha_x = numeric(ncol(dtmb$Z_alpha_x)),
    u_alpha_xs = numeric(ncol(dtmb$Z_alpha_xs)),
    us_alpha_xs = numeric(ncol(dtmb$Z_alpha_xs)),
    u_alpha_a = numeric(ncol(dtmb$Z_alpha_a)),
    u_alpha_as = numeric(ncol(dtmb$Z_alpha_as)),
    u_alpha_xt = numeric(ncol(dtmb$Z_alpha_xt)),
    u_alpha_xa = numeric(ncol(dtmb$Z_alpha_xa)),
    u_alpha_xat = numeric(ncol(dtmb$Z_alpha_xat)),
    u_alpha_xst = numeric(ncol(dtmb$Z_alpha_xst)),
    ##
    log_sigma_lambda_x = log(1.0),
    ui_lambda_x = numeric(ncol(dtmb$Z_lambda_x)),
    ##
    logit_phi_rho_a = 0,
    log_sigma_rho_a = log(2.5),
    logit_phi_rho_as = 2.582,
    log_sigma_rho_as = log(2.5),
    logit_phi_rho_x = 0,
    log_sigma_rho_x = log(2.5),
    logit_phi_rho_xs = 0,
    log_sigma_rho_xs = log(2.5),
    log_sigma_rho_xa = log(0.5),
    ##
    logit_phi_alpha_a = 0,
    log_sigma_alpha_a = log(2.5),
    logit_phi_alpha_as = 2.582,
    log_sigma_alpha_as = log(2.5),
    logit_phi_alpha_x = 0,
    log_sigma_alpha_x = log(2.5),
    logit_phi_alpha_xs = 0,
    log_sigma_alpha_xs = log(2.5),
    log_sigma_alpha_xt = log(2.5),
    log_sigma_alpha_xa = log(2.5),
    log_sigma_alpha_xat = log(2.5),
    log_sigma_alpha_xst = log(2.5),
    ##
    OmegaT_raw = 0,
    log_betaT = 0,
    logit_nu_raw = 0,
    ##
    log_sigma_asfr_x = log(0.5),
    log_sigma_ancrho_x = log(2.5),
    log_sigma_ancalpha_x = log(2.5),
    log_sigma_ancrho_xt = log(2.5),
    log_sigma_ancalpha_xt = log(2.5),
    ##
    log_or_gamma = numeric(ncol(dtmb$Xgamma)),
    log_sigma_or_gamma = log(2.5),
    log_or_gamma_t1t2 = numeric(ncol(dtmb$Xgamma_t2)),
    log_sigma_or_gamma_t1t2 = log(2.5)
  )

  v <- list(data = dtmb,
            par_init = ptmb)
  class(v) <- "naomi_tmb_input"

  v
}

sparse_model_matrix <- function(formula, data, binary_interaction = 1,
                                drop_zero_cols = FALSE) {

  if(is.character(binary_interaction))
    binary_interaction <- data[[binary_interaction]]

  stopifnot(length(binary_interaction) %in% c(1, nrow(data)))

  mm <- Matrix::sparse.model.matrix(formula, data)
  mm <- mm * binary_interaction
  mm <- Matrix::drop0(mm)

  if(drop_zero_cols)
    mm <- mm[ , apply(mm, 2, Matrix::nnzero) > 0]

  mm
}

make_tmb_obj <- function(data, par, calc_outputs = 1L, inner_verbose = FALSE,
                         progress = NULL) {

  data$calc_outputs <- as.integer(calc_outputs)

  obj <- TMB::MakeADFun(data = data,
                        parameters = par,
                        DLL = "naomi",
                        silent = !inner_verbose,
                        random = c("beta_rho",
                                   "beta_alpha", "beta_alpha_t2",
                                   "beta_lambda",
                                   "beta_asfr",
                                   "beta_anc_rho", "beta_anc_alpha",
                                   "beta_anc_rho_t2", "beta_anc_alpha_t2",
                                   "u_rho_x", "us_rho_x",
                                   "u_rho_xs", "us_rho_xs",
                                   "u_rho_a", "u_rho_as",
                                   "u_rho_xa",
                                   ##
                                   "u_alpha_x", "us_alpha_x",
                                   "u_alpha_xs", "us_alpha_xs",
                                   "u_alpha_a", "u_alpha_as",
                                   "u_alpha_xt",
                                   "u_alpha_xa", "u_alpha_xat", "u_alpha_xst",
                                   ##
                                   "ui_lambda_x",
                                   "logit_nu_raw",
                                   ##
                                   "ui_asfr_x",
                                   "ui_anc_rho_x", "ui_anc_alpha_x",
                                   "ui_anc_rho_xt", "ui_anc_alpha_xt",
                                   ##
                                   "log_or_gamma", "log_or_gamma_t1t2"))

  if (!is.null(progress)) {
    obj$fn <- report_progress(obj$fn, progress)
  }

  obj
}

report_progress <- function(fun, progress) {
  fun <- match.fun(fun)
  function(...) {
    progress$iterate_fit()
    fun(...)
  }
}


#' Fit TMB model
#'
#' @param tmb_input Model input data
#' @param outer_verbose If TRUE print function and parameters every iteration
#' @param inner_verbose If TRUE then disable tracing information from TMB
#' @param max_iter maximum number of iterations
#' @param progress Progress printer, if null no progress printed
#'
#' @return Fit model.
#' @export
fit_tmb <- function(tmb_input,
                    outer_verbose = TRUE,
                    inner_verbose = FALSE,
                    max_iter = 250,
                    progress = NULL
                    ) {

  stopifnot(inherits(tmb_input, "naomi_tmb_input"))

  obj <- make_tmb_obj(tmb_input$data, tmb_input$par_init, calc_outputs = 0L,
                      inner_verbose, progress)

  trace <- if (outer_verbose) 1 else 0
  f <- withCallingHandlers(
    stats::nlminb(obj$par, obj$fn, obj$gr,
                  control = list(trace = trace,
                                 iter.max = max_iter)),
    warning = function(w) {
      if (grepl("NA/NaN function evaluation", w$message)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  if(f$convergence != 0)
    warning(paste("convergence error:", f$message))

  if(outer_verbose)
    message(paste("converged:", f$message))

  f$par.fixed <- f$par
  f$par.full <- obj$env$last.par

  objout <- make_tmb_obj(tmb_input$data, tmb_input$par_init, calc_outputs = 1L, inner_verbose)
  f$mode <- objout$report(f$par.full)

  val <- c(f, obj = list(objout))
  class(val) <- "naomi_fit"

  val
}

#' Calculate Posterior Mean and Uncertainty Via TMB `sdreport()`
#'
#' @param naomi_fit Fitted TMB model.
#'
#' @export
report_tmb <- function(naomi_fit) {

  stopifnot(methods::is(fit, "naomi_fit"))
  naomi_fit$sdreport <- TMB::sdreport(naomi_fit$obj, naomi_fit$par,
                                      getReportCovariance = FALSE,
                                      bias.correct = TRUE)
  naomi_fit
}



#' Sample TMB fit
#'
#' @param fit The TMB fit
#' @param nsample Number of samples
#' @param rng_seed seed passed to set.seed.
#' @param random_only Random only
#' @param verbose If TRUE prints additional information.
#'
#' @return Sampled fit.
#' @export
sample_tmb <- function(fit, nsample = 1000, rng_seed = NULL,
                       random_only = TRUE, verbose = FALSE) {

  set.seed(rng_seed)

  stopifnot(methods::is(fit, "naomi_fit"))
  stopifnot(nsample > 1)

  to_tape <- TMB:::isNullPointer(fit$obj$env$ADFun$ptr)
  if (to_tape)
    fit$obj$retape(FALSE)

  if(!random_only) {
    if(verbose) print("Calculating joint precision")
    hess <- sdreport_joint_precision(fit$obj, fit$par.fixed)

    if(verbose) print("Inverting precision for joint covariance")
    cov <- solve(hess)

    if(verbose) print("Drawing sample")
    ## TODO: write a version of rmvnorm that uses precision instead of covariance
    smp <- mvtnorm::rmvnorm(nsample, fit$par.full, cov)

  } else {
    r <- fit$obj$env$random
    par_f <- fit$par.full[-r]

    par_r <- fit$par.full[r]
    hess_r <- fit$obj$env$spHess(fit$par.full, random = TRUE)
    smp_r <- rmvnorm_sparseprec(nsample, par_r, hess_r)

    smp <- matrix(0, nsample, length(fit$par.full))
    smp[ , r] <- smp_r
    smp[ ,-r] <- matrix(par_f, nsample, length(par_f), byrow = TRUE)
    colnames(smp)[r] <- colnames(smp_r)
    colnames(smp)[-r] <- names(par_f)
  }

  if(verbose) print("Simulating outputs")
  sim <- apply(smp, 1, fit$obj$report)

  r <- fit$obj$report()

  if(verbose) print("Returning sample")
  fit$sample <- Map(vapply, list(sim), "[[", lapply(lengths(r), numeric), names(r))
  is_vector <- vapply(fit$sample, inherits, logical(1), "numeric")

  fit$sample[is_vector] <- lapply(fit$sample[is_vector], matrix, nrow = 1)
  names(fit$sample) <- names(r)

  fit
}

rmvnorm_sparseprec <- function(
  n,
  mean = rep(0, nrow(prec)),
  prec = diag(length(mean))
  ) {

  z = matrix(stats::rnorm(n * length(mean)), ncol = n)
  L_inv = Matrix::Cholesky(prec)
  v <- mean + Matrix::solve(methods::as(L_inv, "pMatrix"), Matrix::solve(Matrix::t(methods::as(L_inv, "Matrix")), z))
  as.matrix(Matrix::t(v))
}

create_artattend_Amat <- function(artnum_df, age_groups, sexes, area_aggregation,
                                  df_art_attend, by_residence = FALSE, by_survey = FALSE) {

  ## If by_residence = TRUE, merge by reside_area_id, else aggregate over all
  ## reside_area_id
  by_vars <- c("attend_area_id", "sex", "age_group")
  if (by_residence) {
    by_vars <- c(by_vars, "reside_area_id")
  }

  id_vars <- by_vars
  if (by_survey) {
    id_vars <- c(id_vars, "survey_id")
  }


  if(!("artnum_idx" %in% colnames(artnum_df))) {

    artnum_df$artnum_idx <- seq_len(nrow(artnum_df))

  }

  A_artnum <- artnum_df %>%
    dplyr::select(tidyselect::all_of(id_vars), artnum_idx) %>%
    dplyr::rename(artdat_age_group = age_group,
                  artdat_sex = sex) %>%
    dplyr::left_join(
             get_age_groups() %>%
             dplyr::transmute(
                      artdat_age_group = age_group,
                      artdat_age_start = age_group_start,
                      artdat_age_end = age_group_start + age_group_span
                    ),
             by = "artdat_age_group",
             relationship = "many-to-many"
           ) %>%
    ## Note: this would be much faster with tree data structure for age rather than crossing...
    tidyr::crossing(
             get_age_groups() %>%
             dplyr::filter(age_group %in% age_groups)
           ) %>%
    dplyr::filter(
             artdat_age_start <= age_group_start,
             age_group_start + age_group_span <= artdat_age_end
           ) %>%
    dplyr::left_join(
             data.frame(artdat_sex = c("male", "female", "both", "both", "both"),
                        sex = c("male", "female", "male", "female", "both"),
                        stringsAsFactors = FALSE) %>%
             dplyr::filter(sex %in% sexes),
             by = "artdat_sex",
             multiple = "all",
             relationship = "many-to-many"
    )

  ## Map artattend_area_id to model_area_id
  A_artnum <- A_artnum %>%
    dplyr::left_join(
      area_aggregation,
      by = c("attend_area_id" = "area_id"),
      multiple = "all",
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(attend_area_id = model_area_id,
                  model_area_id = NULL)

  ## Check no areas with duplicated reporting
  art_duplicated_check <- A_artnum %>%
    dplyr::group_by_at(id_vars) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1)

  if (nrow(art_duplicated_check)) {
    stop(paste("ART or ANC data multiply reported for some age/sex strata in areas:",
               paste(unique(art_duplicated_check$attend_area_id), collapse = ", ")))
  }

  ## Merge to ART attendance data frame
  df_art_attend <- df_art_attend %>%
    dplyr::select(tidyselect::all_of(by_vars)) %>%
    dplyr::mutate(
             Aidx = dplyr::row_number(),
             value = 1
           )

  A_artnum <- dplyr::left_join(A_artnum, df_art_attend, by = by_vars, multiple = "all")

  A_artnum <- A_artnum %>%
    {
      Matrix::spMatrix(nrow(artnum_df),
                       nrow(df_art_attend),
                       .$artnum_idx,
                       .$Aidx,
                       .$value)
    }

  A_artnum
}
