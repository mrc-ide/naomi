prepare_tmb_inputs <- function(naomi_mf,
                               prev_dat,
                               artcov_dat,
                               recent_dat,
                               anc_prev_t1_dat,
                               anc_prev_t2_dat,
                               anc_artcov_t1_dat,
                               anc_artcov_t2_dat,
                               artnum_t1_dat,
                               artnum_t2_dat) {

  ## ANC prevalence model matrices
  anc_prev_t1_dat <- anc_prev_t1_dat %>%
    dplyr::left_join(
             naomi_mf$mf_areas,
             by = "area_id"
           )

  anc_artcov_t1_dat <- anc_artcov_t1_dat %>%
    dplyr::left_join(
             naomi_mf$mf_areas,
             by = "area_id"
           )

  anc_prev_t2_dat <- anc_prev_t2_dat %>%
    dplyr::left_join(
             naomi_mf$mf_areas,
             by = "area_id"
           )

  anc_artcov_t2_dat <- anc_artcov_t2_dat %>%
    dplyr::left_join(
             naomi_mf$mf_areas,
             by = "area_id"
           )

  create_anc_Amat <- function(dat, naomi_mf, asfr_col, population_col) {
    A <- dat %>%
      dplyr::inner_join(
               naomi_mf$mf_model,
               by = "area_id"
             ) %>%
      dplyr::transmute(
               area_id,
               anc_idx,
               idx,
               births = !!rlang::sym(asfr_col) * !!rlang::sym(population_col),
             ) %>%
      dplyr::filter(births > 0) %>%
    {
      Matrix::spMatrix(nrow(dat),
                       nrow(naomi_mf$mf_model),
                       .$anc_idx,
                       .$idx,
                       .$births)
    }

    A
  }

  A_anc_prev_t1 <- create_anc_Amat(anc_prev_t1_dat, naomi_mf, "asfr", "population_t1")
  A_anc_prev_t2 <- create_anc_Amat(anc_prev_t2_dat, naomi_mf, "asfr", "population_t2")
  A_anc_artcov_t1 <- create_anc_Amat(anc_artcov_t1_dat, naomi_mf, "asfr", "population_t1")
  A_anc_artcov_t2 <- create_anc_Amat(anc_artcov_t2_dat, naomi_mf, "asfr", "population_t2")

  X_15to49 <- Matrix::t(Matrix::sparse.model.matrix(~-1 + area_idf:age15to49, naomi_mf$mf_model))

  ## ART attendance aggregation

  df_art_attend <- naomi_mf$mf_model %>%
    dplyr::left_join(naomi_mf$mf_artattend, by = c("area_idx" = "reside_area_idx")) %>%
    dplyr::mutate(artattend_idf = forcats::as_factor(artattend_idx),
                  idf = forcats::as_factor(idx))

  Xart_gamma <- Matrix::sparse.model.matrix(~0 + artattend_idf, df_art_attend)
  Xart_idx <- Matrix::sparse.model.matrix(~0 + idf, df_art_attend)


  A_artnum_t1 <-
    artnum_t1_dat %>%
    dplyr::rename(artdat_age_group_id = age_group_id,
                  artdat_sex = sex) %>%
    dplyr::left_join(
             get_age_groups() %>%
             dplyr::transmute(
                      artdat_age_group_id = age_group_id,
                      artdat_age_start = age_group_start,
                      artdat_age_end = age_group_start + age_group_span
                    ),
             by = "artdat_age_group_id"
           ) %>%
    ## Note: this would be much faster with tree data structure for age rather than crossing...
    tidyr::crossing(
             get_age_groups() %>%
             dplyr::filter(age_group_id %in% naomi_mf$age_group_ids)
           ) %>%
    dplyr::filter(
             artdat_age_start <= age_group_start,
             age_group_start + age_group_span <= artdat_age_end
           ) %>%
    dplyr::left_join(
             data.frame(artdat_sex = c("male", "female", "both", "both", "both"),
                        sex = c("male", "female", "male", "female", "both"),
                        stringsAsFactors = FALSE) %>%
             dplyr::filter(sex %in% naomi_mf$sexes),
             by = "artdat_sex"
           ) %>%
    dplyr::left_join(
             naomi_mf$mf_areas %>% dplyr::select(area_id, area_idx),
             by = "area_id"
           ) %>%
    dplyr::left_join(
             df_art_attend %>%
             dplyr::transmute(
                      artattend_area_idx,
                      age_group_id,
                      sex,
                      Aidx = row_number(),
                      value = 1
      ),
      by = c("area_idx" = "artattend_area_idx", "sex" = "sex", "age_group_id" = "age_group_id")
      ) %>%
    {
      Matrix::spMatrix(nrow(artnum_t1_dat),
                       nrow(df_art_attend),
                     .$artnum_idx,
                     .$Aidx,
                     .$value)
    }

    A_artnum_t2 <-
    artnum_t2_dat %>%
    dplyr::rename(artdat_age_group_id = age_group_id,
                  artdat_sex = sex) %>%
    dplyr::left_join(
             get_age_groups() %>%
             dplyr::transmute(
                      artdat_age_group_id = age_group_id,
                      artdat_age_start = age_group_start,
                      artdat_age_end = age_group_start + age_group_span
                    ),
             by = "artdat_age_group_id"
           ) %>%
    ## Note: this would be much faster with tree data structure for age rather than crossing...
    tidyr::crossing(
             get_age_groups() %>%
             dplyr::filter(age_group_id %in% naomi_mf$age_group_ids)
           ) %>%
    dplyr::filter(
             artdat_age_start <= age_group_start,
             age_group_start + age_group_span <= artdat_age_end
           ) %>%
    dplyr::left_join(
             data.frame(artdat_sex = c("male", "female", "both", "both", "both"),
                        sex = c("male", "female", "male", "female", "both"),
                        stringsAsFactors = FALSE) %>%
             dplyr::filter(sex %in% naomi_mf$sexes),
             by = "artdat_sex"
           ) %>%
    dplyr::left_join(
             naomi_mf$mf_areas %>% dplyr::select(area_id, area_idx),
             by = "area_id"
           ) %>%
    dplyr::left_join(
             df_art_attend %>%
             dplyr::transmute(
                      artattend_area_idx,
                      age_group_id,
                      sex,
                      Aidx = row_number(),
                      value = 1
      ),
      by = c("area_idx" = "artattend_area_idx", "sex" = "sex", "age_group_id" = "age_group_id")
      ) %>%
    {
      Matrix::spMatrix(nrow(artnum_t2_dat),
                       nrow(df_art_attend),
                     .$artnum_idx,
                     .$Aidx,
                     .$value)
    }


  ## Construct TMB data and initial parameter vectors

  df <- naomi_mf$mf_model

  ## df <- df %>%
  ##   mutate(age_group_idf = factor(pmin(age_group_id, 12)))

  dtmb <- list(
    population = df$population_t1,
    X_rho = stats::model.matrix(~as.integer(sex == "female"), df),
    X_alpha = stats::model.matrix(~as.integer(sex == "female"), df),
    X_lambda = stats::model.matrix(~as.integer(sex == "female"), df),
    X_ancrho = stats::model.matrix(~1, anc_prev_t1_dat),
    X_ancalpha = stats::model.matrix(~1, anc_artcov_t1_dat),
    Z_x = Matrix::sparse.model.matrix(~0 + area_idf, df),
    Z_a = Matrix::sparse.model.matrix(~0 + age_group_idf, df),
    Z_xs = Matrix::sparse.model.matrix(~0 + area_idf, df) * (df$sex == "female"),
    Z_as = Matrix::sparse.model.matrix(~0 + age_group_idf, df) * (df$sex == "female"),
    ## Z_xa = Matrix::sparse.model.matrix(~0 + area_idf:age_group_idf, df),
    Z_ancrho_x = Matrix::sparse.model.matrix(~0 + area_idf, anc_prev_t1_dat),
    Z_ancalpha_x = Matrix::sparse.model.matrix(~0 + area_idf, anc_prev_t1_dat),
    ##
    Q_x = methods::as(naomi_mf$Q, "dgCMatrix"),
    n_nb = naomi_mf$mf_areas$n_neighbors,
    adj_i = naomi_mf$mf_artattend$reside_area_idx - 1L,
    adj_j = naomi_mf$mf_artattend$artattend_area_idx - 1L,
    gamma_or_mu = dplyr::filter(naomi_mf$mf_artattend, !istar == 1)$gamma_or_mu,
    gamma_or_sigma = dplyr::filter(naomi_mf$mf_artattend, !istar == 1)$gamma_or_sigma,
    Xart_idx = Xart_idx,
    Xart_gamma = Xart_gamma,
    ##
    omega = naomi_mf$omega,
    OmegaT0 = naomi_mf$rita_param$OmegaT0,
    sigma_OmegaT = naomi_mf$rita_param$sigma_OmegaT,
    betaT0 = naomi_mf$rita_param$betaT0,
    sigma_betaT = naomi_mf$rita_param$sigma_betaT,
    ritaT = naomi_mf$rita_param$ritaT,
    ##
    X_15to49 = X_15to49,
    log_lambda_offset = naomi_mf$mf_model$log_lambda_offset,
    ##
    A_out = naomi_mf$A_out,
    idx_prev = prev_dat$idx - 1L,
    x_prev = prev_dat$x,
    n_prev = prev_dat$n,
    idx_artcov = artcov_dat$idx - 1L,
    x_artcov = artcov_dat$x,
    n_artcov = artcov_dat$n,
    idx_recent = recent_dat$idx - 1L,
    x_recent = recent_dat$x,
    n_recent = recent_dat$n,
    A_anc_prev = A_anc_prev_t1,
    x_anc_prev = anc_prev_t1_dat$anc_prev_x,
    n_anc_prev = anc_prev_t1_dat$anc_prev_n,
    A_anc_artcov = A_anc_artcov_t1,
    x_anc_artcov = anc_artcov_t1_dat$anc_artcov_x,
    n_anc_artcov = anc_artcov_t1_dat$anc_artcov_n,
    ##
    A_artnum = A_artnum_t1,
    x_artnum = artnum_t1_dat$current_art
  )


  ptmb <- list(
    beta_rho = numeric(ncol(dtmb$X_rho)),
    beta_alpha = numeric(ncol(dtmb$X_alpha)),
    beta_lambda = numeric(ncol(dtmb$X_lambda)),
    beta_anc_rho = numeric(1),
    beta_anc_alpha = numeric(1),
    us_rho_x = numeric(ncol(dtmb$Z_x)),
    ui_rho_x = numeric(ncol(dtmb$Z_x)),
    us_rho_xs = numeric(ncol(dtmb$Z_xs)),
    ui_rho_xs = numeric(ncol(dtmb$Z_xs)),
    u_rho_a = numeric(ncol(dtmb$Z_a)),
    u_rho_as = numeric(ncol(dtmb$Z_a)),
    ui_anc_rho_x = numeric(ncol(dtmb$Z_x)),
    ui_anc_alpha_x = numeric(ncol(dtmb$Z_x)),
    ##
    us_alpha_x = numeric(ncol(dtmb$Z_x)),
    ui_alpha_x = numeric(ncol(dtmb$Z_x)),
    us_alpha_xs = numeric(ncol(dtmb$Z_xs)),
    ui_alpha_xs = numeric(ncol(dtmb$Z_xs)),
    u_alpha_a = numeric(ncol(dtmb$Z_a)),
    u_alpha_as = numeric(ncol(dtmb$Z_a)),
    ##
    log_sigma_lambda_x = 0,
    ui_lambda_x = numeric(ncol(dtmb$Z_x)),
    ##
    logit_phi_rho_a = 0,
    log_sigma_rho_a = 0,
    logit_phi_rho_as = 0,
    log_sigma_rho_as = 0,
    logit_phi_rho_x = 0,
    log_sigma_rho_x = 0,
    logit_phi_rho_xs = 0,
    log_sigma_rho_xs = 0,
    ##
    logit_phi_alpha_a = 0,
    log_sigma_alpha_a = 0,
    logit_phi_alpha_as = 0,
    log_sigma_alpha_as = 0,
    logit_phi_alpha_x = 0,
    log_sigma_alpha_x = 0,
    logit_phi_alpha_xs = 0,
    log_sigma_alpha_xs = 0,
    ##
    OmegaT_raw = 0,
    log_betaT = 0,
    ##
    log_sigma_ancrho_x = 0,
    log_sigma_ancalpha_x = 0,
    ##
    oddsratio_gamma_art_raw = numeric(sum(dtmb$n_nb))
  )

  v <- list(data_tmb = dtmb,
            parameters_tmb = ptmb)
  class(v) <- "naomi_tmb_input"

  v
}


fit_tmb <- function(tmb_input, outer_verbose = TRUE, inner_verbose = FALSE) {

  stopifnot(inherits(tmb_input, "naomi_tmb_input"))

  obj <- TMB::MakeADFun(data = tmb_input$data_tmb,
                        parameters = tmb_input$parameters_tmb,
                        DLL = "naomi",
                        silent = !inner_verbose,
                        random = c("beta_rho", "beta_alpha", "beta_lambda",
                                   "beta_anc_rho", "beta_anc_alpha",
                                   "us_rho_x", "ui_rho_x",
                                   "us_rho_xs", "ui_rho_xs",
                                   "u_rho_a", "u_rho_as",
                                   ##
                                   "us_alpha_x", "ui_alpha_x",
                                   "us_alpha_xs", "ui_alpha_xs",
                                   "u_alpha_a", "u_alpha_as",
                                   ##
                                   "ui_lambda_x",
                                   ##
                                   "ui_anc_rho_x", "ui_anc_alpha_x",
                                   ##
                                   "oddsratio_gamma_art_raw"))

  trace <- if(outer_verbose) 1 else 0
  f <- stats::nlminb(obj$par, obj$fn, obj$gr, control = list(trace = trace))

  f$par.fixed <- f$par
  f$par.full <- obj$env$last.par

  val <- c(f, obj = list(obj))
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


## Sample from Joint Posterior Distribution
sample_tmb <- function(fit, nsample = 1000, random_only = TRUE, verbose = TRUE) {

  stopifnot(methods::is(fit, "naomi_fit"))
  stopifnot(nsample > 1)

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
    cov_r <- solve(hess_r)
    smp_r <- mvtnorm::rmvnorm(nsample, par_r, cov_r)

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
  is_vector <- vapply(fit$sample, class, character(1)) == "numeric"
  fit$sample[is_vector] <- lapply(fit$sample[is_vector], as.matrix, nrow = 1)
  names(fit$sample) <- names(r)

  fit
}
