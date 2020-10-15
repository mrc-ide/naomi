#' Prepare inputs for TMB model.
#'
#' @param naomi_data  Naomi data object
#'
#' @return Inputs ready for TMB model
#'
#' @seealso [select_naomi_data]
#' @export
prepare_tmb_inputs <- function(naomi_data) {

  stopifnot(is(naomi_data, "naomi_data"))
  stopifnot(is(naomi_data, "naomi_mf"))

  create_anc_Amat <- function(naomi_mf, asfr_col, population_col) {

    A <- naomi_data$mf_model %>%
      dplyr::transmute(
               area_id,
               area_idx,
               idx,
               births = !!rlang::sym(asfr_col) * !!rlang::sym(population_col)
             ) %>%
      {
      Matrix::spMatrix(nrow(naomi_data$mf_areas),
                       nrow(naomi_data$mf_model),
                       .$area_idx,
                       .$idx,
                       .$births)
    }

    A
  }

  A_anc_t1 <- create_anc_Amat(naomi_data, "asfr_t1", "population_t1")
  A_anc_t2 <- create_anc_Amat(naomi_data, "asfr_t2", "population_t2")

  ## ART attendance aggregation

  Xgamma <- sparse_model_matrix(~0 + attend_area_idf:as.integer(jstar != 1),
                                naomi_data$mf_artattend)
  if(naomi_data$artattend_t2) {
    Xgamma_t2 <- Xgamma
  } else {
    Xgamma_t2 <- sparse_model_matrix(~0, naomi_data$mf_artattend)
  }
  
  df_art_attend <- naomi_data$mf_model %>%
    dplyr::rename(reside_area_id = area_id) %>%
    dplyr::left_join(naomi_data$mf_artattend, by = "reside_area_id") %>%
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
             age_group = "00+",
             artnum_idx = dplyr::row_number()
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
  A_15to49f <- Matrix::t(Matrix::sparse.model.matrix(~0 + area_idf:age15to49:as.integer(sex == "female"):population_t1, df))
  df$bin_paed_rho_model <- 1 - df$bin_rho_model
  X_paed_rho_ratio <- sparse_model_matrix(~-1 + area_idf:paed_rho_ratio:bin_paed_rho_model, df)
  paed_rho_ratio_offset <- 0.5 * df$bin_rho_model

  f_rho_a <- if(all(is.na(df$rho_a_fct))) ~0 else ~0 + rho_a_fct
  f_alpha_a <- if(all(is.na(df$alpha_a_fct))) ~0 else ~0 + alpha_a_fct

  if (naomi_data$rho_paed_x_term) {
    f_rho_xa <- ~0 + area_idf
  } else {
    f_rho_xa <- ~0
  }

  ## If no t2 ART data, do not fit a change in ART coverage. Use logit difference
  ## in ART coverage from Spectrum.
  if(nrow(naomi_data$artnum_t2_dat) == 0) {
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
    Lproj_hivpop = naomi_data$Lproj_hivpop,
    Lproj_incid = naomi_data$Lproj_incid,
    Lproj_paed = naomi_data$Lproj_paed,
    projection_duration = naomi_data$projection_duration,
    X_rho = as.matrix(sparse_model_matrix(~female_15plus, df, "bin_rho_model", TRUE)),
    X_alpha = stats::model.matrix(~female_15plus, df),
    X_alpha_t2 = stats::model.matrix(f_alpha_t2, df),
    X_lambda = stats::model.matrix(f_lambda, df),
    X_ancrho = stats::model.matrix(~1, naomi_data$mf_areas),
    X_ancalpha = stats::model.matrix(~1, naomi_data$mf_areas),
    Z_x = sparse_model_matrix(~0 + area_idf, df),
    Z_rho_x = sparse_model_matrix(~0 + area_idf, df, "bin_rho_model", TRUE),
    Z_rho_xs = sparse_model_matrix(~0 + area_idf, df, "female_15plus", TRUE),
    Z_rho_a = sparse_model_matrix(f_rho_a, df, "bin_rho_model", TRUE),
    Z_rho_as = sparse_model_matrix(f_rho_a, df, "female_15plus", TRUE),
    Z_rho_xa = sparse_model_matrix(f_rho_xa, df, "age_below15"),
    Z_alpha_x = sparse_model_matrix(~0 + area_idf, df),
    Z_alpha_xs = sparse_model_matrix(~0 + area_idf, df, "female_15plus", TRUE),
    Z_alpha_a = sparse_model_matrix(f_alpha_a, df),
    Z_alpha_as = sparse_model_matrix(f_alpha_a, df, "female_15plus", TRUE),
    Z_alpha_xt = sparse_model_matrix(f_alpha_xt, df),
    Z_alpha_xa = sparse_model_matrix(f_alpha_xa, df, "age_below15"),
    Z_alpha_xat = sparse_model_matrix(f_alpha_xat, df, "age_below15"),
    Z_lambda_x = sparse_model_matrix(f_lambda_x, df),
    ## Z_xa = Matrix::sparse.model.matrix(~0 + area_idf:age_group_idf, df),
    Z_ancrho_x = sparse_model_matrix(~0 + area_idf, naomi_data$mf_areas),
    Z_ancalpha_x = sparse_model_matrix(~0 + area_idf, naomi_data$mf_areas),
    A_anc_t1 = A_anc_t1,
    A_anc_t2 = A_anc_t2,
    ##
    logit_rho_offset = naomi_data$mf_model$logit_rho_offset * naomi_data$mf_model$bin_rho_model,
    logit_alpha_offset = naomi_data$mf_model$logit_alpha_offset,
    logit_alpha_t1t2_offset = logit_alpha_t1t2_offset,
    ##
    Q_x = methods::as(naomi_data$Q, "dgCMatrix"),
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
    A_15to49f = A_15to49f,
    X_paed_rho_ratio = X_paed_rho_ratio,
    paed_rho_ratio_offset = paed_rho_ratio_offset,
    ##
    idx_prev = naomi_data$prev_dat$idx - 1L,
    x_prev = naomi_data$prev_dat$x_eff,
    n_prev = naomi_data$prev_dat$n_eff,
    idx_artcov = naomi_data$artcov_dat$idx - 1L,
    x_artcov = naomi_data$artcov_dat$x_eff,
    n_artcov = naomi_data$artcov_dat$n_eff,
    idx_vls = naomi_data$vls_dat$idx - 1L,
    x_vls = naomi_data$vls_dat$x_eff,
    n_vls = naomi_data$vls_dat$n_eff,
    idx_recent = naomi_data$recent_dat$idx - 1L,
    x_recent = naomi_data$recent_dat$x_eff,
    n_recent = naomi_data$recent_dat$n_eff,
    ##
    idx_anc_prev_t1 = naomi_data$anc_prev_t1_dat$area_idx - 1L,
    x_anc_prev_t1 = naomi_data$anc_prev_t1_dat$anc_prev_x,
    n_anc_prev_t1 = naomi_data$anc_prev_t1_dat$anc_prev_n,
    idx_anc_artcov_t1 = naomi_data$anc_artcov_t1_dat$area_idx - 1L,
    x_anc_artcov_t1 = naomi_data$anc_artcov_t1_dat$anc_artcov_x,
    n_anc_artcov_t1 = naomi_data$anc_artcov_t1_dat$anc_artcov_n,
    idx_anc_prev_t2 = naomi_data$anc_prev_t2_dat$area_idx - 1L,
    x_anc_prev_t2 = naomi_data$anc_prev_t2_dat$anc_prev_x,
    n_anc_prev_t2 = naomi_data$anc_prev_t2_dat$anc_prev_n,
    idx_anc_artcov_t2 = naomi_data$anc_artcov_t2_dat$area_idx - 1L,
    x_anc_artcov_t2 = naomi_data$anc_artcov_t2_dat$anc_artcov_x,
    n_anc_artcov_t2 = naomi_data$anc_artcov_t2_dat$anc_artcov_n,
    ##
    A_artattend_t1 = A_artattend_t1,
    x_artnum_t1 = naomi_data$artnum_t1_dat$current_art,
    A_artattend_t2 = A_artattend_t2,
    x_artnum_t2 = naomi_data$artnum_t2_dat$current_art,
    A_artattend_mf = A_artattend_mf,
    A_art_reside_attend = A_art_reside_attend,
    ##
    ## Time 3 projection inputs
    population_t3 = df$population_t3,
    Lproj_hivpop_t2t3 = naomi_data$Lproj_hivpop_t2t3,
    Lproj_incid_t2t3 = naomi_data$Lproj_incid_t2t3,
    Lproj_paed_t2t3 = naomi_data$Lproj_paed_t2t3,
    projection_duration_t2t3 = naomi_data$projection_duration_t2t3,
    logit_alpha_t2t3_offset = df$logit_alpha_t2t3_offset,
    log_lambda_t3_offset = df$log_lambda_t3_offset,
    ##
    A_out = naomi_data$A_out,
    calc_outputs = 1L
  )


  ptmb <- list(
    beta_rho = numeric(ncol(dtmb$X_rho)),
    beta_alpha = numeric(ncol(dtmb$X_alpha)),
    beta_alpha_t2 = numeric(ncol(dtmb$X_alpha_t2)),
    beta_lambda = numeric(ncol(dtmb$X_lambda)),
    beta_anc_rho = numeric(1),
    beta_anc_alpha = numeric(1),
    beta_anc_rho_t2 = numeric(1),
    beta_anc_alpha_t2 = numeric(1),
    us_rho_x = numeric(ncol(dtmb$Z_rho_x)),
    ui_rho_x = numeric(ncol(dtmb$Z_rho_x)),
    us_rho_xs = numeric(ncol(dtmb$Z_rho_xs)),
    ui_rho_xs = numeric(ncol(dtmb$Z_rho_xs)),
    u_rho_a = numeric(ncol(dtmb$Z_rho_a)),
    u_rho_as = numeric(ncol(dtmb$Z_rho_as)),
    u_rho_xa = numeric(ncol(dtmb$Z_rho_xa)),
    ui_anc_rho_x = numeric(ncol(dtmb$Z_ancrho_x)),
    ui_anc_alpha_x = numeric(ncol(dtmb$Z_ancalpha_x)),
    ui_anc_rho_xt = numeric(ncol(dtmb$Z_ancrho_x)),
    ui_anc_alpha_xt = numeric(ncol(dtmb$Z_ancalpha_x)),
    ##
    us_alpha_x = numeric(ncol(dtmb$Z_alpha_x)),
    ui_alpha_x = numeric(ncol(dtmb$Z_alpha_x)),
    us_alpha_xs = numeric(ncol(dtmb$Z_alpha_xs)),
    ui_alpha_xs = numeric(ncol(dtmb$Z_alpha_xs)),
    u_alpha_a = numeric(ncol(dtmb$Z_alpha_a)),
    u_alpha_as = numeric(ncol(dtmb$Z_alpha_as)),
    u_alpha_xt = numeric(ncol(dtmb$Z_alpha_xt)),
    u_alpha_xa = numeric(ncol(dtmb$Z_alpha_xa)),
    u_alpha_xat = numeric(ncol(dtmb$Z_alpha_xat)),
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
    ##
    OmegaT_raw = 0,
    log_betaT = 0,
    logit_nu_raw = 0,
    ##
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

make_tmb_obj <- function(data, par, calc_outputs = 1L, inner_verbose = FALSE) {

  data$calc_outputs <- as.integer(calc_outputs)
                                 
  obj <- TMB::MakeADFun(data = data,
                        parameters = par,
                        DLL = "naomi",
                        silent = !inner_verbose,
                        random = c("beta_rho",
                                   "beta_alpha", "beta_alpha_t2",
                                   "beta_lambda",
                                   "beta_anc_rho", "beta_anc_alpha",
                                   "beta_anc_rho_t2", "beta_anc_alpha_t2",
                                   "us_rho_x", "ui_rho_x",
                                   "us_rho_xs", "ui_rho_xs",
                                   "u_rho_a", "u_rho_as",
                                   "u_rho_xa",
                                   ##
                                   "us_alpha_x", "ui_alpha_x",
                                   "us_alpha_xs", "ui_alpha_xs",
                                   "u_alpha_a", "u_alpha_as",
                                   "u_alpha_xt",
                                   "u_alpha_xa", "u_alpha_xat",
                                   ##
                                   "ui_lambda_x",
                                   "logit_nu_raw",
                                   ##
                                   "ui_anc_rho_x", "ui_anc_alpha_x",
                                   "ui_anc_rho_xt", "ui_anc_alpha_xt",
                                   ##
                                   "log_or_gamma", "log_or_gamma_t1t2"))

  obj
}
  

#' Fit TMB model
#'
#' @param tmb_input Model input data
#' @param outer_verbose If TRUE print function and parameters every iteration
#' @param inner_verbose If TRUE then disable tracing information from TMB
#' @param max_iter maximum number of iterations
#'
#' @return Fit model.
#' @export
fit_tmb <- function(tmb_input,
                    outer_verbose = TRUE,
                    inner_verbose = FALSE,
                    max_iter = 250
                    ) {

  stopifnot(inherits(tmb_input, "naomi_tmb_input"))

  obj <- make_tmb_obj(tmb_input$data, tmb_input$par_init, calc_outputs = 0L, inner_verbose)

  trace <- if(outer_verbose) 1 else 0
  f <- withCallingHandlers(
    stats::nlminb(obj$par, obj$fn, obj$gr,
                  control = list(trace = trace,
                                 iter.max = max_iter)),
    warning = function(w) {
      if(grepl("NA/NaN function evaluation", w$message))
        invokeRestart("muffleWarning")
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

rmvnorm_sparseprec <- function(n, mean = rep(0, nrow(prec)), prec = diag(lenth(mean))) {

  z = matrix(rnorm(n * length(mean)), ncol = n)
  L_inv = Matrix::Cholesky(prec)
  v <- mean + Matrix::solve(as(L_inv, "pMatrix"), Matrix::solve(Matrix::t(as(L_inv, "Matrix")), z))
  as.matrix(Matrix::t(v))
}


create_artattend_Amat <- function(artnum_df, age_groups, sexes, area_aggregation,
                                  df_art_attend, by_residence = FALSE) {
  
  ## If by_residence = TRUE, merge by reside_area_id, else aggregate over all
  ## reside_area_id
  by_vars <- c("attend_area_id", "sex", "age_group")
  if(by_residence)
    by_vars <- c(by_vars, "reside_area_id")
  
  A_artnum <- artnum_df %>%
    dplyr::select(tidyselect::all_of(by_vars), artnum_idx) %>%
    dplyr::rename(artdat_age_group = age_group,
                  artdat_sex = sex) %>%
    dplyr::left_join(
             get_age_groups() %>%
             dplyr::transmute(
                      artdat_age_group = age_group,
                      artdat_age_start = age_group_start,
                      artdat_age_end = age_group_start + age_group_span
                    ),
             by = "artdat_age_group"
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
             by = "artdat_sex"
    )

  ## Map artattend_area_id to model_area_id
  A_artnum <- A_artnum %>%
    dplyr::left_join(
      area_aggregation,
      by = c("attend_area_id" = "area_id")
    ) %>%
    dplyr::mutate(attend_area_id = model_area_id,
                  model_area_id = NULL)

  ## Check no areas with duplicated reporting
  art_duplicated_check <- A_artnum %>%  
    dplyr::group_by_at(by_vars) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(n > 1)

  if (nrow(art_duplicated_check)) {
    stop(paste("ART data multiply reported for some age/sex strata in areas:",
               paste(unique(art_duplicated_check$attend_area_id), collapse = ", ")))
  }

  ## Merge to ART attendance data frame
  df_art_attend <- df_art_attend %>%
    dplyr::select(tidyselect::all_of(by_vars)) %>%
    dplyr::mutate(
      df_art_attend_idx = dplyr::row_number(),
      value = 1
    )
  
  A_artnum <- dplyr::left_join(A_artnum,
                               df_art_attend %>%
                               dplyr::select(tidyselect::all_of(by_vars)) %>%
                               dplyr::mutate(
                                 Aidx = dplyr::row_number(),
                                 value = 1),
                               by = by_vars)
  
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
