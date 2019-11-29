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

  ## ANC prevalence model matrices
  anc_prev_t1_dat <- naomi_data$anc_prev_t1_dat %>%
    dplyr::left_join(
             naomi_data$mf_areas,
             by = "area_id"
           )

  anc_artcov_t1_dat <- naomi_data$anc_artcov_t1_dat %>%
    dplyr::left_join(
             naomi_data$mf_areas,
             by = "area_id"
           )

  anc_prev_t2_dat <- naomi_data$anc_prev_t2_dat %>%
    dplyr::left_join(
             naomi_data$mf_areas,
             by = "area_id"
           )

  anc_artcov_t2_dat <- naomi_data$anc_artcov_t2_dat %>%
    dplyr::left_join(
             naomi_data$mf_areas,
             by = "area_id"
           )

  create_anc_Amat <- function(dat, naomi_mf, asfr_col, population_col) {
    A <- dat %>%
      dplyr::inner_join(
               naomi_data$mf_model,
               by = "area_id"
             ) %>%
      dplyr::transmute(
               area_id,
               anc_idx,
               idx,
               births = !!rlang::sym(asfr_col) * !!rlang::sym(population_col)
             ) %>%
      dplyr::filter(births > 0) %>%
    {
      Matrix::spMatrix(nrow(dat),
                       nrow(naomi_data$mf_model),
                       .$anc_idx,
                       .$idx,
                       .$births)
    }

    A
  }

  A_anc_prev_t1 <- create_anc_Amat(anc_prev_t1_dat, naomi_data, "asfr", "population_t1")
  A_anc_prev_t2 <- create_anc_Amat(anc_prev_t2_dat, naomi_data, "asfr", "population_t2")
  A_anc_artcov_t1 <- create_anc_Amat(anc_artcov_t1_dat, naomi_data, "asfr", "population_t1")
  A_anc_artcov_t2 <- create_anc_Amat(anc_artcov_t2_dat, naomi_data, "asfr", "population_t2")

  X_15to49 <- Matrix::t(Matrix::sparse.model.matrix(~-1 + area_idf:age15to49, naomi_data$mf_model))

  ## ART attendance aggregation

  Xgamma <-  Matrix::sparse.model.matrix(~0 + attend_area_idf:as.integer(jstar != 1),
                                         naomi_data$mf_artattend)
  
  df_art_attend <- naomi_data$mf_model %>%
    dplyr::left_join(naomi_data$mf_artattend, by = c("area_idx" = "reside_area_idx")) %>%
    dplyr::mutate(artattend_idf = forcats::as_factor(artattend_idx),
                  idf = forcats::as_factor(idx))

  Xart_gamma <- Matrix::sparse.model.matrix(~0 + artattend_idf, df_art_attend)
  Xart_idx <- Matrix::sparse.model.matrix(~0 + idf, df_art_attend)

  A_artattend_t1 <- create_artattend_Amat(naomi_data$artnum_t1_dat, naomi_data$age_group_ids, naomi_data$sexes, naomi_data$mf_areas, df_art_attend)
  A_artattend_t2 <- create_artattend_Amat(naomi_data$artnum_t2_dat, naomi_data$age_group_ids, naomi_data$sexes, naomi_data$mf_areas, df_art_attend)

  A_artattend_mf <- create_artattend_Amat(dplyr::select(naomi_data$mf_model, area_id, sex, age_group_id, artnum_idx = idx),
                                          naomi_data$age_group_ids, naomi_data$sexes, naomi_data$mf_areas, df_art_attend)

  ## Construct TMB data and initial parameter vectors

  df <- naomi_data$mf_model

  f_rho_a <- if(all(is.na(df$rho_a_fct))) ~0 else ~0 + rho_a_fct
  f_alpha_a <- if(all(is.na(df$alpha_a_fct))) ~0 else ~0 + alpha_a_fct

  dtmb <- list(
    population_t1 = df$population_t1,
    population_t2 = df$population_t2,
    Lproj = naomi_data$Lproj,
    X_rho = stats::model.matrix(~as.integer(sex == "female"), df),
    X_alpha = stats::model.matrix(~as.integer(sex == "female"), df),
    X_alpha_t2 = stats::model.matrix(~1, df),
    X_lambda = stats::model.matrix(~as.integer(sex == "female"), df),
    X_ancrho = stats::model.matrix(~1, anc_prev_t1_dat),
    X_ancalpha = stats::model.matrix(~1, anc_artcov_t1_dat),
    Z_x = Matrix::sparse.model.matrix(~0 + area_idf, df),
    Z_xs = Matrix::sparse.model.matrix(~0 + area_idf, df) * (df$sex == "female"),
    Z_rho_a = Matrix::sparse.model.matrix(f_rho_a, df),
    Z_rho_as = Matrix::sparse.model.matrix(f_rho_a, df) * (df$sex == "female"),
    Z_alpha_a = Matrix::sparse.model.matrix(f_alpha_a, df),
    Z_alpha_as = Matrix::sparse.model.matrix(f_alpha_a, df) * (df$sex == "female"),
    ## Z_xa = Matrix::sparse.model.matrix(~0 + area_idf:age_group_idf, df),
    Z_ancrho_x = Matrix::sparse.model.matrix(~0 + area_idf, anc_prev_t1_dat),
    Z_ancalpha_x = Matrix::sparse.model.matrix(~0 + area_idf, anc_artcov_t1_dat),
    ##
    logit_rho_offset = naomi_data$mf_model$logit_rho_offset,
    logit_alpha_offset = naomi_data$mf_model$logit_alpha_offset,
    ##
    Q_x = methods::as(naomi_data$Q, "dgCMatrix"),
    n_nb = naomi_data$mf_areas$n_neighbors,
    adj_i = naomi_data$mf_artattend$reside_area_idx - 1L,
    adj_j = naomi_data$mf_artattend$artattend_area_idx - 1L,
    Xgamma = Xgamma,
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
    log_lambda_offset = naomi_data$mf_model$log_lambda_offset,
    ##
    A_out = naomi_data$A_out,
    idx_prev = naomi_data$prev_dat$idx - 1L,
    x_prev = naomi_data$prev_dat$x,
    n_prev = naomi_data$prev_dat$n,
    idx_artcov = naomi_data$artcov_dat$idx - 1L,
    x_artcov = naomi_data$artcov_dat$x,
    n_artcov = naomi_data$artcov_dat$n,
    idx_vls = naomi_data$vls_dat$idx - 1L,
    x_vls = naomi_data$vls_dat$x,
    n_vls = naomi_data$vls_dat$n,
    idx_recent = naomi_data$recent_dat$idx - 1L,
    x_recent = naomi_data$recent_dat$x,
    n_recent = naomi_data$recent_dat$n,
    A_anc_prev = A_anc_prev_t1,
    x_anc_prev = anc_prev_t1_dat$anc_prev_x,
    n_anc_prev = anc_prev_t1_dat$anc_prev_n,
    A_anc_artcov = A_anc_artcov_t1,
    x_anc_artcov = anc_artcov_t1_dat$anc_artcov_x,
    n_anc_artcov = anc_artcov_t1_dat$anc_artcov_n,
    ##
    A_artattend_t1 = A_artattend_t1,
    x_artnum_t1 = naomi_data$artnum_t1_dat$current_art,
    A_artattend_t2 = A_artattend_t2,
    x_artnum_t2 = naomi_data$artnum_t2_dat$current_art,
    A_artattend_mf = A_artattend_mf
  )


  ptmb <- list(
    beta_rho = numeric(ncol(dtmb$X_rho)),
    beta_alpha = numeric(ncol(dtmb$X_alpha)),
    beta_lambda = numeric(ncol(dtmb$X_lambda)),
    beta_anc_rho = numeric(1),
    beta_anc_alpha = numeric(1),
    beta_alpha_t2 = numeric(ncol(dtmb$X_alpha_t2)),
    us_rho_x = numeric(ncol(dtmb$Z_x)),
    ui_rho_x = numeric(ncol(dtmb$Z_x)),
    us_rho_xs = numeric(ncol(dtmb$Z_xs)),
    ui_rho_xs = numeric(ncol(dtmb$Z_xs)),
    u_rho_a = numeric(ncol(dtmb$Z_rho_a)),
    u_rho_as = numeric(ncol(dtmb$Z_rho_as)),
    ui_anc_rho_x = numeric(ncol(dtmb$Z_x)),
    ui_anc_alpha_x = numeric(ncol(dtmb$Z_x)),
    ##
    us_alpha_x = numeric(ncol(dtmb$Z_x)),
    ui_alpha_x = numeric(ncol(dtmb$Z_x)),
    us_alpha_xs = numeric(ncol(dtmb$Z_xs)),
    ui_alpha_xs = numeric(ncol(dtmb$Z_xs)),
    u_alpha_a = numeric(ncol(dtmb$Z_alpha_a)),
    u_alpha_as = numeric(ncol(dtmb$Z_alpha_as)),
    u_alpha_xt = numeric(ncol(dtmb$Z_x)),
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
    log_sigma_alpha_xt = 0,
    ##
    OmegaT_raw = 0,
    log_betaT = 0,
    logit_nu_raw = 0,
    ##
    log_sigma_ancrho_x = 0,
    log_sigma_ancalpha_x = 0,
    ##
    log_or_gamma = numeric(ncol(dtmb$Xgamma)),
    log_sigma_or_gamma = 0
  )

  v <- list(data = dtmb,
            par_init = ptmb)
  class(v) <- "naomi_tmb_input"

  v
}


#' Fit TMB model
#'
#' @param tmb_input Model input data
#' @param outer_verbose If TRUE print function and parameters every iteration
#' @param inner_verbose If TRUE then disable tracing information from TMB
#'
#' @return Fit model.
#' @export
fit_tmb <- function(tmb_input, outer_verbose = TRUE, inner_verbose = FALSE) {

  stopifnot(inherits(tmb_input, "naomi_tmb_input"))

  obj <- TMB::MakeADFun(data = tmb_input$data,
                        parameters = tmb_input$par_init,
                        DLL = "naomi",
                        silent = !inner_verbose,
                        random = c("beta_rho", "beta_alpha", "beta_lambda",
                                   "beta_anc_rho", "beta_anc_alpha",
                                   "beta_alpha_t2",
                                   "us_rho_x", "ui_rho_x",
                                   "us_rho_xs", "ui_rho_xs",
                                   "u_rho_a", "u_rho_as",
                                   ##
                                   "us_alpha_x", "ui_alpha_x",
                                   "us_alpha_xs", "ui_alpha_xs",
                                   "u_alpha_a", "u_alpha_as",
                                   "u_alpha_xt",
                                   ##
                                   "ui_lambda_x",
                                   "logit_nu_raw",
                                   ##
                                   "ui_anc_rho_x", "ui_anc_alpha_x",
                                   ##
                                   "log_or_gamma"))

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



#' Sample TMB fit
#'
#' @param fit The TMB fit
#' @param nsample Number of samples
#' @param random_only Random only
#' @param verbose If TRUE prints additional information.
#'
#' @return Sampled fit.
#' @export
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
  is_vector <- vapply(fit$sample, class, character(1)) == "numeric"
  fit$sample[is_vector] <- lapply(fit$sample[is_vector], as.matrix, nrow = 1)
  names(fit$sample) <- names(r)

  fit
}

rmvnorm_sparseprec <- function(n, mean = rep(0, nrow(prec)), prec = diag(lenth(mean))) {

  z = matrix(rnorm(n * length(mean)), ncol = n)
  L_inv = Matrix::Cholesky(prec)
  v <- mean + Matrix::solve(as(L_inv, "pMatrix"), Matrix::solve(Matrix::t(as(L_inv, "Matrix")), z))
  as.matrix(Matrix::t(v))
}


create_artattend_Amat <- function(artnum_df, age_group_ids, sexes, mf_areas, df_art_attend) {

  A_artnum <- artnum_df %>%
    dplyr::select(area_id, sex, age_group_id, artnum_idx) %>%
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
             dplyr::filter(age_group_id %in% age_group_ids)
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
           ) %>%
    dplyr::left_join(
             dplyr::select(mf_areas, area_id, area_idx),
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
      Matrix::spMatrix(nrow(artnum_df),
                       nrow(df_art_attend),
                       .$artnum_idx,
                       .$Aidx,
                       .$value)
    }

  A_artnum
}
