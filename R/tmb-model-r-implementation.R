
#' R implementation of Naomi model
#'
#' @param d list of data inputs (from [`prepare_tmb_inputs()`])
#' @param p list of parameter values (from [`prepare_tmb_inputs()`])
#'
#' @return A list consisting of (1) reported objects from the TMB model,
#'   and (2) the value of the objective function.
#'
#' @details
#' This is primarily written to enable stepping through the TMB model
#' line by line.
#'
#' @export

naomi_objective_function_r <- function(d, p) {

  ## indexing:
  ## * rho: HIV prevalence model
  ## * alpha: ART coverage model
  ## * lambda: HIV incidence model
  ##
  ## * _x: area
  ## * _a: age
  ## * _s: sex
  ## * _t: time

  ## ** Initialize nll **
  val <- 0.0

  ## ** Parameters **

  ## fixed effects
  ## diffuse N(0.0, 5.0) prior distribution

  val <- val - sum(dnorm(p$beta_rho, 0.0, 5.0, TRUE))
  val <- val - sum(dnorm(p$beta_alpha, 0.0, 5.0, TRUE))
  val <- val - sum(dnorm(p$beta_alpha_t2, 0.0, 5.0, TRUE))
  val <- val - sum(dnorm(p$beta_lambda, 0.0, 5.0, TRUE))
  val <- val - sum(dnorm(p$beta_asfr, 0.0, 5.0, TRUE))
  val <- val - sum(dnorm(p$beta_anc_rho, 0.0, 5.0, TRUE))
  val <- val - sum(dnorm(p$beta_anc_alpha, 0.0, 5.0, TRUE))
  val <- val - sum(dnorm(p$beta_anc_rho_t2, 0.0, 5.0, TRUE))
  val <- val - sum(dnorm(p$beta_anc_alpha_t2, 0.0, 5.0, TRUE))


  ## * HIV prevalence model *

  ## hyper parameters

  phi_rho_x <- plogis(p$logit_phi_rho_x)
  val <- val - (log(phi_rho_x) +  log(1 - phi_rho_x))  ## change of variables: logit_phi_x ->v phi_x
  val <- val - dbeta(phi_rho_x, 0.5, 0.5, TRUE)

  sigma_rho_x <- exp(p$log_sigma_rho_x)
  val <- val - (dnorm(sigma_rho_x, 0.0, 2.5, TRUE) + p$log_sigma_rho_x)

  phi_rho_xs <- plogis(p$logit_phi_rho_xs)
  val <- val - (log(phi_rho_xs) +  log(1 - phi_rho_xs))  ## change of variables: logit_phi_xs -> phi_xs
  val <- dbeta(phi_rho_xs, 0.5, 0.5, TRUE)

  sigma_rho_xs <- exp(p$log_sigma_rho_xs)
  val <- val - (dnorm(sigma_rho_xs, 0.0, 2.5, TRUE) + p$log_sigma_rho_xs)

  val <- val - dnorm(p$logit_phi_rho_a, 0.0, 2.582, TRUE)  ## INLA default
  phi_rho_a <- 2.0 * plogis(p$logit_phi_rho_a) - 1.0

  sigma_rho_a <- exp(p$log_sigma_rho_a)
  val <- val - (dnorm(sigma_rho_a, 0.0, 2.5, TRUE) + p$log_sigma_rho_a)

  val <- val - dnorm(p$logit_phi_rho_as, 0.0, 2.582, TRUE)  ## INLA default
  phi_rho_as <- 2.0 * plogis(p$logit_phi_rho_as) - 1.0

  sigma_rho_as <- exp(p$log_sigma_rho_as)
  val <- val - (dnorm(sigma_rho_as, 0.0, 2.5, TRUE) + p$log_sigma_rho_as)

  sigma_rho_xa <- exp(p$log_sigma_rho_xa)
  val <- val - (dnorm(sigma_rho_xa, 0.0, 0.5, TRUE) + p$log_sigma_rho_xa)

  ## latent effects

  val <- val - dnorm(sum(p$us_rho_x), 0.0, 0.001 * length(p$us_rho_x), TRUE) ## soft sum-to-zero constraint
  val <- val - bym2_conditional_lpdf(p$u_rho_x, p$us_rho_x, sigma_rho_x, phi_rho_x, d$Q_x)

  if (length(p$u_rho_xs) > 0) {
    val <- val - dnorm(sum(p$us_rho_xs), 0.0, 0.001 * length(p$us_rho_xs), TRUE) ## soft sum-to-zero constraint
    val <- val - bym2_conditional_lpdf(p$u_rho_xs, p$us_rho_xs, sigma_rho_xs, phi_rho_xs, d$Q_x)
  }

  ## !!! TODO: AR1 NOT YET IMPLEMENTED
  ## if (u_rho_a.size() > 0) {
  ##   val += SCALE(AR1(phi_rho_a), sigma_rho_a)(u_rho_a);
  ## }

  ## ## !!! TODO: AR1 NOT YET IMPLEMENTED
  ## if (u_rho_a.size() > 0){
  ##   val += SCALE(AR1(phi_rho_as), sigma_rho_as)(u_rho_as);
  ## }

  if (length(p$u_rho_xa) > 0) {
    val <- val - dnorm(sum(p$u_rho_xa), 0.0, sigma_rho_xa * 0.001 * length(p$u_rho_xa), TRUE) ## soft sum-to-zero constraint

    ## ICAR model; without contant terms
    icar_nll <- -(nrow(d$Q_x) - d$Q_x_rankdef) * p$log_sigma_rho_xa -
      0.5 / (sigma_rho_xa * sigma_rho_xa) * sum(p$u_rho_xa * (d$Q_x %*% p$u_rho_xa))
    val <- val - icar_nll
  }


  ## * ART coverage model *

  phi_alpha_x <- plogis(p$logit_phi_alpha_x)
  val <- val - (log(phi_alpha_x) +  log(1 - phi_alpha_x))  ## change of variables: logit_phi_x -> phi_x
  val <- val - dbeta(phi_alpha_x, 0.5, 0.5, TRUE)

  sigma_alpha_x <- exp(p$log_sigma_alpha_x)
  val <- val - (dnorm(sigma_alpha_x, 0.0, 2.5, TRUE) + p$log_sigma_alpha_x)

  phi_alpha_xs <- plogis(p$logit_phi_alpha_xs)
  val <- val - (log(phi_alpha_xs) +  log(1 - phi_alpha_xs))  ## change of variables: logit_phi_xs -> phi_xs
  val <- val - dbeta(phi_alpha_xs, 0.5, 0.5, TRUE)

  sigma_alpha_xs <- exp(p$log_sigma_alpha_xs)
  val <- val - (dnorm(sigma_alpha_xs, 0.0, 2.5, TRUE) + p$log_sigma_alpha_xs)

  val <- val - dnorm(p$logit_phi_alpha_a, 0.0, 2.582, TRUE)  ## INLA default
  phi_alpha_a <- 2.0 * plogis(p$logit_phi_alpha_a) - 1.0

  sigma_alpha_a <- exp(p$log_sigma_alpha_a)
  val <- val - (dnorm(sigma_alpha_a, 0.0, 2.5, TRUE) + p$log_sigma_alpha_a)

  val <- val - dnorm(p$logit_phi_alpha_as, 0.0, 2.582, TRUE)  ## INLA default
  phi_alpha_as <- 2.0 * plogis(p$logit_phi_alpha_as) - 1.0

  sigma_alpha_as <- exp(p$log_sigma_alpha_as)
  val <- val - (dnorm(sigma_alpha_as, 0.0, 2.5, TRUE) + p$log_sigma_alpha_as)

  sigma_alpha_xt <- exp(p$log_sigma_alpha_xt)
  val <- val - (dnorm(sigma_alpha_xt, 0.0, 2.5, TRUE) + p$log_sigma_alpha_xt)

  sigma_alpha_xa <- exp(p$log_sigma_alpha_xa)
  val <- val - (dnorm(sigma_alpha_xa, 0.0, 2.5, TRUE) + p$log_sigma_alpha_xa)

  sigma_alpha_xat <- exp(p$log_sigma_alpha_xat)
  val <- val - (dnorm(sigma_alpha_xat, 0.0, 2.5, TRUE) + p$log_sigma_alpha_xat)

  val <- val - dnorm(sum(p$us_alpha_x), 0.0, 0.001 * length(p$us_alpha_x), TRUE) ## soft sum-to-zero constraint
  val <- val - bym2_conditional_lpdf(p$u_alpha_x, p$us_alpha_x, sigma_alpha_x, phi_alpha_x, d$Q_x)

  if (length(p$u_alpha_xs) > 0) {
    val <- val - dnorm(sum(p$us_alpha_xs), 0.0, 0.001 * length(p$us_alpha_xs), TRUE) ## soft sum-to-zero constraint
    val <- val - bym2_conditional_lpdf(p$u_alpha_xs, p$us_alpha_xs, sigma_alpha_xs, phi_alpha_xs, d$Q_x)
  }

  ## ## !!! TODO: AR1 NOT YET IMPLEMENTED
  ## if (u_alpha_a.size() > 0) {
  ##   val += SCALE(AR1(phi_alpha_a), sigma_alpha_a)(u_alpha_a);
  ## }

  ## ## !!! TODO: AR1 NOT YET IMPLEMENTED
  ## if (u_alpha_as.size() > 0) {
  ##   val += SCALE(AR1(phi_alpha_as), sigma_alpha_as)(u_alpha_as);
  ## }

  val <- val - sum(dnorm(p$u_alpha_xt, 0.0, sigma_alpha_xt, TRUE))

  val <- val - sum(dnorm(p$u_alpha_xa, 0.0, sigma_alpha_xa, TRUE))

  val <- val - sum(dnorm(p$u_alpha_xat, 0.0, sigma_alpha_xat, TRUE))

  ## * HIV incidence model *

  val <- val - dnorm(p$OmegaT_raw, 0.0, 1.0, TRUE)
  OmegaT <- d$OmegaT0 + p$OmegaT_raw * d$sigma_OmegaT

  val <- val - (dnorm(exp(p$log_betaT), 0.0, 1.0, TRUE) + p$log_betaT)
  betaT <- d$betaT0 + exp(p$log_betaT) * d$sigma_betaT

  val <- val - dnorm(p$logit_nu_raw, 0.0, 1.0, TRUE)
  nu <- plogis(d$logit_nu_mean + p$logit_nu_raw * d$logit_nu_sd)

  sigma_lambda_x <- exp(p$log_sigma_lambda_x)
  val <- val - (dnorm(sigma_lambda_x, 0.0, 1.0, TRUE) + p$log_sigma_lambda_x)

  val <- val - sum(dnorm(p$ui_lambda_x, 0.0, sigma_lambda_x, TRUE))

  ## * ANC testing model *

  ## district ASFR random effects
  sigma_asfr_x <- exp(p$log_sigma_asfr_x)
  val <- val - (dnorm(sigma_asfr_x, 0.0, 2.5, TRUE) + p$log_sigma_asfr_x)

  val <- val - sum(dnorm(p$ui_asfr_x, 0.0, sigma_asfr_x, TRUE))

  ## ANC prevalence and ART coverage random effects
  sigma_ancrho_x <- exp(p$log_sigma_ancrho_x)
  val <- val - (dnorm(sigma_ancrho_x, 0.0, 2.5, TRUE) + p$log_sigma_ancrho_x)

  sigma_ancalpha_x <- exp(p$log_sigma_ancalpha_x)
  val <- val - (dnorm(sigma_ancalpha_x, 0.0, 2.5, TRUE) + p$log_sigma_ancalpha_x)

  val <- val - sum(dnorm(p$ui_anc_rho_x, 0.0, sigma_ancrho_x, TRUE))

  val <- val - sum(dnorm(p$ui_anc_alpha_x, 0.0, sigma_ancalpha_x, TRUE))

  sigma_ancrho_xt <- exp(p$log_sigma_ancrho_xt)
  val <- val - (dnorm(sigma_ancrho_xt, 0.0, 2.5, TRUE) + p$log_sigma_ancrho_xt)

  sigma_ancalpha_xt <- exp(p$log_sigma_ancalpha_xt)
  val <- val - (dnorm(sigma_ancalpha_xt, 0.0, 2.5, TRUE) + p$log_sigma_ancalpha_xt)

  val <- val - sum(dnorm(p$ui_anc_rho_xt, 0.0, sigma_ancrho_xt, TRUE))

  val <- val - sum(dnorm(p$ui_anc_alpha_xt, 0.0, sigma_ancalpha_xt, TRUE))


  ## * ART attendance model *

  sigma_or_gamma <- exp(p$log_sigma_or_gamma)
  val <- val - (dnorm(sigma_or_gamma, 0.0, 2.5, TRUE) + p$log_sigma_or_gamma)

  val <- val - sum(dnorm(p$log_or_gamma, 0.0, sigma_or_gamma, TRUE))

  sigma_or_gamma_t1t2 <- exp(p$log_sigma_or_gamma_t1t2)
  val <- val - (dnorm(sigma_or_gamma_t1t2, 0.0, 2.5, TRUE) + p$log_sigma_or_gamma_t1t2)

  val <- val - sum(dnorm(p$log_or_gamma_t1t2, 0.0, sigma_or_gamma_t1t2, TRUE))


  ## *** Process model ***

  ## HIV prevalence time 1

  mu_rho <- d$X_rho %*% p$beta_rho +
    d$logit_rho_offset +
    d$Z_rho_x %*% p$u_rho_x +
    d$Z_rho_xs %*% p$u_rho_xs +
    d$Z_rho_a %*% p$u_rho_a +
    d$Z_rho_as %*% p$u_rho_as +
    d$Z_rho_xa %*% p$u_rho_xa
  mu_rho <- as.vector(mu_rho)

  ## paediatric prevalence
  rho_15to49f_t1 <- d$X_15to49f %*% (plogis(mu_rho) * d$population_t1) / (d$X_15to49f %*% d$population_t1)
  mu_rho_paed <- d$X_paed_rho_ratio %*% rho_15to49f_t1 + d$paed_rho_ratio_offset
  mu_rho_paed <- as.vector(mu_rho_paed)
  mu_rho_paed <- qlogis(mu_rho_paed)
  mu_rho <- mu_rho + mu_rho_paed

  
  ## ART coverage time 1

  mu_alpha <- d$X_alpha %*% p$beta_alpha +
    d$logit_alpha_offset +
    d$Z_alpha_x %*% p$u_alpha_x +
    d$Z_alpha_xs %*% p$u_alpha_xs +
    d$Z_alpha_a %*% p$u_alpha_a +
    d$Z_alpha_as %*% p$u_alpha_as +
    d$Z_alpha_xa %*% p$u_alpha_xa
  mu_alpha <- as.vector(mu_alpha)


  rho_t1 <- plogis(mu_rho)
  alpha_t1 <- plogis(mu_alpha)

  plhiv_t1 <- d$population_t1 * rho_t1
  prop_art_t1 <- rho_t1 * alpha_t1
  artnum_t1 <- d$population_t1 * prop_art_t1

  plhiv_15to49_t1 <- d$X_15to49 %*% plhiv_t1
  rho_15to49_t1 <- plhiv_15to49_t1 / as.vector(d$X_15to49 %*% d$population_t1)
  alpha_15to49_t1 <- (d$X_15to49 %*% artnum_t1) / plhiv_15to49_t1

  mu_lambda_t1 <- d$X_lambda %*% p$beta_lambda +
    d$log_lambda_t1_offset +
    d$Z_x %*% (log(rho_15to49_t1) + log(1.0 - d$omega * alpha_15to49_t1)) +
    d$Z_lambda_x %*% p$ui_lambda_x
  mu_lambda_t1 <- as.vector(mu_lambda_t1)

  lambda_adult_t1 <- exp(mu_lambda_t1)

  ## Add paediatric incidence
  lambda_paed_t1 <- as.vector(d$X_paed_lambda_ratio_t1 %*% rho_15to49f_t1)
  lambda_t1 <- lambda_adult_t1 + lambda_paed_t1

  infections_t1 <- lambda_t1 * (d$population_t1 - plhiv_t1)


  ## Projection from t1 to t2

  mu_alpha_t2 <- mu_alpha + d$logit_alpha_t1t2_offset +
    d$X_alpha_t2 %*% p$beta_alpha_t2 +
    d$Z_alpha_xt %*% p$u_alpha_xt +
    d$Z_alpha_xat %*% p$u_alpha_xat
  mu_alpha_t2 <- as.vector(mu_alpha_t2)

  alpha_t2 <- plogis(mu_alpha_t2)

  infections_adult_t1t2 <- lambda_adult_t1 * (d$population_t1 - plhiv_t1)
  plhiv_t2 <- d$Lproj_hivpop_t1t2 %*% plhiv_t1 +
    d$Lproj_incid_t1t2  %*% infections_adult_t1t2 +
    d$Lproj_paed_t1t2  %*% plhiv_t1
  plhiv_t2 <- as.vector(plhiv_t2)

  rho_t2 <- plhiv_t2 / d$population_t2
  prop_art_t2 <- rho_t2 * alpha_t2
  artnum_t2 <- d$population_t2 * prop_art_t2

  plhiv_15to49_t2 <- d$X_15to49 %*% plhiv_t2
  rho_15to49_t2 <- plhiv_15to49_t2 / (d$X_15to49 %*% d$population_t2)
  alpha_15to49_t2 <- (d$X_15to49 %*% artnum_t2) / plhiv_15to49_t2

  mu_lambda_t2 <- d$X_lambda %*% p$beta_lambda +
    d$log_lambda_t2_offset +
    d$Z_x %*% (log(rho_15to49_t2) + log(1.0 - d$omega * alpha_15to49_t2)) +
    d$Z_lambda_x %*% p$ui_lambda_x
  mu_lambda_t2 <- as.vector(mu_lambda_t2)

  lambda_adult_t2 <- exp(mu_lambda_t2)

  ## Add paediatric incidence
  rho_15to49f_t2 <- d$X_15to49f %*% (plogis(mu_rho) * d$population_t2) / (d$X_15to49f %*% d$population_t2)  
  lambda_paed_t2 <- as.vector(d$X_paed_lambda_ratio_t2 %*% rho_15to49f_t2)
  lambda_t2 <- lambda_adult_t2 + lambda_paed_t2

  infections_t2 <- lambda_t2 * (d$population_t2 - plhiv_t2)

  ## likelihood for household survey data

  rho_obs_t1 <- as.vector(d$A_prev %*% plhiv_t1) / as.vector(d$A_prev %*% d$population_t1)
  val <- val - sum(ldbinom(d$x_prev, d$n_prev, rho_obs_t1))

  alpha_obs_t1 <- as.vector(d$A_artcov %*% artnum_t1) / as.vector(d$A_artcov %*% plhiv_t1)
  val <- val - sum(ldbinom(d$x_artcov, d$n_artcov, alpha_obs_t1))

  vls_obs_t1 <- nu * as.vector(d$A_vls %*% artnum_t1) / as.vector(d$A_vls %*% plhiv_t1)
  val <- val - sum(ldbinom(d$x_vls, d$n_vls, vls_obs_t1))

  pR_infections_obs_t1 <- as.vector(d$A_recent %*% infections_t1)
  pR_plhiv_obs_t1 <- as.vector(d$A_recent %*% plhiv_t1)
  pR_population_obs_t1 <- as.vector(d$A_recent %*% d$population_t1)
  pR_lambda_obs_t1 <- pR_infections_obs_t1 / (pR_population_obs_t1 - pR_plhiv_obs_t1)
  pR_rho_obs_t1 <- pR_plhiv_obs_t1 / pR_population_obs_t1
  pR <- 1.0 - exp(-(pR_lambda_obs_t1 * (1.0 - pR_rho_obs_t1) / pR_rho_obs_t1 *
                    (OmegaT - betaT * d$ritaT) + betaT))
  val <- val - sum(ldbinom(d$x_recent, d$n_recent, pR))


  ## ANC prevalence and ART coverage model
  ## Note: currently this operates on the entire population vector, producing
  ##       lots of zeros for males and female age groups not exposed to fertility.
  ##       It would be more computationally efficient to project this to subset
  ##       of female age 15-49 age groups. But I don't know if it would be
  ##       meaningfully more efficient.

  mu_asfr <- d$X_asfr %*% p$beta_asfr +
    d$Z_asfr_x %*% p$ui_asfr_x

  mu_anc_rho_t1 <- mu_rho +
    d$logit_anc_rho_t1_offset +
    d$X_ancrho %*% p$beta_anc_rho +
    d$Z_ancrho_x %*% p$ui_anc_rho_x
  mu_anc_rho_t1 <- as.vector(mu_anc_rho_t1)
  anc_rho_t1 <- plogis(mu_anc_rho_t1)

  mu_anc_alpha_t1 <- mu_alpha +
    d$logit_anc_alpha_t1_offset +
    d$X_ancalpha %*% p$beta_anc_alpha +
    d$Z_ancalpha_x %*% p$ui_anc_alpha_x
  mu_anc_alpha_t1 <- as.vector(mu_anc_alpha_t1)
  anc_alpha_t1 <- plogis(mu_anc_alpha_t1)

  anc_clients_t1 <- d$population_t1 * exp(d$log_asfr_t1_offset + mu_asfr)
  anc_plhiv_t1 <- anc_clients_t1 * anc_rho_t1
  anc_already_art_t1 <- anc_plhiv_t1 * anc_alpha_t1

  mu_anc_rho_t2 <- qlogis(rho_t2) +
    d$logit_anc_rho_t2_offset +
    d$X_ancrho %*% (p$beta_anc_rho + p$beta_anc_rho_t2) +
    d$Z_ancrho_x %*% (p$ui_anc_rho_x + p$ui_anc_rho_xt)
  mu_anc_rho_t2 <- as.vector(mu_anc_rho_t2)
  anc_rho_t2 <- plogis(mu_anc_rho_t2)

  mu_anc_alpha_t2 <- mu_alpha_t2 +
    d$logit_anc_alpha_t2_offset +
    d$X_ancalpha %*% (p$beta_anc_alpha + p$beta_anc_alpha_t2) +
    d$Z_ancalpha_x %*% (p$ui_anc_alpha_x + p$ui_anc_alpha_xt)
  mu_anc_alpha_t2 <- as.vector(mu_anc_alpha_t2)
  anc_alpha_t2 <- plogis(mu_anc_alpha_t2)

  anc_clients_t2 <- d$population_t2 * exp(d$log_asfr_t2_offset + mu_asfr)
  anc_plhiv_t2 <- anc_clients_t2 * anc_rho_t2
  anc_already_art_t2 <- anc_plhiv_t2 * anc_alpha_t2

  ## likelihood for ANC testing observations

  anc_clients_obs_t2 <- as.vector(d$A_anc_clients_t2 %*% anc_clients_t2) * exp(d$offset_anc_clients_t2)
  val <- val - sum(dpois(d$x_anc_clients_t2, anc_clients_obs_t2, TRUE))

  anc_rho_obs_t1 <- as.vector(d$A_anc_prev_t1 %*% anc_plhiv_t1) / as.vector(d$A_anc_prev_t1 %*% anc_clients_t1)
  val <- val - sum(ldbinom(d$x_anc_prev_t1, d$n_anc_prev_t1, anc_rho_obs_t1))

  anc_alpha_obs_t1 <- as.vector(d$A_anc_artcov_t1 %*% anc_already_art_t1) / as.vector(d$A_anc_artcov_t1 %*% anc_plhiv_t1)
  val <- val - sum(ldbinom(d$x_anc_artcov_t1, d$n_anc_artcov_t1, anc_alpha_obs_t1))

  anc_rho_obs_t2 <- as.vector(d$A_anc_prev_t2 %*% anc_plhiv_t2) / as.vector(d$A_anc_prev_t2 %*% anc_clients_t2)
  val <- val - sum(ldbinom(d$x_anc_prev_t2, d$n_anc_prev_t2, anc_rho_obs_t2))

  anc_alpha_obs_t2 <- as.vector(d$A_anc_artcov_t2 %*% anc_already_art_t2) / as.vector(d$A_anc_artcov_t2 %*% anc_plhiv_t2)
  val <- val - sum(ldbinom(d$x_anc_artcov_t2, d$n_anc_artcov_t2, anc_alpha_obs_t2))


  ## * ART attendance model *

  gamma_art <- exp(as.vector(d$Xgamma %*% p$log_or_gamma) + d$log_gamma_offset)
  cum_nb <- 0
  for(i in seq_along(d$n_nb)) {
    gamma_i_idx <- cum_nb + (i-1) + seq.int(d$n_nb[i]+1)
    gamma_art[gamma_i_idx] <- gamma_art[gamma_i_idx] / sum(gamma_art[gamma_i_idx])
    cum_nb <- cum_nb + d$n_nb[i]
  }

  prop_art_ij_t1 <- as.vector(d$Xart_idx %*% prop_art_t1) * as.vector(d$Xart_gamma %*% gamma_art)
  population_ij_t1 <- as.vector(d$Xart_idx %*% d$population_t1)

  artnum_ij_t1 <- population_ij_t1 * prop_art_ij_t1
  A_j_t1 <- as.vector(d$A_artattend_t1 %*% artnum_ij_t1)
  sd_A_j_t1 <- d$A_artattend_t1 %*% (population_ij_t1 * prop_art_ij_t1 * (1 - prop_art_ij_t1))
  sd_A_j_t1 <- sqrt(as.vector(sd_A_j_t1))

  val <- val - sum(dnorm(d$x_artnum_t1, A_j_t1, sd_A_j_t1, TRUE))

  gamma_art_t2 <- exp(as.vector(d$Xgamma %*% p$log_or_gamma + d$Xgamma_t2 %*% p$log_or_gamma_t1t2 + d$log_gamma_offset))
  cum_nb <- 0
  for(i in seq_along(d$n_nb)) {
    gamma_i_idx <- cum_nb + (i-1) + seq.int(d$n_nb[i]+1)
    gamma_art_t2[gamma_i_idx] <-  gamma_art_t2[gamma_i_idx] / sum(gamma_art_t2[gamma_i_idx])
    cum_nb <- cum_nb + d$n_nb[i]
  }

  prop_art_ij_t2 <- as.vector(d$Xart_idx %*% prop_art_t2) * as.vector(d$Xart_gamma %*% gamma_art_t2)
  population_ij_t2 <- as.vector(d$Xart_idx %*% d$population_t2)

  artnum_ij_t2 <- population_ij_t2 * prop_art_ij_t2
  A_j_t2 <- as.vector(d$A_artattend_t2 %*% artnum_ij_t2)
  sd_A_j_t2 <- d$A_artattend_t2 %*% (population_ij_t2 * prop_art_ij_t2 * (1 - prop_art_ij_t2))
  sd_A_j_t2 <- sqrt(as.vector(sd_A_j_t2))

  val <- val - sum(dnorm(d$x_artnum_t2, A_j_t2, sd_A_j_t2, TRUE))

  ## **Calculate model outputs**

  population_t1_out <- as.vector(d$A_out %*% d$population_t1)

  plhiv_t1_out <- as.vector(d$A_out %*% plhiv_t1)
  rho_t1_out <- plhiv_t1_out / population_t1_out

  artnum_t1_out <- as.vector(d$A_out %*% artnum_t1)
  alpha_t1_out <- artnum_t1_out / plhiv_t1_out
  artattend_t1_out <- as.vector(d$A_out %*% (d$A_artattend_mf %*% artnum_ij_t1))
  artattend_ij_t1_out <- as.vector(d$A_art_reside_attend %*% artnum_ij_t1)
  untreated_plhiv_num_t1_out <- plhiv_t1_out - artnum_t1_out

  ## Calculate number of PLHIV who would attend facility in district i
  plhiv_attend_ij_t1 <- as.vector(d$Xart_idx %*% plhiv_t1) * as.vector(d$Xart_gamma %*% gamma_art)
  plhiv_attend_t1_out <- as.vector(d$A_out %*% (d$A_artattend_mf %*% plhiv_attend_ij_t1))
  untreated_plhiv_attend_t1_out <- plhiv_attend_t1_out - artattend_t1_out

  unaware_plhiv_num_t1 <- (plhiv_t1 - artnum_t1) * d$unaware_untreated_prop_t1
  unaware_plhiv_num_t1_out <- as.vector(d$A_out %*% unaware_plhiv_num_t1)
  aware_plhiv_num_t1_out <- plhiv_t1_out - unaware_plhiv_num_t1_out
  aware_plhiv_prop_t1_out <- aware_plhiv_num_t1_out / plhiv_t1_out

  infections_t1_out <- as.vector(d$A_out %*% infections_t1)
  lambda_t1_out <- infections_t1_out / (population_t1_out - plhiv_t1_out)

  population_t2_out <- as.vector(d$A_out %*% d$population_t2)

  plhiv_t2_out <- as.vector(d$A_out %*% plhiv_t2)
  rho_t2_out <- plhiv_t2_out / population_t2_out

  artnum_t2_out <- as.vector(d$A_out %*% artnum_t2)
  alpha_t2_out <- artnum_t2_out / plhiv_t2_out
  artattend_t2_out <- as.vector(d$A_out %*% (d$A_artattend_mf %*% artnum_ij_t2))
  artattend_ij_t2_out <- as.vector(d$A_art_reside_attend %*% artnum_ij_t2)
  untreated_plhiv_num_t2_out <- plhiv_t2_out - artnum_t2_out

  ## Calculate number of PLHIV who would attend facility in district i
  plhiv_attend_ij_t2 <- as.vector(d$Xart_idx %*% plhiv_t2) * as.vector(d$Xart_gamma %*% gamma_art_t2)
  plhiv_attend_t2_out <- as.vector(d$A_out %*% (d$A_artattend_mf %*% plhiv_attend_ij_t2))
  untreated_plhiv_attend_t2_out <- plhiv_attend_t2_out - artattend_t2_out


  unaware_plhiv_num_t2 <- (plhiv_t2 - artnum_t2) * d$unaware_untreated_prop_t2
  unaware_plhiv_num_t2_out <- as.vector(d$A_out %*% unaware_plhiv_num_t2)
  aware_plhiv_num_t2_out <- plhiv_t2_out - unaware_plhiv_num_t2_out
  aware_plhiv_prop_t2_out <- aware_plhiv_num_t2_out / plhiv_t2_out

  infections_t2_out <- as.vector(d$A_out %*% infections_t2)
  lambda_t2_out <- infections_t2_out / (population_t2_out - plhiv_t2_out)

  anc_clients_t1_out <- as.vector(d$A_anc_out %*% anc_clients_t1)
  anc_plhiv_t1_out <- as.vector(d$A_anc_out %*% anc_plhiv_t1)
  anc_already_art_t1_out <- as.vector(d$A_anc_out %*% anc_already_art_t1)

  ## Note: assuming that:
  ##  (1) anc_known_pos is equivalent to anc_already_art
  ##  (2) All ANC attendees are diagnosed and initated on ART.
  anc_art_new_t1_out <- anc_plhiv_t1_out - anc_already_art_t1_out
  anc_known_pos_t1_out <- anc_already_art_t1_out
  anc_tested_pos_t1_out <- anc_plhiv_t1_out - anc_known_pos_t1_out
  anc_tested_neg_t1_out <- anc_clients_t1_out - anc_plhiv_t1_out

  anc_rho_t1_out <- anc_plhiv_t1_out / anc_clients_t1_out
  anc_alpha_t1_out <- anc_already_art_t1_out / anc_plhiv_t1_out

  anc_clients_t2_out <- as.vector(d$A_anc_out %*% anc_clients_t2)
  anc_plhiv_t2_out <- as.vector(d$A_anc_out %*% anc_plhiv_t2)
  anc_already_art_t2_out <- as.vector(d$A_anc_out %*% anc_already_art_t2)

  anc_art_new_t2_out <- anc_plhiv_t2_out - anc_already_art_t2_out
  anc_known_pos_t2_out <- anc_already_art_t2_out
  anc_tested_pos_t2_out <- anc_plhiv_t2_out - anc_known_pos_t2_out
  anc_tested_neg_t2_out <- anc_clients_t2_out - anc_plhiv_t2_out

  anc_rho_t2_out <- anc_plhiv_t2_out / anc_clients_t2_out
  anc_alpha_t2_out <- anc_already_art_t2_out / anc_plhiv_t2_out


  report_t1 <- list(population_t1_out              = population_t1_out,
                    rho_t1_out                     = rho_t1_out,
                    plhiv_t1_out                   = plhiv_t1_out,
                    alpha_t1_out                   = alpha_t1_out,
                    artnum_t1_out                  = artnum_t1_out,
                    artattend_t1_out               = artattend_t1_out,
                    artattend_ij_t1_out            = artattend_ij_t1_out,
                    untreated_plhiv_num_t1_out     = untreated_plhiv_num_t1_out,
                    plhiv_attend_t1_out            = plhiv_attend_t1_out,
                    untreated_plhiv_attend_t1_out  = untreated_plhiv_attend_t1_out,
                    aware_plhiv_prop_t1_out        = aware_plhiv_prop_t1_out,
                    aware_plhiv_num_t1_out         = aware_plhiv_num_t1_out,
                    unaware_plhiv_num_t1_out       = unaware_plhiv_num_t1_out,
                    lambda_t1_out                  = lambda_t1_out,
                    infections_t1_out              = infections_t1_out,
                    anc_clients_t1_out             = anc_clients_t1_out,
                    anc_plhiv_t1_out               = anc_plhiv_t1_out,
                    anc_already_art_t1_out         = anc_already_art_t1_out,
                    anc_art_new_t1_out             = anc_art_new_t1_out,
                    anc_known_pos_t1_out           = anc_known_pos_t1_out,
                    anc_tested_pos_t1_out          = anc_tested_pos_t1_out,
                    anc_tested_neg_t1_out          = anc_tested_neg_t1_out,
                    anc_rho_t1_out                 = anc_rho_t1_out,
                    anc_alpha_t1_out               = anc_alpha_t1_out)

  report_t2 <- list(population_t2_out              = population_t2_out,
                    rho_t2_out                     = rho_t2_out,
                    plhiv_t2_out                   = plhiv_t2_out,
                    alpha_t2_out                   = alpha_t2_out,
                    artnum_t2_out                  = artnum_t2_out,
                    artattend_t2_out               = artattend_t2_out,
                    artattend_ij_t2_out            = artattend_ij_t2_out,
                    untreated_plhiv_num_t2_out     = untreated_plhiv_num_t2_out,
                    plhiv_attend_t2_out            = plhiv_attend_t2_out,
                    untreated_plhiv_attend_t2_out  = untreated_plhiv_attend_t2_out,
                    aware_plhiv_prop_t2_out        = aware_plhiv_prop_t2_out,
                    aware_plhiv_num_t2_out         = aware_plhiv_num_t2_out,
                    unaware_plhiv_num_t2_out       = unaware_plhiv_num_t2_out,
                    lambda_t2_out                  = lambda_t2_out,
                    infections_t2_out              = infections_t2_out,
                    anc_clients_t2_out             = anc_clients_t2_out,
                    anc_plhiv_t2_out               = anc_plhiv_t2_out,
                    anc_already_art_t2_out         = anc_already_art_t2_out,
                    anc_art_new_t2_out             = anc_art_new_t2_out,
                    anc_known_pos_t2_out           = anc_known_pos_t2_out,
                    anc_tested_pos_t2_out          = anc_tested_pos_t2_out,
                    anc_tested_neg_t2_out          = anc_tested_neg_t2_out,
                    anc_rho_t2_out                 = anc_rho_t2_out,
                    anc_alpha_t2_out               = anc_alpha_t2_out)

 
  ## Projection to time 3
  ## No data involved, so calculated after likelihood

  mu_alpha_t3 <- mu_alpha_t2 + d$logit_alpha_t2t3_offset
  alpha_t3 <- plogis(mu_alpha_t3)

  infections_adult_t2t3 <- lambda_adult_t2 * (d$population_t2 - plhiv_t2)
  plhiv_t3 <- as.vector(d$Lproj_hivpop_t2t3 %*% plhiv_t2 +
                        d$Lproj_incid_t2t3 %*% infections_adult_t2t3 +
                        d$Lproj_paed_t2t3 %*% plhiv_t2)

  rho_t3 <- plhiv_t3 / d$population_t3
  prop_art_t3 <- rho_t3 * alpha_t3
  artnum_t3 <- d$population_t3 * prop_art_t3

  plhiv_15to49_t3 <- as.vector(d$X_15to49 %*% plhiv_t3)
  rho_15to49_t3 <- plhiv_15to49_t3 / as.vector(d$X_15to49 %*% d$population_t3)
  alpha_15to49_t3 <- as.vector(d$X_15to49 %*% artnum_t3) / plhiv_15to49_t3

  mu_lambda_t3 <- d$X_lambda %*% p$beta_lambda +
    d$log_lambda_t3_offset +
    d$Z_x %*% (log(rho_15to49_t3) + log(1.0 - d$omega * alpha_15to49_t3)) +
    d$Z_lambda_x %*% p$ui_lambda_x

  lambda_adult_t3 <- exp(mu_lambda_t3)

  ## Add paediatric incidence
  rho_15to49f_t3 <- d$X_15to49f %*% (plogis(mu_rho) * d$population_t3) / (d$X_15to49f %*% d$population_t3)  
  lambda_paed_t3 <- as.vector(d$X_paed_lambda_ratio_t3 %*% rho_15to49f_t3)
  lambda_t3 <- lambda_adult_t3 + lambda_paed_t3

  
  infections_t3 <- lambda_t3 * (d$population_t3 - plhiv_t3)


  ## Note: currently assuming same district effects parameters from t2 for t3
  mu_anc_rho_t3 <- qlogis(rho_t3) +
    d$logit_anc_rho_t2_offset +
    d$X_ancrho %*% (p$beta_anc_rho + p$beta_anc_rho_t2) +
    d$Z_ancrho_x %*% (p$ui_anc_rho_x + p$ui_anc_rho_xt)
  mu_anc_rho_t3 <- as.vector(mu_anc_rho_t3)
  anc_rho_t3 <- plogis(mu_anc_rho_t3)

  mu_anc_alpha_t3 <- mu_alpha_t3 +
    d$logit_anc_alpha_t3_offset +
    d$X_ancalpha %*% (p$beta_anc_alpha + p$beta_anc_alpha_t2) +
    d$Z_ancalpha_x %*% (p$ui_anc_alpha_x + p$ui_anc_alpha_xt)
  mu_anc_alpha_t3 <- as.vector(mu_anc_alpha_t3)
  anc_alpha_t3 <- plogis(mu_anc_alpha_t3)

  anc_clients_t3 <- d$population_t3 * exp(d$log_asfr_t3_offset + mu_asfr)
  anc_plhiv_t3 <- anc_clients_t3 * anc_rho_t3
  anc_already_art_t3 <- anc_plhiv_t3 * anc_alpha_t3


  prop_art_ij_t3 <- as.vector(d$Xart_idx %*% prop_art_t3) * as.vector(d$Xart_gamma %*% gamma_art_t2)  ## Note: using same ART attendance as T2
  population_ij_t3 <- as.vector(d$Xart_idx %*% d$population_t3)
  artnum_ij_t3 <- population_ij_t3 * prop_art_ij_t3

  population_t3_out <- as.vector(d$A_out %*% d$population_t3)
  plhiv_t3_out <- as.vector(d$A_out %*% plhiv_t3)
  rho_t3_out <- plhiv_t3_out / population_t3_out
  artnum_t3_out <- as.vector(d$A_out %*% artnum_t3)
  alpha_t3_out <- artnum_t3_out / plhiv_t3_out
  artattend_t3_out <- as.vector(d$A_out %*% (d$A_artattend_mf %*% artnum_ij_t3))
  artattend_ij_t3_out <- as.vector(d$A_art_reside_attend %*% artnum_ij_t3)
  untreated_plhiv_num_t3_out <- plhiv_t3_out - artnum_t3_out

  ## Calculate number of PLHIV who would attend facility in district i
  plhiv_attend_ij_t3 <- as.vector(d$Xart_idx %*% plhiv_t3) * as.vector(d$Xart_gamma %*% gamma_art_t2)
  plhiv_attend_t3_out <- as.vector(d$A_out %*% (d$A_artattend_mf %*% plhiv_attend_ij_t3))
  untreated_plhiv_attend_t3_out <- plhiv_attend_t3_out - artattend_t3_out


  unaware_plhiv_num_t3 <- (plhiv_t3 - artnum_t3) * d$unaware_untreated_prop_t3
  unaware_plhiv_num_t3_out <- as.vector(d$A_out %*% unaware_plhiv_num_t3)
  aware_plhiv_num_t3_out <- plhiv_t3_out - unaware_plhiv_num_t3_out
  aware_plhiv_prop_t3_out <- aware_plhiv_num_t3_out / plhiv_t3_out

  infections_t3_out <- as.vector(d$A_out %*% infections_t3)
  lambda_t3_out <- infections_t3_out / (population_t3_out - plhiv_t3_out)

  anc_clients_t3_out <- as.vector(d$A_anc_out %*% anc_clients_t3)
  anc_plhiv_t3_out <- as.vector(d$A_anc_out %*% anc_plhiv_t3)
  anc_already_art_t3_out <- as.vector(d$A_anc_out %*% anc_already_art_t3)
  anc_art_new_t3_out <- anc_plhiv_t3_out - anc_already_art_t3_out
  anc_known_pos_t3_out <- anc_already_art_t3_out
  anc_tested_pos_t3_out <- anc_plhiv_t3_out - anc_known_pos_t3_out
  anc_tested_neg_t3_out <- anc_clients_t3_out - anc_plhiv_t3_out

  anc_rho_t3_out <- anc_plhiv_t3_out / anc_clients_t3_out
  anc_alpha_t3_out <- anc_already_art_t3_out / anc_plhiv_t3_out


  report_t3 <- list(population_t3_out              = population_t3_out,
                    rho_t3_out                     = rho_t3_out,
                    plhiv_t3_out                   = plhiv_t3_out,
                    alpha_t3_out                   = alpha_t3_out,
                    artnum_t3_out                  = artnum_t3_out,
                    artattend_t3_out               = artattend_t3_out,
                    artattend_ij_t3_out            = artattend_ij_t3_out,
                    untreated_plhiv_num_t3_out     = untreated_plhiv_num_t3_out,
                    plhiv_attend_t3_out            = plhiv_attend_t3_out,
                    untreated_plhiv_attend_t3_out  = untreated_plhiv_attend_t3_out,
                    aware_plhiv_prop_t3_out        = aware_plhiv_prop_t3_out,
                    aware_plhiv_num_t3_out         = aware_plhiv_num_t3_out,
                    unaware_plhiv_num_t3_out       = unaware_plhiv_num_t3_out,
                    lambda_t3_out                  = lambda_t3_out,
                    infections_t3_out              = infections_t3_out,
                    anc_clients_t3_out             = anc_clients_t3_out,
                    anc_plhiv_t3_out               = anc_plhiv_t3_out,
                    anc_already_art_t3_out         = anc_already_art_t3_out,
                    anc_art_new_t3_out             = anc_art_new_t3_out,
                    anc_known_pos_t3_out           = anc_known_pos_t3_out,
                    anc_tested_pos_t3_out          = anc_tested_pos_t3_out,
                    anc_tested_neg_t3_out          = anc_tested_neg_t3_out,
                    anc_rho_t3_out                 = anc_rho_t3_out,
                    anc_alpha_t3_out               = anc_alpha_t3_out)


  v <- list(val = unname(val),
       report = c(report_t1, report_t2, report_t3))
}

#' Log posterior density of BYM2
#'
#' @details
#'
#'
#' Log posterior density of BYM2 with INLA conditional parameterisation
#'
#' Calculate the joint LPDF of parameter vector (x, u) where
#' x = sigma * (sqrt(phi) * u + sqrt(1-phi) * v) with u a ICAR structured
#' component u ~ N(0, Q^-1) and v is an IID effect v ~ N(0, 1). Calculation
#' proceeds by conditioning P(<x, u>) = P(x | u) * P(u). See Reibler et al.
#' Section 3.4.
#'
#' @param x vector of random effects.
#' @param u vector of spatial component of random effect.
#' @param sigma marginal standard deviation (>0).
#' @param phi proportion of marginal variance explained by spatial structured
#'            component u (phi \in [0, 1]).
#' @param Q scaled structure matrix for spatial component.
#'
#' @return Log probability density of x and u.
#'
#' @details
#' The $\sqrt(2\pi)^{-2*n}$ and $|Q|^{1/2}$ terms are dropped.
#' Returns the _positive_ log PDF (different from builtin TMB
#' functions. Thus should typically be implemented as `nll -= bym2_conditional_lpdf(...)`.
#'
#' @noRd
#' 
bym2_conditional_lpdf <- function(x, u, sigma, phi, Q) {

  ## constant terms omitted: -0.5 * (n + rank(Q)) * log(2*pi) + 0.5 * log|Q|
  val <- 0.0
  val <- val - 0.5 * length(x) * (2 * log(sigma) + log(1 - phi))  # normalising constant
  val <- val - 0.5 / (sigma * sigma * (1 - phi)) * sum(x * x)
  val <- val + sqrt(phi) / (sigma * (1 - phi)) * sum(x * u)
  val <- val - 0.5 * sum(u %*% Q %*% u)
  val <- val - 0.5 * phi / (1 - phi) * sum(u * u)

  val
}


#' Binomial distribution log-density permitting non-integer counts
#'
#' @examples
#'
#' ldbinom(c(1.2, 3, 1.9), c(7.5, 12.4, 4), c(0.3, 0.2, 0.7))
#'
#' ## For integer counts, returns same as dbinom(..., log = TRUE)
#' x <- c(1, 3, 2)
#' size <- c(7, 12, 4)
#' prob <- c(0.3, 0.2, 0.7)
#' ldbinom(x, size, prob)
#' dbinom(x, size, prob, log = TRUE)
#'
#' @noRd
#' 
ldbinom <- function(x, size, prob){
  lgamma(size+1) - lgamma(x+1) - lgamma(size-x+1) +
    x*log(prob) + (size-x)*log(1-prob)
}
