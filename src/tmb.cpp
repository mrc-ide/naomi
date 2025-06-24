#define TMB_LIB_INIT R_init_naomi
#include <TMB.hpp>


/** Log posterior density of BYM2 with INLA conditional parameterisation
 *
 * Calculate the joint LPDF of parameter vector (x, u) where
 * x = sigma * (sqrt(phi) * u + sqrt(1-phi) * v) with u a ICAR structured
 * component u ~ N(0, Q^-1) and v is an IID effect v ~ N(0, 1). Calculation
 * proceeds by conditioning P(<x, u>) = P(x | u) * P(u). See Reibler et al.
 * Section 3.4.
 *
 * @param x vector of random effects.
 * @param u vector of spatial component of random effect.
 * @param sigma marginal standard deviation (>0).
 * @param phi proportion of marginal variance explained by spatial structured
 *            component u (phi \in [0, 1]).
 * @param Q scaled structure matrix for spatial component.
 *
 * @return Log probability density of x and u.
 *
 * @note
 * The $\sqrt(2\pi)^{-2*n}$ and $|Q|^{1/2}$ terms are dropped.
 * Returns the _positive_ log PDF (different from builtin TMB
 * functions. Thus should typically be implemented as `nll -= bym2_conditional_lpdf(...)`.
 */
template<class Type>
Type bym2_conditional_lpdf(const vector<Type> x,
                           const vector<Type> u,
                           const Type sigma,
                           const Type phi,
                           const Eigen::SparseMatrix<Type> Q) {

  Type val(0.0);

  // constant terms omitted: -0.5 * (n + rank(Q)) * log(2*pi) + 0.5 * log|Q|
  val += -0.5 * x.size() * (2 * log(sigma) + log(1 - phi));  // normalising constant
  val += -0.5 / (sigma * sigma * (1 - phi)) * (x * x).sum();
  val += sqrt(phi) / (sigma * (1 - phi)) * (x * u).sum();
  val += -0.5 * (u * (Q * u)).sum();
  val += -0.5 * phi / (1 - phi) * (u * u).sum();

  return(val);
}

template<class Type>
Type objective_function<Type>::operator() ()
{

  // indexing:
  //
  // * rho: HIV prevalence model
  // * alpha: ART coverage model
  // * lambda: HIV incidence model
  //
  // * _x: area
  // * _a: age
  // * _s: sex
  // * _t: time

  using namespace density;

  // ** Data **

  // Population
  DATA_VECTOR(population_t1);
  DATA_VECTOR(population_t2);
  DATA_SPARSE_MATRIX(Lproj_hivpop_t1t2);
  DATA_SPARSE_MATRIX(Lproj_incid_t1t2);
  DATA_SPARSE_MATRIX(Lproj_paed_t1t2);

  // Design matrices
  DATA_MATRIX(X_rho);
  DATA_MATRIX(X_alpha);
  DATA_MATRIX(X_alpha_t2);
  DATA_MATRIX(X_lambda);

  DATA_MATRIX(X_asfr);
  DATA_MATRIX(X_ancrho);
  DATA_MATRIX(X_ancalpha);

  DATA_SPARSE_MATRIX(Z_rho_x);
  DATA_SPARSE_MATRIX(Z_rho_xs);
  DATA_SPARSE_MATRIX(Z_rho_a);
  DATA_SPARSE_MATRIX(Z_rho_as);
  DATA_SPARSE_MATRIX(Z_rho_xa);

  DATA_SPARSE_MATRIX(Z_alpha_x);
  DATA_SPARSE_MATRIX(Z_alpha_xs);
  DATA_SPARSE_MATRIX(Z_alpha_a);
  DATA_SPARSE_MATRIX(Z_alpha_as);
  DATA_SPARSE_MATRIX(Z_alpha_xt);
  DATA_SPARSE_MATRIX(Z_alpha_xa);
  DATA_SPARSE_MATRIX(Z_alpha_xat);
  DATA_SPARSE_MATRIX(Z_alpha_xst);

  DATA_MATRIX(X_alpha_t2t3);
  DATA_SPARSE_MATRIX(Z_alpha_xt_t2t3);
  DATA_SPARSE_MATRIX(Z_alpha_xat_t2t3);
  DATA_SPARSE_MATRIX(Z_alpha_xst_t2t3);

  DATA_SPARSE_MATRIX(Z_x);
  DATA_SPARSE_MATRIX(Z_lambda_x);

  DATA_VECTOR(logit_rho_offset);
  DATA_VECTOR(logit_alpha_offset);
  DATA_VECTOR(logit_alpha_t1t2_offset);

  DATA_VECTOR(log_asfr_t1_offset);
  DATA_VECTOR(log_asfr_t2_offset);
  DATA_VECTOR(log_asfr_t3_offset);
  DATA_VECTOR(log_asfr_t4_offset);
  DATA_VECTOR(log_asfr_t5_offset);
  DATA_VECTOR(log_asfr_t6_offset);

  DATA_VECTOR(logit_anc_rho_t1_offset);
  DATA_VECTOR(logit_anc_rho_t2_offset);
  DATA_VECTOR(logit_anc_rho_t3_offset);
  DATA_VECTOR(logit_anc_rho_t4_offset);
  DATA_VECTOR(logit_anc_rho_t5_offset);
  DATA_VECTOR(logit_anc_rho_t6_offset);

  DATA_VECTOR(logit_anc_alpha_t1_offset);
  DATA_VECTOR(logit_anc_alpha_t2_offset);
  DATA_VECTOR(logit_anc_alpha_t3_offset);
  DATA_VECTOR(logit_anc_alpha_t4_offset);
  DATA_VECTOR(logit_anc_alpha_t5_offset);
  DATA_VECTOR(logit_anc_alpha_t6_offset);

  DATA_SPARSE_MATRIX(Z_asfr_x);
  DATA_SPARSE_MATRIX(Z_ancrho_x);
  DATA_SPARSE_MATRIX(Z_ancalpha_x);


  // Precision matrix for ICAR area model
  DATA_SPARSE_MATRIX(Q_x);
  DATA_SCALAR(Q_x_rankdef);


  // /////////////////////
  // // Likelihood data //
  // /////////////////////

  // Survey data

  DATA_VECTOR(n_prev_t1);
  DATA_VECTOR(x_prev_t1);
  DATA_SPARSE_MATRIX(A_prev_t1);

  DATA_VECTOR(n_artcov_t1);
  DATA_VECTOR(x_artcov_t1);
  DATA_SPARSE_MATRIX(A_artcov_t1);

  DATA_VECTOR(n_vls_t1);
  DATA_VECTOR(x_vls_t1);
  DATA_SPARSE_MATRIX(A_vls_t1);

  DATA_VECTOR(n_recent_t1);
  DATA_VECTOR(x_recent_t1);
  DATA_SPARSE_MATRIX(A_recent_t1);


  DATA_VECTOR(n_prev_t2);
  DATA_VECTOR(x_prev_t2);
  DATA_SPARSE_MATRIX(A_prev_t2);

  DATA_VECTOR(n_artcov_t2);
  DATA_VECTOR(x_artcov_t2);
  DATA_SPARSE_MATRIX(A_artcov_t2);

  DATA_VECTOR(n_vls_t2);
  DATA_VECTOR(x_vls_t2);
  DATA_SPARSE_MATRIX(A_vls_t2);

  DATA_VECTOR(n_recent_t2);
  DATA_VECTOR(x_recent_t2);
  DATA_SPARSE_MATRIX(A_recent_t2);


  // ANC data

  DATA_VECTOR(x_anc_clients_t3);
  DATA_VECTOR(offset_anc_clients_t3);
  DATA_SPARSE_MATRIX(A_anc_clients_t3);

  DATA_VECTOR(n_anc_prev_t1);
  DATA_VECTOR(x_anc_prev_t1);
  DATA_SPARSE_MATRIX(A_anc_prev_t1);

  DATA_VECTOR(n_anc_artcov_t1);
  DATA_VECTOR(x_anc_artcov_t1);
  DATA_SPARSE_MATRIX(A_anc_artcov_t1);

  DATA_VECTOR(n_anc_prev_t2);
  DATA_VECTOR(x_anc_prev_t2);
  DATA_SPARSE_MATRIX(A_anc_prev_t2);

  DATA_VECTOR(n_anc_artcov_t2);
  DATA_VECTOR(x_anc_artcov_t2);
  DATA_SPARSE_MATRIX(A_anc_artcov_t2);

  DATA_VECTOR(n_anc_prev_t3);
  DATA_VECTOR(x_anc_prev_t3);
  DATA_SPARSE_MATRIX(A_anc_prev_t3);

  DATA_VECTOR(n_anc_artcov_t3);
  DATA_VECTOR(x_anc_artcov_t3);
  DATA_SPARSE_MATRIX(A_anc_artcov_t3);

  // ART programme data

  DATA_SPARSE_MATRIX(A_artattend_t1);
  DATA_VECTOR(x_artnum_t1);
  DATA_VECTOR(x_artnum_public_t1);
  DATA_VECTOR(x_artnum_medaid_t1);
  DATA_VECTOR(x_artnum_cashpay_t1);

  DATA_SPARSE_MATRIX(A_artattend_t2);
  DATA_VECTOR(x_artnum_t2);
  DATA_VECTOR(x_artnum_public_t2);
  DATA_VECTOR(x_artnum_medaid_t2);
  DATA_VECTOR(x_artnum_cashpay_t2);

  DATA_SPARSE_MATRIX(A_artattend_t3);
  DATA_VECTOR(x_artnum_t3);
  DATA_VECTOR(x_artnum_public_t3);
  DATA_VECTOR(x_artnum_medaid_t3);
  DATA_VECTOR(x_artnum_cashpay_t3);

  DATA_SPARSE_MATRIX(A_artattend_mf);
  DATA_SPARSE_MATRIX(A_art_reside_attend);

  DATA_IVECTOR(n_nb);
  DATA_IVECTOR(adj_i);
  DATA_IVECTOR(adj_j);

  DATA_SPARSE_MATRIX(Xgamma);
  DATA_SPARSE_MATRIX(Xgamma_t2);
  DATA_SPARSE_MATRIX(Xgamma_t3);
  DATA_VECTOR(log_gamma_offset);


  DATA_SPARSE_MATRIX(Xart_idx);
  DATA_SPARSE_MATRIX(Xart_gamma);


  // Foreign ART attendance
  DATA_VECTOR(recent_immigration_prop);
  DATA_VECTOR(recent_migrant_hivprev_15to49);

  DATA_SCALAR(foreign_art_eta_t1);
  DATA_SCALAR(foreign_art_eta_t2);
  DATA_SCALAR(foreign_art_eta_t3);
  DATA_SCALAR(foreign_art_eta_t4);
  DATA_SCALAR(foreign_art_eta_t5);
  DATA_SCALAR(foreign_art_eta_t6);

  // ART sector model
  DATA_MATRIX(X_art_medaid);
  DATA_MATRIX(X_art_cashpay);

  DATA_SPARSE_MATRIX(Z_art_medaid_x);
  DATA_SPARSE_MATRIX(Z_art_medaid_xs);
  DATA_SPARSE_MATRIX(Z_art_medaid_xa);

  DATA_SPARSE_MATRIX(Z_art_cashpay_x);
  DATA_SPARSE_MATRIX(Z_art_cashpay_xs);
  DATA_SPARSE_MATRIX(Z_art_cashpay_xa);

  DATA_MATRIX(X_art_medaid_t1t2);
  DATA_MATRIX(X_art_cashpay_t1t2);

  DATA_SPARSE_MATRIX(Z_art_medaid_x_t1t2);
  DATA_SPARSE_MATRIX(Z_art_medaid_xs_t1t2);
  DATA_SPARSE_MATRIX(Z_art_medaid_xa_t1t2);

  DATA_SPARSE_MATRIX(Z_art_cashpay_x_t1t2);
  DATA_SPARSE_MATRIX(Z_art_cashpay_xs_t1t2);
  DATA_SPARSE_MATRIX(Z_art_cashpay_xa_t1t2);

  DATA_MATRIX(X_art_medaid_t2t3);
  DATA_MATRIX(X_art_cashpay_t2t3);

  DATA_SPARSE_MATRIX(Z_art_medaid_x_t2t3);
  DATA_SPARSE_MATRIX(Z_art_medaid_xs_t2t3);
  DATA_SPARSE_MATRIX(Z_art_medaid_xa_t2t3);

  DATA_SPARSE_MATRIX(Z_art_cashpay_x_t2t3);
  DATA_SPARSE_MATRIX(Z_art_cashpay_xs_t2t3);
  DATA_SPARSE_MATRIX(Z_art_cashpay_xa_t2t3);

  // Incidence model
  DATA_SCALAR(omega);
  DATA_SCALAR(OmegaT0);
  DATA_SCALAR(sigma_OmegaT);
  DATA_SCALAR(betaT0);
  DATA_SCALAR(sigma_betaT);
  DATA_SCALAR(ritaT);
  DATA_SCALAR(logit_nu_mean);
  DATA_SCALAR(logit_nu_sd);

  DATA_SPARSE_MATRIX(X_15to49);
  DATA_VECTOR(log_lambda_t1_offset);
  DATA_VECTOR(log_lambda_t2_offset);

  // Paediatric prevalence and incidence ratio model

  DATA_SPARSE_MATRIX(X_15to49f);
  DATA_SPARSE_MATRIX(X_paed_rho_ratio);
  DATA_VECTOR(paed_rho_ratio_offset);

  DATA_SPARSE_MATRIX(X_paed_lambda_ratio_t1);
  DATA_SPARSE_MATRIX(X_paed_lambda_ratio_t2);

  // ** Initialize nll **
  Type val(0);

  // ** Parameters **

  // fixed effects
  // diffuse N(0.0, 5.0) prior distribution

  PARAMETER_VECTOR(beta_rho);
  val -= dnorm(beta_rho, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(beta_alpha);
  val -= dnorm(beta_alpha, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(beta_alpha_t2);
  val -= dnorm(beta_alpha_t2, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(beta_alpha_t2t3);
  val -= dnorm(beta_alpha_t2t3, 0.0, 5.0, true).sum();


  PARAMETER_VECTOR(beta_lambda);
  val -= dnorm(beta_lambda, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(beta_asfr);
  val -= dnorm(beta_asfr, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(beta_anc_rho);
  val -= dnorm(beta_anc_rho, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(beta_anc_alpha);
  val -= dnorm(beta_anc_alpha, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(beta_anc_rho_t2);
  val -= dnorm(beta_anc_rho_t2, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(beta_anc_alpha_t2);
  val -= dnorm(beta_anc_alpha_t2, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(beta_anc_rho_t3);
  val -= dnorm(beta_anc_rho_t3, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(beta_anc_alpha_t3);
  val -= dnorm(beta_anc_alpha_t3, 0.0, 5.0, true).sum();



  // * HIV prevalence model *

  // hyper parameters

  PARAMETER(logit_phi_rho_x);
  Type phi_rho_x(invlogit(logit_phi_rho_x));
  val -= log(phi_rho_x) +  log(1 - phi_rho_x);  // change of variables: logit_phi_x ->v phi_x
  val -= dbeta(phi_rho_x, Type(0.5), Type(0.5), true);

  PARAMETER(log_sigma_rho_x);
  Type sigma_rho_x(exp(log_sigma_rho_x));
  val -= dnorm(sigma_rho_x, Type(0.0), Type(2.5), true) + log_sigma_rho_x;

  PARAMETER(logit_phi_rho_xs);
  Type phi_rho_xs(invlogit(logit_phi_rho_xs));
  val -= log(phi_rho_xs) +  log(1 - phi_rho_xs);  // change of variables: logit_phi_xs -> phi_xs
  val -= dbeta(phi_rho_xs, Type(0.5), Type(0.5), true);

  PARAMETER(log_sigma_rho_xs);
  Type sigma_rho_xs(exp(log_sigma_rho_xs));
  val -= dnorm(sigma_rho_xs, Type(0.0), Type(2.5), true) + log_sigma_rho_xs;

  PARAMETER(logit_phi_rho_a);
  val -= dnorm(logit_phi_rho_a, Type(0.0), Type(2.582), true);  // INLA default
  Type phi_rho_a(2.0 * invlogit(logit_phi_rho_a) - 1.0);

  PARAMETER(log_sigma_rho_a);
  Type sigma_rho_a(exp(log_sigma_rho_a));
  val -= dnorm(sigma_rho_a, Type(0.0), Type(2.5), true) + log_sigma_rho_a;

  PARAMETER(logit_phi_rho_as);
  val -= dnorm(logit_phi_rho_as, Type(0.0), Type(2.582), true);  // INLA default
  Type phi_rho_as(2.0 * invlogit(logit_phi_rho_as) - 1.0);

  PARAMETER(log_sigma_rho_as);
  Type sigma_rho_as(exp(log_sigma_rho_as));
  val -= dnorm(sigma_rho_as, Type(0.0), Type(2.5), true) + log_sigma_rho_as;

  PARAMETER(log_sigma_rho_xa);
  Type sigma_rho_xa(exp(log_sigma_rho_xa));
  val -= dnorm(sigma_rho_xa, Type(0.0), Type(0.5), true) + log_sigma_rho_xa;

  // latent effects

  PARAMETER_VECTOR(u_rho_x);
  PARAMETER_VECTOR(us_rho_x);
  val -= dnorm(sum(us_rho_x), Type(0.0), Type(0.001) * us_rho_x.size(), true); // soft sum-to-zero constraint
  val -= bym2_conditional_lpdf(u_rho_x, us_rho_x, sigma_rho_x, phi_rho_x, Q_x);


  PARAMETER_VECTOR(u_rho_xs);
  PARAMETER_VECTOR(us_rho_xs);
  if (u_rho_xs.size()) {
    val -= dnorm(sum(us_rho_xs), Type(0.0), Type(0.001) * us_rho_xs.size(), true); // soft sum-to-zero constraint
    val -= bym2_conditional_lpdf(u_rho_xs, us_rho_xs, sigma_rho_xs, phi_rho_xs, Q_x);
  }

  PARAMETER_VECTOR(u_rho_a);
  if(u_rho_a.size() > 0)
    val += SCALE(AR1(phi_rho_a), sigma_rho_a)(u_rho_a);

  PARAMETER_VECTOR(u_rho_as);
  if(u_rho_a.size() > 0)
    val += SCALE(AR1(phi_rho_as), sigma_rho_as)(u_rho_as);

  PARAMETER_VECTOR(u_rho_xa);
  if (u_rho_xa.size() > 0) {
    val -= dnorm(sum(u_rho_xa), Type(0.0), sigma_rho_xa * Type(0.001) * u_rho_xa.size(), true); // soft sum-to-zero constraint

    val -= -(Q_x.rows() - Q_x_rankdef) * log_sigma_rho_xa -
      0.5 / (sigma_rho_xa * sigma_rho_xa) * (u_rho_xa * (Q_x * u_rho_xa)).sum();
  }


  // * ART coverage model *

  PARAMETER(logit_phi_alpha_x);
  Type phi_alpha_x(invlogit(logit_phi_alpha_x));
  val -= log(phi_alpha_x) +  log(1 - phi_alpha_x);  // change of variables: logit_phi_x -> phi_x
  val -= dbeta(phi_alpha_x, Type(0.5), Type(0.5), true);

  PARAMETER(log_sigma_alpha_x);
  Type sigma_alpha_x(exp(log_sigma_alpha_x));
  val -= dnorm(sigma_alpha_x, Type(0.0), Type(2.5), true) + log_sigma_alpha_x;

  PARAMETER(logit_phi_alpha_xs);
  Type phi_alpha_xs(invlogit(logit_phi_alpha_xs));
  val -= log(phi_alpha_xs) +  log(1 - phi_alpha_xs);  // change of variables: logit_phi_xs -> phi_xs
  val -= dbeta(phi_alpha_xs, Type(0.5), Type(0.5), true);

  PARAMETER(log_sigma_alpha_xs);
  Type sigma_alpha_xs(exp(log_sigma_alpha_xs));
  val -= dnorm(sigma_alpha_xs, Type(0.0), Type(2.5), true) + log_sigma_alpha_xs;

  PARAMETER(logit_phi_alpha_a);
  val -= dnorm(logit_phi_alpha_a, Type(0.0), Type(2.582), true);  // INLA default
  Type phi_alpha_a(2.0 * invlogit(logit_phi_alpha_a) - 1.0);

  PARAMETER(log_sigma_alpha_a);
  Type sigma_alpha_a(exp(log_sigma_alpha_a));
  val -= dnorm(sigma_alpha_a, Type(0.0), Type(2.5), true) + log_sigma_alpha_a;

  PARAMETER(logit_phi_alpha_as);
  val -= dnorm(logit_phi_alpha_as, Type(0.0), Type(2.582), true);  // INLA default
  Type phi_alpha_as(2.0 * invlogit(logit_phi_alpha_as) - 1.0);

  PARAMETER(log_sigma_alpha_as);
  Type sigma_alpha_as(exp(log_sigma_alpha_as));
  val -= dnorm(sigma_alpha_as, Type(0.0), Type(2.5), true) + log_sigma_alpha_as;

  PARAMETER(log_sigma_alpha_xt);
  Type sigma_alpha_xt(exp(log_sigma_alpha_xt));
  val -= dnorm(sigma_alpha_xt, Type(0.0), Type(2.5), true) + log_sigma_alpha_xt;

  PARAMETER(log_sigma_alpha_xa);
  Type sigma_alpha_xa(exp(log_sigma_alpha_xa));
  val -= dnorm(sigma_alpha_xa, Type(0.0), Type(2.5), true) + log_sigma_alpha_xa;

  PARAMETER(log_sigma_alpha_xat);
  Type sigma_alpha_xat(exp(log_sigma_alpha_xat));
  val -= dnorm(sigma_alpha_xat, Type(0.0), Type(2.5), true) + log_sigma_alpha_xat;

  PARAMETER(log_sigma_alpha_xst);
  Type sigma_alpha_xst(exp(log_sigma_alpha_xst));
  val -= dnorm(sigma_alpha_xst, Type(0.0), Type(2.5), true) + log_sigma_alpha_xst;

  PARAMETER_VECTOR(u_alpha_x);
  PARAMETER_VECTOR(us_alpha_x);
  val -= dnorm(sum(us_alpha_x), Type(0.0), Type(0.001) * us_alpha_x.size(), true); // soft sum-to-zero constraint
  val -= bym2_conditional_lpdf(u_alpha_x, us_alpha_x, sigma_alpha_x, phi_alpha_x, Q_x);

  PARAMETER_VECTOR(u_alpha_xs);
  PARAMETER_VECTOR(us_alpha_xs);
  if (u_alpha_xs.size()) {
    val -= dnorm(sum(us_alpha_xs), Type(0.0), Type(0.001) * us_alpha_xs.size(), true); // soft sum-to-zero constraint
    val -= bym2_conditional_lpdf(u_alpha_xs, us_alpha_xs, sigma_alpha_xs, phi_alpha_xs, Q_x);
  }

  PARAMETER_VECTOR(u_alpha_a);
  if(u_alpha_a.size() > 0)
    val += SCALE(AR1(phi_alpha_a), sigma_alpha_a)(u_alpha_a);

  PARAMETER_VECTOR(u_alpha_as);
  if(u_alpha_as.size() > 0)
    val += SCALE(AR1(phi_alpha_as), sigma_alpha_as)(u_alpha_as);

  PARAMETER_VECTOR(u_alpha_xt);
  val -= dnorm(u_alpha_xt, 0.0, sigma_alpha_xt, true).sum();

  PARAMETER_VECTOR(u_alpha_xa);
  val -= dnorm(u_alpha_xa, 0.0, sigma_alpha_xa, true).sum();

  PARAMETER_VECTOR(u_alpha_xat);
  val -= dnorm(u_alpha_xat, 0.0, sigma_alpha_xat, true).sum();

  PARAMETER_VECTOR(u_alpha_xst);
  val -= dnorm(u_alpha_xst, 0.0, sigma_alpha_xst, true).sum();


  // ART coverage t2 to t3

  PARAMETER(log_sigma_alpha_xt_t2t3);
  Type sigma_alpha_xt_t2t3(exp(log_sigma_alpha_xt_t2t3));
  val -= dnorm(sigma_alpha_xt_t2t3, Type(0.0), Type(2.5), true) + log_sigma_alpha_xt_t2t3;

  PARAMETER(log_sigma_alpha_xat_t2t3);
  Type sigma_alpha_xat_t2t3(exp(log_sigma_alpha_xat_t2t3));
  val -= dnorm(sigma_alpha_xat_t2t3, Type(0.0), Type(2.5), true) + log_sigma_alpha_xat_t2t3;

  PARAMETER(log_sigma_alpha_xst_t2t3);
  Type sigma_alpha_xst_t2t3(exp(log_sigma_alpha_xst_t2t3));
  val -= dnorm(sigma_alpha_xst_t2t3, Type(0.0), Type(2.5), true) + log_sigma_alpha_xst_t2t3;


  PARAMETER_VECTOR(u_alpha_xt_t2t3);
  val -= dnorm(u_alpha_xt_t2t3, 0.0, sigma_alpha_xt_t2t3, true).sum();

  PARAMETER_VECTOR(u_alpha_xat_t2t3);
  val -= dnorm(u_alpha_xat_t2t3, 0.0, sigma_alpha_xat_t2t3, true).sum();

  PARAMETER_VECTOR(u_alpha_xst_t2t3);
  val -= dnorm(u_alpha_xst_t2t3, 0.0, sigma_alpha_xst_t2t3, true).sum();


  // * HIV incidence model *

  PARAMETER(OmegaT_raw);
  val -= dnorm(OmegaT_raw, Type(0.0), Type(1.0), true);
  Type OmegaT = OmegaT0 + OmegaT_raw * sigma_OmegaT;

  PARAMETER(log_betaT);
  val -= dnorm(exp(log_betaT), Type(0.0), Type(1.0), true) + log_betaT;
  Type betaT = betaT0 + exp(log_betaT) * sigma_betaT;

  PARAMETER(logit_nu_raw);
  val -= dnorm(logit_nu_raw, Type(0.0), Type(1.0), true);
  Type nu = invlogit(logit_nu_mean + logit_nu_raw * logit_nu_sd);

  PARAMETER(log_sigma_lambda_x);
  Type sigma_lambda_x(exp(log_sigma_lambda_x));
  val -= dnorm(sigma_lambda_x, Type(0.0), Type(1.0), true) + log_sigma_lambda_x;

  PARAMETER_VECTOR(ui_lambda_x);
  val -= sum(dnorm(ui_lambda_x, 0.0, sigma_lambda_x, true));

  // * ANC testing model *

  // district ASFR random effects
  PARAMETER(log_sigma_asfr_x);
  Type sigma_asfr_x(exp(log_sigma_asfr_x));
  val -= dnorm(sigma_asfr_x, Type(0.0), Type(2.5), true) + log_sigma_asfr_x;

  PARAMETER_VECTOR(ui_asfr_x);
  val -= sum(dnorm(ui_asfr_x, 0.0, sigma_asfr_x, true));

  // ANC prevalence and ART coverage random effects
  PARAMETER(log_sigma_ancrho_x);
  Type sigma_ancrho_x(exp(log_sigma_ancrho_x));
  val -= dnorm(sigma_ancrho_x, Type(0.0), Type(2.5), true) + log_sigma_ancrho_x;

  PARAMETER(log_sigma_ancalpha_x);
  Type sigma_ancalpha_x(exp(log_sigma_ancalpha_x));
  val -= dnorm(sigma_ancalpha_x, Type(0.0), Type(2.5), true) + log_sigma_ancalpha_x;

  PARAMETER_VECTOR(ui_anc_rho_x);
  val -= sum(dnorm(ui_anc_rho_x, 0.0, sigma_ancrho_x, true));

  PARAMETER_VECTOR(ui_anc_alpha_x);
  val -= sum(dnorm(ui_anc_alpha_x, 0.0, sigma_ancalpha_x, true));

  PARAMETER(log_sigma_ancrho_xt);
  Type sigma_ancrho_xt(exp(log_sigma_ancrho_xt));
  val -= dnorm(sigma_ancrho_xt, Type(0.0), Type(2.5), true) + log_sigma_ancrho_xt;

  PARAMETER(log_sigma_ancalpha_xt);
  Type sigma_ancalpha_xt(exp(log_sigma_ancalpha_xt));
  val -= dnorm(sigma_ancalpha_xt, Type(0.0), Type(2.5), true) + log_sigma_ancalpha_xt;

  PARAMETER_VECTOR(ui_anc_rho_xt);
  val -= sum(dnorm(ui_anc_rho_xt, 0.0, sigma_ancrho_xt, true));

  PARAMETER_VECTOR(ui_anc_alpha_xt);
  val -= sum(dnorm(ui_anc_alpha_xt, 0.0, sigma_ancalpha_xt, true));


  PARAMETER_VECTOR(ui_anc_rho_xt2t3);
  val -= sum(dnorm(ui_anc_rho_xt2t3, 0.0, sigma_ancrho_xt, true));
  // !!! NOTE: this has assumed same sigma_ancrho_xt; maybe better to make it scale with years

  PARAMETER_VECTOR(ui_anc_alpha_xt2t3);
  val -= sum(dnorm(ui_anc_alpha_xt2t3, 0.0, sigma_ancalpha_xt, true));
  // !!! NOTE: this has assumed same sigma_ancalpha_xt; maybe better to make it scale with years


  // * ART attendance model *

  PARAMETER(log_sigma_or_gamma);
  Type sigma_or_gamma(exp(log_sigma_or_gamma));
  val -= dnorm(sigma_or_gamma, Type(0.0), Type(2.5), true) + log_sigma_or_gamma;

  PARAMETER_VECTOR(log_or_gamma);
  val -= dnorm(log_or_gamma, 0.0, sigma_or_gamma, true).sum();

  PARAMETER(log_sigma_or_gamma_t1t2);
  Type sigma_or_gamma_t1t2(exp(log_sigma_or_gamma_t1t2));
  val -= dnorm(sigma_or_gamma_t1t2, Type(0.0), Type(2.5), true) + log_sigma_or_gamma_t1t2;

  PARAMETER_VECTOR(log_or_gamma_t1t2);
  val -= dnorm(log_or_gamma_t1t2, 0.0, sigma_or_gamma_t1t2, true).sum();

  PARAMETER(log_sigma_or_gamma_t2t3);
  Type sigma_or_gamma_t2t3(exp(log_sigma_or_gamma_t2t3));
  val -= dnorm(sigma_or_gamma_t2t3, Type(0.0), Type(2.5), true) + log_sigma_or_gamma_t2t3;

  PARAMETER_VECTOR(log_or_gamma_t2t3);
  val -= dnorm(log_or_gamma_t2t3, 0.0, sigma_or_gamma_t2t3, true).sum();


  // * ART sector model *

  // Foreign ART eta scalar
  // +/- 10% relative standard error on foreign ART scalar
  PARAMETER(log_foreign_art_eta_scalar);
  val -= dnorm(log_foreign_art_eta_scalar, Type(0.0), Type(1.0), true);
  Type foreign_art_eta_scalar = exp(log_foreign_art_eta_scalar);
  
  // Baseline odds ratios

  PARAMETER_VECTOR(beta_art_medaid);
  val -= dnorm(beta_art_medaid, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(beta_art_cashpay);
  val -= dnorm(beta_art_cashpay, 0.0, 5.0, true).sum();

  PARAMETER(log_sigma_art_medaid_x);
  Type sigma_art_medaid_x(exp(log_sigma_art_medaid_x));
  val -= dnorm(sigma_art_medaid_x, Type(0.0), Type(2.5), true) + log_sigma_art_medaid_x;

  PARAMETER(log_sigma_art_cashpay_x);
  Type sigma_art_cashpay_x(exp(log_sigma_art_cashpay_x));
  val -= dnorm(sigma_art_cashpay_x, Type(0.0), Type(2.5), true) + log_sigma_art_cashpay_x;

  PARAMETER(log_sigma_art_medaid_xs);
  Type sigma_art_medaid_xs(exp(log_sigma_art_medaid_xs));
  val -= dnorm(sigma_art_medaid_xs, Type(0.0), Type(2.5), true) + log_sigma_art_medaid_xs;

  PARAMETER(log_sigma_art_cashpay_xs);
  Type sigma_art_cashpay_xs(exp(log_sigma_art_cashpay_xs));
  val -= dnorm(sigma_art_cashpay_xs, Type(0.0), Type(2.5), true) + log_sigma_art_cashpay_xs;

  PARAMETER(log_sigma_art_medaid_xa);
  Type sigma_art_medaid_xa(exp(log_sigma_art_medaid_xa));
  val -= dnorm(sigma_art_medaid_xa, Type(0.0), Type(2.5), true) + log_sigma_art_medaid_xa;

  PARAMETER(log_sigma_art_cashpay_xa);
  Type sigma_art_cashpay_xa(exp(log_sigma_art_cashpay_xa));
  val -= dnorm(sigma_art_cashpay_xa, Type(0.0), Type(2.5), true) + log_sigma_art_cashpay_xa;

  PARAMETER_VECTOR(u_art_medaid_x);
  val -= sum(dnorm(u_art_medaid_x, 0.0, sigma_art_medaid_x, true));

  PARAMETER_VECTOR(u_art_cashpay_x);
  val -= sum(dnorm(u_art_cashpay_x, 0.0, sigma_art_cashpay_x, true));

  PARAMETER_VECTOR(u_art_medaid_xs);
  val -= sum(dnorm(u_art_medaid_xs, 0.0, sigma_art_medaid_xs, true));

  PARAMETER_VECTOR(u_art_cashpay_xs);
  val -= sum(dnorm(u_art_cashpay_xs, 0.0, sigma_art_cashpay_xs, true));

  PARAMETER_VECTOR(u_art_medaid_xa);
  val -= sum(dnorm(u_art_medaid_xa, 0.0, sigma_art_medaid_xa, true));

  PARAMETER_VECTOR(u_art_cashpay_xa);
  val -= sum(dnorm(u_art_cashpay_xa, 0.0, sigma_art_cashpay_xa, true));


  // Change T1 to T2 odds ratios

  PARAMETER_VECTOR(beta_art_medaid_t1t2);
  val -= dnorm(beta_art_medaid_t1t2, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(beta_art_cashpay_t1t2);
  val -= dnorm(beta_art_cashpay_t1t2, 0.0, 5.0, true).sum();

  PARAMETER(log_sigma_art_medaid_xt);
  Type sigma_art_medaid_xt(exp(log_sigma_art_medaid_xt));
  val -= dnorm(sigma_art_medaid_xt, Type(0.0), Type(2.5), true) + log_sigma_art_medaid_xt;

  PARAMETER(log_sigma_art_cashpay_xt);
  Type sigma_art_cashpay_xt(exp(log_sigma_art_cashpay_xt));
  val -= dnorm(sigma_art_cashpay_xt, Type(0.0), Type(2.5), true) + log_sigma_art_cashpay_xt;

  PARAMETER(log_sigma_art_medaid_xst);
  Type sigma_art_medaid_xst(exp(log_sigma_art_medaid_xst));
  val -= dnorm(sigma_art_medaid_xst, Type(0.0), Type(2.5), true) + log_sigma_art_medaid_xst;

  PARAMETER(log_sigma_art_cashpay_xst);
  Type sigma_art_cashpay_xst(exp(log_sigma_art_cashpay_xst));
  val -= dnorm(sigma_art_cashpay_xst, Type(0.0), Type(2.5), true) + log_sigma_art_cashpay_xst;

  PARAMETER(log_sigma_art_medaid_xat);
  Type sigma_art_medaid_xat(exp(log_sigma_art_medaid_xat));
  val -= dnorm(sigma_art_medaid_xat, Type(0.0), Type(2.5), true) + log_sigma_art_medaid_xat;

  PARAMETER(log_sigma_art_cashpay_xat);
  Type sigma_art_cashpay_xat(exp(log_sigma_art_cashpay_xat));
  val -= dnorm(sigma_art_cashpay_xat, Type(0.0), Type(2.5), true) + log_sigma_art_cashpay_xat;

  PARAMETER_VECTOR(u_art_medaid_x_t1t2);
  val -= sum(dnorm(u_art_medaid_x_t1t2, 0.0, sigma_art_medaid_xt, true));

  PARAMETER_VECTOR(u_art_cashpay_x_t1t2);
  val -= sum(dnorm(u_art_cashpay_x_t1t2, 0.0, sigma_art_cashpay_xt, true));

  PARAMETER_VECTOR(u_art_medaid_xs_t1t2);
  val -= sum(dnorm(u_art_medaid_xs_t1t2, 0.0, sigma_art_medaid_xst, true));

  PARAMETER_VECTOR(u_art_cashpay_xs_t1t2);
  val -= sum(dnorm(u_art_cashpay_xs_t1t2, 0.0, sigma_art_cashpay_xst, true));

  PARAMETER_VECTOR(u_art_medaid_xa_t1t2);
  val -= sum(dnorm(u_art_medaid_xa_t1t2, 0.0, sigma_art_medaid_xat, true));

  PARAMETER_VECTOR(u_art_cashpay_xa_t1t2);
  val -= sum(dnorm(u_art_cashpay_xa_t1t2, 0.0, sigma_art_cashpay_xat, true));


  // Change T2 to T3 odds ratios
  // Note: same hyperparameters used as T1 to T2

  PARAMETER_VECTOR(beta_art_medaid_t2t3);
  val -= dnorm(beta_art_medaid_t2t3, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(beta_art_cashpay_t2t3);
  val -= dnorm(beta_art_cashpay_t2t3, 0.0, 5.0, true).sum();

  PARAMETER_VECTOR(u_art_medaid_x_t2t3);
  val -= sum(dnorm(u_art_medaid_x_t2t3, 0.0, sigma_art_medaid_xt, true));

  PARAMETER_VECTOR(u_art_cashpay_x_t2t3);
  val -= sum(dnorm(u_art_cashpay_x_t2t3, 0.0, sigma_art_cashpay_xt, true));

  PARAMETER_VECTOR(u_art_medaid_xs_t2t3);
  val -= sum(dnorm(u_art_medaid_xs_t2t3, 0.0, sigma_art_medaid_xst, true));

  PARAMETER_VECTOR(u_art_cashpay_xs_t2t3);
  val -= sum(dnorm(u_art_cashpay_xs_t2t3, 0.0, sigma_art_cashpay_xst, true));

  PARAMETER_VECTOR(u_art_medaid_xa_t2t3);
  val -= sum(dnorm(u_art_medaid_xa_t2t3, 0.0, sigma_art_medaid_xat, true));

  PARAMETER_VECTOR(u_art_cashpay_xa_t2t3);
  val -= sum(dnorm(u_art_cashpay_xa_t2t3, 0.0, sigma_art_cashpay_xat, true));


  // *** Process model ***

  // HIV prevalence time 1

  vector<Type> mu_rho_t1(X_rho * beta_rho +
                         logit_rho_offset +
                         Z_rho_x * u_rho_x +
                         Z_rho_xs * u_rho_xs +
                         Z_rho_a * u_rho_a +
                         Z_rho_as * u_rho_as +
                         Z_rho_xa * u_rho_xa);

  // paediatric prevalence

  vector<Type> rho_15to49f_t1((X_15to49f * vector<Type>(invlogit(mu_rho_t1) * population_t1)) / (X_15to49f * population_t1));
  vector<Type> mu_rho_paed(X_paed_rho_ratio * rho_15to49f_t1 + paed_rho_ratio_offset);
  mu_rho_paed = logit(mu_rho_paed);
  mu_rho_t1 += mu_rho_paed;

  // ART coverage time 1

  vector<Type> mu_alpha_t1(X_alpha * beta_alpha +
                           logit_alpha_offset +
                           Z_alpha_x * u_alpha_x +
                           Z_alpha_xs * u_alpha_xs +
                           Z_alpha_a * u_alpha_a +
                           Z_alpha_as * u_alpha_as +
                           Z_alpha_xa * u_alpha_xa);


  vector<Type> rho_t1(invlogit(mu_rho_t1));
  vector<Type> alpha_t1(invlogit(mu_alpha_t1));

  vector<Type> plhiv_t1(population_t1 * rho_t1);
  vector<Type> prop_art_t1(rho_t1 * alpha_t1);
  vector<Type> artnum_t1(population_t1 * prop_art_t1);

  vector<Type> plhiv_15to49_t1(X_15to49 * plhiv_t1);
  vector<Type> rho_15to49_t1(plhiv_15to49_t1 / (X_15to49 * population_t1));
  vector<Type> alpha_15to49_t1((X_15to49 * artnum_t1) / plhiv_15to49_t1);

  vector<Type> mu_lambda_t1(X_lambda * beta_lambda + log_lambda_t1_offset +
                            Z_x * vector<Type>(log(rho_15to49_t1) + log(1.0 - omega * alpha_15to49_t1)) +
                            Z_lambda_x * ui_lambda_x);

  vector<Type> lambda_adult_t1(exp(mu_lambda_t1));

  // Add paediatric incidence
  vector<Type> lambda_paed_t1(X_paed_lambda_ratio_t1 * rho_15to49f_t1);
  vector<Type> lambda_t1(lambda_adult_t1 + lambda_paed_t1);

  vector<Type> infections_t1(lambda_t1 * (population_t1 - plhiv_t1));


  // Projection from t1 to t2

  vector<Type> mu_alpha_t2(mu_alpha_t1 + logit_alpha_t1t2_offset +
                           X_alpha_t2 * beta_alpha_t2 +
                           Z_alpha_xt * u_alpha_xt +
                           Z_alpha_xat * u_alpha_xat +
                           Z_alpha_xst * u_alpha_xst);
  vector<Type> alpha_t2(invlogit(mu_alpha_t2));

  vector<Type> infections_adult_t1t2(lambda_adult_t1 * (population_t1 - plhiv_t1));
  vector<Type> plhiv_t2(Lproj_hivpop_t1t2 * plhiv_t1 + Lproj_incid_t1t2 * infections_adult_t1t2 + Lproj_paed_t1t2 * plhiv_t1);

  vector<Type> rho_t2(plhiv_t2 / population_t2);
  vector<Type> prop_art_t2(rho_t2 * alpha_t2);
  vector<Type> artnum_t2(population_t2 * prop_art_t2);


  vector<Type> plhiv_15to49_t2(X_15to49 * plhiv_t2);
  vector<Type> rho_15to49_t2(plhiv_15to49_t2 / (X_15to49 * population_t2));
  vector<Type> alpha_15to49_t2((X_15to49 * artnum_t2) / plhiv_15to49_t2);

  vector<Type> mu_lambda_t2(X_lambda * beta_lambda + log_lambda_t2_offset +
                            Z_x * vector<Type>(log(rho_15to49_t2) + log(1.0 - omega * alpha_15to49_t2)) +
                            Z_lambda_x * ui_lambda_x);

  vector<Type> lambda_adult_t2(exp(mu_lambda_t2));

  // Add paediatric incidence
  vector<Type> rho_15to49f_t2((X_15to49f * vector<Type>(invlogit(mu_rho_t1) * population_t2)) / (X_15to49f * population_t2));
  vector<Type> lambda_paed_t2(X_paed_lambda_ratio_t2 * rho_15to49f_t2);
  vector<Type> lambda_t2(lambda_adult_t2 + lambda_paed_t2);

  vector<Type> infections_t2(lambda_t2 * (population_t2 - plhiv_t2));


  // Projection from t2 to t3

  DATA_VECTOR(population_t3);
  DATA_SPARSE_MATRIX(Lproj_hivpop_t2t3);
  DATA_SPARSE_MATRIX(Lproj_incid_t2t3);
  DATA_SPARSE_MATRIX(Lproj_paed_t2t3);
  DATA_VECTOR(logit_alpha_t2t3_offset);
  DATA_VECTOR(log_lambda_t3_offset);
  DATA_SPARSE_MATRIX(X_paed_lambda_ratio_t3);

  vector<Type> mu_alpha_t3(mu_alpha_t2 + logit_alpha_t2t3_offset +
                           X_alpha_t2t3 * beta_alpha_t2t3 +
                           Z_alpha_xt_t2t3 * u_alpha_xt_t2t3 +
                           Z_alpha_xat_t2t3 * u_alpha_xat_t2t3 +
                           Z_alpha_xst_t2t3 * u_alpha_xst_t2t3);
  vector<Type> alpha_t3(invlogit(mu_alpha_t3));

  vector<Type> infections_adult_t2t3(lambda_adult_t2 * (population_t2 - plhiv_t2));
  vector<Type> plhiv_t3(Lproj_hivpop_t2t3 * plhiv_t2 + Lproj_incid_t2t3 * infections_adult_t2t3 + Lproj_paed_t2t3 * plhiv_t2);

  vector<Type> rho_t3(plhiv_t3 / population_t3);
  vector<Type> prop_art_t3(rho_t3 * alpha_t3);
  vector<Type> artnum_t3(population_t3 * prop_art_t3);


  vector<Type> plhiv_15to49_t3(X_15to49 * plhiv_t3);
  vector<Type> rho_15to49_t3(plhiv_15to49_t3 / (X_15to49 * population_t3));
  vector<Type> alpha_15to49_t3((X_15to49 * artnum_t3) / plhiv_15to49_t3);

  vector<Type> mu_lambda_t3(X_lambda * beta_lambda +
                            log_lambda_t3_offset +
                            Z_x * vector<Type>(log(rho_15to49_t3) + log(1.0 - omega * alpha_15to49_t3)) +
                            Z_lambda_x * ui_lambda_x);

  vector<Type> lambda_adult_t3(exp(mu_lambda_t3));

  // Add paediatric incidence
  vector<Type> rho_15to49f_t3((X_15to49f * vector<Type>(rho_t3 * population_t3)) / (X_15to49f * population_t3));
  vector<Type> lambda_paed_t3(X_paed_lambda_ratio_t3 * rho_15to49f_t3);
  vector<Type> lambda_t3(lambda_adult_t3 + lambda_paed_t3);

  vector<Type> infections_t3(lambda_t3 * (population_t3 - plhiv_t3));



  // likelihood for household survey data

  vector<Type> rho_obs_t1((A_prev_t1 * plhiv_t1) / (A_prev_t1 * population_t1));
  vector<Type> hhs_prev_t1_ll = dbinom(x_prev_t1, n_prev_t1, rho_obs_t1, true);
  val -= sum(hhs_prev_t1_ll);

  vector<Type> alpha_obs_t1((A_artcov_t1 * artnum_t1) / (A_artcov_t1 * plhiv_t1));
  vector<Type> hhs_artcov_t1_ll = dbinom(x_artcov_t1, n_artcov_t1, alpha_obs_t1, true);
  val -= sum(hhs_artcov_t1_ll);

  vector<Type> vls_obs_t1(nu * (A_vls_t1 * artnum_t1) / (A_vls_t1 * plhiv_t1));
  vector<Type> hhs_vls_t1_ll = dbinom(x_vls_t1, n_vls_t1, vls_obs_t1, true);
  val -= sum(hhs_vls_t1_ll);

  vector<Type> pR_infections_obs_t1(A_recent_t1 * infections_t1);
  vector<Type> pR_plhiv_obs_t1(A_recent_t1 * plhiv_t1);
  vector<Type> pR_population_obs_t1(A_recent_t1 * population_t1);
  vector<Type> pR_lambda_obs_t1(pR_infections_obs_t1 / (pR_population_obs_t1 - pR_plhiv_obs_t1));
  vector<Type> pR_rho_obs_t1(pR_plhiv_obs_t1 / pR_population_obs_t1);
  vector<Type> pR_t1(1.0 - exp(-(pR_lambda_obs_t1 * (1.0 - pR_rho_obs_t1) / pR_rho_obs_t1 *
                                 (OmegaT - betaT * ritaT) + betaT)));
  vector<Type> hhs_recent_t1_ll = dbinom(x_recent_t1, n_recent_t1, pR_t1, true);
  val -= sum(hhs_recent_t1_ll);


  vector<Type> rho_obs_t2((A_prev_t2 * plhiv_t2) / (A_prev_t2 * population_t2));
  vector<Type> hhs_prev_t2_ll = dbinom(x_prev_t2, n_prev_t2, rho_obs_t2, true);
  val -= sum(hhs_prev_t2_ll);

  vector<Type> alpha_obs_t2((A_artcov_t2 * artnum_t2) / (A_artcov_t2 * plhiv_t2));
  vector<Type> hhs_artcov_t2_ll = dbinom(x_artcov_t2, n_artcov_t2, alpha_obs_t2, true);
  val -= sum(hhs_artcov_t2_ll);

  vector<Type> vls_obs_t2(nu * (A_vls_t2 * artnum_t2) / (A_vls_t2 * plhiv_t2));
  vector<Type> hhs_vls_t2_ll = dbinom(x_vls_t2, n_vls_t2, vls_obs_t2, true);
  val -= sum(hhs_vls_t2_ll);

  vector<Type> pR_infections_obs_t2(A_recent_t2 * infections_t2);
  vector<Type> pR_plhiv_obs_t2(A_recent_t2 * plhiv_t2);
  vector<Type> pR_population_obs_t2(A_recent_t2 * population_t2);
  vector<Type> pR_lambda_obs_t2(pR_infections_obs_t2 / (pR_population_obs_t2 - pR_plhiv_obs_t2));
  vector<Type> pR_rho_obs_t2(pR_plhiv_obs_t2 / pR_population_obs_t2);
  vector<Type> pR_t2(1.0 - exp(-(pR_lambda_obs_t2 * (1.0 - pR_rho_obs_t2) / pR_rho_obs_t2 *
                                 (OmegaT - betaT * ritaT) + betaT)));
  vector<Type> hhs_recent_t2_ll = dbinom(x_recent_t2, n_recent_t2, pR_t2, true);
  val -= sum(hhs_recent_t2_ll);


  // ANC prevalence and ART coverage model
  // Note: currently this operates on the entire population vector, producing
  //       lots of zeros for males and female age groups not exposed to fertility.
  //       It would be more computationally efficient to project this to subset
  //       of female age 15-49 age groups. But I don't know if it would be
  //       meaningfully more efficient.

  vector<Type> mu_asfr(X_asfr * beta_asfr +
                       Z_asfr_x * ui_asfr_x);

  vector<Type> mu_anc_rho_t1(mu_rho_t1 +
                             logit_anc_rho_t1_offset +
                             X_ancrho * beta_anc_rho +
                             Z_ancrho_x * ui_anc_rho_x);
  vector<Type> anc_rho_t1(invlogit(mu_anc_rho_t1));

  vector<Type> mu_anc_alpha_t1(mu_alpha_t1 +
                               logit_anc_alpha_t1_offset +
                               X_ancalpha * beta_anc_alpha +
                               Z_ancalpha_x * ui_anc_alpha_x);
  vector<Type> anc_alpha_t1(invlogit(mu_anc_alpha_t1));

  vector<Type> anc_clients_t1(population_t1 * exp(log_asfr_t1_offset + mu_asfr));
  vector<Type> anc_plhiv_t1(anc_clients_t1 * anc_rho_t1);
  vector<Type> anc_already_art_t1(anc_plhiv_t1 * anc_alpha_t1);

  vector<Type> mu_anc_rho_t2(logit(rho_t2) +
                             logit_anc_rho_t2_offset +
                             X_ancrho * vector<Type>(beta_anc_rho + beta_anc_rho_t2) +
                             Z_ancrho_x * vector<Type>(ui_anc_rho_x + ui_anc_rho_xt));
  vector<Type> anc_rho_t2(invlogit(mu_anc_rho_t2));

  vector<Type> mu_anc_alpha_t2(mu_alpha_t2 +
                               logit_anc_alpha_t2_offset +
                               X_ancalpha * vector<Type>(beta_anc_alpha + beta_anc_alpha_t2) +
                               Z_ancalpha_x * vector<Type>(ui_anc_alpha_x + ui_anc_alpha_xt));
  vector<Type> anc_alpha_t2(invlogit(mu_anc_alpha_t2));

  vector<Type> anc_clients_t2(population_t2 * exp(log_asfr_t2_offset + mu_asfr));
  vector<Type> anc_plhiv_t2(anc_clients_t2 * anc_rho_t2);
  vector<Type> anc_already_art_t2(anc_plhiv_t2 * anc_alpha_t2);


  vector<Type> mu_anc_rho_t3(logit(rho_t3) +
                             logit_anc_rho_t3_offset +
                             X_ancrho * vector<Type>(beta_anc_rho + beta_anc_rho_t2 + beta_anc_rho_t3) +
                             Z_ancrho_x * vector<Type>(ui_anc_rho_x + ui_anc_rho_xt + ui_anc_rho_xt2t3));
  vector<Type> anc_rho_t3(invlogit(mu_anc_rho_t3));

  vector<Type> mu_anc_alpha_t3(mu_alpha_t3 +
                               logit_anc_alpha_t3_offset +
                               X_ancalpha * vector<Type>(beta_anc_alpha + beta_anc_alpha_t2 + beta_anc_alpha_t3) +
                               Z_ancalpha_x * vector<Type>(ui_anc_alpha_x + ui_anc_alpha_xt + ui_anc_alpha_xt2t3));
  vector<Type> anc_alpha_t3(invlogit(mu_anc_alpha_t3));

  vector<Type> anc_clients_t3(population_t3 * exp(log_asfr_t3_offset + mu_asfr));
  vector<Type> anc_plhiv_t3(anc_clients_t3 * anc_rho_t3);
  vector<Type> anc_already_art_t3(anc_plhiv_t3 * anc_alpha_t3);

  // likelihood for ANC testing observations

  vector<Type> anc_clients_obs_t3((A_anc_clients_t3 * anc_clients_t3) * exp(offset_anc_clients_t3));
  vector<Type> anc_clients_obs_t3_ll = dpois(x_anc_clients_t3, anc_clients_obs_t3, true);
  val -= sum(anc_clients_obs_t3_ll);

  vector<Type> anc_rho_obs_t1(A_anc_prev_t1 * anc_plhiv_t1 / (A_anc_prev_t1 * anc_clients_t1));
  vector<Type> anc_rho_obs_t1_ll = dbinom(x_anc_prev_t1, n_anc_prev_t1, anc_rho_obs_t1, true);
  val -= sum(anc_rho_obs_t1_ll);

  vector<Type> anc_alpha_obs_t1(A_anc_artcov_t1 * anc_already_art_t1 / (A_anc_artcov_t1 * anc_plhiv_t1));
  vector<Type> anc_alpha_obs_t1_ll = dbinom(x_anc_artcov_t1, n_anc_artcov_t1, anc_alpha_obs_t1, true);
  val -= sum(anc_alpha_obs_t1_ll);

  vector<Type> anc_rho_obs_t2(A_anc_prev_t2 * anc_plhiv_t2 / (A_anc_prev_t2 * anc_clients_t2));
  vector<Type> anc_rho_obs_t2_ll = dbinom(x_anc_prev_t2, n_anc_prev_t2, anc_rho_obs_t2, true);
  val -= sum(anc_rho_obs_t2_ll);

  vector<Type> anc_alpha_obs_t2(A_anc_artcov_t2 * anc_already_art_t2 / (A_anc_artcov_t2 * anc_plhiv_t2));
  vector<Type> anc_alpha_obs_t2_ll = dbinom(x_anc_artcov_t2, n_anc_artcov_t2, anc_alpha_obs_t2, true);
  val -= sum(anc_alpha_obs_t2_ll);

  vector<Type> anc_rho_obs_t3(A_anc_prev_t3 * anc_plhiv_t3 / (A_anc_prev_t3 * anc_clients_t3));
  vector<Type> anc_rho_obs_t3_ll = dbinom(x_anc_prev_t3, n_anc_prev_t3, anc_rho_obs_t3, true);
  val -= sum(anc_rho_obs_t3_ll);

  vector<Type> anc_alpha_obs_t3(A_anc_artcov_t3 * anc_already_art_t3 / (A_anc_artcov_t3 * anc_plhiv_t3));
  vector<Type> anc_alpha_obs_t3_ll = dbinom(x_anc_artcov_t3, n_anc_artcov_t3, anc_alpha_obs_t3, true);
  val -= sum(anc_alpha_obs_t3_ll);


  // * ART attendance model *

  vector<Type> gamma_art_t1(exp(Xgamma * log_or_gamma + log_gamma_offset));
  int cum_nb = 0;
  for(int i = 0; i < n_nb.size(); i++){
    Type cum_exp_or_gamma_i = 0.0;
    for(int j = 0; j < n_nb[i]+1; j++)
      cum_exp_or_gamma_i += gamma_art_t1[cum_nb + i + j];
    for(int j = 0; j < n_nb[i]+1; j++)
      gamma_art_t1[cum_nb + i + j] /= cum_exp_or_gamma_i;
    cum_nb += n_nb[i];
  }

  vector<Type> immigrant_theta_t1(recent_migrant_hivprev_15to49 / (Z_x * rho_15to49_t1));
  vector<Type> foreign_art_prop_t1(foreign_art_eta_scalar * foreign_art_eta_t1 * immigrant_theta_t1 * recent_immigration_prop);
  vector<Type> prop_art_domestic_t1(prop_art_t1 * (1.0 - foreign_art_prop_t1));


  vector<Type> mu_art_medaid_t1(X_art_medaid * beta_art_medaid +
                                Z_art_medaid_x * u_art_medaid_x +
                                Z_art_medaid_xs * u_art_medaid_xs +
                                Z_art_medaid_xa * u_art_medaid_xa);

  vector<Type> mu_art_cashpay_t1(X_art_cashpay * beta_art_cashpay +
                                 Z_art_cashpay_x * u_art_cashpay_x +
                                 Z_art_cashpay_xs * u_art_cashpay_xs +
                                 Z_art_cashpay_xa * u_art_cashpay_xa);

  vector<Type> exp_mu_art_medaid_t1(exp(mu_art_medaid_t1));
  vector<Type> exp_mu_art_cashpay_t1(exp(mu_art_cashpay_t1));

  vector<Type> p_art_public_t1(1.0 / (1.0 + exp_mu_art_medaid_t1 + exp_mu_art_cashpay_t1));
  vector<Type> p_art_medaid_t1(exp_mu_art_medaid_t1 / (1.0 + exp_mu_art_medaid_t1 + exp_mu_art_cashpay_t1));
  vector<Type> p_art_cashpay_t1(exp_mu_art_cashpay_t1 / (1.0 + exp_mu_art_medaid_t1 + exp_mu_art_cashpay_t1));

  vector<Type> prop_art_public_t1(prop_art_domestic_t1 * p_art_public_t1);
  vector<Type> prop_art_medaid_t1(prop_art_domestic_t1 * p_art_medaid_t1);
  vector<Type> prop_art_cashpay_t1(prop_art_domestic_t1 * p_art_cashpay_t1);
  vector<Type> prop_art_foreign_t1(prop_art_t1 - prop_art_domestic_t1);
  vector<Type> prop_art_public_ij_t1((Xart_idx * prop_art_public_t1) * (Xart_gamma * gamma_art_t1));
  vector<Type> prop_art_medaid_ij_t1((Xart_idx * prop_art_medaid_t1) * (Xart_gamma * gamma_art_t1));
  vector<Type> prop_art_cashpay_ij_t1((Xart_idx * prop_art_cashpay_t1) * (Xart_gamma * gamma_art_t1));
  vector<Type> prop_art_foreign_ij_t1((Xart_idx * prop_art_foreign_t1) * (Xart_gamma * gamma_art_t1));
  vector<Type> population_ij_t1(Xart_idx * population_t1);

  vector<Type> artnum_public_ij_t1(population_ij_t1 * prop_art_public_ij_t1);
  vector<Type> artnum_medaid_ij_t1(population_ij_t1 * prop_art_medaid_ij_t1);
  vector<Type> artnum_cashpay_ij_t1(population_ij_t1 * prop_art_cashpay_ij_t1);
  vector<Type> artnum_foreign_ij_t1(population_ij_t1 * prop_art_foreign_ij_t1);

  vector<Type> artnum_ij_t1(artnum_public_ij_t1 + artnum_medaid_ij_t1 + artnum_cashpay_ij_t1 + artnum_foreign_ij_t1);

  vector<Type> A_public_j_t1(A_artattend_t1 * artnum_public_ij_t1);
  vector<Type> A_medaid_j_t1(A_artattend_t1 * artnum_medaid_ij_t1);
  vector<Type> A_cashpay_j_t1(A_artattend_t1 * artnum_cashpay_ij_t1);

  vector<Type> sd_A_public_j_t1(A_artattend_t1 * vector<Type>(population_ij_t1 * prop_art_public_ij_t1 * (1 - prop_art_public_ij_t1)));
  vector<Type> sd_A_medaid_j_t1(A_artattend_t1 * vector<Type>(population_ij_t1 * prop_art_medaid_ij_t1 * (1 - prop_art_medaid_ij_t1)));
  vector<Type> sd_A_cashpay_j_t1(A_artattend_t1 * vector<Type>(population_ij_t1 * prop_art_cashpay_ij_t1 * (1 - prop_art_cashpay_ij_t1)));

  sd_A_public_j_t1 = sd_A_public_j_t1.sqrt();
  sd_A_medaid_j_t1 = sd_A_medaid_j_t1.sqrt();
  sd_A_cashpay_j_t1 = sd_A_cashpay_j_t1.sqrt();

  vector<Type> artnum_public_t1_ll = dnorm(x_artnum_public_t1, A_public_j_t1, sd_A_public_j_t1, true);
  vector<Type> artnum_medaid_t1_ll = dnorm(x_artnum_medaid_t1, A_medaid_j_t1, sd_A_medaid_j_t1, true);
  vector<Type> artnum_cashpay_t1_ll = dnorm(x_artnum_cashpay_t1, A_cashpay_j_t1, sd_A_cashpay_j_t1, true);

  val -= sum(artnum_public_t1_ll);
  val -= sum(artnum_medaid_t1_ll);
  val -= sum(artnum_cashpay_t1_ll);

  /*
    vector<Type> prop_art_ij_t1((Xart_idx * prop_art_domestic_t1) * (Xart_gamma * gamma_art_t1));
    vector<Type> A_j_t1(A_artattend_t1 * vector<Type>(population_ij_t1 * prop_art_ij_t1));
    vector<Type> sd_A_j_t1(A_artattend_t1 * vector<Type>(population_ij_t1 * prop_art_ij_t1 * (1 - prop_art_ij_t1)));
    sd_A_j_t1 = sd_A_j_t1.sqrt();
    vector<Type> artnum_t1_ll = dnorm(x_artnum_t1, A_j_t1, sd_A_j_t1, true);
    val -= sum(artnum_t1_ll);
  */

  vector<Type> gamma_art_t2(exp(Xgamma * log_or_gamma +
                                Xgamma_t2 * log_or_gamma_t1t2 +
                                log_gamma_offset));
  cum_nb = 0;
  for(int i = 0; i < n_nb.size(); i++){
    Type cum_exp_or_gamma_i = 0.0;
    for(int j = 0; j < n_nb[i]+1; j++)
      cum_exp_or_gamma_i += gamma_art_t2[cum_nb + i + j];
    for(int j = 0; j < n_nb[i]+1; j++)
      gamma_art_t2[cum_nb + i + j] /= cum_exp_or_gamma_i;
    cum_nb += n_nb[i];
  }

  vector<Type> immigrant_theta_t2(recent_migrant_hivprev_15to49 / (Z_x * rho_15to49_t2));
  vector<Type> foreign_art_prop_t2(foreign_art_eta_scalar * foreign_art_eta_t2 * immigrant_theta_t2 * recent_immigration_prop);
  vector<Type> prop_art_domestic_t2(prop_art_t2 * (1.0 - foreign_art_prop_t2));

  vector<Type> mu_art_medaid_t2(mu_art_medaid_t1 +
                                X_art_medaid_t1t2 * beta_art_medaid_t1t2 +
                                Z_art_medaid_x_t1t2 * u_art_medaid_x_t1t2 +
                                Z_art_medaid_xs_t1t2 * u_art_medaid_xs_t1t2 +
                                Z_art_medaid_xa_t1t2 * u_art_medaid_xa_t1t2);
  vector<Type> mu_art_cashpay_t2(mu_art_cashpay_t1 +
                                 X_art_cashpay_t1t2 * beta_art_cashpay_t1t2 +
                                 Z_art_cashpay_x_t1t2 * u_art_cashpay_x_t1t2 +
                                 Z_art_cashpay_xs_t1t2 * u_art_cashpay_xs_t1t2 +
                                 Z_art_cashpay_xa_t1t2 * u_art_cashpay_xa_t1t2);

  vector<Type> exp_mu_art_medaid_t2(exp(mu_art_medaid_t2));
  vector<Type> exp_mu_art_cashpay_t2(exp(mu_art_cashpay_t2));

  vector<Type> p_art_public_t2(1.0 / (1.0 + exp_mu_art_medaid_t2 + exp_mu_art_cashpay_t2));
  vector<Type> p_art_medaid_t2(exp_mu_art_medaid_t2 / (1.0 + exp_mu_art_medaid_t2 + exp_mu_art_cashpay_t2));
  vector<Type> p_art_cashpay_t2(exp_mu_art_cashpay_t2 / (1.0 + exp_mu_art_medaid_t2 + exp_mu_art_cashpay_t2));

  vector<Type> prop_art_public_t2(prop_art_domestic_t2 * p_art_public_t2);
  vector<Type> prop_art_medaid_t2(prop_art_domestic_t2 * p_art_medaid_t2);
  vector<Type> prop_art_cashpay_t2(prop_art_domestic_t2 * p_art_cashpay_t2);
  vector<Type> prop_art_foreign_t2(prop_art_t2 - prop_art_domestic_t2);
  vector<Type> prop_art_public_ij_t2((Xart_idx * prop_art_public_t2) * (Xart_gamma * gamma_art_t2));
  vector<Type> prop_art_medaid_ij_t2((Xart_idx * prop_art_medaid_t2) * (Xart_gamma * gamma_art_t2));
  vector<Type> prop_art_cashpay_ij_t2((Xart_idx * prop_art_cashpay_t2) * (Xart_gamma * gamma_art_t2));
  vector<Type> prop_art_foreign_ij_t2((Xart_idx * prop_art_foreign_t2) * (Xart_gamma * gamma_art_t2));
  vector<Type> population_ij_t2(Xart_idx * population_t2);

  vector<Type> artnum_public_ij_t2(population_ij_t2 * prop_art_public_ij_t2);
  vector<Type> artnum_medaid_ij_t2(population_ij_t2 * prop_art_medaid_ij_t2);
  vector<Type> artnum_cashpay_ij_t2(population_ij_t2 * prop_art_cashpay_ij_t2);
  vector<Type> artnum_foreign_ij_t2(population_ij_t2 * prop_art_foreign_ij_t2);

  vector<Type> artnum_ij_t2(artnum_public_ij_t2 + artnum_medaid_ij_t2 + artnum_cashpay_ij_t2 + artnum_foreign_ij_t2);

  vector<Type> A_public_j_t2(A_artattend_t2 * artnum_public_ij_t2);
  vector<Type> A_medaid_j_t2(A_artattend_t2 * artnum_medaid_ij_t2);
  vector<Type> A_cashpay_j_t2(A_artattend_t2 * artnum_cashpay_ij_t2);

  vector<Type> sd_A_public_j_t2(A_artattend_t2 * vector<Type>(population_ij_t2 * prop_art_public_ij_t2 * (1 - prop_art_public_ij_t2)));
  vector<Type> sd_A_medaid_j_t2(A_artattend_t2 * vector<Type>(population_ij_t2 * prop_art_medaid_ij_t2 * (1 - prop_art_medaid_ij_t2)));
  vector<Type> sd_A_cashpay_j_t2(A_artattend_t2 * vector<Type>(population_ij_t2 * prop_art_cashpay_ij_t2 * (1 - prop_art_cashpay_ij_t2)));

  sd_A_public_j_t2 = sd_A_public_j_t2.sqrt();
  sd_A_medaid_j_t2 = sd_A_medaid_j_t2.sqrt();
  sd_A_cashpay_j_t2 = sd_A_cashpay_j_t2.sqrt();

  vector<Type> artnum_public_t2_ll = dnorm(x_artnum_public_t2, A_public_j_t2, sd_A_public_j_t2, true);
  vector<Type> artnum_medaid_t2_ll = dnorm(x_artnum_medaid_t2, A_medaid_j_t2, sd_A_medaid_j_t2, true);
  vector<Type> artnum_cashpay_t2_ll = dnorm(x_artnum_cashpay_t2, A_cashpay_j_t2, sd_A_cashpay_j_t2, true);

  val -= sum(artnum_public_t2_ll);
  val -= sum(artnum_medaid_t2_ll);
  val -= sum(artnum_cashpay_t2_ll);

  /*
    vector<Type> prop_art_ij_t2((Xart_idx * prop_art_domestic_t2) * (Xart_gamma * gamma_art_t2));
    vector<Type> A_j_t2(A_artattend_t2 * vector<Type>(population_ij_t2 * prop_art_ij_t2));
    vector<Type> sd_A_j_t2(A_artattend_t2 * vector<Type>(population_ij_t2 * prop_art_ij_t2 * (1 - prop_art_ij_t2)));
    sd_A_j_t2 = sd_A_j_t2.sqrt();
    vector<Type> artnum_t2_ll = dnorm(x_artnum_t2, A_j_t2, sd_A_j_t2, true);
    val -= sum(artnum_t2_ll);
  */

  vector<Type> gamma_art_t3(exp(Xgamma * log_or_gamma +
                                Xgamma_t2 * log_or_gamma_t1t2 +
                                Xgamma_t3 * log_or_gamma_t2t3 +
                                log_gamma_offset));
  cum_nb = 0;
  for(int i = 0; i < n_nb.size(); i++){
    Type cum_exp_or_gamma_i = 0.0;
    for(int j = 0; j < n_nb[i]+1; j++)
      cum_exp_or_gamma_i += gamma_art_t3[cum_nb + i + j];
    for(int j = 0; j < n_nb[i]+1; j++)
      gamma_art_t3[cum_nb + i + j] /= cum_exp_or_gamma_i;
    cum_nb += n_nb[i];
  }

  vector<Type> immigrant_theta_t3(recent_migrant_hivprev_15to49 / (Z_x * rho_15to49_t3));
  vector<Type> foreign_art_prop_t3(foreign_art_eta_scalar * foreign_art_eta_t3 * immigrant_theta_t3 * recent_immigration_prop);
  vector<Type> prop_art_domestic_t3(prop_art_t3 * (1.0 - foreign_art_prop_t3));


  vector<Type> mu_art_medaid_t3(mu_art_medaid_t2 +
                                X_art_medaid_t2t3 * beta_art_medaid_t2t3 +
                                Z_art_medaid_x_t2t3 * u_art_medaid_x_t2t3 +
                                Z_art_medaid_xs_t2t3 * u_art_medaid_xs_t2t3 +
                                Z_art_medaid_xa_t2t3 * u_art_medaid_xa_t2t3);
  vector<Type> mu_art_cashpay_t3(mu_art_cashpay_t2 +
                                 X_art_cashpay_t2t3 * beta_art_cashpay_t2t3 +
                                 Z_art_cashpay_x_t2t3 * u_art_cashpay_x_t2t3 +
                                 Z_art_cashpay_xs_t2t3 * u_art_cashpay_xs_t2t3 +
                                 Z_art_cashpay_xa_t2t3 * u_art_cashpay_xa_t2t3);


  vector<Type> exp_mu_art_medaid_t3(exp(mu_art_medaid_t3));
  vector<Type> exp_mu_art_cashpay_t3(exp(mu_art_cashpay_t3));

  vector<Type> p_art_public_t3(1.0 / (1.0 + exp_mu_art_medaid_t3 + exp_mu_art_cashpay_t3));
  vector<Type> p_art_medaid_t3(exp_mu_art_medaid_t3 / (1.0 + exp_mu_art_medaid_t3 + exp_mu_art_cashpay_t3));
  vector<Type> p_art_cashpay_t3(exp_mu_art_cashpay_t3 / (1.0 + exp_mu_art_medaid_t3 + exp_mu_art_cashpay_t3));

  vector<Type> prop_art_public_t3(prop_art_domestic_t3 * p_art_public_t3);
  vector<Type> prop_art_medaid_t3(prop_art_domestic_t3 * p_art_medaid_t3);
  vector<Type> prop_art_cashpay_t3(prop_art_domestic_t3 * p_art_cashpay_t3);
  vector<Type> prop_art_foreign_t3(prop_art_t3 - prop_art_domestic_t3);
  vector<Type> prop_art_public_ij_t3((Xart_idx * prop_art_public_t3) * (Xart_gamma * gamma_art_t3));
  vector<Type> prop_art_medaid_ij_t3((Xart_idx * prop_art_medaid_t3) * (Xart_gamma * gamma_art_t3));
  vector<Type> prop_art_cashpay_ij_t3((Xart_idx * prop_art_cashpay_t3) * (Xart_gamma * gamma_art_t3));
  vector<Type> prop_art_foreign_ij_t3((Xart_idx * prop_art_foreign_t3) * (Xart_gamma * gamma_art_t3));
  vector<Type> population_ij_t3(Xart_idx * population_t3);

  vector<Type> artnum_public_ij_t3(population_ij_t3 * prop_art_public_ij_t3);
  vector<Type> artnum_medaid_ij_t3(population_ij_t3 * prop_art_medaid_ij_t3);
  vector<Type> artnum_cashpay_ij_t3(population_ij_t3 * prop_art_cashpay_ij_t3);
  vector<Type> artnum_foreign_ij_t3(population_ij_t3 * prop_art_foreign_ij_t3);

  vector<Type> artnum_ij_t3(artnum_public_ij_t3 + artnum_medaid_ij_t3 + artnum_cashpay_ij_t3 + artnum_foreign_ij_t3);

  vector<Type> A_public_j_t3(A_artattend_t3 * artnum_public_ij_t3);
  vector<Type> A_medaid_j_t3(A_artattend_t3 * artnum_medaid_ij_t3);
  vector<Type> A_cashpay_j_t3(A_artattend_t3 * artnum_cashpay_ij_t3);

  vector<Type> sd_A_public_j_t3(A_artattend_t3 * vector<Type>(population_ij_t3 * prop_art_public_ij_t3 * (1 - prop_art_public_ij_t3)));
  vector<Type> sd_A_medaid_j_t3(A_artattend_t3 * vector<Type>(population_ij_t3 * prop_art_medaid_ij_t3 * (1 - prop_art_medaid_ij_t3)));
  vector<Type> sd_A_cashpay_j_t3(A_artattend_t3 * vector<Type>(population_ij_t3 * prop_art_cashpay_ij_t3 * (1 - prop_art_cashpay_ij_t3)));

  sd_A_public_j_t3 = sd_A_public_j_t3.sqrt();
  sd_A_medaid_j_t3 = sd_A_medaid_j_t3.sqrt();
  sd_A_cashpay_j_t3 = sd_A_cashpay_j_t3.sqrt();

  vector<Type> artnum_public_t3_ll = dnorm(x_artnum_public_t3, A_public_j_t3, sd_A_public_j_t3, true);
  vector<Type> artnum_medaid_t3_ll = dnorm(x_artnum_medaid_t3, A_medaid_j_t3, sd_A_medaid_j_t3, true);
  vector<Type> artnum_cashpay_t3_ll = dnorm(x_artnum_cashpay_t3, A_cashpay_j_t3, sd_A_cashpay_j_t3, true);

  val -= sum(artnum_public_t3_ll);
  val -= sum(artnum_medaid_t3_ll);
  val -= sum(artnum_cashpay_t3_ll);

  /*
    vector<Type> prop_art_ij_t3((Xart_idx * prop_art_domestic_t3) * (Xart_gamma * gamma_art_t3));
    vector<Type> A_j_t3(A_artattend_t3 * vector<Type>(population_ij_t3 * prop_art_ij_t3));
    vector<Type> sd_A_j_t3(A_artattend_t3 * vector<Type>(population_ij_t3 * prop_art_ij_t3 * (1 - prop_art_ij_t3)));
    sd_A_j_t3 = sd_A_j_t3.sqrt();
    vector<Type> artnum_t3_ll = dnorm(x_artnum_t3, A_j_t3, sd_A_j_t3, true);
    val -= sum(artnum_t3_ll);
  */

  // Calculate model outputs

  DATA_SPARSE_MATRIX(A_out);
  DATA_SPARSE_MATRIX(A_anc_out);
  DATA_INTEGER(calc_outputs);
  DATA_INTEGER(report_likelihood);
  
  // Proportion unaware among the untreated population
  // Fixed input from Spectrum
  DATA_VECTOR(unaware_untreated_prop_t1);
  DATA_VECTOR(unaware_untreated_prop_t2);
  DATA_VECTOR(unaware_untreated_prop_t3);
  DATA_VECTOR(unaware_untreated_prop_t4);
  DATA_VECTOR(unaware_untreated_prop_t5);
  DATA_VECTOR(unaware_untreated_prop_t6);

  if(calc_outputs) {

    vector<Type> population_t1_out(A_out * population_t1);

    vector<Type> plhiv_t1_out(A_out * plhiv_t1);
    vector<Type> rho_t1_out(plhiv_t1_out / population_t1_out);

    vector<Type> artnum_t1_out(A_out * artnum_t1);
    vector<Type> alpha_t1_out(artnum_t1_out / plhiv_t1_out);
    vector<Type> artattend_t1_out(A_out * (A_artattend_mf * artnum_ij_t1));
    vector<Type> artattend_ij_t1_out(A_art_reside_attend * artnum_ij_t1);
    vector<Type> untreated_plhiv_num_t1_out(plhiv_t1_out - artnum_t1_out);

    vector<Type> artnum_public_t1_out(A_out * vector<Type>(population_t1 * prop_art_public_t1));
    vector<Type> artnum_medaid_t1_out(A_out * vector<Type>(population_t1 * prop_art_medaid_t1));
    vector<Type> artnum_cashpay_t1_out(A_out * vector<Type>(population_t1 * prop_art_cashpay_t1));
    vector<Type> artnum_foreign_t1_out(A_out * vector<Type>(population_t1 * prop_art_foreign_t1));

    vector<Type> artprop_public_t1_out(artnum_public_t1_out / artnum_t1_out);
    vector<Type> artprop_medaid_t1_out(artnum_medaid_t1_out / artnum_t1_out);
    vector<Type> artprop_cashpay_t1_out(artnum_cashpay_t1_out / artnum_t1_out);
    vector<Type> artprop_foreign_t1_out(artnum_foreign_t1_out / artnum_t1_out);

    vector<Type> artattend_public_t1_out(A_out * (A_artattend_mf * artnum_public_ij_t1));
    vector<Type> artattend_medaid_t1_out(A_out * (A_artattend_mf * artnum_medaid_ij_t1));
    vector<Type> artattend_cashpay_t1_out(A_out * (A_artattend_mf * artnum_cashpay_ij_t1));
    vector<Type> artattend_foreign_t1_out(A_out * (A_artattend_mf * artnum_foreign_ij_t1));

    vector<Type> artattendprop_public_t1_out(artattend_public_t1_out / artattend_t1_out);
    vector<Type> artattendprop_medaid_t1_out(artattend_medaid_t1_out / artattend_t1_out);
    vector<Type> artattendprop_cashpay_t1_out(artattend_cashpay_t1_out / artattend_t1_out);
    vector<Type> artattendprop_foreign_t1_out(artattend_foreign_t1_out / artattend_t1_out);

    // Calculate number of PLHIV who attend facility in district i; denominator for artattend
    vector<Type> plhiv_attend_ij_t1((Xart_idx * plhiv_t1) * (Xart_gamma * gamma_art_t1));
    vector<Type> plhiv_attend_t1_out(A_out * (A_artattend_mf * plhiv_attend_ij_t1));
    vector<Type> untreated_plhiv_attend_t1_out(plhiv_attend_t1_out - artattend_t1_out);

    vector<Type> unaware_plhiv_num_t1((plhiv_t1 - artnum_t1) * unaware_untreated_prop_t1);
    vector<Type> unaware_plhiv_num_t1_out(A_out * unaware_plhiv_num_t1);
    vector<Type> aware_plhiv_num_t1_out(plhiv_t1_out - unaware_plhiv_num_t1_out);
    vector<Type> aware_plhiv_prop_t1_out(aware_plhiv_num_t1_out / plhiv_t1_out);

    vector<Type> unaware_plhiv_attend_ij_t1((Xart_idx * unaware_plhiv_num_t1) * (Xart_gamma * gamma_art_t1));
    vector<Type> unaware_plhiv_attend_t1_out(A_out * (A_artattend_mf * unaware_plhiv_attend_ij_t1));
    vector<Type> aware_plhiv_attend_t1_out(plhiv_attend_t1_out - unaware_plhiv_attend_t1_out);

    vector<Type> artnum_nonpublic_t1(artnum_medaid_t1_out + artnum_cashpay_t1_out + artnum_foreign_t1_out);
    vector<Type> plhiv_attend_public_t1_out(plhiv_attend_t1_out - artnum_nonpublic_t1);
    vector<Type> aware_plhiv_attend_public_t1_out(aware_plhiv_attend_t1_out - artnum_nonpublic_t1);
    

    vector<Type> infections_t1_out(A_out * infections_t1);
    vector<Type> lambda_t1_out(infections_t1_out / (population_t1_out - plhiv_t1_out));

    vector<Type> population_t2_out(A_out * population_t2);

    vector<Type> plhiv_t2_out(A_out * plhiv_t2);
    vector<Type> rho_t2_out(plhiv_t2_out / population_t2_out);

    vector<Type> artnum_t2_out(A_out * artnum_t2);
    vector<Type> alpha_t2_out(artnum_t2_out / plhiv_t2_out);
    vector<Type> artattend_t2_out(A_out * (A_artattend_mf * artnum_ij_t2));
    vector<Type> artattend_ij_t2_out(A_art_reside_attend * artnum_ij_t2);
    vector<Type> untreated_plhiv_num_t2_out(plhiv_t2_out - artnum_t2_out);

    vector<Type> artnum_public_t2_out(A_out * vector<Type>(population_t2 * prop_art_public_t2));
    vector<Type> artnum_medaid_t2_out(A_out * vector<Type>(population_t2 * prop_art_medaid_t2));
    vector<Type> artnum_cashpay_t2_out(A_out * vector<Type>(population_t2 * prop_art_cashpay_t2));
    vector<Type> artnum_foreign_t2_out(A_out * vector<Type>(population_t2 * prop_art_foreign_t2));

    vector<Type> artprop_public_t2_out(artnum_public_t2_out / artnum_t2_out);
    vector<Type> artprop_medaid_t2_out(artnum_medaid_t2_out / artnum_t2_out);
    vector<Type> artprop_cashpay_t2_out(artnum_cashpay_t2_out / artnum_t2_out);
    vector<Type> artprop_foreign_t2_out(artnum_foreign_t2_out / artnum_t2_out);

    vector<Type> artattend_public_t2_out(A_out * (A_artattend_mf * artnum_public_ij_t2));
    vector<Type> artattend_medaid_t2_out(A_out * (A_artattend_mf * artnum_medaid_ij_t2));
    vector<Type> artattend_cashpay_t2_out(A_out * (A_artattend_mf * artnum_cashpay_ij_t2));
    vector<Type> artattend_foreign_t2_out(A_out * (A_artattend_mf * artnum_foreign_ij_t2));

    vector<Type> artattendprop_public_t2_out(artattend_public_t2_out / artattend_t2_out);
    vector<Type> artattendprop_medaid_t2_out(artattend_medaid_t2_out / artattend_t2_out);
    vector<Type> artattendprop_cashpay_t2_out(artattend_cashpay_t2_out / artattend_t2_out);
    vector<Type> artattendprop_foreign_t2_out(artattend_foreign_t2_out / artattend_t2_out);


    // Calculate number of PLHIV who attend facility in district i; denominator for artattend
    vector<Type> plhiv_attend_ij_t2((Xart_idx * plhiv_t2) * (Xart_gamma * gamma_art_t2));
    vector<Type> plhiv_attend_t2_out(A_out * (A_artattend_mf * plhiv_attend_ij_t2));
    vector<Type> untreated_plhiv_attend_t2_out(plhiv_attend_t2_out - artattend_t2_out);

    vector<Type> unaware_plhiv_num_t2((plhiv_t2 - artnum_t2) * unaware_untreated_prop_t2);
    vector<Type> unaware_plhiv_num_t2_out(A_out * unaware_plhiv_num_t2);
    vector<Type> aware_plhiv_num_t2_out(plhiv_t2_out - unaware_plhiv_num_t2_out);
    vector<Type> aware_plhiv_prop_t2_out(aware_plhiv_num_t2_out / plhiv_t2_out);

    vector<Type> unaware_plhiv_attend_ij_t2((Xart_idx * unaware_plhiv_num_t2) * (Xart_gamma * gamma_art_t2));
    vector<Type> unaware_plhiv_attend_t2_out(A_out * (A_artattend_mf * unaware_plhiv_attend_ij_t2));
    vector<Type> aware_plhiv_attend_t2_out(plhiv_attend_t2_out - unaware_plhiv_attend_t2_out);

    vector<Type> artnum_nonpublic_t2(artnum_medaid_t2_out + artnum_cashpay_t2_out + artnum_foreign_t2_out);
    vector<Type> plhiv_attend_public_t2_out(plhiv_attend_t2_out - artnum_nonpublic_t2);
    vector<Type> aware_plhiv_attend_public_t2_out(aware_plhiv_attend_t2_out - artnum_nonpublic_t2);
    

    vector<Type> infections_t2_out(A_out * infections_t2);
    vector<Type> lambda_t2_out(infections_t2_out / (population_t2_out - plhiv_t2_out));

    vector<Type> anc_clients_t1_out(A_anc_out * anc_clients_t1);
    vector<Type> anc_plhiv_t1_out(A_anc_out * anc_plhiv_t1);
    vector<Type> anc_already_art_t1_out(A_anc_out * anc_already_art_t1);

    // Note: assuming that:
    //  (1) anc_known_pos is equivalent to anc_already_art
    //  (2) All ANC attendees are diagnosed and initated on ART.
    vector<Type> anc_art_new_t1_out(anc_plhiv_t1_out - anc_already_art_t1_out);
    vector<Type> anc_known_pos_t1_out(anc_already_art_t1_out);
    vector<Type> anc_tested_pos_t1_out(anc_plhiv_t1_out - anc_known_pos_t1_out);
    vector<Type> anc_tested_neg_t1_out(anc_clients_t1_out - anc_plhiv_t1_out);

    vector<Type> anc_rho_t1_out(anc_plhiv_t1_out / anc_clients_t1_out);
    vector<Type> anc_alpha_t1_out(anc_already_art_t1_out / anc_plhiv_t1_out);

    vector<Type> anc_clients_t2_out(A_anc_out * anc_clients_t2);
    vector<Type> anc_plhiv_t2_out(A_anc_out * anc_plhiv_t2);
    vector<Type> anc_already_art_t2_out(A_anc_out * anc_already_art_t2);

    vector<Type> anc_art_new_t2_out(anc_plhiv_t2_out - anc_already_art_t2_out);
    vector<Type> anc_known_pos_t2_out(anc_already_art_t2_out);
    vector<Type> anc_tested_pos_t2_out(anc_plhiv_t2_out - anc_known_pos_t2_out);
    vector<Type> anc_tested_neg_t2_out(anc_clients_t2_out - anc_plhiv_t2_out);

    vector<Type> anc_rho_t2_out(anc_plhiv_t2_out / anc_clients_t2_out);
    vector<Type> anc_alpha_t2_out(anc_already_art_t2_out / anc_plhiv_t2_out);

    REPORT(population_t1_out);
    REPORT(rho_t1_out);
    REPORT(plhiv_t1_out);
    REPORT(alpha_t1_out);
    REPORT(artnum_t1_out);
    REPORT(artattend_t1_out);
    REPORT(artattend_ij_t1_out);
    REPORT(untreated_plhiv_num_t1_out);
    REPORT(plhiv_attend_t1_out);
    REPORT(untreated_plhiv_attend_t1_out);
    REPORT(artnum_public_t1_out);
    REPORT(artnum_medaid_t1_out);
    REPORT(artnum_cashpay_t1_out);
    REPORT(artnum_foreign_t1_out);
    REPORT(artprop_public_t1_out);
    REPORT(artprop_medaid_t1_out);
    REPORT(artprop_cashpay_t1_out);
    REPORT(artprop_foreign_t1_out);
    REPORT(artattend_public_t1_out);
    REPORT(artattend_medaid_t1_out);
    REPORT(artattend_cashpay_t1_out);
    REPORT(artattend_foreign_t1_out);
    REPORT(artattendprop_public_t1_out);
    REPORT(artattendprop_medaid_t1_out);
    REPORT(artattendprop_cashpay_t1_out);
    REPORT(artattendprop_foreign_t1_out);
    REPORT(aware_plhiv_prop_t1_out);
    REPORT(aware_plhiv_num_t1_out);
    REPORT(unaware_plhiv_num_t1_out);
    REPORT(aware_plhiv_attend_t1_out);
    REPORT(unaware_plhiv_attend_t1_out);
    REPORT(plhiv_attend_public_t1_out);
    REPORT(aware_plhiv_attend_public_t1_out);    
    REPORT(lambda_t1_out);
    REPORT(infections_t1_out);
    REPORT(anc_clients_t1_out);
    REPORT(anc_plhiv_t1_out);
    REPORT(anc_already_art_t1_out);
    REPORT(anc_art_new_t1_out);
    REPORT(anc_known_pos_t1_out);
    REPORT(anc_tested_pos_t1_out);
    REPORT(anc_tested_neg_t1_out);
    REPORT(anc_rho_t1_out);
    REPORT(anc_alpha_t1_out);

    REPORT(population_t2_out);
    REPORT(rho_t2_out);
    REPORT(plhiv_t2_out);
    REPORT(alpha_t2_out);
    REPORT(artnum_t2_out);
    REPORT(artattend_t2_out);
    REPORT(artattend_ij_t2_out);
    REPORT(untreated_plhiv_num_t2_out);
    REPORT(plhiv_attend_t2_out);
    REPORT(untreated_plhiv_attend_t2_out);
    REPORT(artnum_public_t2_out);
    REPORT(artnum_medaid_t2_out);
    REPORT(artnum_cashpay_t2_out);
    REPORT(artnum_foreign_t2_out);
    REPORT(artprop_public_t2_out);
    REPORT(artprop_medaid_t2_out);
    REPORT(artprop_cashpay_t2_out);
    REPORT(artprop_foreign_t2_out);
    REPORT(artattend_public_t2_out);
    REPORT(artattend_medaid_t2_out);
    REPORT(artattend_cashpay_t2_out);
    REPORT(artattend_foreign_t2_out);
    REPORT(artattendprop_public_t2_out);
    REPORT(artattendprop_medaid_t2_out);
    REPORT(artattendprop_cashpay_t2_out);
    REPORT(artattendprop_foreign_t2_out);
    REPORT(aware_plhiv_prop_t2_out);
    REPORT(aware_plhiv_num_t2_out);
    REPORT(unaware_plhiv_num_t2_out);
    REPORT(aware_plhiv_attend_t2_out);
    REPORT(unaware_plhiv_attend_t2_out);
    REPORT(plhiv_attend_public_t2_out);
    REPORT(aware_plhiv_attend_public_t2_out);    
    REPORT(lambda_t2_out);
    REPORT(infections_t2_out);
    REPORT(anc_clients_t2_out);
    REPORT(anc_plhiv_t2_out);
    REPORT(anc_already_art_t2_out);
    REPORT(anc_art_new_t2_out);
    REPORT(anc_known_pos_t2_out);
    REPORT(anc_tested_pos_t2_out);
    REPORT(anc_tested_neg_t2_out);
    REPORT(anc_rho_t2_out);
    REPORT(anc_alpha_t2_out);


    // Projection to time 3

    vector<Type> population_t3_out(A_out * population_t3);
    vector<Type> plhiv_t3_out(A_out * plhiv_t3);
    vector<Type> rho_t3_out(plhiv_t3_out / population_t3_out);
    vector<Type> artnum_t3_out(A_out * artnum_t3);
    vector<Type> alpha_t3_out(artnum_t3_out / plhiv_t3_out);
    vector<Type> artattend_t3_out(A_out * (A_artattend_mf * artnum_ij_t3));
    vector<Type> artattend_ij_t3_out(A_art_reside_attend * artnum_ij_t3);
    vector<Type> untreated_plhiv_num_t3_out(plhiv_t3_out - artnum_t3_out);

    vector<Type> artnum_public_t3_out(A_out * vector<Type>(population_t3 * prop_art_public_t3));
    vector<Type> artnum_medaid_t3_out(A_out * vector<Type>(population_t3 * prop_art_medaid_t3));
    vector<Type> artnum_cashpay_t3_out(A_out * vector<Type>(population_t3 * prop_art_cashpay_t3));
    vector<Type> artnum_foreign_t3_out(A_out * vector<Type>(population_t3 * prop_art_foreign_t3));

    vector<Type> artprop_public_t3_out(artnum_public_t3_out / artnum_t3_out);
    vector<Type> artprop_medaid_t3_out(artnum_medaid_t3_out / artnum_t3_out);
    vector<Type> artprop_cashpay_t3_out(artnum_cashpay_t3_out / artnum_t3_out);
    vector<Type> artprop_foreign_t3_out(artnum_foreign_t3_out / artnum_t3_out);

    vector<Type> artattend_public_t3_out(A_out * (A_artattend_mf * artnum_public_ij_t3));
    vector<Type> artattend_medaid_t3_out(A_out * (A_artattend_mf * artnum_medaid_ij_t3));
    vector<Type> artattend_cashpay_t3_out(A_out * (A_artattend_mf * artnum_cashpay_ij_t3));
    vector<Type> artattend_foreign_t3_out(A_out * (A_artattend_mf * artnum_foreign_ij_t3));

    vector<Type> artattendprop_public_t3_out(artattend_public_t3_out / artattend_t3_out);
    vector<Type> artattendprop_medaid_t3_out(artattend_medaid_t3_out / artattend_t3_out);
    vector<Type> artattendprop_cashpay_t3_out(artattend_cashpay_t3_out / artattend_t3_out);
    vector<Type> artattendprop_foreign_t3_out(artattend_foreign_t3_out / artattend_t3_out);


    // Calculate number of PLHIV who attend facility in district i; denominator for artattend
    vector<Type> plhiv_attend_ij_t3((Xart_idx * plhiv_t3) * (Xart_gamma * gamma_art_t3));
    vector<Type> plhiv_attend_t3_out(A_out * (A_artattend_mf * plhiv_attend_ij_t3));
    vector<Type> untreated_plhiv_attend_t3_out(plhiv_attend_t3_out - artattend_t3_out);


    vector<Type> unaware_plhiv_num_t3((plhiv_t3 - artnum_t3) * unaware_untreated_prop_t3);
    vector<Type> unaware_plhiv_num_t3_out(A_out * unaware_plhiv_num_t3);
    vector<Type> aware_plhiv_num_t3_out(plhiv_t3_out - unaware_plhiv_num_t3_out);
    vector<Type> aware_plhiv_prop_t3_out(aware_plhiv_num_t3_out / plhiv_t3_out);

    vector<Type> unaware_plhiv_attend_ij_t3((Xart_idx * unaware_plhiv_num_t3) * (Xart_gamma * gamma_art_t3));  // Note: using same ART attendance as T2
    vector<Type> unaware_plhiv_attend_t3_out(A_out * (A_artattend_mf * unaware_plhiv_attend_ij_t3));
    vector<Type> aware_plhiv_attend_t3_out(plhiv_attend_t3_out - unaware_plhiv_attend_t3_out);

    vector<Type> artnum_nonpublic_t3(artnum_medaid_t3_out + artnum_cashpay_t3_out + artnum_foreign_t3_out);
    vector<Type> plhiv_attend_public_t3_out(plhiv_attend_t3_out - artnum_nonpublic_t3);
    vector<Type> aware_plhiv_attend_public_t3_out(aware_plhiv_attend_t3_out - artnum_nonpublic_t3);


    vector<Type> infections_t3_out(A_out * infections_t3);
    vector<Type> lambda_t3_out(infections_t3_out / (population_t3_out - plhiv_t3_out));

    vector<Type> anc_clients_t3_out(A_anc_out * anc_clients_t3);
    vector<Type> anc_plhiv_t3_out(A_anc_out * anc_plhiv_t3);
    vector<Type> anc_already_art_t3_out(A_anc_out * anc_already_art_t3);
    vector<Type> anc_art_new_t3_out(anc_plhiv_t3_out - anc_already_art_t3_out);
    vector<Type> anc_known_pos_t3_out(anc_already_art_t3_out);
    vector<Type> anc_tested_pos_t3_out(anc_plhiv_t3_out - anc_known_pos_t3_out);
    vector<Type> anc_tested_neg_t3_out(anc_clients_t3_out - anc_plhiv_t3_out);

    vector<Type> anc_rho_t3_out(anc_plhiv_t3_out / anc_clients_t3_out);
    vector<Type> anc_alpha_t3_out(anc_already_art_t3_out / anc_plhiv_t3_out);


    REPORT(population_t3_out);
    REPORT(rho_t3_out);
    REPORT(plhiv_t3_out);
    REPORT(alpha_t3_out);
    REPORT(artnum_t3_out);
    REPORT(artattend_t3_out);
    REPORT(artattend_ij_t3_out);
    REPORT(untreated_plhiv_num_t3_out);
    REPORT(plhiv_attend_t3_out);
    REPORT(untreated_plhiv_attend_t3_out);
    REPORT(artnum_public_t3_out);
    REPORT(artnum_medaid_t3_out);
    REPORT(artnum_cashpay_t3_out);
    REPORT(artnum_foreign_t3_out);
    REPORT(artprop_public_t3_out);
    REPORT(artprop_medaid_t3_out);
    REPORT(artprop_cashpay_t3_out);
    REPORT(artprop_foreign_t3_out);
    REPORT(artattend_public_t3_out);
    REPORT(artattend_medaid_t3_out);
    REPORT(artattend_cashpay_t3_out);
    REPORT(artattend_foreign_t3_out);
    REPORT(artattendprop_public_t3_out);
    REPORT(artattendprop_medaid_t3_out);
    REPORT(artattendprop_cashpay_t3_out);
    REPORT(artattendprop_foreign_t3_out);
    REPORT(aware_plhiv_prop_t3_out);
    REPORT(aware_plhiv_num_t3_out);
    REPORT(unaware_plhiv_num_t3_out);
    REPORT(aware_plhiv_attend_t3_out);
    REPORT(unaware_plhiv_attend_t3_out);
    REPORT(plhiv_attend_public_t3_out);
    REPORT(aware_plhiv_attend_public_t3_out);    
    REPORT(lambda_t3_out);
    REPORT(infections_t3_out);
    REPORT(anc_clients_t3_out);
    REPORT(anc_plhiv_t3_out);
    REPORT(anc_already_art_t3_out);
    REPORT(anc_art_new_t3_out);
    REPORT(anc_known_pos_t3_out);
    REPORT(anc_tested_pos_t3_out);
    REPORT(anc_tested_neg_t3_out);
    REPORT(anc_rho_t3_out);
    REPORT(anc_alpha_t3_out);


    // Projection to time 4
    // Only PLHIV, ART and infections calculated. No ANC or awareness indicators

    DATA_VECTOR(population_t4);
    DATA_SPARSE_MATRIX(Lproj_hivpop_t3t4);
    DATA_SPARSE_MATRIX(Lproj_incid_t3t4);
    DATA_SPARSE_MATRIX(Lproj_paed_t3t4);
    DATA_VECTOR(logit_alpha_t3t4_offset);
    DATA_VECTOR(log_lambda_t4_offset);
    DATA_SPARSE_MATRIX(X_paed_lambda_ratio_t4);

    vector<Type> mu_alpha_t4(mu_alpha_t3 + logit_alpha_t3t4_offset);
    vector<Type> alpha_t4(invlogit(mu_alpha_t4));

    vector<Type> infections_adult_t3t4(lambda_adult_t3 * (population_t3 - plhiv_t3));
    vector<Type> plhiv_t4(Lproj_hivpop_t3t4 * plhiv_t3 + Lproj_incid_t3t4 * infections_adult_t3t4 + Lproj_paed_t3t4 * plhiv_t3);

    vector<Type> rho_t4(plhiv_t4 / population_t4);
    vector<Type> prop_art_t4(rho_t4 * alpha_t4);
    vector<Type> artnum_t4(population_t4 * prop_art_t4);

    vector<Type> plhiv_15to49_t4(X_15to49 * plhiv_t4);
    vector<Type> rho_15to49_t4(plhiv_15to49_t4 / (X_15to49 * population_t4));
    vector<Type> alpha_15to49_t4((X_15to49 * artnum_t4) / plhiv_15to49_t4);

    vector<Type> mu_lambda_t4(X_lambda * beta_lambda + log_lambda_t4_offset +
                              Z_x * vector<Type>(log(rho_15to49_t4) + log(1.0 - omega * alpha_15to49_t4)) +
                              Z_lambda_x * ui_lambda_x);

    vector<Type> lambda_adult_t4(exp(mu_lambda_t4));

    // Add paediatric incidence
    vector<Type> rho_15to49f_t4((X_15to49f * vector<Type>(rho_t4 * population_t4)) / (X_15to49f * population_t4));
    vector<Type> lambda_paed_t4(X_paed_lambda_ratio_t4 * rho_15to49f_t4);
    vector<Type> lambda_t4(lambda_adult_t4 + lambda_paed_t4);

    vector<Type> infections_t4(lambda_t4 * (population_t4 - plhiv_t4));

    // Note: currently assuming same district effects parameters from t3 for t4
    vector<Type> mu_anc_rho_t4(logit(rho_t4) +
                               logit_anc_rho_t4_offset +
                               X_ancrho * vector<Type>(beta_anc_rho + beta_anc_rho_t2 + beta_anc_rho_t3) +
                               Z_ancrho_x * vector<Type>(ui_anc_rho_x + ui_anc_rho_xt + ui_anc_rho_xt2t3));
    vector<Type> anc_rho_t4(invlogit(mu_anc_rho_t4));

    vector<Type> mu_anc_alpha_t4(mu_alpha_t4 +
                                 logit_anc_alpha_t4_offset +
                                 X_ancalpha * vector<Type>(beta_anc_alpha + beta_anc_alpha_t2 + beta_anc_alpha_t3) +
                                 Z_ancalpha_x * vector<Type>(ui_anc_alpha_x + ui_anc_alpha_xt + ui_anc_alpha_xt2t3));
    vector<Type> anc_alpha_t4(invlogit(mu_anc_alpha_t4));

    vector<Type> anc_clients_t4(population_t4 * exp(log_asfr_t4_offset + mu_asfr));
    vector<Type> anc_plhiv_t4(anc_clients_t4 * anc_rho_t4);
    vector<Type> anc_already_art_t4(anc_plhiv_t4 * anc_alpha_t4);


    vector<Type> gamma_art_t4(gamma_art_t3);
    
    vector<Type> immigrant_theta_t4(recent_migrant_hivprev_15to49 / (Z_x * rho_15to49_t4));
    vector<Type> foreign_art_prop_t4(foreign_art_eta_scalar * foreign_art_eta_t4 * immigrant_theta_t4 * recent_immigration_prop);
    vector<Type> prop_art_domestic_t4(prop_art_t4 * (1.0 - foreign_art_prop_t4));


    vector<Type> mu_art_medaid_t4(mu_art_medaid_t3);
    vector<Type> mu_art_cashpay_t4(mu_art_cashpay_t3);


    vector<Type> exp_mu_art_medaid_t4(exp(mu_art_medaid_t4));
    vector<Type> exp_mu_art_cashpay_t4(exp(mu_art_cashpay_t4));

    vector<Type> p_art_public_t4(1.0 / (1.0 + exp_mu_art_medaid_t4 + exp_mu_art_cashpay_t4));
    vector<Type> p_art_medaid_t4(exp_mu_art_medaid_t4 / (1.0 + exp_mu_art_medaid_t4 + exp_mu_art_cashpay_t4));
    vector<Type> p_art_cashpay_t4(exp_mu_art_cashpay_t4 / (1.0 + exp_mu_art_medaid_t4 + exp_mu_art_cashpay_t4));

    vector<Type> prop_art_public_t4(prop_art_domestic_t4 * p_art_public_t4);
    vector<Type> prop_art_medaid_t4(prop_art_domestic_t4 * p_art_medaid_t4);
    vector<Type> prop_art_cashpay_t4(prop_art_domestic_t4 * p_art_cashpay_t4);
    vector<Type> prop_art_foreign_t4(prop_art_t4 - prop_art_domestic_t4);
    vector<Type> prop_art_public_ij_t4((Xart_idx * prop_art_public_t4) * (Xart_gamma * gamma_art_t4));
    vector<Type> prop_art_medaid_ij_t4((Xart_idx * prop_art_medaid_t4) * (Xart_gamma * gamma_art_t4));
    vector<Type> prop_art_cashpay_ij_t4((Xart_idx * prop_art_cashpay_t4) * (Xart_gamma * gamma_art_t4));
    vector<Type> prop_art_foreign_ij_t4((Xart_idx * prop_art_foreign_t4) * (Xart_gamma * gamma_art_t4));
    vector<Type> population_ij_t4(Xart_idx * population_t4);

    vector<Type> artnum_public_ij_t4(population_ij_t4 * prop_art_public_ij_t4);
    vector<Type> artnum_medaid_ij_t4(population_ij_t4 * prop_art_medaid_ij_t4);
    vector<Type> artnum_cashpay_ij_t4(population_ij_t4 * prop_art_cashpay_ij_t4);
    vector<Type> artnum_foreign_ij_t4(population_ij_t4 * prop_art_foreign_ij_t4);

    vector<Type> artnum_ij_t4(artnum_public_ij_t4 + artnum_medaid_ij_t4 + artnum_cashpay_ij_t4 + artnum_foreign_ij_t4);


    vector<Type> population_t4_out(A_out * population_t4);
    vector<Type> plhiv_t4_out(A_out * plhiv_t4);
    vector<Type> rho_t4_out(plhiv_t4_out / population_t4_out);
    vector<Type> artnum_t4_out(A_out * artnum_t4);
    vector<Type> alpha_t4_out(artnum_t4_out / plhiv_t4_out);
    vector<Type> artattend_t4_out(A_out * (A_artattend_mf * artnum_ij_t4));
    vector<Type> artattend_ij_t4_out(A_art_reside_attend * artnum_ij_t4);
    vector<Type> untreated_plhiv_num_t4_out(plhiv_t4_out - artnum_t4_out);

    vector<Type> artnum_public_t4_out(A_out * vector<Type>(population_t4 * prop_art_public_t4));
    vector<Type> artnum_medaid_t4_out(A_out * vector<Type>(population_t4 * prop_art_medaid_t4));
    vector<Type> artnum_cashpay_t4_out(A_out * vector<Type>(population_t4 * prop_art_cashpay_t4));
    vector<Type> artnum_foreign_t4_out(A_out * vector<Type>(population_t4 * prop_art_foreign_t4));

    vector<Type> artprop_public_t4_out(artnum_public_t4_out / artnum_t4_out);
    vector<Type> artprop_medaid_t4_out(artnum_medaid_t4_out / artnum_t4_out);
    vector<Type> artprop_cashpay_t4_out(artnum_cashpay_t4_out / artnum_t4_out);
    vector<Type> artprop_foreign_t4_out(artnum_foreign_t4_out / artnum_t4_out);

    vector<Type> artattend_public_t4_out(A_out * (A_artattend_mf * artnum_public_ij_t4));
    vector<Type> artattend_medaid_t4_out(A_out * (A_artattend_mf * artnum_medaid_ij_t4));
    vector<Type> artattend_cashpay_t4_out(A_out * (A_artattend_mf * artnum_cashpay_ij_t4));
    vector<Type> artattend_foreign_t4_out(A_out * (A_artattend_mf * artnum_foreign_ij_t4));

    vector<Type> artattendprop_public_t4_out(artattend_public_t4_out / artattend_t4_out);
    vector<Type> artattendprop_medaid_t4_out(artattend_medaid_t4_out / artattend_t4_out);
    vector<Type> artattendprop_cashpay_t4_out(artattend_cashpay_t4_out / artattend_t4_out);
    vector<Type> artattendprop_foreign_t4_out(artattend_foreign_t4_out / artattend_t4_out);

    // Calculate number of PLHIV who attend facility in district i; denominator for artattend
    vector<Type> plhiv_attend_ij_t4((Xart_idx * plhiv_t4) * (Xart_gamma * gamma_art_t3)); // Note: using same ART attendance as T3
    vector<Type> plhiv_attend_t4_out(A_out * (A_artattend_mf * plhiv_attend_ij_t4));
    vector<Type> untreated_plhiv_attend_t4_out(plhiv_attend_t4_out - artattend_t4_out);

    vector<Type> unaware_plhiv_num_t4((plhiv_t4 - artnum_t4) * unaware_untreated_prop_t4);
    vector<Type> unaware_plhiv_num_t4_out(A_out * unaware_plhiv_num_t4);
    vector<Type> aware_plhiv_num_t4_out(plhiv_t4_out - unaware_plhiv_num_t4_out);
    vector<Type> aware_plhiv_prop_t4_out(aware_plhiv_num_t4_out / plhiv_t4_out);

    vector<Type> unaware_plhiv_attend_ij_t4((Xart_idx * unaware_plhiv_num_t4) * (Xart_gamma * gamma_art_t3));  // Note: using same ART attendance as T3
    vector<Type> unaware_plhiv_attend_t4_out(A_out * (A_artattend_mf * unaware_plhiv_attend_ij_t4));
    vector<Type> aware_plhiv_attend_t4_out(plhiv_attend_t4_out - unaware_plhiv_attend_t4_out);

    vector<Type> artnum_nonpublic_t4(artnum_medaid_t4_out + artnum_cashpay_t4_out + artnum_foreign_t4_out);
    vector<Type> plhiv_attend_public_t4_out(plhiv_attend_t4_out - artnum_nonpublic_t4);
    vector<Type> aware_plhiv_attend_public_t4_out(aware_plhiv_attend_t4_out - artnum_nonpublic_t4);

    
    vector<Type> infections_t4_out(A_out * infections_t4);
    vector<Type> lambda_t4_out(infections_t4_out / (population_t4_out - plhiv_t4_out));

    vector<Type> anc_clients_t4_out(A_anc_out * anc_clients_t4);
    vector<Type> anc_plhiv_t4_out(A_anc_out * anc_plhiv_t4);
    vector<Type> anc_already_art_t4_out(A_anc_out * anc_already_art_t4);
    vector<Type> anc_art_new_t4_out(anc_plhiv_t4_out - anc_already_art_t4_out);
    vector<Type> anc_known_pos_t4_out(anc_already_art_t4_out);
    vector<Type> anc_tested_pos_t4_out(anc_plhiv_t4_out - anc_known_pos_t4_out);
    vector<Type> anc_tested_neg_t4_out(anc_clients_t4_out - anc_plhiv_t4_out);

    vector<Type> anc_rho_t4_out(anc_plhiv_t4_out / anc_clients_t4_out);
    vector<Type> anc_alpha_t4_out(anc_already_art_t4_out / anc_plhiv_t4_out);

    REPORT(population_t4_out);
    REPORT(rho_t4_out);
    REPORT(plhiv_t4_out);
    REPORT(alpha_t4_out);
    REPORT(artnum_t4_out);
    REPORT(artattend_t4_out);
    REPORT(artattend_ij_t4_out);
    REPORT(untreated_plhiv_num_t4_out);
    REPORT(plhiv_attend_t4_out);
    REPORT(untreated_plhiv_attend_t4_out);
    REPORT(artnum_public_t4_out);
    REPORT(artnum_medaid_t4_out);
    REPORT(artnum_cashpay_t4_out);
    REPORT(artnum_foreign_t4_out);
    REPORT(artprop_public_t4_out);
    REPORT(artprop_medaid_t4_out);
    REPORT(artprop_cashpay_t4_out);
    REPORT(artprop_foreign_t4_out);
    REPORT(artattend_public_t4_out);
    REPORT(artattend_medaid_t4_out);
    REPORT(artattend_cashpay_t4_out);
    REPORT(artattend_foreign_t4_out);
    REPORT(artattendprop_public_t4_out);
    REPORT(artattendprop_medaid_t4_out);
    REPORT(artattendprop_cashpay_t4_out);
    REPORT(artattendprop_foreign_t4_out);
    REPORT(aware_plhiv_prop_t4_out);
    REPORT(aware_plhiv_num_t4_out);
    REPORT(unaware_plhiv_num_t4_out);
    REPORT(aware_plhiv_attend_t4_out);
    REPORT(unaware_plhiv_attend_t4_out);
    REPORT(plhiv_attend_public_t4_out);
    REPORT(aware_plhiv_attend_public_t4_out);
    REPORT(lambda_t4_out);
    REPORT(infections_t4_out);
    REPORT(anc_clients_t4_out);
    REPORT(anc_plhiv_t4_out);
    REPORT(anc_already_art_t4_out);
    REPORT(anc_art_new_t4_out);
    REPORT(anc_known_pos_t4_out);
    REPORT(anc_tested_pos_t4_out);
    REPORT(anc_tested_neg_t4_out);
    REPORT(anc_rho_t4_out);
    REPORT(anc_alpha_t4_out);

    REPORT(anc_clients_t4_out);
    REPORT(anc_plhiv_t4_out);
    REPORT(anc_already_art_t4_out);
    REPORT(anc_art_new_t4_out);
    REPORT(anc_known_pos_t4_out);
    REPORT(anc_tested_pos_t4_out);
    REPORT(anc_tested_neg_t4_out);
    REPORT(anc_rho_t4_out);
    REPORT(anc_alpha_t4_out);


    // Projection to time 5
    // Only PLHIV, ART and infections calculated. No ANC or awareness indicators

    DATA_VECTOR(population_t5);
    DATA_SPARSE_MATRIX(Lproj_hivpop_t4t5);
    DATA_SPARSE_MATRIX(Lproj_incid_t4t5);
    DATA_SPARSE_MATRIX(Lproj_paed_t4t5);
    DATA_VECTOR(logit_alpha_t4t5_offset);
    DATA_VECTOR(log_lambda_t5_offset);
    DATA_SPARSE_MATRIX(X_paed_lambda_ratio_t5);

    vector<Type> mu_alpha_t5(mu_alpha_t4 + logit_alpha_t4t5_offset);
    vector<Type> alpha_t5(invlogit(mu_alpha_t5));

    vector<Type> infections_adult_t4t5(lambda_adult_t4 * (population_t4 - plhiv_t4));
    vector<Type> plhiv_t5(Lproj_hivpop_t4t5 * plhiv_t4 + Lproj_incid_t4t5 * infections_adult_t4t5 + Lproj_paed_t4t5 * plhiv_t4);

    vector<Type> rho_t5(plhiv_t5 / population_t5);
    vector<Type> prop_art_t5(rho_t5 * alpha_t5);
    vector<Type> artnum_t5(population_t5 * prop_art_t5);

    vector<Type> plhiv_15to49_t5(X_15to49 * plhiv_t5);
    vector<Type> rho_15to49_t5(plhiv_15to49_t5 / (X_15to49 * population_t5));
    vector<Type> alpha_15to49_t5((X_15to49 * artnum_t5) / plhiv_15to49_t5);

    vector<Type> mu_lambda_t5(X_lambda * beta_lambda + log_lambda_t5_offset +
                              Z_x * vector<Type>(log(rho_15to49_t5) + log(1.0 - omega * alpha_15to49_t5)) +
                              Z_lambda_x * ui_lambda_x);

    vector<Type> lambda_adult_t5(exp(mu_lambda_t5));

    // Add paediatric incidence
    vector<Type> rho_15to49f_t5((X_15to49f * vector<Type>(rho_t5 * population_t5)) / (X_15to49f * population_t5));
    vector<Type> lambda_paed_t5(X_paed_lambda_ratio_t5 * rho_15to49f_t5);
    vector<Type> lambda_t5(lambda_adult_t5 + lambda_paed_t5);

    vector<Type> infections_t5(lambda_t5 * (population_t5 - plhiv_t5));

    vector<Type> gamma_art_t5(gamma_art_t4);
	
    vector<Type> immigrant_theta_t5(recent_migrant_hivprev_15to49 / (Z_x * rho_15to49_t5));
    vector<Type> foreign_art_prop_t5(foreign_art_eta_scalar * foreign_art_eta_t5 * immigrant_theta_t5 * recent_immigration_prop);
    vector<Type> prop_art_domestic_t5(prop_art_t5 * (1.0 - foreign_art_prop_t5));


    vector<Type> mu_art_medaid_t5(mu_art_medaid_t4);
    vector<Type> mu_art_cashpay_t5(mu_art_cashpay_t4);


    vector<Type> exp_mu_art_medaid_t5(exp(mu_art_medaid_t5));
    vector<Type> exp_mu_art_cashpay_t5(exp(mu_art_cashpay_t5));

    vector<Type> p_art_public_t5(1.0 / (1.0 + exp_mu_art_medaid_t5 + exp_mu_art_cashpay_t5));
    vector<Type> p_art_medaid_t5(exp_mu_art_medaid_t5 / (1.0 + exp_mu_art_medaid_t5 + exp_mu_art_cashpay_t5));
    vector<Type> p_art_cashpay_t5(exp_mu_art_cashpay_t5 / (1.0 + exp_mu_art_medaid_t5 + exp_mu_art_cashpay_t5));

    vector<Type> prop_art_public_t5(prop_art_domestic_t5 * p_art_public_t5);
    vector<Type> prop_art_medaid_t5(prop_art_domestic_t5 * p_art_medaid_t5);
    vector<Type> prop_art_cashpay_t5(prop_art_domestic_t5 * p_art_cashpay_t5);
    vector<Type> prop_art_foreign_t5(prop_art_t5 - prop_art_domestic_t5);
    vector<Type> prop_art_public_ij_t5((Xart_idx * prop_art_public_t5) * (Xart_gamma * gamma_art_t5));
    vector<Type> prop_art_medaid_ij_t5((Xart_idx * prop_art_medaid_t5) * (Xart_gamma * gamma_art_t5));
    vector<Type> prop_art_cashpay_ij_t5((Xart_idx * prop_art_cashpay_t5) * (Xart_gamma * gamma_art_t5));
    vector<Type> prop_art_foreign_ij_t5((Xart_idx * prop_art_foreign_t5) * (Xart_gamma * gamma_art_t5));
    vector<Type> population_ij_t5(Xart_idx * population_t5);

    vector<Type> artnum_public_ij_t5(population_ij_t5 * prop_art_public_ij_t5);
    vector<Type> artnum_medaid_ij_t5(population_ij_t5 * prop_art_medaid_ij_t5);
    vector<Type> artnum_cashpay_ij_t5(population_ij_t5 * prop_art_cashpay_ij_t5);
    vector<Type> artnum_foreign_ij_t5(population_ij_t5 * prop_art_foreign_ij_t5);

    vector<Type> artnum_ij_t5(artnum_public_ij_t5 + artnum_medaid_ij_t5 + artnum_cashpay_ij_t5 + artnum_foreign_ij_t5);


    vector<Type> population_t5_out(A_out * population_t5);
    vector<Type> plhiv_t5_out(A_out * plhiv_t5);
    vector<Type> rho_t5_out(plhiv_t5_out / population_t5_out);
    vector<Type> artnum_t5_out(A_out * artnum_t5);
    vector<Type> alpha_t5_out(artnum_t5_out / plhiv_t5_out);
    vector<Type> artattend_t5_out(A_out * (A_artattend_mf * artnum_ij_t5));
    vector<Type> artattend_ij_t5_out(A_art_reside_attend * artnum_ij_t5);
    vector<Type> untreated_plhiv_num_t5_out(plhiv_t5_out - artnum_t5_out);

    vector<Type> artnum_public_t5_out(A_out * vector<Type>(population_t5 * prop_art_public_t5));
    vector<Type> artnum_medaid_t5_out(A_out * vector<Type>(population_t5 * prop_art_medaid_t5));
    vector<Type> artnum_cashpay_t5_out(A_out * vector<Type>(population_t5 * prop_art_cashpay_t5));
    vector<Type> artnum_foreign_t5_out(A_out * vector<Type>(population_t5 * prop_art_foreign_t5));

    vector<Type> artprop_public_t5_out(artnum_public_t5_out / artnum_t5_out);
    vector<Type> artprop_medaid_t5_out(artnum_medaid_t5_out / artnum_t5_out);
    vector<Type> artprop_cashpay_t5_out(artnum_cashpay_t5_out / artnum_t5_out);
    vector<Type> artprop_foreign_t5_out(artnum_foreign_t5_out / artnum_t5_out);

    vector<Type> artattend_public_t5_out(A_out * (A_artattend_mf * artnum_public_ij_t5));
    vector<Type> artattend_medaid_t5_out(A_out * (A_artattend_mf * artnum_medaid_ij_t5));
    vector<Type> artattend_cashpay_t5_out(A_out * (A_artattend_mf * artnum_cashpay_ij_t5));
    vector<Type> artattend_foreign_t5_out(A_out * (A_artattend_mf * artnum_foreign_ij_t5));

    vector<Type> artattendprop_public_t5_out(artattend_public_t5_out / artattend_t5_out);
    vector<Type> artattendprop_medaid_t5_out(artattend_medaid_t5_out / artattend_t5_out);
    vector<Type> artattendprop_cashpay_t5_out(artattend_cashpay_t5_out / artattend_t5_out);
    vector<Type> artattendprop_foreign_t5_out(artattend_foreign_t5_out / artattend_t5_out);

    // Note: currently assuming same district effects parameters from t3 for t5
    vector<Type> mu_anc_rho_t5(logit(rho_t5) +
                               logit_anc_rho_t5_offset +
                               X_ancrho * vector<Type>(beta_anc_rho + beta_anc_rho_t2 + beta_anc_rho_t3) +
                               Z_ancrho_x * vector<Type>(ui_anc_rho_x + ui_anc_rho_xt + ui_anc_rho_xt2t3));
    vector<Type> anc_rho_t5(invlogit(mu_anc_rho_t5));

    vector<Type> mu_anc_alpha_t5(mu_alpha_t5 +
                                 logit_anc_alpha_t5_offset +
                                 X_ancalpha * vector<Type>(beta_anc_alpha + beta_anc_alpha_t2 + beta_anc_alpha_t3) +
                                 Z_ancalpha_x * vector<Type>(ui_anc_alpha_x + ui_anc_alpha_xt + ui_anc_alpha_xt2t3));
    vector<Type> anc_alpha_t5(invlogit(mu_anc_alpha_t5));

    vector<Type> anc_clients_t5(population_t5 * exp(log_asfr_t5_offset + mu_asfr));
    vector<Type> anc_plhiv_t5(anc_clients_t5 * anc_rho_t5);
    vector<Type> anc_already_art_t5(anc_plhiv_t5 * anc_alpha_t5);

    // Calculate number of PLHIV who attend facility in district i; denominator for artattend
    vector<Type> plhiv_attend_ij_t5((Xart_idx * plhiv_t5) * (Xart_gamma * gamma_art_t3)); // Note: using same ART attendance as T3
    vector<Type> plhiv_attend_t5_out(A_out * (A_artattend_mf * plhiv_attend_ij_t5));
    vector<Type> untreated_plhiv_attend_t5_out(plhiv_attend_t5_out - artattend_t5_out);

    vector<Type> unaware_plhiv_num_t5((plhiv_t5 - artnum_t5) * unaware_untreated_prop_t5);
    vector<Type> unaware_plhiv_num_t5_out(A_out * unaware_plhiv_num_t5);
    vector<Type> aware_plhiv_num_t5_out(plhiv_t5_out - unaware_plhiv_num_t5_out);
    vector<Type> aware_plhiv_prop_t5_out(aware_plhiv_num_t5_out / plhiv_t5_out);

    vector<Type> unaware_plhiv_attend_ij_t5((Xart_idx * unaware_plhiv_num_t5) * (Xart_gamma * gamma_art_t3));  // Note: using same ART attendance as T3
    vector<Type> unaware_plhiv_attend_t5_out(A_out * (A_artattend_mf * unaware_plhiv_attend_ij_t5));
    vector<Type> aware_plhiv_attend_t5_out(plhiv_attend_t5_out - unaware_plhiv_attend_t5_out);

    vector<Type> artnum_nonpublic_t5(artnum_medaid_t5_out + artnum_cashpay_t5_out + artnum_foreign_t5_out);
    vector<Type> plhiv_attend_public_t5_out(plhiv_attend_t5_out - artnum_nonpublic_t5);
    vector<Type> aware_plhiv_attend_public_t5_out(aware_plhiv_attend_t5_out - artnum_nonpublic_t5);
    

    vector<Type> infections_t5_out(A_out * infections_t5);
    vector<Type> lambda_t5_out(infections_t5_out / (population_t5_out - plhiv_t5_out));

    vector<Type> anc_clients_t5_out(A_anc_out * anc_clients_t5);
    vector<Type> anc_plhiv_t5_out(A_anc_out * anc_plhiv_t5);
    vector<Type> anc_already_art_t5_out(A_anc_out * anc_already_art_t5);
    vector<Type> anc_art_new_t5_out(anc_plhiv_t5_out - anc_already_art_t5_out);
    vector<Type> anc_known_pos_t5_out(anc_already_art_t5_out);
    vector<Type> anc_tested_pos_t5_out(anc_plhiv_t5_out - anc_known_pos_t5_out);
    vector<Type> anc_tested_neg_t5_out(anc_clients_t5_out - anc_plhiv_t5_out);

    vector<Type> anc_rho_t5_out(anc_plhiv_t5_out / anc_clients_t5_out);
    vector<Type> anc_alpha_t5_out(anc_already_art_t5_out / anc_plhiv_t5_out);

    REPORT(population_t5_out);
    REPORT(rho_t5_out);
    REPORT(plhiv_t5_out);
    REPORT(alpha_t5_out);
    REPORT(artnum_t5_out);
    REPORT(artattend_t5_out);
    REPORT(artattend_ij_t5_out);
    REPORT(untreated_plhiv_num_t5_out);
    REPORT(plhiv_attend_t5_out);
    REPORT(untreated_plhiv_attend_t5_out);
    REPORT(artnum_public_t5_out);
    REPORT(artnum_medaid_t5_out);
    REPORT(artnum_cashpay_t5_out);
    REPORT(artnum_foreign_t5_out);
    REPORT(artprop_public_t5_out);
    REPORT(artprop_medaid_t5_out);
    REPORT(artprop_cashpay_t5_out);
    REPORT(artprop_foreign_t5_out);
    REPORT(artattend_public_t5_out);
    REPORT(artattend_medaid_t5_out);
    REPORT(artattend_cashpay_t5_out);
    REPORT(artattend_foreign_t5_out);
    REPORT(artattendprop_public_t5_out);
    REPORT(artattendprop_medaid_t5_out);
    REPORT(artattendprop_cashpay_t5_out);
    REPORT(artattendprop_foreign_t5_out);
    REPORT(aware_plhiv_prop_t5_out);
    REPORT(aware_plhiv_num_t5_out);
    REPORT(unaware_plhiv_num_t5_out);
    REPORT(aware_plhiv_attend_t5_out);
    REPORT(unaware_plhiv_attend_t5_out);
    REPORT(plhiv_attend_public_t5_out);
    REPORT(aware_plhiv_attend_public_t5_out);
    REPORT(lambda_t5_out);
    REPORT(infections_t5_out);
    REPORT(anc_clients_t5_out);
    REPORT(anc_plhiv_t5_out);
    REPORT(anc_already_art_t5_out);
    REPORT(anc_art_new_t5_out);
    REPORT(anc_known_pos_t5_out);
    REPORT(anc_tested_pos_t5_out);
    REPORT(anc_tested_neg_t5_out);
    REPORT(anc_rho_t5_out);
    REPORT(anc_alpha_t5_out);


    // Projection to time 6
    // Only PLHIV, ART and infections calculated. No ANC or awareness indicators

    DATA_VECTOR(population_t6);
    DATA_SPARSE_MATRIX(Lproj_hivpop_t5t6);
    DATA_SPARSE_MATRIX(Lproj_incid_t5t6);
    DATA_SPARSE_MATRIX(Lproj_paed_t5t6);
    DATA_VECTOR(logit_alpha_t5t6_offset);
    DATA_VECTOR(log_lambda_t6_offset);
    DATA_SPARSE_MATRIX(X_paed_lambda_ratio_t6);

    vector<Type> mu_alpha_t6(mu_alpha_t5 + logit_alpha_t5t6_offset);
    vector<Type> alpha_t6(invlogit(mu_alpha_t6));

    vector<Type> infections_adult_t5t6(lambda_adult_t5 * (population_t5 - plhiv_t5));
    vector<Type> plhiv_t6(Lproj_hivpop_t5t6 * plhiv_t5 + Lproj_incid_t5t6 * infections_adult_t5t6 + Lproj_paed_t5t6 * plhiv_t5);

    vector<Type> rho_t6(plhiv_t6 / population_t6);
    vector<Type> prop_art_t6(rho_t6 * alpha_t6);
    vector<Type> artnum_t6(population_t6 * prop_art_t6);

    vector<Type> plhiv_15to49_t6(X_15to49 * plhiv_t6);
    vector<Type> rho_15to49_t6(plhiv_15to49_t6 / (X_15to49 * population_t6));
    vector<Type> alpha_15to49_t6((X_15to49 * artnum_t6) / plhiv_15to49_t6);

    vector<Type> mu_lambda_t6(X_lambda * beta_lambda + log_lambda_t6_offset +
                              Z_x * vector<Type>(log(rho_15to49_t6) + log(1.0 - omega * alpha_15to49_t6)) +
                              Z_lambda_x * ui_lambda_x);

    vector<Type> lambda_adult_t6(exp(mu_lambda_t6));

    // Add paediatric incidence
    vector<Type> rho_15to49f_t6((X_15to49f * vector<Type>(rho_t6 * population_t6)) / (X_15to49f * population_t6));

    vector<Type> lambda_paed_t6(X_paed_lambda_ratio_t6 * rho_15to49f_t6);
    vector<Type> lambda_t6(lambda_adult_t6 + lambda_paed_t6);

    vector<Type> infections_t6(lambda_t6 * (population_t6 - plhiv_t6));

    vector<Type> gamma_art_t6(gamma_art_t5);
    
    vector<Type> immigrant_theta_t6(recent_migrant_hivprev_15to49 / (Z_x * rho_15to49_t6));
    vector<Type> foreign_art_prop_t6(foreign_art_eta_scalar * foreign_art_eta_t6 * immigrant_theta_t6 * recent_immigration_prop);
    vector<Type> prop_art_domestic_t6(prop_art_t6 * (1.0 - foreign_art_prop_t6));


    vector<Type> mu_art_medaid_t6(mu_art_medaid_t5);
    vector<Type> mu_art_cashpay_t6(mu_art_cashpay_t5);


    vector<Type> exp_mu_art_medaid_t6(exp(mu_art_medaid_t6));
    vector<Type> exp_mu_art_cashpay_t6(exp(mu_art_cashpay_t6));

    vector<Type> p_art_public_t6(1.0 / (1.0 + exp_mu_art_medaid_t6 + exp_mu_art_cashpay_t6));
    vector<Type> p_art_medaid_t6(exp_mu_art_medaid_t6 / (1.0 + exp_mu_art_medaid_t6 + exp_mu_art_cashpay_t6));
    vector<Type> p_art_cashpay_t6(exp_mu_art_cashpay_t6 / (1.0 + exp_mu_art_medaid_t6 + exp_mu_art_cashpay_t6));

    vector<Type> prop_art_public_t6(prop_art_domestic_t6 * p_art_public_t6);
    vector<Type> prop_art_medaid_t6(prop_art_domestic_t6 * p_art_medaid_t6);
    vector<Type> prop_art_cashpay_t6(prop_art_domestic_t6 * p_art_cashpay_t6);
    vector<Type> prop_art_foreign_t6(prop_art_t6 - prop_art_domestic_t6);
    vector<Type> prop_art_public_ij_t6((Xart_idx * prop_art_public_t6) * (Xart_gamma * gamma_art_t6));
    vector<Type> prop_art_medaid_ij_t6((Xart_idx * prop_art_medaid_t6) * (Xart_gamma * gamma_art_t6));
    vector<Type> prop_art_cashpay_ij_t6((Xart_idx * prop_art_cashpay_t6) * (Xart_gamma * gamma_art_t6));
    vector<Type> prop_art_foreign_ij_t6((Xart_idx * prop_art_foreign_t6) * (Xart_gamma * gamma_art_t6));
    vector<Type> population_ij_t6(Xart_idx * population_t6);

    vector<Type> artnum_public_ij_t6(population_ij_t6 * prop_art_public_ij_t6);
    vector<Type> artnum_medaid_ij_t6(population_ij_t6 * prop_art_medaid_ij_t6);
    vector<Type> artnum_cashpay_ij_t6(population_ij_t6 * prop_art_cashpay_ij_t6);
    vector<Type> artnum_foreign_ij_t6(population_ij_t6 * prop_art_foreign_ij_t6);

    vector<Type> artnum_ij_t6(artnum_public_ij_t6 + artnum_medaid_ij_t6 + artnum_cashpay_ij_t6 + artnum_foreign_ij_t6);

    vector<Type> population_t6_out(A_out * population_t6);
    vector<Type> plhiv_t6_out(A_out * plhiv_t6);
    vector<Type> rho_t6_out(plhiv_t6_out / population_t6_out);
    vector<Type> artnum_t6_out(A_out * artnum_t6);
    vector<Type> alpha_t6_out(artnum_t6_out / plhiv_t6_out);
    vector<Type> artattend_t6_out(A_out * (A_artattend_mf * artnum_ij_t6));
    vector<Type> artattend_ij_t6_out(A_art_reside_attend * artnum_ij_t6);
    vector<Type> untreated_plhiv_num_t6_out(plhiv_t6_out - artnum_t6_out);

    vector<Type> artnum_public_t6_out(A_out * vector<Type>(population_t6 * prop_art_public_t6));
    vector<Type> artnum_medaid_t6_out(A_out * vector<Type>(population_t6 * prop_art_medaid_t6));
    vector<Type> artnum_cashpay_t6_out(A_out * vector<Type>(population_t6 * prop_art_cashpay_t6));
    vector<Type> artnum_foreign_t6_out(A_out * vector<Type>(population_t6 * prop_art_foreign_t6));

    vector<Type> artprop_public_t6_out(artnum_public_t6_out / artnum_t6_out);
    vector<Type> artprop_medaid_t6_out(artnum_medaid_t6_out / artnum_t6_out);
    vector<Type> artprop_cashpay_t6_out(artnum_cashpay_t6_out / artnum_t6_out);
    vector<Type> artprop_foreign_t6_out(artnum_foreign_t6_out / artnum_t6_out);

    vector<Type> artattend_public_t6_out(A_out * (A_artattend_mf * artnum_public_ij_t6));
    vector<Type> artattend_medaid_t6_out(A_out * (A_artattend_mf * artnum_medaid_ij_t6));
    vector<Type> artattend_cashpay_t6_out(A_out * (A_artattend_mf * artnum_cashpay_ij_t6));
    vector<Type> artattend_foreign_t6_out(A_out * (A_artattend_mf * artnum_foreign_ij_t6));

    vector<Type> artattendprop_public_t6_out(artattend_public_t6_out / artattend_t6_out);
    vector<Type> artattendprop_medaid_t6_out(artattend_medaid_t6_out / artattend_t6_out);
    vector<Type> artattendprop_cashpay_t6_out(artattend_cashpay_t6_out / artattend_t6_out);
    vector<Type> artattendprop_foreign_t6_out(artattend_foreign_t6_out / artattend_t6_out);

    // Note: currently assuming same district effects parameters from t3 for t6
    vector<Type> mu_anc_rho_t6(logit(rho_t6) +
                               logit_anc_rho_t6_offset +
                               X_ancrho * vector<Type>(beta_anc_rho + beta_anc_rho_t2 + beta_anc_rho_t3) +
                               Z_ancrho_x * vector<Type>(ui_anc_rho_x + ui_anc_rho_xt + ui_anc_rho_xt2t3));
    vector<Type> anc_rho_t6(invlogit(mu_anc_rho_t6));

    vector<Type> mu_anc_alpha_t6(mu_alpha_t6 +
                                 logit_anc_alpha_t6_offset +
                                 X_ancalpha * vector<Type>(beta_anc_alpha + beta_anc_alpha_t2 + beta_anc_alpha_t3) +
                                 Z_ancalpha_x * vector<Type>(ui_anc_alpha_x + ui_anc_alpha_xt + ui_anc_alpha_xt2t3));
    vector<Type> anc_alpha_t6(invlogit(mu_anc_alpha_t6));

    vector<Type> anc_clients_t6(population_t6 * exp(log_asfr_t6_offset + mu_asfr));
    vector<Type> anc_plhiv_t6(anc_clients_t6 * anc_rho_t6);
    vector<Type> anc_already_art_t6(anc_plhiv_t6 * anc_alpha_t6);

    // Calculate number of PLHIV who attend facility in district i; denominator for artattend
    vector<Type> plhiv_attend_ij_t6((Xart_idx * plhiv_t6) * (Xart_gamma * gamma_art_t3)); // Note: using same ART attendance as T3
    vector<Type> plhiv_attend_t6_out(A_out * (A_artattend_mf * plhiv_attend_ij_t6));
    vector<Type> untreated_plhiv_attend_t6_out(plhiv_attend_t6_out - artattend_t6_out);

    vector<Type> unaware_plhiv_num_t6((plhiv_t6 - artnum_t6) * unaware_untreated_prop_t6);
    vector<Type> unaware_plhiv_num_t6_out(A_out * unaware_plhiv_num_t6);
    vector<Type> aware_plhiv_num_t6_out(plhiv_t6_out - unaware_plhiv_num_t6_out);
    vector<Type> aware_plhiv_prop_t6_out(aware_plhiv_num_t6_out / plhiv_t6_out);

    vector<Type> unaware_plhiv_attend_ij_t6((Xart_idx * unaware_plhiv_num_t6) * (Xart_gamma * gamma_art_t3));  // Note: using same ART attendance as T3
    vector<Type> unaware_plhiv_attend_t6_out(A_out * (A_artattend_mf * unaware_plhiv_attend_ij_t6));
    vector<Type> aware_plhiv_attend_t6_out(plhiv_attend_t6_out - unaware_plhiv_attend_t6_out);

    vector<Type> artnum_nonpublic_t6(artnum_medaid_t6_out + artnum_cashpay_t6_out + artnum_foreign_t6_out);
    vector<Type> plhiv_attend_public_t6_out(plhiv_attend_t6_out - artnum_nonpublic_t6);
    vector<Type> aware_plhiv_attend_public_t6_out(aware_plhiv_attend_t6_out - artnum_nonpublic_t6);

    
    vector<Type> infections_t6_out(A_out * infections_t6);
    vector<Type> lambda_t6_out(infections_t6_out / (population_t6_out - plhiv_t6_out));

    vector<Type> anc_clients_t6_out(A_anc_out * anc_clients_t6);
    vector<Type> anc_plhiv_t6_out(A_anc_out * anc_plhiv_t6);
    vector<Type> anc_already_art_t6_out(A_anc_out * anc_already_art_t6);
    vector<Type> anc_art_new_t6_out(anc_plhiv_t6_out - anc_already_art_t6_out);
    vector<Type> anc_known_pos_t6_out(anc_already_art_t6_out);
    vector<Type> anc_tested_pos_t6_out(anc_plhiv_t6_out - anc_known_pos_t6_out);
    vector<Type> anc_tested_neg_t6_out(anc_clients_t6_out - anc_plhiv_t6_out);

    vector<Type> anc_rho_t6_out(anc_plhiv_t6_out / anc_clients_t6_out);
    vector<Type> anc_alpha_t6_out(anc_already_art_t6_out / anc_plhiv_t6_out);

    REPORT(population_t6_out);
    REPORT(rho_t6_out);
    REPORT(plhiv_t6_out);
    REPORT(alpha_t6_out);
    REPORT(artnum_t6_out);
    REPORT(artattend_t6_out);
    REPORT(artattend_ij_t6_out);
    REPORT(untreated_plhiv_num_t6_out);
    REPORT(plhiv_attend_t6_out);
    REPORT(untreated_plhiv_attend_t6_out);
    REPORT(artnum_public_t6_out);
    REPORT(artnum_medaid_t6_out);
    REPORT(artnum_cashpay_t6_out);
    REPORT(artnum_foreign_t6_out);
    REPORT(artprop_public_t6_out);
    REPORT(artprop_medaid_t6_out);
    REPORT(artprop_cashpay_t6_out);
    REPORT(artprop_foreign_t6_out);
    REPORT(artattend_public_t6_out);
    REPORT(artattend_medaid_t6_out);
    REPORT(artattend_cashpay_t6_out);
    REPORT(artattend_foreign_t6_out);
    REPORT(artattendprop_public_t6_out);
    REPORT(artattendprop_medaid_t6_out);
    REPORT(artattendprop_cashpay_t6_out);
    REPORT(artattendprop_foreign_t6_out);
    REPORT(aware_plhiv_prop_t6_out);
    REPORT(aware_plhiv_num_t6_out);
    REPORT(unaware_plhiv_num_t6_out);
    REPORT(aware_plhiv_attend_t6_out);
    REPORT(unaware_plhiv_attend_t6_out);
    REPORT(plhiv_attend_public_t6_out);
    REPORT(aware_plhiv_attend_public_t6_out);
    REPORT(lambda_t6_out);
    REPORT(infections_t6_out);
    REPORT(anc_clients_t6_out);
    REPORT(anc_plhiv_t6_out);
    REPORT(anc_already_art_t6_out);
    REPORT(anc_art_new_t6_out);
    REPORT(anc_known_pos_t6_out);
    REPORT(anc_tested_pos_t6_out);
    REPORT(anc_tested_neg_t6_out);
    REPORT(anc_rho_t6_out);
    REPORT(anc_alpha_t6_out);
  }

  if(report_likelihood){

    REPORT(hhs_prev_t1_ll);
    REPORT(hhs_artcov_t1_ll);
    REPORT(hhs_vls_t1_ll);
    REPORT(hhs_recent_t1_ll);
    REPORT(hhs_prev_t2_ll);
    REPORT(hhs_artcov_t2_ll);
    REPORT(hhs_vls_t2_ll);
    REPORT(hhs_recent_t2_ll);
    // REPORT(artnum_public_t1_ll);
    // REPORT(artnum_medaid_t1_ll);
    // REPORT(artnum_cashpay_t1_ll);
    // REPORT(artnum_public_t2_ll);
    // REPORT(artnum_medaid_t2_ll);
    // REPORT(artnum_cashpay_t2_ll);
    // REPORT(artnum_public_t3_ll);
    // REPORT(artnum_medaid_t3_ll);
    // REPORT(artnum_cashpay_t3_ll);
    REPORT(anc_rho_obs_t1_ll);
    REPORT(anc_rho_obs_t2_ll);
    REPORT(anc_rho_obs_t3_ll);
    REPORT(anc_alpha_obs_t1_ll);
    REPORT(anc_alpha_obs_t2_ll);
    REPORT(anc_alpha_obs_t3_ll);
    REPORT(anc_clients_obs_t3_ll);

  }

  return val;
}
