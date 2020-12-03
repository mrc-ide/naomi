#define TMB_LIB_INIT R_init_naomi
#include <TMB.hpp>

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
  DATA_SPARSE_MATRIX(Lproj_hivpop);
  DATA_SPARSE_MATRIX(Lproj_incid);
  DATA_SPARSE_MATRIX(Lproj_paed);
  DATA_SCALAR(projection_duration);

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

  DATA_SPARSE_MATRIX(Z_x);
  DATA_SPARSE_MATRIX(Z_lambda_x);

  DATA_VECTOR(logit_rho_offset);
  DATA_VECTOR(logit_alpha_offset);
  DATA_VECTOR(logit_alpha_t1t2_offset);

  DATA_VECTOR(log_asfr_t1_offset);
  DATA_VECTOR(log_asfr_t2_offset);
  DATA_VECTOR(log_asfr_t3_offset);

  DATA_VECTOR(logit_anc_rho_t1_offset);
  DATA_VECTOR(logit_anc_rho_t2_offset);
  DATA_VECTOR(logit_anc_rho_t3_offset);

  DATA_VECTOR(logit_anc_alpha_t1_offset);
  DATA_VECTOR(logit_anc_alpha_t2_offset);
  DATA_VECTOR(logit_anc_alpha_t3_offset);

  DATA_SPARSE_MATRIX(Z_asfr_x);
  DATA_SPARSE_MATRIX(Z_ancrho_x);
  DATA_SPARSE_MATRIX(Z_ancalpha_x);


  // Precision matrix for ICAR area model
  DATA_SPARSE_MATRIX(Q_x);

  DATA_IVECTOR(idx_prev);
  DATA_VECTOR(n_prev);
  DATA_VECTOR(x_prev);

  DATA_IVECTOR(idx_artcov);
  DATA_VECTOR(n_artcov);
  DATA_VECTOR(x_artcov);

  DATA_IVECTOR(idx_vls);
  DATA_VECTOR(n_vls);
  DATA_VECTOR(x_vls);

  DATA_IVECTOR(idx_recent);
  DATA_VECTOR(n_recent);
  DATA_VECTOR(x_recent);

  DATA_VECTOR(x_anc_clients_t2);
  DATA_VECTOR(offset_anc_clients_t2);
  DATA_SPARSE_MATRIX(A_anc_clients_t2);

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

  DATA_SPARSE_MATRIX(A_artattend_t1);
  DATA_VECTOR(x_artnum_t1);

  DATA_SPARSE_MATRIX(A_artattend_t2);
  DATA_VECTOR(x_artnum_t2);

  DATA_SPARSE_MATRIX(A_artattend_mf);
  DATA_SPARSE_MATRIX(A_art_reside_attend);

  DATA_IVECTOR(n_nb);
  DATA_IVECTOR(adj_i);
  DATA_IVECTOR(adj_j);

  DATA_SPARSE_MATRIX(Xgamma);
  DATA_SPARSE_MATRIX(Xgamma_t2);
  DATA_VECTOR(log_gamma_offset);


  DATA_SPARSE_MATRIX(Xart_idx);
  DATA_SPARSE_MATRIX(Xart_gamma);


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

  // Paediatric prevalence ratio model

  DATA_SPARSE_MATRIX(A_15to49f);
  DATA_SPARSE_MATRIX(X_paed_rho_ratio);
  DATA_VECTOR(paed_rho_ratio_offset);

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

  PARAMETER_VECTOR(us_rho_x);
  val -= Type(-0.5) * (us_rho_x * (Q_x * us_rho_x)).sum();
  val -= dnorm(sum(us_rho_x), Type(0.0), Type(0.001) * us_rho_x.size(), true); // soft sum-to-zero constraint

  PARAMETER_VECTOR(ui_rho_x);
  val -= sum(dnorm(ui_rho_x, 0.0, 1.0, true));


  PARAMETER_VECTOR(us_rho_xs);
  val -= Type(-0.5) * (us_rho_xs * (Q_x * us_rho_xs)).sum();
  val -= dnorm(sum(us_rho_xs), Type(0.0), Type(0.001) * us_rho_xs.size(), true); // soft sum-to-zero constraint

  PARAMETER_VECTOR(ui_rho_xs);
  val -= sum(dnorm(ui_rho_xs, 0.0, 1.0, true));

  PARAMETER_VECTOR(u_rho_a);
  if(u_rho_a.size() > 0)
    val += AR1(phi_rho_a)(u_rho_a);

  PARAMETER_VECTOR(u_rho_as);
  if(u_rho_as.size() > 0)
    val += AR1(phi_rho_as)(u_rho_as);

  PARAMETER_VECTOR(u_rho_xa);
  if (u_rho_xa.size() > 0) {
    val -= Type(-0.5) * (u_rho_xa * (Q_x * u_rho_xa)).sum();
    val -= dnorm(sum(u_rho_xa), Type(0.0), Type(0.001) * u_rho_xa.size(), true); // soft sum-to-zero constraint
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

  PARAMETER_VECTOR(us_alpha_x);
  val -= Type(-0.5) * (us_alpha_x * (Q_x * us_alpha_x)).sum();
  val -= dnorm(sum(us_alpha_x), Type(0.0), Type(0.001) * us_alpha_x.size(), true); // soft sum-to-zero constraint

  PARAMETER_VECTOR(ui_alpha_x);
  val -= sum(dnorm(ui_alpha_x, 0.0, 1.0, true));

  PARAMETER_VECTOR(us_alpha_xs);
  val -= Type(-0.5) * (us_alpha_xs * (Q_x * us_alpha_xs)).sum();
  val -= dnorm(sum(us_alpha_xs), Type(0.0), Type(0.001) * us_alpha_xs.size(), true); // soft sum-to-zero constraint

  PARAMETER_VECTOR(ui_alpha_xs);
  val -= sum(dnorm(ui_alpha_xs, 0.0, 1.0, true));

  PARAMETER_VECTOR(u_alpha_a);
  if(u_alpha_a.size() > 0)
    val += AR1(phi_alpha_a)(u_alpha_a);

  PARAMETER_VECTOR(u_alpha_as);
  if(u_alpha_as.size() > 0)
    val += AR1(phi_alpha_as)(u_alpha_as);

  PARAMETER_VECTOR(u_alpha_xt);
  val -= sum(dnorm(u_alpha_xt, 0.0, 1.0, true));

  PARAMETER_VECTOR(u_alpha_xa);
  val -= sum(dnorm(u_alpha_xa, 0.0, 1.0, true));

  PARAMETER_VECTOR(u_alpha_xat);
  val -= sum(dnorm(u_alpha_xat, 0.0, 1.0, true));

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
  val -= sum(dnorm(ui_lambda_x, 0.0, 1.0, true));

  // * ANC testing model *

  // district ASFR random effects
  PARAMETER(log_sigma_asfr_x);
  Type sigma_asfr_x(exp(log_sigma_asfr_x));
  val -= dnorm(sigma_asfr_x, Type(0.0), Type(2.5), true) + log_sigma_asfr_x;

  PARAMETER_VECTOR(ui_asfr_x);
  val -= sum(dnorm(ui_asfr_x, 0.0, 1.0, true));

  // ANC prevalence and ART coverage random effects
  PARAMETER(log_sigma_ancrho_x);
  Type sigma_ancrho_x(exp(log_sigma_ancrho_x));
  val -= dnorm(sigma_ancrho_x, Type(0.0), Type(2.5), true) + log_sigma_ancrho_x;

  PARAMETER(log_sigma_ancalpha_x);
  Type sigma_ancalpha_x(exp(log_sigma_ancalpha_x));
  val -= dnorm(sigma_ancalpha_x, Type(0.0), Type(2.5), true) + log_sigma_ancalpha_x;

  PARAMETER_VECTOR(ui_anc_rho_x);
  val -= sum(dnorm(ui_anc_rho_x, 0.0, 1.0, true));

  PARAMETER_VECTOR(ui_anc_alpha_x);
  val -= sum(dnorm(ui_anc_alpha_x, 0.0, 1.0, true));

  PARAMETER(log_sigma_ancrho_xt);
  Type sigma_ancrho_xt(exp(log_sigma_ancrho_xt));
  val -= dnorm(sigma_ancrho_xt, Type(0.0), Type(2.5), true) + log_sigma_ancrho_xt;

  PARAMETER(log_sigma_ancalpha_xt);
  Type sigma_ancalpha_xt(exp(log_sigma_ancalpha_xt));
  val -= dnorm(sigma_ancalpha_xt, Type(0.0), Type(2.5), true) + log_sigma_ancalpha_xt;

  PARAMETER_VECTOR(ui_anc_rho_xt);
  val -= sum(dnorm(ui_anc_rho_xt, 0.0, 1.0, true));

  PARAMETER_VECTOR(ui_anc_alpha_xt);
  val -= sum(dnorm(ui_anc_alpha_xt, 0.0, 1.0, true));


  // * ART attendance model *

  PARAMETER(log_sigma_or_gamma);
  Type sigma_or_gamma(exp(log_sigma_or_gamma));
  val -= dnorm(sigma_or_gamma, Type(0.0), Type(2.5), true) + log_sigma_or_gamma;

  PARAMETER_VECTOR(log_or_gamma);
  val -= dnorm(log_or_gamma, 0.0, 1.0, true).sum();

  PARAMETER(log_sigma_or_gamma_t1t2);
  Type sigma_or_gamma_t1t2(exp(log_sigma_or_gamma_t1t2));
  val -= dnorm(sigma_or_gamma_t1t2, Type(0.0), Type(2.5), true) + log_sigma_or_gamma_t1t2;
    
  PARAMETER_VECTOR(log_or_gamma_t1t2);
  val -= dnorm(log_or_gamma_t1t2, 0.0, 1.0, true).sum();


  // *** Process model ***

  // HIV prevalence time 1
  
  vector<Type> u_rho_x(sqrt(phi_rho_x) * us_rho_x + sqrt(1 - phi_rho_x) * ui_rho_x);
  vector<Type> u_rho_xs(sqrt(phi_rho_xs) * us_rho_xs + sqrt(1 - phi_rho_xs) * ui_rho_xs);
  vector<Type> mu_rho(X_rho * beta_rho +
                      logit_rho_offset +
                      Z_rho_x * u_rho_x * sigma_rho_x +
                      Z_rho_xs * u_rho_xs * sigma_rho_xs +
                      Z_rho_a * u_rho_a * sigma_rho_a +
                      Z_rho_as * u_rho_as * sigma_rho_as +
		      Z_rho_xa * u_rho_xa * sigma_rho_xa);

  // paediatric prevalence

  vector<Type> ones(X_rho.rows());
  ones.fill(1.0);

  vector<Type> rho_15to49_f((A_15to49f * invlogit(mu_rho)) / (A_15to49f * ones));
  vector<Type> mu_rho_paed(X_paed_rho_ratio * rho_15to49_f + paed_rho_ratio_offset);
  mu_rho_paed = logit(mu_rho_paed);
  mu_rho += mu_rho_paed;

  // ART coverage time 1
  
  vector<Type> u_alpha_x(sqrt(phi_alpha_x) * us_alpha_x + sqrt(1 - phi_alpha_x) * ui_alpha_x);
  vector<Type> u_alpha_xs(sqrt(phi_alpha_xs) * us_alpha_xs + sqrt(1 - phi_alpha_xs) * ui_alpha_xs);
  vector<Type> mu_alpha(X_alpha * beta_alpha +
                        logit_alpha_offset +
                        Z_alpha_x * u_alpha_x * sigma_alpha_x +
                        Z_alpha_xs * u_alpha_xs * sigma_alpha_xs +
                        Z_alpha_a * u_alpha_a * sigma_alpha_a +
                        Z_alpha_as * u_alpha_as * sigma_alpha_as +
                        Z_alpha_xa * u_alpha_xa * sigma_alpha_xa);


  vector<Type> rho_t1(invlogit(mu_rho));
  vector<Type> alpha_t1(invlogit(mu_alpha));

  vector<Type> plhiv_t1(population_t1 * rho_t1);
  vector<Type> prop_art_t1(rho_t1 * alpha_t1);
  vector<Type> artnum_t1(population_t1 * prop_art_t1);

  vector<Type> plhiv_15to49_t1(X_15to49 * plhiv_t1);
  vector<Type> rho_15to49_t1(plhiv_15to49_t1 / (X_15to49 * population_t1));
  vector<Type> alpha_15to49_t1((X_15to49 * artnum_t1) / plhiv_15to49_t1);

  vector<Type> mu_lambda_t1(X_lambda * beta_lambda + log_lambda_t1_offset +
                            Z_x * vector<Type>(log(rho_15to49_t1) + log(1.0 - omega * alpha_15to49_t1)) +
                            Z_lambda_x * ui_lambda_x * sigma_lambda_x);

  vector<Type> lambda_t1(exp(mu_lambda_t1));
  vector<Type> infections_t1(lambda_t1 * (population_t1 - plhiv_t1));


  // Projection from t1 to t2

  vector<Type> mu_alpha_t2(mu_alpha + logit_alpha_t1t2_offset +
                           X_alpha_t2 * beta_alpha_t2 +
                           Z_alpha_xt * u_alpha_xt * sigma_alpha_xt +
                           Z_alpha_xat * u_alpha_xat * sigma_alpha_xat);
  vector<Type> alpha_t2(invlogit(mu_alpha_t2));

  vector<Type> infections_t1t2((1 - exp(-lambda_t1 * projection_duration)) * (population_t1 - plhiv_t1));
  vector<Type> plhiv_t2(Lproj_hivpop * plhiv_t1 + Lproj_incid * infections_t1t2 + Lproj_paed * plhiv_t1);

  vector<Type> rho_t2(plhiv_t2 / population_t2);
  vector<Type> prop_art_t2(rho_t2 * alpha_t2);
  vector<Type> artnum_t2(population_t2 * prop_art_t2);


  vector<Type> plhiv_15to49_t2(X_15to49 * plhiv_t2);
  vector<Type> rho_15to49_t2(plhiv_15to49_t2 / (X_15to49 * population_t2));
  vector<Type> alpha_15to49_t2((X_15to49 * artnum_t2) / plhiv_15to49_t2);

  vector<Type> mu_lambda_t2(X_lambda * beta_lambda + log_lambda_t2_offset +
                            Z_x * vector<Type>(log(rho_15to49_t2) + log(1.0 - omega * alpha_15to49_t2)) +
                            Z_lambda_x * ui_lambda_x * sigma_lambda_x);

  vector<Type> lambda_t2(exp(mu_lambda_t2));
  vector<Type> infections_t2(lambda_t2 * (population_t2 - plhiv_t2));

  // likelihood for household survey data

  for(int i = 0; i < idx_prev.size(); i++)
    val -= dbinom_robust(x_prev[i], n_prev[i], mu_rho[idx_prev[i]], true);

  for(int i = 0; i < idx_artcov.size(); i++)
    val -= dbinom_robust(x_artcov[i], n_artcov[i], mu_alpha[idx_artcov[i]], true);

  for(int i = 0; i < idx_vls.size(); i++)
    val -= dbinom(x_vls[i], n_vls[i], alpha_t1[idx_vls[i]] * nu, true);

  vector<Type> pR_i(idx_recent.size());
  for(int i = 0; i < idx_recent.size(); i++) {
    int idx = idx_recent[i];
    Type pR = 1.0 - exp(-(exp(mu_lambda_t1[idx]) * (1 - rho_t1[idx]) / rho_t1[idx] * (OmegaT - betaT * ritaT) + betaT));
    pR_i[i] = pR;
    val -= dbinom(x_recent[i], n_recent[i], pR, true);
  }


  // ANC prevalence and ART coverage model
  // Note: currently this operates on the entire population vector, producing 
  //       lots of zeros for males and female age groups not exposed to fertility.
  //       It would be more computationally efficient to project this to subset 
  //       of female age 15-49 age groups. But I don't know if it would be
  //       meaningfully more efficient.

  vector<Type> mu_asfr(X_asfr * beta_asfr +
		       Z_asfr_x * ui_asfr_x * sigma_asfr_x);
		       
  vector<Type> mu_anc_rho_t1(mu_rho +
			     logit_anc_rho_t1_offset + 
			     X_ancrho * beta_anc_rho +
			     Z_ancrho_x * ui_anc_rho_x * sigma_ancrho_x);
  vector<Type> anc_rho_t1(invlogit(mu_anc_rho_t1));
  
  vector<Type> mu_anc_alpha_t1(mu_alpha +
			       logit_anc_alpha_t1_offset + 
			       X_ancalpha * beta_anc_alpha +
			       Z_ancalpha_x * ui_anc_alpha_x * sigma_ancalpha_x);
  vector<Type> anc_alpha_t1(invlogit(mu_anc_alpha_t1));
  
  vector<Type> anc_clients_t1(population_t1 * exp(log_asfr_t1_offset + mu_asfr));
  vector<Type> anc_plhiv_t1(anc_clients_t1 * anc_rho_t1);
  vector<Type> anc_already_art_t1(anc_plhiv_t1 * anc_alpha_t1);

  vector<Type> mu_anc_rho_t2(logit(rho_t2) +
			     logit_anc_rho_t2_offset + 
			     X_ancrho * vector<Type>(beta_anc_rho + beta_anc_rho_t2) +
			     Z_ancrho_x * vector<Type>(ui_anc_rho_x * sigma_ancrho_x + ui_anc_rho_xt * sigma_ancrho_xt));
  vector<Type> anc_rho_t2(invlogit(mu_anc_rho_t2));

  vector<Type> mu_anc_alpha_t2(mu_alpha_t2 +
			       logit_anc_alpha_t2_offset + 
			       X_ancalpha * vector<Type>(beta_anc_alpha + beta_anc_alpha_t2) +
			       Z_ancalpha_x * vector<Type>(ui_anc_alpha_x * sigma_ancalpha_x + ui_anc_alpha_xt * sigma_ancalpha_xt));
  vector<Type> anc_alpha_t2(invlogit(mu_anc_alpha_t2));

  vector<Type> anc_clients_t2(population_t2 * exp(log_asfr_t2_offset + mu_asfr));
  vector<Type> anc_plhiv_t2(anc_clients_t2 * anc_rho_t2);
  vector<Type> anc_already_art_t2(anc_plhiv_t2 * anc_alpha_t2);

  // likelihood for ANC testing observations

  vector<Type> anc_clients_obs_t2((A_anc_clients_t2 * anc_clients_t2) * exp(offset_anc_clients_t2));
  val -= dpois(x_anc_clients_t2, anc_clients_obs_t2, true).sum();
	       
  vector<Type> anc_rho_obs_t1(A_anc_prev_t1 * anc_plhiv_t1 / (A_anc_prev_t1 * anc_clients_t1));
  val -= dbinom(x_anc_prev_t1, n_anc_prev_t1, anc_rho_obs_t1, true).sum();

  vector<Type> anc_alpha_obs_t1(A_anc_artcov_t1 * anc_already_art_t1 / (A_anc_artcov_t1 * anc_plhiv_t1));
  val -= dbinom(x_anc_artcov_t1, n_anc_artcov_t1, anc_alpha_obs_t1, true).sum();

  vector<Type> anc_rho_obs_t2(A_anc_prev_t2 * anc_plhiv_t2 / (A_anc_prev_t2 * anc_clients_t2));
  val -= dbinom(x_anc_prev_t2, n_anc_prev_t2, anc_rho_obs_t2, true).sum();

  vector<Type> anc_alpha_obs_t2(A_anc_artcov_t2 * anc_already_art_t2 / (A_anc_artcov_t2 * anc_plhiv_t2));
  val -= dbinom(x_anc_artcov_t2, n_anc_artcov_t2, anc_alpha_obs_t2, true).sum();

  
  // * ART attendance model *

  vector<Type> gamma_art(exp(Xgamma * log_or_gamma * sigma_or_gamma + log_gamma_offset));
  int cum_nb = 0;
  for(int i = 0; i < n_nb.size(); i++){
    Type cum_exp_or_gamma_i = 0.0;
    for(int j = 0; j < n_nb[i]+1; j++)
      cum_exp_or_gamma_i += gamma_art[cum_nb + i + j];
    for(int j = 0; j < n_nb[i]+1; j++)
      gamma_art[cum_nb + i + j] /= cum_exp_or_gamma_i;
    cum_nb += n_nb[i];
  }

  vector<Type> prop_art_ij_t1((Xart_idx * prop_art_t1) * (Xart_gamma * gamma_art));
  vector<Type> population_ij_t1(Xart_idx * population_t1);

  vector<Type> artnum_ij_t1(population_ij_t1 * prop_art_ij_t1);
  vector<Type> A_j_t1(A_artattend_t1 * artnum_ij_t1);
  vector<Type> sd_A_j_t1(A_artattend_t1 * vector<Type>(population_ij_t1 * prop_art_ij_t1 * (1 - prop_art_ij_t1)));
  sd_A_j_t1 = sd_A_j_t1.sqrt();

  val -= sum(dnorm(x_artnum_t1, A_j_t1, sd_A_j_t1, true));

  vector<Type> gamma_art_t2(exp(Xgamma * log_or_gamma * sigma_or_gamma + Xgamma_t2 * log_or_gamma_t1t2 * sigma_or_gamma_t1t2 + log_gamma_offset));
  cum_nb = 0;
  for(int i = 0; i < n_nb.size(); i++){
    Type cum_exp_or_gamma_i = 0.0;
    for(int j = 0; j < n_nb[i]+1; j++)
      cum_exp_or_gamma_i += gamma_art_t2[cum_nb + i + j];
    for(int j = 0; j < n_nb[i]+1; j++)
      gamma_art_t2[cum_nb + i + j] /= cum_exp_or_gamma_i;
    cum_nb += n_nb[i];
  }
  
  vector<Type> prop_art_ij_t2((Xart_idx * prop_art_t2) * (Xart_gamma * gamma_art_t2));
  vector<Type> population_ij_t2(Xart_idx * population_t2);

  vector<Type> artnum_ij_t2(population_ij_t2 * prop_art_ij_t2);
  vector<Type> A_j_t2(A_artattend_t2 * artnum_ij_t2);
  vector<Type> sd_A_j_t2(A_artattend_t2 * vector<Type>(population_ij_t2 * prop_art_ij_t2 * (1 - prop_art_ij_t2)));
  sd_A_j_t2 = sd_A_j_t2.sqrt();

  val -= sum(dnorm(x_artnum_t2, A_j_t2, sd_A_j_t2, true));

  // Calculate model outputs

  DATA_SPARSE_MATRIX(A_out);
  DATA_SPARSE_MATRIX(A_anc_out);
  DATA_INTEGER(calc_outputs);

  if(calc_outputs) {

    vector<Type> population_t1_out(A_out * population_t1);

    vector<Type> plhiv_t1_out(A_out * plhiv_t1);
    vector<Type> rho_t1_out(plhiv_t1_out / population_t1_out);

    vector<Type> artnum_t1_out(A_out * artnum_t1);
    vector<Type> alpha_t1_out(artnum_t1_out / plhiv_t1_out);
    vector<Type> artattend_t1_out(A_out * (A_artattend_mf * artnum_ij_t1));
    vector<Type> artattend_ij_t1_out(A_art_reside_attend * artnum_ij_t1);

    vector<Type> infections_t1_out(A_out * infections_t1);
    vector<Type> lambda_t1_out(infections_t1_out / (population_t1_out - plhiv_t1_out));

    vector<Type> population_t2_out(A_out * population_t2);

    vector<Type> plhiv_t2_out(A_out * plhiv_t2);
    vector<Type> rho_t2_out(plhiv_t2_out / population_t2_out);

    vector<Type> artnum_t2_out(A_out * artnum_t2);
    vector<Type> alpha_t2_out(artnum_t2_out / plhiv_t2_out);
    vector<Type> artattend_t2_out(A_out * (A_artattend_mf * artnum_ij_t2));
    vector<Type> artattend_ij_t2_out(A_art_reside_attend * artnum_ij_t2);

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
    // No data involved, so calculated after likelihood

    DATA_VECTOR(population_t3);
    DATA_SPARSE_MATRIX(Lproj_hivpop_t2t3);
    DATA_SPARSE_MATRIX(Lproj_incid_t2t3);
    DATA_SPARSE_MATRIX(Lproj_paed_t2t3);
    DATA_SCALAR(projection_duration_t2t3);
    DATA_VECTOR(logit_alpha_t2t3_offset);
    DATA_VECTOR(log_lambda_t3_offset);

    vector<Type> mu_alpha_t3(mu_alpha_t2 + logit_alpha_t2t3_offset);
    vector<Type> alpha_t3(invlogit(mu_alpha_t3));
    
    vector<Type> infections_t2t3((1 - exp(-lambda_t2 * projection_duration_t2t3)) * (population_t2 - plhiv_t2));
    vector<Type> plhiv_t3(Lproj_hivpop_t2t3 * plhiv_t2 + Lproj_incid_t2t3 * infections_t2t3 + Lproj_paed_t2t3 * plhiv_t2);
    
    vector<Type> rho_t3(plhiv_t3 / population_t3);
    vector<Type> prop_art_t3(rho_t3 * alpha_t3);
    vector<Type> artnum_t3(population_t3 * prop_art_t3);
    
    vector<Type> plhiv_15to49_t3(X_15to49 * plhiv_t3);
    vector<Type> rho_15to49_t3(plhiv_15to49_t3 / (X_15to49 * population_t3));
    vector<Type> alpha_15to49_t3((X_15to49 * artnum_t3) / plhiv_15to49_t3);
    
    vector<Type> mu_lambda_t3(X_lambda * beta_lambda + log_lambda_t3_offset +
			      Z_x * vector<Type>(log(rho_15to49_t3) + log(1.0 - omega * alpha_15to49_t3)) +
			      Z_lambda_x * ui_lambda_x * sigma_lambda_x);
    
    vector<Type> infections_t3(exp(mu_lambda_t3) * (population_t3 - plhiv_t3));


    // Note: currently assuming same district effects parameters from t2 for t3
    vector<Type> mu_anc_rho_t3(logit(rho_t3) +
			       logit_anc_rho_t2_offset + 
			       X_ancrho * vector<Type>(beta_anc_rho + beta_anc_rho_t2) +
			       Z_ancrho_x * vector<Type>(ui_anc_rho_x * sigma_ancrho_x + ui_anc_rho_xt * sigma_ancrho_xt));
    vector<Type> anc_rho_t3(invlogit(mu_anc_rho_t3));
    
    vector<Type> mu_anc_alpha_t3(mu_alpha_t3 +
				 logit_anc_alpha_t3_offset + 
				 X_ancalpha * vector<Type>(beta_anc_alpha + beta_anc_alpha_t2) +
				 Z_ancalpha_x * vector<Type>(ui_anc_alpha_x * sigma_ancalpha_x + ui_anc_alpha_xt * sigma_ancalpha_xt));
    vector<Type> anc_alpha_t3(invlogit(mu_anc_alpha_t3));
    
    vector<Type> anc_clients_t3(population_t3 * exp(log_asfr_t3_offset + mu_asfr));
    vector<Type> anc_plhiv_t3(anc_clients_t3 * anc_rho_t3);
    vector<Type> anc_already_art_t3(anc_plhiv_t3 * anc_alpha_t3);

    
    vector<Type> prop_art_ij_t3((Xart_idx * prop_art_t3) * (Xart_gamma * gamma_art_t2));  // Note: using same ART attendance as T2
    vector<Type> population_ij_t3(Xart_idx * population_t3);
    vector<Type> artnum_ij_t3(population_ij_t3 * prop_art_ij_t3);

    vector<Type> population_t3_out(A_out * population_t3);
    vector<Type> plhiv_t3_out(A_out * plhiv_t3);
    vector<Type> rho_t3_out(plhiv_t3_out / population_t3_out);
    vector<Type> artnum_t3_out(A_out * artnum_t3);
    vector<Type> alpha_t3_out(artnum_t3_out / plhiv_t3_out);
    vector<Type> artattend_t3_out(A_out * (A_artattend_mf * artnum_ij_t3));
    vector<Type> artattend_ij_t3_out(A_art_reside_attend * artnum_ij_t3);
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
  }
  
  return val;
}
