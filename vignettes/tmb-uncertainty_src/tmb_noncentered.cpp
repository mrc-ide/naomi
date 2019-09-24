#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{

  using namespace density;

  // ** Data **

  // Population
  DATA_VECTOR(population);

  // Design matrices
  DATA_MATRIX(X_rho);

  DATA_SPARSE_MATRIX(Z_x);
  DATA_SPARSE_MATRIX(Z_xs);
  DATA_SPARSE_MATRIX(Z_a);
  DATA_SPARSE_MATRIX(Z_as);

  // Precision matrix for ICAR area model
  DATA_SPARSE_MATRIX(Q_x);

  DATA_IVECTOR(idx_prev);
  DATA_VECTOR(n_prev);
  DATA_VECTOR(x_prev);

  DATA_SPARSE_MATRIX(A_out);


  // ** Initialize nll **
  Type val(0);

  // ** Parameters **

  // fixed effects
  // diffuse N(0.0, 5.0) prior distribution

  PARAMETER_VECTOR(beta_rho);
  val -= dnorm(beta_rho, 0.0, 5.0, true).sum();


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
  val += AR1(phi_rho_a)(u_rho_a);

  PARAMETER_VECTOR(u_rho_as);
  val += AR1(phi_rho_as)(u_rho_as);


  vector<Type> u_rho_x(sqrt(phi_rho_x) * us_rho_x + sqrt(1 - phi_rho_x) * ui_rho_x);
  vector<Type> u_rho_xs(sqrt(phi_rho_xs) * us_rho_xs + sqrt(1 - phi_rho_xs) * ui_rho_xs);
  vector<Type> mu_rho(X_rho * beta_rho +
		      Z_x * u_rho_x * sigma_rho_x +
		      Z_xs * u_rho_xs * sigma_rho_xs +
		      Z_a * u_rho_a * sigma_rho_a +
		      Z_as * u_rho_as * sigma_rho_as);
  vector<Type> rho(invlogit(mu_rho));
  vector<Type> plhiv(population * rho);

  // likelihood

  for(int i = 0; i < idx_prev.size(); i++)
    val -= dbinom_robust(x_prev[i], n_prev[i], mu_rho[idx_prev[i]], true);


  // Calculate model outputs

  vector<Type> population_out(A_out * population);
  vector<Type> plhiv_out(A_out * plhiv);
  vector<Type> rho_out(plhiv_out / population_out);

  REPORT(mu_rho);

  REPORT(population_out);
  REPORT(rho_out);
  REPORT(plhiv_out);
  
  ADREPORT(rho_out);
  
  return val;
}
