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

  // hyper parameters

  PARAMETER(logit_phi_rho_x);
  Type phi_rho_x(invlogit(logit_phi_rho_x));
  val -= log(phi_rho_x) +  log(1 - phi_rho_x);  // change of variables: logit_phi_x ->v phi_x
  val -= dbeta(phi_rho_x, Type(0.5), Type(0.5), true);

  PARAMETER(log_sigma_rho_x);
  Type sigma_rho_x(exp(log_sigma_rho_x));
  val -= dnorm(sigma_rho_x, Type(0.0), Type(2.5), true) + log_sigma_rho_x;

  // latent effects

  PARAMETER_VECTOR(us_rho_x);
  val -= Type(-0.5) * (us_rho_x * (Q_x * us_rho_x)).sum();
  val -= dnorm(sum(us_rho_x), Type(0.0), Type(0.001) * us_rho_x.size(), true); // soft sum-to-zero constraint

  PARAMETER_VECTOR(ui_rho_x);
  val -= sum(dnorm(ui_rho_x, 0.0, 1.0, true));

  vector<Type> u_rho_x(sqrt(phi_rho_x) * us_rho_x + sqrt(1 - phi_rho_x) * ui_rho_x);
  vector<Type> mu_rho(X_rho * beta_rho +
   		      Z_x * u_rho_x * sigma_rho_x);
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

  ADREPORT(mu_rho);
  ADREPORT(rho_out);
  
  return val;
}
