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
  DATA_VECTOR(population);

  // Design matrices
  DATA_MATRIX(X_rho);
  DATA_MATRIX(X_alpha);

  DATA_MATRIX(Z_x);
  DATA_MATRIX(Z_xs);
  DATA_MATRIX(Z_a);
  DATA_MATRIX(Z_as);

  // Precision matrix for ICAR area model
  DATA_MATRIX(Q_x);
  
  DATA_IVECTOR(idx_prev);
  DATA_VECTOR(n_prev);
  DATA_VECTOR(x_prev);

  DATA_IVECTOR(idx_artcov);
  DATA_VECTOR(n_artcov);
  DATA_VECTOR(x_artcov);

  DATA_SPARSE_MATRIX(A_out);
  
  // ** Parameters **

  PARAMETER_VECTOR(beta_rho);
  PARAMETER_VECTOR(beta_alpha);

  PARAMETER_VECTOR(us_rho_x);
  PARAMETER_VECTOR(ui_rho_x);

  PARAMETER_VECTOR(us_rho_xs);
  PARAMETER_VECTOR(ui_rho_xs);
  
  PARAMETER_VECTOR(u_rho_a);
  PARAMETER_VECTOR(u_rho_as);

  PARAMETER(logit_phi_rho_x);
  PARAMETER(log_sigma_rho_x);

  PARAMETER(logit_phi_rho_xs);
  PARAMETER(log_sigma_rho_xs);

  PARAMETER(logit_phi_rho_a);
  PARAMETER(log_sigma_rho_a);

  PARAMETER(logit_phi_rho_as);
  PARAMETER(log_sigma_rho_as);


  Type phi_rho_a(invlogit(logit_phi_rho_a));
  Type sigma_rho_a(exp(log_sigma_rho_a));

  Type phi_rho_as(invlogit(logit_phi_rho_as));
  Type sigma_rho_as(exp(log_sigma_rho_as));

  Type phi_rho_x(invlogit(logit_phi_rho_x));
  Type sigma_rho_x(exp(log_sigma_rho_x));

  Type phi_rho_xs(invlogit(logit_phi_rho_xs));
  Type sigma_rho_xs(exp(log_sigma_rho_xs));

  vector<Type> u_rho_x(sqrt(phi_rho_x) * us_rho_x + sqrt(1 - phi_rho_x) * ui_rho_x);
  vector<Type> u_rho_xs(sqrt(phi_rho_xs) * us_rho_xs + sqrt(1 - phi_rho_xs) * ui_rho_xs);
		      
    
  vector<Type> mu_rho(X_rho * beta_rho +
		      Z_x * u_rho_x * sigma_rho_x +
		      Z_xs * u_rho_xs * sigma_rho_xs +
		      Z_a * u_rho_a * sigma_rho_a +
		      Z_as * u_rho_as * sigma_rho_as);

  vector<Type> mu_alpha(X_alpha * beta_alpha);

    
  // initialize nll
  Type val(0);

  // hyperparameter priors

  val -= dnorm(exp(log_sigma_rho_x), Type(0.0), Type(2.5), true) + log_sigma_rho_x;
    
  val -= log(phi_rho_x) +  log(1 - phi_rho_x);  // change of variables: logit_phi_x -> phi_x
  val -= dbeta(phi_rho_x, Type(0.5), Type(0.5), true);

  val -= Type(-0.5) * (us_rho_x * (Q_x * us_rho_x)).sum();
  val -= dnorm(sum(us_rho_x), Type(0.0), Type(0.001) * us_rho_x.size(), true); // soft sum-to-zero constraint

  val -= sum(dnorm(ui_rho_x, 0.0, 1.0, true));


  val -= dnorm(exp(log_sigma_rho_xs), Type(0.0), Type(2.5), true) + log_sigma_rho_xs;
    
  val -= log(phi_rho_xs) +  log(1 - phi_rho_xs);  // change of variables: logit_phi_xs -> phi_xs
  val -= dbeta(phi_rho_xs, Type(0.5), Type(0.5), true);

  val -= Type(-0.5) * (us_rho_xs * (Q_x * us_rho_xs)).sum();
  val -= dnorm(sum(us_rho_xs), Type(0.0), Type(0.001) * us_rho_xs.size(), true); // soft sum-to-zero constraint

  val -= sum(dnorm(ui_rho_xs, 0.0, 1.0, true));

  
  val += AR1(phi_rho_a)(u_rho_a);
  val += AR1(phi_rho_as)(u_rho_as);

  // likelihood    

  for(int i = 0; i < idx_prev.size(); i++)
    val -= dbinom_robust(x_prev[i], n_prev[i], mu_rho[idx_prev[i]], true);

  for(int i = 0; i < idx_artcov.size(); i++)
    val -= dbinom_robust(x_artcov[i], n_artcov[i], mu_alpha[idx_artcov[i]], true);

  vector<Type> rho(invlogit(mu_rho));
  vector<Type> plhiv_out(A_out * vector<Type>(rho * population));
  vector<Type> rho_out(plhiv_out / (A_out * population));

  vector<Type> alpha(invlogit(mu_alpha));
  vector<Type> artnum_out(A_out * vector<Type>(alpha * rho * population));
  vector<Type> alpha_out(artnum_out / plhiv_out);

  REPORT(mu_rho);
  REPORT(phi_rho_a);
  REPORT(sigma_rho_a);
  REPORT(rho_out);
  REPORT(plhiv_out);

  REPORT(alpha_out);
  REPORT(artnum_out);

  ADREPORT(plhiv_out);
  ADREPORT(rho_out);
  ADREPORT(artnum_out);
  ADREPORT(alpha_out);

  return val;
}
