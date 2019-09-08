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

  DATA_SPARSE_MATRIX(Z_x);
  DATA_SPARSE_MATRIX(Z_xs);
  DATA_SPARSE_MATRIX(Z_a);
  DATA_SPARSE_MATRIX(Z_as);

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

  DATA_SPARSE_MATRIX(A_anc_prev);
  DATA_VECTOR(n_anc_prev);
  DATA_VECTOR(x_anc_prev);
  
  DATA_SPARSE_MATRIX(A_anc_artcov);
  DATA_VECTOR(n_anc_artcov);
  DATA_VECTOR(x_anc_artcov);

  DATA_SPARSE_MATRIX(A_artnum);
  DATA_VECTOR(x_artnum);

  DATA_IVECTOR(n_nb);
  DATA_IVECTOR(adj_i);
  DATA_IVECTOR(adj_j);
  DATA_VECTOR(gamma_or_mu);
  DATA_VECTOR(gamma_or_sigma);
  DATA_SPARSE_MATRIX(Xart_idx);
  DATA_SPARSE_MATRIX(Xart_gamma);
  
  DATA_SPARSE_MATRIX(A_out);

  // ** Initialize nll **
  Type val(0);
  
  // ** Parameters **

  // fixed effects
  
  PARAMETER_VECTOR(beta_rho);
  PARAMETER_VECTOR(beta_alpha);

  PARAMETER(beta_anc_rho);
  PARAMETER(beta_anc_alpha);

  
  // * HIV prevalence model *
    
  // hyper parameters

  PARAMETER(logit_phi_rho_x);
  Type phi_rho_x(invlogit(logit_phi_rho_x));
  val -= log(phi_rho_x) +  log(1 - phi_rho_x);  // change of variables: logit_phi_x -> phi_x
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
  Type phi_rho_a(invlogit(logit_phi_rho_a));
    
  PARAMETER(log_sigma_rho_a);
  Type sigma_rho_a(exp(log_sigma_rho_a));  

  PARAMETER(logit_phi_rho_as);
  Type phi_rho_as(invlogit(logit_phi_rho_as));

  PARAMETER(log_sigma_rho_as);
  Type sigma_rho_as(exp(log_sigma_rho_as));


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
  val -= dnorm(exp(log_sigma_alpha_xs), Type(0.0), Type(2.5), true) + log_sigma_alpha_xs;

  PARAMETER(logit_phi_alpha_a);
  Type phi_alpha_a(invlogit(logit_phi_alpha_a));
  
  PARAMETER(log_sigma_alpha_a);
  Type sigma_alpha_a(exp(log_sigma_alpha_a));

  PARAMETER(logit_phi_alpha_as);
  Type phi_alpha_as(invlogit(logit_phi_alpha_as));
    
  PARAMETER(log_sigma_alpha_as);
  Type sigma_alpha_as(exp(log_sigma_alpha_as));

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
  val += AR1(phi_alpha_a)(u_alpha_a);
      
  PARAMETER_VECTOR(u_alpha_as);
  val += AR1(phi_alpha_as)(u_alpha_as);


  // * ANC testing model *

  PARAMETER(log_sigma_ancrho_x);
  Type sigma_ancrho_x(exp(log_sigma_ancrho_x));

  PARAMETER(log_sigma_ancalpha_x);
  Type sigma_ancalpha_x(exp(log_sigma_ancalpha_x));
  
  PARAMETER_VECTOR(ui_anc_rho_x);
  val -= sum(dnorm(ui_anc_rho_x, 0.0, 1.0, true));
    
  PARAMETER_VECTOR(ui_anc_alpha_x);
  val -= sum(dnorm(ui_anc_alpha_x, 0.0, 1.0, true));

  
  // * ART attendance model *
  
  PARAMETER_VECTOR(oddsratio_gamma_art_raw);
  val -= dnorm(oddsratio_gamma_art_raw, 0.0, 1.0, true).sum();
  vector<Type> oddsratio_gamma_art(oddsratio_gamma_art_raw * gamma_or_sigma + gamma_or_mu);

  

  vector<Type> u_rho_x(sqrt(phi_rho_x) * us_rho_x + sqrt(1 - phi_rho_x) * ui_rho_x);
  vector<Type> u_rho_xs(sqrt(phi_rho_xs) * us_rho_xs + sqrt(1 - phi_rho_xs) * ui_rho_xs);      
  vector<Type> mu_rho(X_rho * beta_rho +
		      Z_x * u_rho_x * sigma_rho_x +
		      Z_xs * u_rho_xs * sigma_rho_xs +
		      Z_a * u_rho_a * sigma_rho_a +
		      Z_as * u_rho_as * sigma_rho_as);

  vector<Type> u_alpha_x(sqrt(phi_alpha_x) * us_alpha_x + sqrt(1 - phi_alpha_x) * ui_alpha_x);
  vector<Type> u_alpha_xs(sqrt(phi_alpha_xs) * us_alpha_xs + sqrt(1 - phi_alpha_xs) * ui_alpha_xs);      
  vector<Type> mu_alpha(X_alpha * beta_alpha +
  			Z_x * u_alpha_x * sigma_alpha_x +
  			Z_xs * u_alpha_xs * sigma_alpha_xs +
  			Z_a * u_alpha_a * sigma_alpha_a +
  			Z_as * u_alpha_as * sigma_alpha_as);

  
  // likelihood    

  for(int i = 0; i < idx_prev.size(); i++)
    val -= dbinom_robust(x_prev[i], n_prev[i], mu_rho[idx_prev[i]], true);

  for(int i = 0; i < idx_artcov.size(); i++)
    val -= dbinom_robust(x_artcov[i], n_artcov[i], mu_alpha[idx_artcov[i]], true);

  vector<Type> rho(invlogit(mu_rho));
  vector<Type> alpha(invlogit(mu_alpha));

  vector<Type> ones(rho.size());
  ones.fill(1.0);
  
  vector<Type> mu_anc_rho(A_anc_prev * rho / (A_anc_prev * ones));
  mu_anc_rho = logit(mu_anc_rho) + beta_anc_rho + Z_ancrho_x * ui_anc_rho_x * sigma_ancrho_x;
  val -= sum(dbinom_robust(x_anc_prev, n_anc_prev, mu_anc_rho, true));

  vector<Type> mu_anc_alpha(A_anc_artcov * vector<Type>(rho * alpha) / (A_anc_artcov * rho));
  mu_anc_alpha = logit(mu_anc_alpha) + beta_anc_alpha + Z_ancalpha_x * ui_anc_alpha_x * sigma_ancalpha_x;
  val -= sum(dbinom_robust(x_anc_artcov, n_anc_artcov, mu_anc_alpha, true));


  // * ART attendance model *

  vector<Type> gamma_art(adj_i.size());
  int cum_nb = 0; 
  for(int i = 0; i < n_nb.size(); i++){
    Type cum_exp_or_gamma_i = 1.0;
    for(int j = 0; j < n_nb[i]; j++)
      cum_exp_or_gamma_i += gamma_art[cum_nb + i + j] = exp(oddsratio_gamma_art[cum_nb + j]);
    for(int j = 0; j < n_nb[i]; j++)
      gamma_art[cum_nb + i + j] /= cum_exp_or_gamma_i;
    gamma_art[cum_nb + i + n_nb[i]] = 1.0 / cum_exp_or_gamma_i;
    cum_nb += n_nb[i];
  }

  vector<Type> prop_art(rho * alpha);
  vector<Type> artnum(population * prop_art);

  vector<Type> prop_art_ij((Xart_idx * prop_art) * (Xart_gamma * gamma_art));
  vector<Type> population_ij(Xart_idx * population);
  
  vector<Type> A_j(A_artnum * vector<Type>(population_ij * prop_art_ij));
  vector<Type> sd_A_j(A_artnum * vector<Type>(population_ij * prop_art_ij * (1 - prop_art_ij)));
  sd_A_j = sd_A_j.sqrt();

  val -= sum(dnorm(x_artnum, A_j, sd_A_j, true));

  REPORT(A_j);
  REPORT(sd_A_j);
		       
  vector<Type> plhiv_out(A_out * vector<Type>(rho * population));
  vector<Type> rho_out(plhiv_out / (A_out * population));

  vector<Type> artnum_out(A_out * artnum);
  vector<Type> alpha_out(artnum_out / plhiv_out);

  REPORT(mu_rho);
  REPORT(mu_alpha);
  REPORT(phi_rho_a);
  REPORT(sigma_rho_a);
  REPORT(rho_out);
  REPORT(plhiv_out);

  REPORT(mu_anc_rho);
  REPORT(mu_anc_alpha)

  REPORT(alpha_out);
  REPORT(artnum_out);
  REPORT(gamma_art);

  ADREPORT(plhiv_out);
  ADREPORT(rho_out);
  ADREPORT(artnum_out);
  ADREPORT(alpha_out);

  return val;
}
