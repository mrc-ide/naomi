#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{

  using namespace density;


  // ** Data **

  DATA_MATRIX(X);
  DATA_MATRIX(Z_area);
  DATA_MATRIX(Z_area_sex);
  DATA_MATRIX(Z_age);
  DATA_MATRIX(Z_age_sex);

  // Precision matrix for ICAR area model
  DATA_MATRIX(Q_area);
  
  DATA_IVECTOR(idx_prev);
  DATA_VECTOR(n_prev);
  DATA_VECTOR(x_prev);

  
  // ** Parameters **

  PARAMETER_VECTOR(beta);
  PARAMETER_VECTOR(us_area);
  PARAMETER_VECTOR(ui_area);

  PARAMETER_VECTOR(us_area_sex);
  PARAMETER_VECTOR(ui_area_sex);
  
  PARAMETER_VECTOR(u_age);
  PARAMETER_VECTOR(u_age_sex);

  PARAMETER(logit_phi_area);
  PARAMETER(log_sigma_area);

  PARAMETER(logit_phi_area_sex);
  PARAMETER(log_sigma_area_sex);

  PARAMETER(logit_phi_age);
  PARAMETER(log_sigma_age);

  PARAMETER(logit_phi_age_sex);
  PARAMETER(log_sigma_age_sex);


  Type phi_age(invlogit(logit_phi_age));
  Type sigma_age(exp(log_sigma_age));

  Type phi_age_sex(invlogit(logit_phi_age_sex));
  Type sigma_age_sex(exp(log_sigma_age_sex));

  Type phi_area(invlogit(logit_phi_area));
  Type sigma_area(exp(log_sigma_area));

  Type phi_area_sex(invlogit(logit_phi_area_sex));
  Type sigma_area_sex(exp(log_sigma_area_sex));

  vector<Type> u_area(sqrt(phi_area) * us_area + sqrt(1 - phi_area) * ui_area);
  vector<Type> u_area_sex(sqrt(phi_area_sex) * us_area_sex + sqrt(1 - phi_area_sex) * ui_area_sex);
		      
    
  vector<Type> mu_rho(X * beta +		      
		      Z_area * u_area * sigma_area +
		      Z_area_sex * u_area_sex * sigma_area_sex +
		      Z_age * u_age * sigma_age +
		      Z_age_sex * u_age_sex * sigma_age_sex);

    
  // initialize nll
  Type val(0);

  // hyperparameter priors

  val -= dnorm(exp(log_sigma_area), Type(0.0), Type(2.5), true) + log_sigma_area;
    
  val -= log(phi_area) +  log(1 - phi_area);  // change of variables: logit_phi_area -> phi_area
  val -= dbeta(phi_area, Type(0.5), Type(0.5), true);

  val -= Type(-0.5) * (us_area * (Q_area * us_area)).sum();
  val -= dnorm(sum(us_area), Type(0.0), Type(0.001) * us_area.size(), true); // soft sum-to-zero constraint

  val -= sum(dnorm(ui_area, 0.0, 1.0, true));


  val -= dnorm(exp(log_sigma_area_sex), Type(0.0), Type(2.5), true) + log_sigma_area_sex;
    
  val -= log(phi_area_sex) +  log(1 - phi_area_sex);  // change of variables: logit_phi_area_sex -> phi_area_sex
  val -= dbeta(phi_area_sex, Type(0.5), Type(0.5), true);

  val -= Type(-0.5) * (us_area_sex * (Q_area * us_area_sex)).sum();
  val -= dnorm(sum(us_area_sex), Type(0.0), Type(0.001) * us_area_sex.size(), true); // soft sum-to-zero constraint

  val -= sum(dnorm(ui_area_sex, 0.0, 1.0, true));

  
  val += AR1(phi_age)(u_age);
  val += AR1(phi_age_sex)(u_age_sex);

  // likelihood    

  for(int i = 0; i < idx_prev.size(); i++)
    val -= dbinom_robust(x_prev[i], n_prev[i], mu_rho[idx_prev[i]], true);

  REPORT(mu_rho);
  REPORT(phi_age);
  REPORT(sigma_age);

  return val;
}
