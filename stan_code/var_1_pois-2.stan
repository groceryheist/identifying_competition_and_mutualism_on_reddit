// stan_code/var_1.stan

data{
  int<lower=0> forecast_len;  // length of the forecast to generate
  int<lower=0> N; // length of the time series
  int<lower=0> K; // number of series
  int Y[N,K]; // output matrix
}

/* transformed data{ */
/*   vector[K] Y_log[N]; */
/*   for(n in 1:N) */
/*     Y_log[n] = to_vector(log(Y[n])); */
/* } */

parameters{
  /* vector[K] alpha_hat_location; // intrinsic growth rates */
  /* vector<lower=0>[K] alpha_hat_scale; // intrinsic growth rates */
  /* matrix[K,K] beta_hat_location; // community matrix of competition / correlation coefficients */
  /* matrix<lower=0>[K,K] beta_hat_scale; */
  /* vector<lower=0>[K] tau_scale; */
  /* vector[K] tau_location; */
  vector<lower=0>[K] tau;
  // LJK parameterize the covariance matrix 
  cholesky_factor_corr[K] L_Omega; // covariance matrix in cholesky form
  matrix[K,K] beta;
  vector[K] alpha;
  vector[K] lambda[N-1];
 
}

transformed parameters{
  matrix[K,K] L_Sigma_1;
  matrix[K,K] L_Sigma_2;

  /* beta = beta_hat_location + beta_hat_scale; */
  /* alpha = alpha_hat_location + alpha_hat_scale; */
  L_Sigma_1 = diag_pre_multiply(tau, L_Omega_1);
  L_Sigma_2 = diag_pre_multiply(tau, L_Omega_2);  
}

model{
  vector[K] mu[N-1];
  for (n in 1:N-1){

    mu[n] = exp(alpha) + beta*to_vector(Y[n]);
  }

  L_Omega_1 ~ lkj_corr_cholesky(1); // prior on Omega
  L_Omega_2 ~ lkj_corr_cholesky(1); // prior on Omega
  /* tau_location ~ cauchy(0,1); */
  /* tau_scale ~ cauchy(0,1); */
  /* tau ~ normal(tau_location,tau_scale); */
  /* to_vector(beta_hat_location) ~ normal(0,0.5); */
  /* to_vector(beta_hat_scale) ~ cauchy(0,0.5); */
  /* alpha_hat_location ~ normal(0,1); */
  /* alpha_hat_scale ~ cauchy(0,1); */

  
  alpha ~ multi_normal_cholesky(0, L_Sigma_1)
  beta ~ multi_normal_cholesky(0, L_Sigma_2);
  
  // try this with map_rect, but maybe it can be better vectorized too

  // could map_rect_this
  for(n in 2:N)
    Y[n] ~ poisson(exp(lambda[n-1]) + mu[n-1]);
}

/* generated quantities{ */
/*   int y_new[forecast_len,K]; */
/*   vector[K] y_new_log[forecast_len]; */
/*   vector[K] lambda_new[forecast_len]; */
/*   cov_matrix[K] Sigma; */
/*   { */

/*     Sigma = multiply_lower_tri_self_transpose(L_Sigma); */
/*     lambda_new[1] = multi_normal_cholesky_rng(alpha+beta*Y_log[N], L_Sigma); */
/*     y_new[1] = poisson_log_rng(lambda_new[1]); */
/*     y_new_log[1] = to_vector(y_new[1]); */
/*     for (n in 2:forecast_len){ */
/*       lambda_new[n] = multi_normal_cholesky_rng(alpha + beta*y_new_log[n-1], L_Sigma); */
/*       y_new[n] = poisson_log_rng(lambda_new[n]); */
/*       y_new_log[n] = to_vector(y_new[n]); */
/*     } */
/*   } */
/* } */

