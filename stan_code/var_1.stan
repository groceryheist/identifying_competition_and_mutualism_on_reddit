// stan_code/var_1.stan

data{
  int<lower=0> forecast_len;  // length of the forecast to generate
  int<lower=0> N; // length of the time series
  int<lower=0> K; // number of series
  vector[K] Y[N]; // output matrix
}

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
 
}

transformed parameters{
  matrix[K,K] L_Sigma;

  /* beta = beta_hat_location + beta_hat_scale; */
  /* alpha = alpha_hat_location + alpha_hat_scale; */
  L_Sigma = diag_pre_multiply(tau, L_Omega);
}

model{
  vector[K] mu[N-1];

  for (n in 1:N-1){
    mu[n] = alpha + beta*Y[n];
  }

  L_Omega ~ lkj_corr_cholesky(1); // prior on Omega
  /* tau_location ~ cauchy(0,1); */
  /* tau_scale ~ cauchy(0,1); */
  /* tau ~ normal(tau_location,tau_scale); */
  /* to_vector(beta_hat_location) ~ normal(0,0.5); */
  /* to_vector(beta_hat_scale) ~ cauchy(0,0.5); */
  /* alpha_hat_location ~ normal(0,1); */
  /* alpha_hat_scale ~ cauchy(0,1); */

  to_vector(beta) ~ normal(0,0.8);
  alpha ~ normal(0,4);
  
  // try this with map_rect, but maybe it can be better vectorized too

  Y[2:N] ~ multi_normal_cholesky(mu, L_Sigma);
}
generated quantities{
  vector[K] y_new[forecast_len];
  cov_matrix[K] Sigma;
  {
    Sigma = multiply_lower_tri_self_transpose(L_Sigma);
    y_new[1] = multi_normal_cholesky_rng(alpha+beta*Y[N], L_Sigma);
    for (n in 2:forecast_len){
      y_new[n] = multi_normal_cholesky_rng(alpha + beta*y_new[n-1], L_Sigma);
    }
  }
}

