// stan_code/var_1_pois.stan
// a poisson var model for count data generated by a VAR poisson process

data{
  int<lower=0> forecast_len;  // length of the forecast to generate
  int<lower=0> N; // length of the time series
  int<lower=0> K; // number of series
  // array notation is [rows, columns]
  int<lower=0> Y[K,N]; // output matrix
}

parameters{
  vector<lower=0>[K] tau;
  // LJK parameterize the covariance matrix 
  cholesky_factor_corr[K] L_Omega; // covariance matrix in cholesky form

  // scaling trick from https://discourse.mc-stan.org/t/help-with-poisson-model/3352/45
  // just a trick to avoid integer overflow in rng
  matrix[K,K] beta;
  vector[K] alpha;
  
  // latent poisson parameters
  vector[K] lambda[N];
  
}

transformed parameters{
  matrix[K,K] L_Sigma;

  /* beta = beta_hat_location + beta_hat_scale; */
  /* alpha = alpha_hat_location + alpha_hat_scale; */
  L_Sigma = diag_pre_multiply(tau, L_Omega);
}

model{
  // it's possible to have divergent samples that will lead mu to blow up and throw exceptions.
  vector[K] mu[N];
  mu[1] = (alpha + lambda[1]);
  for (n in 2:N){
    mu[n] = alpha + beta*(mu[n-1]) + lambda[n];
  }

  tau ~ cauchy(0,10);
  lambda[1] ~ cauchy(0,1);
  lambda[2:N] ~ multi_normal_cholesky(rep_vector(0.0,K), L_Sigma);
  L_Omega ~ lkj_corr_cholesky(1.0); // prior on Omega

  // we can try constraining beta to be stationary.
  // It probably won't work. This means that we'll have exceptions to ignore.
  to_vector(beta) ~ normal(0,0.8);
  alpha ~ cauchy(0,20); // intercepts can be all over 
  
  // try this with map_rect, but maybe it can be better vectorized too
  for(k in 1:K)
    Y[k] ~ poisson_log(mu[1:N,k]);
}

generated quantities{
  // stan doesn't have a 64 bit rng, so let's just forecast lambdas
  // and then we'll generate mu/y in R/python downstream.
  vector[K] lambda_new[forecast_len];
 cov_matrix[K] Sigma;
  {
    // just so we output the covariance matrix
    Sigma = multiply_lower_tri_self_transpose(L_Sigma);
    lambda_new[1]  = multi_normal_cholesky_rng(rep_vector(0.0,K), L_Sigma);
    for (n in 2:forecast_len){
      lambda_new[n] = multi_normal_cholesky_rng(rep_vector(0.0,K), L_Sigma);
    }
  }
}