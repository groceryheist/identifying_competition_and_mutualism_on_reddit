data{
  
  int N; //# observations
  int K; //# dimensions of Y
  int C; //# of countries
  int T; //# of time periods in the panel
  
  int<lower = 1, upper=C> country[N]; //country id for each obs
  int<lower = 1, upper=T> time[N]; //time period id for each obs

  matrix[N,K] Y; //the outcome matrix - each variable's time series stacked
  
}

parameters{
  
  //individual level
  corr_matrix[K] Omega_local[C]; //cholesky factor - correlation of errors btwn regions
  vector<lower = 0>[K] tau[C]; //scale for residuals
  matrix[K, K] z_beta[C]; //untransformed betas 
  vector[K] z_alpha[C]; //untransformed intercepts
  
  //hierarchical priors
  real<lower = 0, upper = 1> rho; //pooling coefficient
  corr_matrix[K] Omega_global;
  vector[K] tau_loc; //mean for variance scaling factor
  vector<lower=0>[K] tau_scale; //scale for tau
  matrix[K, K] bhat_location; //mean for prior on beta
  matrix<lower = 0>[K, K] bhat_scale; //scale for prior on beta
  vector[K] ahat_location; //means for prior on intercepts
  vector<lower = 0>[K] ahat_scale; //variance for intercept prior
  
  
}

transformed parameters{
  
  matrix[K, K] beta[C]; //VAR(1) coefficients, country specific
  vector[K] alpha[C]; //country specific intercepts
  corr_matrix[K] Omega[C];
  corr_matrix[K]
  corr_matrix[K] omega_all[C];
  
  for(c in 1:C){
    //recentering random effects
    alpha[c] = ahat_location + ahat_scale .*z_alpha[c];
    beta[c] = bhat_location + bhat_scale*z_beta[c];
    Omega[c] = rho*Omega_global + (1-rho)*Omega_local[c];
  }
  
  for(c in 1:C){
  omega_all[1:K, c] = to_vector(quad_form_diag(Omega[c], tau[c]));
  }
  
}

model{

  //hyperpriors
  rho ~ beta(2,2);
  tau_loc ~ cauchy(0,1);
  tau_scale ~ cauchy(0,1);
  ahat_location ~ normal(0,1);
  ahat_scale ~ cauchy(0, 1); 
  to_vector(bhat_location) ~ normal(0, 0.5);
  to_vector(bhat_scale) ~ cauchy(0, 0.5);
  Omega_global ~ lkj_corr(1);

  
  //hierarchical priors
  for(c in 1:C){
    //non-centered paramaterization to avoid convergence issues
    z_alpha[c] ~ normal(0, 1);
    to_vector(z_beta[c]) ~ normal(0, 1);
    tau[c] ~ normal(tau_loc, tau_scale);
    Omega_local[c] ~ lkj_corr(10);
  }
  
  //likelihood
  
  {
  
  for(n in 1:N){
    if(time[n] > 1){
      Y[n] ~ multi_normal(alpha[country[n]] + beta[country[n]]*Y[n-1]',
      omega_all[country[n]]);
    }
  }
  
  
}
}

