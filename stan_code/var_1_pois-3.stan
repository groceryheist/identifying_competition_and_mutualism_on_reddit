data {
  int<lower=0> N; // No. of time periods observed
  int<lower=0> K; // No. of variables in y_t
  int Y[N, K]; // Obs of state variables
}

transformed data {
  vector[K] Y_real[N]; // declare count as array with vectors (real) for lhs evaluation
  int Y_obs[K - 1, N]; // declare Y observations where lagged values are available
  for(n in 1:N) {
    Y_real[n] = to_vector(Y[n]);
    }

  for(n in 2:N) {
    Y_obs[n-1] = Y[n];
    }
}
parameters {
  matrix[K, K] beta;
  cholesky_factor_corr[K] L_Omega;
  vector<lower=0>[K] tau;
  vector[K] alpha; // constant of each Y
}
transformed parameters {
  matrix[K,K] L_Sigma;
  L_Sigma = diag_pre_multiply(tau, L_Omega);
}
model {
//  vector[nY] Y_lat[nT-nP]; // declare latent Y
  vector[nY] lambda[nT-nP]; // declare latent Y
  real Y_lat[nT - nP, nY]; // declare latent Y
  vector[nY] mus[nT-nP]; // declare mus_t
  for (t in 1:(nT-nP)) { // for each t
    mus[t] = A + C * X[t+nP]; // mu = A + C X
    for (p in 1:nP) // for each lag
      mus[t] += B[p] * Y_real[t+nP-p]; // mu = mu + B_p Y_t-p
  }
  L_corr_noise ~ lkj_corr_cholesky(2.0); // prior for correlation noise
  sd_noise ~ normal(0, 1); // prior for standard deviation noise
  A ~ normal(0, 1); // prior vor intercepts

  for (p in 1:nP)
    to_vector(B[p]) ~ normal(0, 1); // priors for B
  lambda ~ multi_normal_cholesky(mus,L_sigma);

  for(t in 1:nT) {
    Y_obs[t] ~ poisson_log(Y_lat[t]); // Y_lat is 2 dim array of real (equivalent to int)
    lambda[t] = to_vector(Y_lat[t]); // transorm to array
    }
}
generated quantities {
  matrix[nY,nY] Sigma;
  Sigma = L_sigma * L_sigma';
}
