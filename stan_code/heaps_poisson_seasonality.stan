functions {
  /* Function to compute the matrix square root */
  /* This approach uses diagonalization */ 
  matrix sqrtm(matrix A) {
    int m = rows(A);
    vector[m] root_root_evals = sqrt(sqrt(eigenvalues_sym(A)));
    matrix[m, m] evecs = eigenvectors_sym(A);
    matrix[m, m] eprod = diag_post_multiply(evecs, root_root_evals);
    return tcrossprod(eprod);
  }
  /* Function to transform A to P (inverse of part 2 of reparameterisation) */
  matrix AtoP(matrix A) {
    int m = rows(A);
    matrix[m, m] B = tcrossprod(A);
    for(i in 1:m) B[i, i] += 1.0;
    return mdivide_left_spd(sqrtm(B), A);
  }
  /* Function to perform the reverse mapping from Appendix A.2. The details of
     how to perform Step 1 are in Section S2.3 of the Supplementary Materials.
     Returned: a (2 x p) array of (m x m) matrices; the (1, i)-th component
               of the array is phi_i and the (2, i)-th component of the array
               is Gamma_{i-1}*/
  matrix[,] rev_mapping(matrix[] P, matrix Sigma) {
    int p = size(P);
    int m = rows(Sigma);
    matrix[m, m] phi_for[p, p];   matrix[m, m] phi_rev[p, p];
    matrix[m, m] Sigma_for[p+1];  matrix[m, m] Sigma_rev[p+1];
    matrix[m, m] S_for;           matrix[m, m] S_rev;
    matrix[m, m] S_for_list[p+1];
    matrix[m, m] Gamma_trans[p+1];
    matrix[m, m] phiGamma[2, p];
    // Step 1:
    Sigma_for[p+1] = Sigma;
    S_for_list[p+1] = sqrtm(Sigma);
    for(s in 1:p) {
      // In this block of code S_rev is B^{-1} and S_for is a working matrix
      S_for = - tcrossprod(P[p-s+1]);
      for(i in 1:m) S_for[i, i] += 1.0;
      S_rev = sqrtm(S_for);
      S_for_list[p-s+1] = mdivide_right_spd(mdivide_left_spd(S_rev, 
                              sqrtm(quad_form_sym(Sigma_for[p-s+2], S_rev))), S_rev);
      Sigma_for[p-s+1] = tcrossprod(S_for_list[p-s+1]);
    }
    // Step 2:
    Sigma_rev[1] = Sigma_for[1];
    Gamma_trans[1] = Sigma_for[1];
    for(s in 0:(p-1)) {
      S_for = S_for_list[s+1];
      S_rev = sqrtm(Sigma_rev[s+1]);
      phi_for[s+1, s+1] = mdivide_right_spd(S_for * P[s+1], S_rev);
      phi_rev[s+1, s+1] = mdivide_right_spd(S_rev * P[s+1]', S_for);
      Gamma_trans[s+2] = phi_for[s+1, s+1] * Sigma_rev[s+1];
      if(s>=1) {
        for(k in 1:s) {
          phi_for[s+1, k] = phi_for[s, k] - phi_for[s+1, s+1] * phi_rev[s, s-k+1];
          phi_rev[s+1, k] = phi_rev[s, k] - phi_rev[s+1, s+1] * phi_for[s, s-k+1];
        }
        for(k in 1:s) Gamma_trans[s+2] = Gamma_trans[s+2] + phi_for[s, k] * Gamma_trans[s+2-k];
      }
      Sigma_rev[s+2] = Sigma_rev[s+1] - quad_form_sym(Sigma_for[s+1], phi_rev[s+1, s+1]');
    }
    for(i in 1:p) phiGamma[1, i] = phi_for[p, i];
    for(i in 1:p) phiGamma[2, i] = Gamma_trans[i]';
    return phiGamma;
  }


}

// for the poisson model things are the same except at the latent lambda level and the poisson_log link.
data {
  int<lower=1> m; // Dimension of observation vector
  int<lower=1> p; // Order of VAR model
  int<lower=1> N; // Length of time series
  int<lower=0> y[m,N]; // output matrix
  // Hyperparameters in exchangeable prior for the A_i (see Section 3.2.1)
  vector[2] es;
  vector<lower=0>[2] fs;
  vector<lower=0>[2] gs;
  vector<lower=0>[2] hs;
  // Hyperparameters in exchangeable inverse Wishart prior for Sigma
  real<lower=0> scale_diag;                    // Diagonal element in scale matrix
  real<lower=-scale_diag/(m-1)> scale_offdiag; // Off-diagonal element in scale matrix
  real<lower=m+3> df;                          /* Degrees of freedom (limit ensures 
                                                 finite variance) */
  real alpha; //hyperparameters in prior for mu

  real<lower=0> sd0; 
  real<lower=0> g0;
  real<lower=0> h0;

  int<lower=0> forecast_len;

  int <lower=0> n_seas; // number of series with seasonality
  int <lower=1> seasonality_idx[n_seas]; // which series have seasonality
  int <lower=1> n_seas_levels[n_seas]; // now many levels does each seasonal series have?
  int <lower=1> season[n_seas,N+forecast_len]; // seasonality data (dummy vars);
}

transformed data {

  matrix[m, m] scale_mat;            // Scale-matrix in prior for Sigma
  vector[0] global[m];
  real xr[m,0];

  for(i in 1:m) {
    for(j in 1:m) {
      if(i==j) scale_mat[i, j] = scale_diag;
      else scale_mat[i, j] = scale_offdiag;
    }
  }
}

parameters {
  matrix[m, m] A[p];   // The A_i
  cov_matrix[m] Sigma; // Error variance, Sigma
  // Means and precisions in top-level prior for the diagonal and off-diagonal
  // elements in the A_i
  vector[m] mu; // mean of VAR process
  real mu0;
  real<lower=0> omega0;

  vector[p] Amu[2];
  vector<lower=0>[p] Aomega[2];
  vector[m] lambda[N]; 
  real beta_seas[sum(n_seas_levels)]; // seasonality coefficients  
}

transformed parameters {
  vector[p*m] lambda_1top;                // lambda_1, ..., lambda_p
  matrix[m, m] phi[p];    // The phi_i
  cov_matrix[p*m] Gamma; // (Stationary) variance of (y_1, ..., y_p)

  {
    matrix[m, m] P[p];
    matrix[m, m] phiGamma[2, p];
    for(i in 1:p) P[i] = AtoP(A[i]);
    phiGamma = rev_mapping(P, Sigma);
    phi = phiGamma[1];
    for(i in 1:p) {
      for(j in 1:p) {
        if(i<=j) Gamma[((i-1)*m+1):(i*m), ((j-1)*m+1):(j*m)] = phiGamma[2, j-i+1];
        else Gamma[((i-1)*m+1):(i*m), ((j-1)*m+1):(j*m)] = phiGamma[2, i-j+1]';
      }
    }
  }

  for(t in 1:p) lambda_1top[((t-1)*m+1):(t*m)] = lambda[t];
  
}

model {
  vector[p*m] mut_init;    // Marginal mean of (lambda_1^T, ..., lambda_p^T)^T
  vector[m] mut_rest[N-p]; // Conditional means of lambda_{p+1}, ..., lambda_{N}

  // Likelihood:
  for(t in 1:p) mut_init[((t-1)*m+1):(t*m)] = mu;
  for(t in (p+1):N) {
    mut_rest[t-p] = mu;
    for(i in 1:p) {
      // do this without multiplying parameters by parameters. That's a recipe for failure. 
      mut_rest[t-p] += phi[i] * (lambda[t-i] - mu);
    }
  }
  {
    int sea_beta_idx = 1;
    // for each communtiy with seasonality
    for(s in 1:n_seas){
      int i;
      // i is the current m
      i = seasonality_idx[s];
      // for each level of seasonality
      // remember vectorization in stan only helps for sampling. We're writing C++.
      for(j in 1:n_seas_levels[s]){
	for(t in 1:p){
	  if(season[s,t] == j)
	    mut_init[((t-1)*m+1)+i-1] += beta_seas[sea_beta_idx];
	}
	for(t in (p+1):N){
	  if(season[s,t] == j)
	    mut_rest[t-p,i] += beta_seas[sea_beta_idx];
	}
	sea_beta_idx = 1 + sea_beta_idx;
      }
    }
  }


  beta_seas ~ normal(0,2);

  lambda_1top ~ multi_normal(mut_init, Gamma);
  
  lambda[(p+1):N] ~  multi_normal(mut_rest, Sigma);

  for(i in 1:m){
    y[i] ~ poisson_log(lambda[1:N,i]);
  }

  // Prior:
  mu ~ normal(mu0, omega0);

  Sigma ~ inv_wishart(df, scale_mat);
  for(s in 1:p) {
    diagonal(A[s]) ~ normal(Amu[1, s], 1 / sqrt(Aomega[1, s]));
    for(i in 1:m) {
      for(j in 1:m) {
        if(i != j) A[s, i, j] ~ normal(Amu[2, s], 1 / sqrt(Aomega[2, s]));
      }
    }
  }
  // Hyperprior:
  for(i in 1:2) {
    Amu[i] ~ normal(es[i], fs[i]);
    Aomega[i] ~ gamma(gs[i], hs[i]); 
  }
  
  mu0 ~ normal(alpha, sd0);
  omega0 ~ gamma(g0, h0);
}


generated quantities {
  vector[m] lambda_new[forecast_len];
  vector[m] mut_forecast[forecast_len];

  vector[m] lambda_new_no_seasonality[forecast_len];
  vector[m] mut_forecast_no_seasonality[forecast_len];

  // for each of the first p forecasts, we need information from lambda
  {
    int sea_beta_idx;

    // here t indexes the index of the forecast
    for(t in 1:p){
      mut_forecast[t] = mu;
      // i indexes the lag
      for(i in 1:p){
	if(t <= i) mut_forecast[t] += phi[i] * (lambda[N-i+t] - mu);
	else mut_forecast[t] += phi[i] * (lambda_new[t-i] - mu);
      }
      mut_forecast_no_seasonality[t] = mut_forecast[t];
      lambda_new_no_seasonality[t] = multi_normal_rng(mut_forecast[t],Sigma);

      sea_beta_idx = 1;
      for(s in 1:n_seas){
	int i;
	// i is the current m
	i = seasonality_idx[s];
	for(j in 1:n_seas_levels[s]){
	  // this is the bug. should be season N +tt
	  if(season[s,t+N] == j)
	    mut_forecast[t,i] += beta_seas[sea_beta_idx];
	}
	sea_beta_idx = 1 + sea_beta_idx;
      }
      lambda_new[t] = multi_normal_rng(mut_forecast[t], Sigma);
    }
  }
  {
    int sea_beta_idx = 1;

    for(t in (p+1):forecast_len){
      mut_forecast[t] = mu;
      for(i in 1:p){
	mut_forecast[t] += phi[i] * (lambda_new[t-i] - mu);
      }
     
      mut_forecast_no_seasonality[t] = mut_forecast[t];
      lambda_new_no_seasonality[t] = multi_normal_rng(mut_forecast[t],Sigma);

     for(s in 1:n_seas){
       int i;
     // i is the current m
       sea_beta_idx = 1;
       i = seasonality_idx[s];
       for(j in 1:n_seas_levels[s]){
	 if(season[s,t] == j)
	   mut_forecast[t,i] += beta_seas[sea_beta_idx];
       }
       sea_beta_idx = 1 + sea_beta_idx;
     }
     lambda_new[t] = multi_normal_rng(mut_forecast[t], Sigma);
   }
  }
    // for each communtiy with seasonality
      // for each level of seasonality
      // remember vectorization in stan only helps for sampling. We're writing C++.

}
