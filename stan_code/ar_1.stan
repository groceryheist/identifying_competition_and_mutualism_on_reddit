// stan_code/ar_1.stan
// the model fits a vector y, of N datapoints

data {
  int<lower=0> N;
  vector[N] y;
  int<lower=0> forecast_len;
}

parameters{
  real alpha; // the intercept / growth rate
  real beta; // autoregressive coefficient
  real<lower=0> sigma; // outcome variance
}

model {

  beta ~ normal(0,0.8); // place a relatively narrow prior over beta because we expect a stationary distribtion so beta \in (-1,1).

  alpha ~ normal(0, 10); // alpha gets a diffuse prior
  
  sigma ~ normal(0, 10); // stan recommended prior for variances

  y[2:N] ~ normal(alpha + beta * y[1:(N-1)], sigma); // fit the model in vectorized form
}

// use this block to generate time series with the same parameters
generated quantities{
  vector[forecast_len] y_new;
  {
    y_new[1] = y[N];
    for (n in 2:forecast_len)
      y_new[n] = normal_rng(alpha + beta * y_new[n-1], sigma);
  }
}
