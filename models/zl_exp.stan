data {
  int<lower=0> N;
  int y[N];
  int n[N];
  real yr[N];
}

parameters {
  real<lower=0, upper=1> p0;
  real<lower=0> beta;
}

transformed parameters {
  real<lower=0, upper=1> prob[N];
  
  for (i in 1:N) {
    prob[i] = p0 * (1 - exponential_cdf(yr[i], beta));
  }
}

model {
  for (i in 1:N) {
    target += binomial_lpmf(y[i] | n[i], prob[i]);
  }
}

