data {
  int<lower=0> N;
  int<lower=0> M;
  matrix[N, M] x;
  vector[N] y;
}
parameters {
  vector[M] beta;
  real<lower=0> sigma;
}
model {
  y ~ normal(x * beta, sigma);
      for(i in 1:M)
        beta[i] ~ normal(0, 1000);
   sigma ~ chi_square(2); 
}
