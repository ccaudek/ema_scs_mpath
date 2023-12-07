data {
  int<lower=0> N; // Total number of observations
  int<lower=0> P; // Number of participants
  int<lower=0> D; // Number of days
  array[N] int<lower=1, upper=P> participant; // Participant index for each observation
  array[N] int<lower=1, upper=D> day; // Day index for each observation
  array[N] real CS; // Compassionate Self measures
  array[N] real UCS; // Uncompassionate Self measures
  array[N] real neg_affect; // Negative Affect measures
  array[N] real decentering; // Decentering measures
  array[N] real context_eval; // Context evaluation measures
}

parameters {
  // Fixed effects
  real alpha_cs; // Intercept for CS
  vector[3] beta_cs; // Coefficients for CS
  real a; // Intercept for UCS as a function of CS
  real<lower=0> b; // Inverse scaling factor for UCS
  real<lower=2> nu; // Degrees of freedom for t-distribution

  // Random intercepts for participants
  array[P] real participant_intercept_cs;
  
  // Random intercepts for days within participants
  array[P] vector[D] day_intercept_cs;

  // Standard deviations for random effects
  real<lower=0> sigma_participant_cs;
  real<lower=0> sigma_day_cs;
  
  // Error terms
  real<lower=0> sigma_cs; // Error term for CS model
}

model {
  // Priors
  alpha_cs ~ normal(0, 5);
  beta_cs ~ normal(0, 1);
  a ~ normal(0, 5);
  b ~ normal(1, 0.5); 
  nu ~ gamma(2, 0.1); 
  
  participant_intercept_cs ~ normal(0, sigma_participant_cs);
  for (p in 1:P) {
    day_intercept_cs[p] ~ normal(0, sigma_day_cs);
  }
  
  sigma_participant_cs ~ exponential(1);
  sigma_day_cs ~ exponential(1);
  sigma_cs ~ exponential(1);

  // Likelihood for CS
  for (n in 1:N) {
    real mu_cs = alpha_cs + beta_cs[1] * neg_affect[n] + beta_cs[2] * decentering[n] + beta_cs[3] * context_eval[n] + participant_intercept_cs[participant[n]] + day_intercept_cs[participant[n], day[n]];
    CS[n] ~ student_t(nu, mu_cs, sigma_cs);
  }

  // Likelihood for UCS using t-distribution
  for (n in 1:N) {
    UCS[n] ~ student_t(nu, a - b * CS[n], sigma_cs);
  }
}

generated quantities {
  // Posterior predictive samples and log-likelihood
  array[N] real y_rep_CS;
  array[N] real y_rep_UCS;
  array[N] real log_lik_CS;
  array[N] real log_lik_UCS;

  for (n in 1:N) {
    real mu_cs = alpha_cs + beta_cs[1] * neg_affect[n] + beta_cs[2] * decentering[n] + beta_cs[3] * context_eval[n] + participant_intercept_cs[participant[n]] + day_intercept_cs[participant[n], day[n]];
    y_rep_CS[n] = student_t_rng(nu, mu_cs, sigma_cs);
    y_rep_UCS[n] = student_t_rng(nu, a - b * y_rep_CS[n], sigma_cs);
    log_lik_CS[n] = student_t_lpdf(CS[n] | nu, mu_cs, sigma_cs);
    log_lik_UCS[n] = student_t_lpdf(UCS[n] | nu, a - b * CS[n], sigma_cs);
  }
}

