data {
  int<lower=0> N; // Total number of observations
  int<lower=0> PD; // Number of participant-day combinations
  array[N] int<lower=1, upper=PD> participant_day; // Combined participant-day index for each observation
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

  // Skewness parameters
  real alpha_skew_cs; // Skewness parameter for CS
  real alpha_skew_ucs; // Skewness parameter for UCS

  // Combined participant-day random effects
  vector[PD] participant_day_effect;
  real<lower=0> sigma_participant_day; // SD for combined participant-day effect

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

  alpha_skew_cs ~ normal(0, 1); // Prior for CS skewness
  alpha_skew_ucs ~ normal(0, 1); // Prior for UCS skewness

  participant_day_effect ~ normal(0, sigma_participant_day);
  sigma_participant_day ~ exponential(1);
  sigma_cs ~ exponential(1);

  // Likelihood for CS using skew-normal distribution
  for (n in 1:N) {
    real mu_cs = alpha_cs + beta_cs[1] * neg_affect[n] + beta_cs[2] * decentering[n] + beta_cs[3] * context_eval[n] + participant_day_effect[participant_day[n]];
    CS[n] ~ skew_normal(mu_cs, sigma_cs, alpha_skew_cs);
  }

  // Likelihood for UCS using skew-normal distribution
  for (n in 1:N) {
    UCS[n] ~ skew_normal(a - b * CS[n], sigma_cs, alpha_skew_ucs);
  }
}

generated quantities {
  // Posterior predictive samples and log-likelihood
  array[N] real y_rep_CS;
  array[N] real y_rep_UCS;
  array[N] real log_lik_CS;
  array[N] real log_lik_UCS;

  for (n in 1:N) {
    real mu_cs = alpha_cs + beta_cs[1] * neg_affect[n] + beta_cs[2] * decentering[n] + beta_cs[3] * context_eval[n] + participant_day_effect[participant_day[n]];
    y_rep_CS[n] = skew_normal_rng(mu_cs, sigma_cs, alpha_skew_cs);
    y_rep_UCS[n] = skew_normal_rng(a - b * y_rep_CS[n], sigma_cs, alpha_skew_ucs);
    log_lik_CS[n] = skew_normal_lpdf(CS[n] | mu_cs, sigma_cs, alpha_skew_cs);
    log_lik_UCS[n] = skew_normal_lpdf(UCS[n] | a - b * CS[n], sigma_cs, alpha_skew_ucs);
  }
}
