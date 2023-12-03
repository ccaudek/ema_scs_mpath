data {
  int<lower=0> N; // Total number of observations
  int<lower=0> P; // Number of participants
  array[N] int<lower=1, upper=P> participant; // Participant index for each observation
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

  // Random intercepts for participants
  array[P] real participant_intercept_cs;
  real<lower=0> sigma_participant_cs; // SD of participant intercepts for CS

  // Error terms
  real<lower=0> sigma_cs; // Error term for CS model
}

model {
  // Priors
  alpha_cs ~ normal(0, 5);
  beta_cs ~ normal(0, 1);
  a ~ normal(0, 5);
  b ~ normal(1, 0.5); // Assuming a positive scaling factor
  participant_intercept_cs ~ normal(0, 1);
  sigma_participant_cs ~ exponential(1);
  sigma_cs ~ exponential(1);

  // Likelihood for CS
  for (n in 1:N) {
    CS[n] ~ normal(alpha_cs + beta_cs[1] * neg_affect[n] + beta_cs[2] * decentering[n] + beta_cs[3] * context_eval[n] + participant_intercept_cs[participant[n]], sigma_cs);
  }

  // UCS is defined as a function of CS
  for (n in 1:N) {
    UCS[n] ~ normal(a - b * CS[n], sigma_cs); // Assuming same error term as CS
  }
}

generated quantities {
  // Posterior predictive samples
  array[N] real y_rep_CS;
  array[N] real y_rep_UCS;

  // Log-likelihood for each observation
  array[N] real log_lik_CS;
  array[N] real log_lik_UCS;

  for (n in 1:N) {
    // Generate posterior predictive samples
    y_rep_CS[n] = normal_rng(alpha_cs + beta_cs[1] * neg_affect[n] + beta_cs[2] * decentering[n] + beta_cs[3] * context_eval[n] + participant_intercept_cs[participant[n]], sigma_cs);
    y_rep_UCS[n] = a - b * y_rep_CS[n];

    // Calculate the log-likelihood
    log_lik_CS[n] = normal_lpdf(CS[n] | alpha_cs + beta_cs[1] * neg_affect[n] + beta_cs[2] * decentering[n] + beta_cs[3] * context_eval[n] + participant_intercept_cs[participant[n]], sigma_cs);
    log_lik_UCS[n] = normal_lpdf(UCS[n] | a - b * CS[n], sigma_cs);
  }
}









