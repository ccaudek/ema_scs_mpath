// This is the final model for the test of the Bipolar Continuum Hypothesis
// by using 5 measurements within a single day, for all participants.

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
  real alpha_ucs; // Intercept for UCS
  real beta_cs; // Coefficient for CS on UCS
  array[3] real beta_covariates; // Coefficients for other covariates
  array[P] real participant_intercept; // Random intercepts for participants
  real<lower=0> sigma_participant; // SD of participant intercepts
  real<lower=0> sigma_ucs; // Error term for UCS model
}

model {
  // Priors
  alpha_ucs ~ normal(0, 2.5);
  beta_cs ~ normal(0, 1);
  beta_covariates ~ normal(0, 1);
  participant_intercept ~ normal(0, 1);
  sigma_participant ~ exponential(1);
  sigma_ucs ~ exponential(1);

  // Likelihood for UCS
  for (n in 1:N) {
    UCS[n] ~ normal(alpha_ucs + beta_cs * CS[n] +
                    beta_covariates[1] * neg_affect[n] +
                    beta_covariates[2] * decentering[n] +
                    beta_covariates[3] * context_eval[n] +
                    participant_intercept[participant[n]],
                    sigma_ucs);
  }
}

generated quantities {
  array[N] real pred_UCS;
  for (n in 1:N) {
    pred_UCS[n] = normal_rng(alpha_ucs + beta_cs * CS[n] +
                             beta_covariates[1] * neg_affect[n] +
                             beta_covariates[2] * decentering[n] +
                             beta_covariates[3] * context_eval[n] +
                             participant_intercept[participant[n]],
                             sigma_ucs);
  }
}
