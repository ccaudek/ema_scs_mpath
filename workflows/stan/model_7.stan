data {
  int<lower=0> N; // Total number of observations
  int<lower=0> PD; // Number of participant-day combinations
  int<lower=0> P; // Number of participants
  array[N] int<lower=1, upper=PD> participant_day; // Combined participant-day index for each observation
  array[N] int<lower=1, upper=P> participant; // Participant index for each observation
  array[N] real CS; // Compassionate Self measures
  array[N] real UCS; // Uncompassionate Self measures
  array[N] real neg_affect;
  array[N] real decentering;
  array[N] real context_eval;
}

parameters {
  real alpha_cs; // Intercept for CS
  vector[3] beta_cs; // Coefficients for CS
  real alpha_ucs; // Intercept for UCS
  real<lower=0> b; // Scaling factor for UCS
  vector[PD] participant_day_effect; // Random effects for participant-day
  vector[P] alpha_skew; // Skewness parameter for each participant
  real<lower=0> sigma_cs; // Standard deviation for CS
  real<lower=0> sigma_ucs; // Standard deviation for UCS
}

model {
  // Priors
  alpha_cs ~ normal(0, 5);
  beta_cs ~ normal(0, 1);
  alpha_ucs ~ normal(0, 5);
  b ~ normal(1, 0.5);
  participant_day_effect ~ normal(0, 1);
  alpha_skew ~ normal(0, 1);
  sigma_cs ~ exponential(1);
  sigma_ucs ~ exponential(1);

  // Likelihood for CS
  for (n in 1:N) {
    CS[n] ~ normal(alpha_cs + beta_cs[1] * neg_affect[n] + beta_cs[2] * decentering[n] + beta_cs[3] * context_eval[n] + participant_day_effect[participant_day[n]], sigma_cs);
  }

  // Likelihood for UCS with participant-specific skewness
  for (n in 1:N) {
    UCS[n] ~ skew_normal(alpha_ucs - b * CS[n], sigma_ucs, alpha_skew[participant[n]]);
  }
}

generated quantities {
  array[N] real y_rep_CS;
  array[N] real y_rep_UCS;

  for (n in 1:N) {
    y_rep_CS[n] = normal_rng(alpha_cs + beta_cs[1] * neg_affect[n] + beta_cs[2] * decentering[n] + beta_cs[3] * context_eval[n] + participant_day_effect[participant_day[n]], sigma_cs);
    y_rep_UCS[n] = skew_normal_rng(alpha_ucs - b * y_rep_CS[n], sigma_ucs, alpha_skew[participant[n]]);
  }
}
