data {
  int<lower=0> N; // Total number of observations
  int<lower=0> P; // Number of participants
  int<lower=0> D; // Number of days
  int<lower=0> M; // Number of measurements per day per participant
  array[N] int<lower=1, upper=P> participant; // Participant index for each observation
  array[N] int<lower=1, upper=D> day; // Day index for each observation
  array[N] int<lower=1, upper=M> measurement; // Measurement index for each observation
  array[N] real CS; // Compassionate Self measures
  array[N] real UCS; // Uncompassionate Self measures
  array[N] real neg_affect; // Negative Affect measures
  array[N] real decentering; // Decentering measures
  array[N] real context_eval; // Context evaluation measures
}

parameters {
  real alpha_ucs; // Intercept for UCS
  real beta_cs; // Overall effect of CS on UCS
  array[3] real beta_covariates; // Coefficients for other covariates

  // Random intercepts
  vector[P] z_participant;
  vector[D] z_day;
  vector[M] z_measurement;

  // Random slopes for CS at the participant level
  vector[P] z_participant_slope_cs;

  real<lower=0> sigma_participant; // SD of participant intercepts
  real<lower=0> sigma_day; // SD of day intercepts
  real<lower=0> sigma_measurement; // SD of measurement intercepts
  real<lower=0> sigma_participant_slope_cs; // SD of participant slopes for CS
  real<lower=0> sigma_ucs; // Error term for UCS model
  real<lower=0> nu; // Degrees of freedom for t-distribution
}

model {
  // Priors
  alpha_ucs ~ normal(0, 1);
  beta_cs ~ normal(0, 1);
  beta_covariates ~ normal(0, 1);

  z_participant ~ normal(0, 1);
  z_day ~ normal(0, 1);
  z_measurement ~ normal(0, 1);
  z_participant_slope_cs ~ normal(0, 1);

  sigma_participant ~ exponential(1);
  sigma_day ~ exponential(1);
  sigma_measurement ~ exponential(1);
  sigma_participant_slope_cs ~ exponential(1);
  sigma_ucs ~ exponential(1);
  nu ~ gamma(2, 0.1);

  // Likelihood for UCS using t-distribution
  for (n in 1:N) {
    UCS[n] ~ student_t(
      nu,
      alpha_ucs +
      (beta_cs + sigma_participant_slope_cs * z_participant_slope_cs[participant[n]]) * CS[n] +
      beta_covariates[1] * neg_affect[n] +
      beta_covariates[2] * decentering[n] +
      beta_covariates[3] * context_eval[n] +
      sigma_participant * z_participant[participant[n]] +
      sigma_day * z_day[day[n]] +
      sigma_measurement * z_measurement[measurement[n]],
      sigma_ucs
    );
  }
}

generated quantities {
  array[N] real pred_UCS;
  array[N] real log_lik;

  for (n in 1:N) {
    pred_UCS[n] = student_t_rng(
      nu,
      alpha_ucs +
      (beta_cs + sigma_participant_slope_cs * z_participant_slope_cs[participant[n]]) * CS[n] +
      beta_covariates[1] * neg_affect[n] +
      beta_covariates[2] * decentering[n] +
      beta_covariates[3] * context_eval[n] +
      sigma_participant * z_participant[participant[n]] +
      sigma_day * z_day[day[n]] +
      sigma_measurement * z_measurement[measurement[n]],
      sigma_ucs
    );

    log_lik[n] = student_t_lpdf(
      UCS[n] |
      nu,
      alpha_ucs +
      (beta_cs + sigma_participant_slope_cs * z_participant_slope_cs[participant[n]]) * CS[n] +
      beta_covariates[1] * neg_affect[n] +
      beta_covariates[2] * decentering[n] +
      beta_covariates[3] * context_eval[n] +
      sigma_participant * z_participant[participant[n]] +
      sigma_day * z_day[day[n]] +
      sigma_measurement * z_measurement[measurement[n]],
      sigma_ucs
    );
  }
}
