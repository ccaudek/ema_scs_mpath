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
  real beta_cs; // Coefficient for CS on UCS
  array[3] real beta_covariates; // Coefficients for other covariates
  
  // Non-centered parameterization for random effects
  vector[P] z_participant;
  vector[D] z_day;
  vector[M] z_measurement;

  real<lower=0> sigma_participant; // SD of participant intercepts
  real<lower=0> sigma_day; // SD of day intercepts
  real<lower=0> sigma_measurement; // SD of measurement intercepts
  real<lower=0> sigma_ucs; // Error term for UCS model
  real<lower=0> nu; // Degrees of freedom for t-distribution
}

model {
  // Priors
  alpha_ucs ~ normal(0, 0.5);
  beta_cs ~ normal(0, 1);
  beta_covariates ~ normal(0, 1);
  
  // Priors for non-centered parameters
  z_participant ~ normal(0, 1);
  z_day ~ normal(0, 1);
  z_measurement ~ normal(0, 1);
  
  sigma_participant ~ exponential(1);
  sigma_day ~ exponential(1);
  sigma_measurement ~ exponential(1);
  sigma_ucs ~ exponential(1);
  nu ~ gamma(2, 0.1);

  // Likelihood for UCS using t-distribution
  for (n in 1:N) {
    UCS[n] ~ student_t(
      nu,
      alpha_ucs + beta_cs * CS[n] +
      beta_covariates[1] * neg_affect[n] +
      beta_covariates[2] * decentering[n] +
      beta_covariates[3] * context_eval[n] +
      sigma_participant * z_participant[participant[n]] + // Adjusted for non-centered parametrization
      sigma_day * z_day[day[n]] + // Adjusted for non-centered parametrization
      sigma_measurement * z_measurement[measurement[n]], // Adjusted for non-centered parametrization
      sigma_ucs
    );
  }
}

generated quantities {
  array[N] real pred_UCS;
  array[N] real log_lik;

  for (n in 1:N) {
    // Generate predictive samples
    pred_UCS[n] = student_t_rng(
      nu,
      alpha_ucs + beta_cs * CS[n] +
      beta_covariates[1] * neg_affect[n] +
      beta_covariates[2] * decentering[n] +
      beta_covariates[3] * context_eval[n] +
      sigma_participant * z_participant[participant[n]] +
      sigma_day * z_day[day[n]] +
      sigma_measurement * z_measurement[measurement[n]],
      sigma_ucs
    );

    // Calculate log-likelihood for each observation
    log_lik[n] = student_t_lpdf(
      UCS[n] |
      nu,
      alpha_ucs + beta_cs * CS[n] +
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
