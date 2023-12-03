data {
  int<lower=0> N;
  int<lower=0> PD;
  array[N] int<lower=1, upper=PD> participant_day;
  array[N] real CS;
  array[N] real UCS;
  array[N] real neg_affect;
  array[N] real decentering;
  array[N] real context_eval;
}

parameters {
  real alpha_cs;
  vector[3] beta_cs;
  real<lower=0> b; // You might consider re-evaluating this parameter as well
  vector[PD] participant_day_effect;
  real<lower=0> sigma_participant_day;
  real<lower=0> sigma_cs;
  real<lower=0> sigma_ucs;
}

model {
  // Priors
  alpha_cs ~ normal(0, 5);
  beta_cs ~ normal(0, 1);
  b ~ normal(1, 0.5); // Review this prior based on domain knowledge
  participant_day_effect ~ normal(0, sigma_participant_day);
  sigma_participant_day ~ exponential(1);
  sigma_cs ~ exponential(1);
  sigma_ucs ~ exponential(1);

  // Likelihood for CS
  for (n in 1:N) {
    CS[n] ~ normal(alpha_cs + beta_cs[1] * neg_affect[n] + beta_cs[2] * decentering[n] + beta_cs[3] * context_eval[n] + participant_day_effect[participant_day[n]], sigma_cs);
  }

  // Simplified model for UCS
  for (n in 1:N) {
    UCS[n] ~ normal(b * CS[n], sigma_ucs); // Directly relating UCS to CS
  }
}

generated quantities {
  array[N] real y_rep_CS;
  array[N] real y_rep_UCS;
  for (n in 1:N) {
    y_rep_CS[n] = normal_rng(alpha_cs + beta_cs[1] * neg_affect[n] + beta_cs[2] * decentering[n] + beta_cs[3] * context_eval[n] + participant_day_effect[participant_day[n]], sigma_cs);
    y_rep_UCS[n] = normal_rng(b * y_rep_CS[n], sigma_ucs);
  }
}
