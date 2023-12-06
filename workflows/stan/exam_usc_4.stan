// adds to exam_sc_3.stan the skew normal distribution, to capture the asymmetry 
// in the data.

data {
  int<lower=1> N; // Number of observations
  int<lower=1> J; // Number of subjects
  int<lower=1> D; // Number of days
  int<lower=1> M; // Number of measurements per day
  array[N] int<lower=1, upper=J> subj; // Subject index
  array[N] int<lower=1, upper=D> day; // Day index
  array[N] int<lower=1, upper=M> meas; // Measurement index
  array[N] real nsc; // Dependent variable
  array[N] real exam_day_pre; // 1 if exam day is 'pre', 0 otherwise
  array[N] real exam_day_post; // 1 if exam day is 'post', 0 otherwise
}

parameters {
  real alpha; // Global intercept
  array[J] real alpha_j; // Random intercepts for subjects
  array[D] real alpha_d; // Random intercepts for days
  array[M] real alpha_m; // Random intercepts for measurements
  real beta_pre; // Main effect of exam day 'pre'
  real beta_post; // Main effect of exam day 'post'
  array[J] real beta_j_pre; // Random slopes for exam_day_pre
  array[J] real beta_j_post; // Random slopes for exam_day_post
  real<lower=0> sigma; // Standard deviation for psc
  real<lower=0> sigma_j; // SD for subject random intercepts
  real<lower=0> sigma_d; // SD for day random intercepts
  real<lower=0> sigma_m; // SD for measurement random intercepts
  real<lower=0> sigma_beta_j_pre; // SD for random slopes (pre)
  real<lower=0> sigma_beta_j_post; // SD for random slopes (post)
  real skewness; // Skewness parameter for the skew normal distribution
}

model {
  // Priors
  alpha ~ normal(0, 2.5);
  alpha_j ~ normal(0, sigma_j);
  alpha_d ~ normal(0, sigma_d);
  alpha_m ~ normal(0, sigma_m);
  beta_pre ~ normal(0, 1);
  beta_post ~ normal(0, 1);
  beta_j_pre ~ normal(0, sigma_beta_j_pre);
  beta_j_post ~ normal(0, sigma_beta_j_post);
  sigma ~ exponential(1);
  sigma_j ~ exponential(1);
  sigma_d ~ exponential(1);
  sigma_m ~ exponential(1);
  sigma_beta_j_pre ~ exponential(1);
  sigma_beta_j_post ~ exponential(1);
  skewness ~ normal(0, 1);

  // Likelihood
  for (n in 1:N) {
    nsc[n] ~ skew_normal(
      alpha + alpha_j[subj[n]] + alpha_d[day[n]] + alpha_m[meas[n]] +
      (beta_pre + beta_j_pre[subj[n]]) * exam_day_pre[n] +
      (beta_post + beta_j_post[subj[n]]) * exam_day_post[n],
      sigma, skewness
    );
  }
}

generated quantities {
  array[N] real y_rep;
  array[N] real log_lik;
  
  for (n in 1:N) {
    y_rep[n] = skew_normal_rng(
      alpha + alpha_j[subj[n]] + alpha_d[day[n]] + alpha_m[meas[n]] +
      (beta_pre + beta_j_pre[subj[n]]) * exam_day_pre[n] +
      (beta_post + beta_j_post[subj[n]]) * exam_day_post[n],
      sigma, skewness
    );
    
    log_lik[n] = skew_normal_lpdf(
      nsc[n] |
      alpha + alpha_j[subj[n]] + alpha_d[day[n]] + alpha_m[meas[n]] +
      (beta_pre + beta_j_pre[subj[n]]) * exam_day_pre[n] +
      (beta_post + beta_j_post[subj[n]]) * exam_day_post[n],
      sigma, skewness
    );
  }
}

