data {
  int<lower=1> N; // Number of observations
  int<lower=1> J; // Number of subjects
  int<lower=1> K; // Number of days per subject
  array[N] int<lower=1, upper=J> subj; // Subject index
  array[N] int<lower=1, upper=K> day; // Day index
  array[N] real psc; // Dependent variable
  array[N] real exam_day_pre; // 1 if exam day is 'pre', 0 otherwise
  array[N] real exam_day_post; // 1 if exam day is 'post', 0 otherwise
}

parameters {
  real alpha; // Global intercept
  array[J] real alpha_j; // Random intercepts for subjects
  array[J, K] real alpha_k; // Random intercepts for days nested within subjects
  real beta_pre; // Effect of exam day 'pre'
  real beta_post; // Effect of exam day 'post'
  array[J] real beta_j_pre; // Random slopes for exam_day_pre
  array[J] real beta_j_post; // Random slopes for exam_day_post
  real<lower=0> sigma; // Standard deviation for psc
  real<lower=0> sigma_j; // SD for subject random intercepts
  real<lower=0> sigma_k; // SD for day random intercepts
  real<lower=0> sigma_beta_j; // SD for random slopes
}

model {
  // Priors
  alpha ~ normal(0, 2.5);
  alpha_j ~ normal(0, sigma_j);
  for (j in 1:J) {
    alpha_k[j] ~ normal(0, sigma_k);
  }
  beta_pre ~ normal(0, 1);
  beta_post ~ normal(0, 1);
  beta_j_pre ~ normal(0, sigma_beta_j);
  beta_j_post ~ normal(0, sigma_beta_j);
  sigma ~ exponential(1);
  sigma_j ~ exponential(1);
  sigma_k ~ exponential(1);
  sigma_beta_j ~ exponential(1);

  // Likelihood
  for (n in 1:N) {
    psc[n] ~ student_t(
      3, alpha + alpha_j[subj[n]] + alpha_k[subj[n]][day[n]] + 
      beta_pre * exam_day_pre[n] + beta_post * exam_day_post[n] + 
      beta_j_pre[subj[n]] * exam_day_pre[n] + beta_j_post[subj[n]] * exam_day_post[n], 
      sigma);
  }
}

generated quantities {
  array[N] real y_rep; // Posterior predictive samples
  array[N] real log_lik; // Log-likelihood for each observation

  for (n in 1:N) {
    // Generate posterior predictive samples
    y_rep[n] = student_t_rng(
      3, 
      alpha + alpha_j[subj[n]] + alpha_k[subj[n]][day[n]] + 
      beta_pre * exam_day_pre[n] + beta_post * exam_day_post[n] + 
      beta_j_pre[subj[n]] * exam_day_pre[n] + beta_j_post[subj[n]] * exam_day_post[n], 
      sigma);

    // Calculate the log-likelihood for each observation
    log_lik[n] = student_t_lpdf(
      psc[n] | 3, 
      alpha + alpha_j[subj[n]] + alpha_k[subj[n]][day[n]] + 
      beta_pre * exam_day_pre[n] + beta_post * exam_day_post[n] + 
      beta_j_pre[subj[n]] * exam_day_pre[n] + beta_j_post[subj[n]] * exam_day_post[n], 
      sigma);
  }
}






