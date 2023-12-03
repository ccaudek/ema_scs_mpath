library("brms")
library("here")
library("cmdstanr")
library("posterior")
options(posterior.num_args=list(sigfig=2)) # by default summaries with 2 significant digits
library("rstan")
library("ggplot2")
library("bayesplot")
color_scheme_set("brightblue")
theme_set(bayesplot::theme_default(base_family = "sans"))
library("loo")
library("tidyr")
library("dplyr")
options(pillar.neg=FALSE)
library("gridExtra")
library("ggdist")
SEED <- 48927 # set random seed for reproducability






d <- readRDS(here("data", "prep", "ema", "data_for_model_comparisons.rds"))

day1_df <- d |> 
  dplyr::filter(day == 1)

# Assuming your dataframe is named day1_df
# Ensure user_id is converted to a numeric factor for participant indexing
day1_df$user_id <- as.numeric(as.factor(day1_df$user_id))

# Prepare the list for Stan
stan_data <- list(
  N = nrow(day1_df),                                   # Total number of observations
  P = length(unique(day1_df$user_id)),                 # Number of unique participants
  participant = day1_df$user_id,                       # Participant index for each observation
  CS = day1_df$state_cs,                               # Compassionate Self measures
  UCS = day1_df$state_ucs,                             # Uncompassionate Self measures
  neg_affect = day1_df$na_moment,                      # Negative Affect measures
  decentering = day1_df$dec_moment,                    # Decentering measures
  context_eval = day1_df$con_moment                    # Context evaluation measures
)

# Verify the structure
str(stan_data)

# Compile the Stan model
stan_file <- 'model_2.stan'  # Adjust the path if your Stan file is in a different folder
mod <- cmdstan_model(stan_file)

# Fit the model to the data
fit <- mod$sample(
  data = stan_data,
  chains = 4,             # Number of MCMC chains
  parallel_chains = 4,    # Number of chains to run in parallel (adjust based on your CPU)
  iter_warmup = 2000,     # Number of warmup iterations per chain
  iter_sampling = 2000,   # Number of sampling iterations per chain
  seed = 1234             # Set a seed for reproducibility
)

# Check the summary of the model fit
print(fit$summary())

fit_draws <- fit$draws() # extract the posterior draws
mcmc_trace(fit_draws, pars = c("a", "b"))
mcmc_trace(fit_draws, pars = c("beta_cs[1]", "beta_cs[2]", "beta_cs[3]"))
mcmc_trace(fit_draws, pars = c("sigma_cs"))

parameters <- c(
  "alpha_cs", "beta_cs[1]", "beta_cs[2]", "beta_cs[3]", "a", "b",
  "sigma_participant_cs", "sigma_cs"
)

rhats <- rhat(fit, pars = parameters)
mcmc_rhat(rhats)

eff_ratio <- neff_ratio(fit, pars = parameters)
eff_ratio
mcmc_neff(eff_ratio)

mcmc_acf(fit_draws, pars = parameters)


y <- stan_data$CS

stanfit <- read_stan_csv(fit$output_files())
# extract the fitted values
y_rep <- extract(stanfit)[["y_rep_CS"]]
ppc_dens_overlay(y = y, yrep = y_rep[1:100, ])

y_rep <- extract(stanfit)[["y_rep_UCS"]]
ppc_dens_overlay(y = y, yrep = y_rep[1:100, ])

# Hierarchical model -----------------------------------------------------------

# Filter the data for day 1 (as per your current script)
day1_df <- d |> 
  dplyr::filter(day == 1)

# Convert user_id to a numeric factor for participant indexing
day1_df$user_id <- as.numeric(as.factor(day1_df$user_id))

# Create a day index - assuming 'day' is a column in your dataframe
# If days are already numbered sequentially and consistently across participants,
# you can use them directly. Otherwise, create a factor and then convert to numeric.
day1_df$day_index <- as.numeric(as.factor(day1_df$day))

# Prepare the list for Stan
stan_data <- list(
  N = nrow(day1_df),                                   # Total number of observations
  P = length(unique(day1_df$user_id)),                 # Number of unique participants
  D = length(unique(day1_df$day_index)),               # Number of days
  participant = day1_df$user_id,                       # Participant index for each observation
  day = day1_df$day_index,                             # Day index for each observation
  CS = day1_df$state_cs,                               # Compassionate Self measures
  UCS = day1_df$state_ucs,                             # Uncompassionate Self measures
  neg_affect = day1_df$na_moment,                      # Negative Affect measures
  decentering = day1_df$dec_moment,                    # Decentering measures
  context_eval = day1_df$con_moment                    # Context evaluation measures
)

# Compile the Stan model
stan_file <- 'model_4.stan'  # Adjust the path if your Stan file is in a different folder
mod <- cmdstan_model(stan_file)

# Fit the model to the data
fit <- mod$sample(
  data = stan_data,
  chains = 4,             # Number of MCMC chains
  parallel_chains = 4,    # Number of chains to run in parallel (adjust based on your CPU)
  iter_warmup = 2000,     # Number of warmup iterations per chain
  iter_sampling = 2000,   # Number of sampling iterations per chain
  seed = 1234             # Set a seed for reproducibility
)

# Check the summary of the model fit
print(fit$summary())

fit_draws <- fit$draws() # extract the posterior draws
mcmc_trace(fit_draws, pars = c("a", "b"))
mcmc_trace(fit_draws, pars = c("beta_cs[1]", "beta_cs[2]", "beta_cs[3]"))
mcmc_trace(fit_draws, pars = c("sigma_cs"))

parameters <- c(
  "alpha_cs", "beta_cs[1]", "beta_cs[2]", "beta_cs[3]", "a", "b",
  "sigma_participant_cs", "sigma_cs"
)

rhats <- rhat(fit, pars = parameters)
mcmc_rhat(rhats)

eff_ratio <- neff_ratio(fit, pars = parameters)
eff_ratio
mcmc_neff(eff_ratio)

mcmc_acf(fit_draws, pars = parameters)


y <- stan_data$CS

stanfit <- read_stan_csv(fit$output_files())
# extract the fitted values
y_rep <- extract(stanfit)[["y_rep_CS"]]
ppc_dens_overlay(y = y, yrep = y_rep[1:100, ])

y_rep <- extract(stanfit)[["y_rep_UCS"]]
ppc_dens_overlay(y = y, yrep = y_rep[1:100, ])


#################

# Assuming 'user_id' represents participants and 'day' represents days
day1_df$user_id <- as.numeric(as.factor(day1_df$user_id))
day1_df$day_index <- as.numeric(as.factor(day1_df$day))

# Create a unique index for each participant-day combination
day1_df$participant_day <- as.numeric(factor(paste(day1_df$user_id, day1_df$day_index, sep = "_")))

# Number of unique participants
P <- length(unique(day1_df$user_id))

# Prepare the list for Stan
stan_data <- list(
  N = nrow(day1_df),
  PD = length(unique(day1_df$participant_day)),
  P = P,
  participant_day = day1_df$participant_day,
  participant = day1_df$user_id,  # Participant index
  CS = day1_df$state_cs,
  UCS = day1_df$state_ucs,
  neg_affect = day1_df$na_moment,
  decentering = day1_df$dec_moment,
  context_eval = day1_df$con_moment
)

# Compile the Stan model
stan_file <- 'model_8.stan'  # Adjust the path if your Stan file is in a different folder
mod <- cmdstan_model(stan_file)

fit <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 8000,
  thin = 5, # Thinning the chains
  seed = 1234
)

# Check the summary of the model fit
print(fit$summary())

parameters <- c(
  "beta_cs", "alpha_ucs", 
  "beta_covariates[1]", "beta_covariates[2]", "beta_covariates[3]",
  # "participant_intercept",
  # "sigma_participant", 
  "sigma_ucs"
)

# summarise all variables with default and additional summary measures
fit$summary(
  variables = parameters,
  posterior::default_summary_measures(),
  extra_quantiles = ~posterior::quantile2(., probs = c(.0275, .975))
)

draws_arr <- fit$draws() # or format="array"
str(draws_arr)

mcmc_acf(draws_arr, pars = parameters)

# draws x variables data frame
draws_df <- fit$draws(format = "df")
str(draws_df)
print(draws_df)

color_scheme_set("blue")
mcmc_hist(fit$draws("beta_cs"))

mcmc_hist(fit$draws("beta_covariates[1]"))
mcmc_hist(fit$draws("beta_covariates[2]"))
mcmc_hist(fit$draws("beta_covariates[3]"))


str(fit$sampler_diagnostics())

# this is a draws_df object from the posterior package
str(fit$sampler_diagnostics(format = "df"))
fit$diagnostic_summary()

stanfit <- rstan::read_stan_csv(fit$output_files())
str(stanfit)

y_rep <- as.matrix(stanfit, pars = "pred_UCS")
dim(y_rep)

y <- stan_data$UCS
ppc_dens_overlay(y, y_rep[1:200, ])


# Now model 9 ------------------------------------------------------------------

# Convert 'user_id' and 'day' to numeric indices
d$user_id_numeric <- as.numeric(as.factor(d$user_id))
d$day_numeric <- as.numeric(as.factor(d$day))

# Get the number of unique participants and days
P <- length(unique(d$user_id_numeric))
D <- length(unique(d$day_numeric))

# Prepare the list for Stan
stan_data <- list(
  N = nrow(d),
  P = P,
  D = D,
  participant = d$user_id_numeric,
  day = d$day_numeric,
  CS = d$state_cs,
  UCS = d$state_ucs,
  neg_affect = d$na_moment,
  decentering = d$dec_moment,
  context_eval = d$con_moment
)

# Print the structure of the stan_data to confirm
str(stan_data)

# Compile the Stan model
stan_file <- 'model_9.stan'  # Adjust the path if your Stan file is in a different folder

# Sample from the posterior 
mod <- cmdstan_model(stan_file)

fit <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = SEED
)

# Check the summary of the model fit
fit$summary()

# Plot a histogram of the posterior draws with bayesplot 
draws <- fit$draws(format="df")
mcmc_hist(draws, pars="beta_cs") # + xlim(c(0,1))

draws |>
  ggplot(aes(x=beta_cs)) + 
  stat_dotsinterval() 

params <- c(
  "beta_cs", # "alpha_ucs", 
  "beta_covariates[1]", "beta_covariates[2]", "beta_covariates[3]",
  "sigma_ucs"
)

temps <- fit$draws(format = "df") |>
  as_tibble() |>
  select(all_of(params))

mcmc_areas(temps) + xlab('')

stanfit <- rstan::read_stan_csv(fit$output_files())

# In order to use the PPC functions from the bayesplot package we need a 
# vector y of outcome values
y <- stan_data$UCS

# and a matrix yrep of draws from the posterior predictive distribution
y_rep <- as.matrix(stanfit, pars = "pred_UCS")
# dim(y_rep)

# The first is a comparison of the distribution of y and the distributions of 
# some of the simulated datasets (rows) in the yrep matrix.
ppc_dens_overlay(y, y_rep[1:200, ])

ppc_ecdf_overlay(y, y_rep[sample(nrow(y_rep), 25), ])

# ECDF and ECDF difference plot of the PIT values of y compared to yrep
# with 99% simultaneous confidence bands.
ppc_pit_ecdf(y, y_rep, prob = 0.99, plot_diff = FALSE)

ppc_hist(y, y_rep[1:8, ])
ppc_boxplot(y, y_rep[1:8, ])
ppc_dens(y, y_rep[200:202, ])

ppc_stat(y, y_rep, stat = "mean")
ppc_stat(y, y_rep, stat = "median")
ppc_stat(y, y_rep, stat = "sd")

rhats <- rhat(fit, pars = params)
mcmc_rhat(rhats)

eff_ratio <- neff_ratio(fit, pars = params)
mcmc_neff(eff_ratio)

fit_draws <- fit$draws() # extract the posterior draws
mcmc_acf(fit_draws, pars = params)

rstan::traceplot(stanfit, pars = "beta_cs")


# fatto fino a qui! ------------------------------------------------------------

















#########################


fit_draws <- fit$draws() # extract the posterior draws
mcmc_trace(fit_draws, pars = c("beta_cs"))
# mcmc_trace(fit_draws, pars = c("beta_cs[1]", "beta_cs[2]", "beta_cs[3]"))
# mcmc_trace(fit_draws, pars = c("sigma_cs"))



rhats <- rhat(fit, pars = parameters)
mcmc_rhat(rhats)

eff_ratio <- neff_ratio(fit, pars = parameters)
eff_ratio
mcmc_neff(eff_ratio)

mcmc_acf(fit_draws, pars = parameters)

y <- stan_data$UCS

stanfit <- read_stan_csv(fit$output_files())
# extract the fitted values
y_rep <- extract(stanfit)[["pred_UCS"]]
ppc_dens_overlay(y = y, yrep = y_rep[1:100, ])

y_rep <- extract(stanfit)[["y_rep_UCS"]]
ppc_dens_overlay(y = y, yrep = y_rep[1:100, ])

stanfit <- rstan::read_stan_csv(fit$output_files())

list_of_draws <- extract(stanfit)
print(names(list_of_draws))



rstan::traceplot(stanfit, pars = "b")

mcmc_hist(fit$draws("b"))

# Extract posterior samples
posterior_samples <- as.data.frame(extract(stanfit))
summary(posterior_samples[c("b")])

# Posterior distribution plots for 'a' and 'b'
color_scheme_set("blue")
mcmc_areas(posterior_samples, pars = c("b"))

# Bayesian hypothesis testing for 'b'
b_positive <- mean(posterior_samples$b > 0)
print(b_positive)  # Proportion of posterior where 'b' is positive

parameters <- c(
  "alpha_cs"   ,            "beta_cs"   ,             "alpha_ucs"   ,          
  "b"          ,                 
  "sigma_cs"    ,           "sigma_ucs"
)

stan_plot(stanfit, pars = parameters)

fit_summary <- summary(stanfit, pars = parameters)
print(fit_summary$summary)

mu_b_summary <- summary(stanfit, pars = c("beta_cs"), probs = c(0.01, 0.99))$summary
print(mu_b_summary)

mu_beta_cov_summary <- summary(
  stanfit, pars = c("beta_covariates[1]", "beta_covariates[2]", "beta_covariates[3]"), probs = c(0.01, 0.99))$summary
print(mu_beta_cov_summary)


# this is a draws_array object from the posterior package
str(fit$sampler_diagnostics())

# this is a draws_df object from the posterior package
str(fit$sampler_diagnostics(format = "df"))

fit$diagnostic_summary()

stanfit <- rstan::read_stan_csv(fit$output_files())

mcmc_hist(fit$draws("beta_cs[1]"))
mcmc_hist(fit$draws("beta_cs[2]"))
mcmc_hist(fit$draws("beta_cs[3]"))

rstan::traceplot(stanfit, pars = "alpha_cs")
rstan::traceplot(stanfit, pars = "beta_cs[1]")
rstan::traceplot(stanfit, pars = "a")
rstan::traceplot(stanfit, pars = "b")

library(rstan)
library(bayesplot)

# Assuming stanfit is the object obtained from rstan::read_stan_csv(fit$output_files())
# Extract posterior samples
posterior_samples <- as.data.frame(extract(stanfit))

# Summary for 'a' and 'b'
summary(posterior_samples[c("a", "b")])












# Extract observed data
observed_CS <- stan_data$CS
observed_UCS <- stan_data$UCS

# Extract posterior predictive samples
posterior_samples <- extract(stanfit)
predicted_CS <- posterior_samples$pred_CS
predicted_UCS <- posterior_samples$pred_UCS

# Ensure the predicted data is in the correct format (matrix)
# If predicted_CS and predicted_UCS are not matrices, convert them
if (!is.matrix(predicted_CS)) {
  predicted_CS <- as.matrix(predicted_CS)
}
if (!is.matrix(predicted_UCS)) {
  predicted_UCS <- as.matrix(predicted_UCS)
}

# Reshape the predicted data into a matrix format
predicted_CS_matrix <- t(predicted_CS)
predicted_UCS_matrix <- t(predicted_UCS)

# Perform posterior predictive checks for CS
pp_check(y = observed_CS, yrep = predicted_CS_matrix, type = "dens_overlay")












# Assuming your data frame is named 'data' and has the columns:
# participant, day, CS, UCS, neg_affect, decentering, context_eval

d1 <- d
d1$nsc <- as.vector(scale(d1$nsc))
d1$psc <- as.vector(scale(d1$psc))
d1$neg_aff <- as.vector(scale(d1$neg_aff))
d1$context <- as.vector(scale(d1$context))
d1$dec <- as.vector(scale(d1$dec))


# Define priors
priors <- c(
  prior(normal(0, 5), nlpar = "a"),
  prior(normal(1, 0.5), nlpar = "b", lb = 0),
  prior(normal(0, 1), class = "b"),
  prior(normal(0, 1), class = "sd")
)

# Fit the model
fit <- brm(
  nlform, 
  data = d1, 
  backend = "cmdstanr",
  family = gaussian(), 
  prior = priors
)

# Check summary of the model
summary(fit)

