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
SEED <- 48927 # set random seed for reproducibility


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

# Model 9 is a hierarchical version of Model 8: For every subject, it uses the 
# data off all days, with 5 measurements for each day.

# Data preparation. 
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
stan_file <- 'model_9_t.stan'  # Adjust the path if your Stan file is in a different folder

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


# Model 9 with random intercept for moment -------------------------------------

# Convert 'user_id', 'day', and 'moment' to numeric indices
d$user_id_numeric <- as.numeric(as.factor(d$user_id))
d$day_numeric <- as.numeric(as.factor(d$day))
d$moment_numeric <- as.numeric(as.factor(d$moment))

# Get the number of unique participants, days, and measurements
P <- length(unique(d$user_id_numeric))
D <- length(unique(d$day_numeric))
M <- length(unique(d$moment_numeric))  # Assuming this is 5 as per your description

# Create a unique index for each observation combining participant, day, and measurement
d$pd_index <- as.numeric(factor(paste(d$user_id_numeric, d$day_numeric, d$moment_numeric, sep = "_")))

# Prepare the list for Stan
stan_data <- list(
  N = nrow(d),
  P = P,
  D = D,
  M = M,
  participant = d$user_id_numeric,
  day = d$day_numeric,
  measurement = d$moment_numeric,
  CS = d$state_cs,
  UCS = d$state_ucs,
  neg_affect = d$na_moment,
  decentering = d$dec_moment,
  context_eval = d$con_moment
)

# Print the structure of the stan_data to confirm
str(stan_data)

# Compile the Stan model
stan_file <- here::here("workflows", "stan", "model_9_rnd_slopes.stan") 
mod <- cmdstan_model(stan_file)

# Sample from the posterior 
fit <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  adapt_delta = 0.95,  
  max_treedepth = 12, 
  seed = SEED
)


# Post-processing ----

fit$diagnostic_summary()

names(mod$variables()$parameters)
# [1] "alpha_ucs"                  "beta_cs"                   
# [3] "beta_covariates"            "z_participant"             
# [5] "z_day"                      "z_measurement"             
# [7] "z_participant_slope_cs"     "sigma_participant"         
# [9] "sigma_day"                  "sigma_measurement"         
# [11] "sigma_participant_slope_cs" "sigma_ucs"                 
# [13] "nu"  

# Check the summary of the model fit
# fit$summary()

# Plot a histogram of the posterior draws with bayesplot 
draws <- fit$draws(format="df")
mcmc_hist(draws, pars="beta_cs") # + xlim(c(0,1))

draws |>
  ggplot(aes(x=beta_cs)) + 
  stat_dotsinterval() 

params <- c(
  "beta_cs",
  "beta_covariates[1]", "beta_covariates[2]", "beta_covariates[3]",
  "sigma_ucs",
  "sigma_participant", "sigma_day", "sigma_measurement", 
  "sigma_participant_slope_cs"
  # Including specific indices for random effects is necessary
  # e.g., "z_participant[1]", "z_day[1]", "z_measurement[1]", "z_participant_slope_cs[1]"
)

temps <- fit$draws(format = "df") |>
  as_tibble() |>
  select(all_of(params))

mcmc_intervals(temps, point_size = 1, prob = 0.50, prob_outer = 0.89) + xlab('')

# Your existing code for mcmc_intervals
p <- mcmc_intervals(temps, point_size = 1, prob = 0.50, prob_outer = 0.89) + xlab('')

# Figure for paper -------------------------------------------------------------

# Define the desired order of parameters
param_order <- c(
  "sigma_ucs",
  "sigma_participant_slope_cs",
  "sigma_participant", "sigma_day", "sigma_measurement",
  "beta_covariates[3]", "beta_covariates[2]", "beta_covariates[1]", 
  "beta_cs"
)

# Reorder the temps data frame according to the desired order
temps_ordered <- temps[param_order]

# Generate the mcmc_intervals plot
p <- mcmc_intervals(temps_ordered, point_size = 1, prob = 0.50, prob_outer = 0.89) + 
  xlab('')

# Define custom labels
custom_labels <- c(
  `beta_cs` = expression(beta[CS]),
  `beta_covariates[1]` = expression(beta[negative~affect]),
  `beta_covariates[2]` = expression(beta[decentering]),
  `beta_covariates[3]` = expression(beta[context~valence]),
  `sigma_ucs` = expression(sigma[UCS]),
  `sigma_participant` = expression(sigma[participant]),
  `sigma_day` = expression(sigma[day]),
  `sigma_measurement` = expression(sigma[measurement]),
  `sigma_participant_slope_cs` = expression(sigma[participant~slope~CS])
)

# Apply custom labels to the plot
p + scale_y_discrete(labels = custom_labels)

# ------------------------------------------------------------------------------

mcmc_intervals_data(temps, prob = 0.50, prob_outer = 0.89) 
# parameter               outer_width inner_width point_est       ll        l        m        h       hh
# <fct>                         <dbl>       <dbl> <chr>        <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
# 1 beta_cs                        0.89         0.5 median    -0.478   -0.455   -0.438   -4.22e-1 -0.399  
# 2 beta_covariates[1]             0.89         0.5 median     0.0609   0.0678   0.0727   7.79e-2  0.0847 
# 3 beta_covariates[2]             0.89         0.5 median    -0.0960  -0.0893  -0.0845  -7.97e-2 -0.0733 
# 4 beta_covariates[3]             0.89         0.5 median    -0.0143  -0.00837 -0.00378  4.90e-4  0.00621
# 5 sigma_ucs                      0.89         0.5 median     0.390    0.397    0.401    4.06e-1  0.413  
# 6 sigma_participant              0.89         0.5 median     0.552    0.583    0.606    6.29e-1  0.664  
# 7 sigma_day                      0.89         0.5 median     0.00932  0.0177   0.0234   2.94e-2  0.0401 
# 8 sigma_measurement              0.89         0.5 median     0.00102  0.00468  0.00971  1.69e-2  0.0329 
# 9 sigma_participant_slop…        0.89         0.5 median     0.254    0.269    0.282    2.96e-1  0.316  

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

# Trace plots
mcmc_trace(fit_draws, pars = c("beta_cs"))
mcmc_trace(
  fit_draws, 
  pars = c("beta_covariates[1]", "beta_covariates[2]", "beta_covariates[3]")
)
mcmc_trace(fit_draws, pars = c("sigma_ucs"))

mcmc_trace(fit_draws, pars = c("sigma_participant_slope_cs"))

loo(fit$draws("log_lik"))

# saveRDS(
#   fit, 
#   here::here(
#     "data", "prep", "ema", "brms_fits", "mod_9_rnd_slopes.RDS"
#   )
# )

fit$save_object(
  file = here::here(
    "data", "prep", "ema", "brms_fits", "mod_9_rnd_slopes.RDS"
  )
)

# fatto fino a qui! ------------------------------------------------------------


d |> 
  group_by(exam_day) |> 
  summarize(
    nsc = mean(nsc, na.rm = TRUE, trim = 0.1),
    psc = mean(psc, na.rm = TRUE, trim = 0.1)
  )



# fit_combined <- brm(
#   bf(nsc ~ exam_day + (1 + exam_day | user_id)) + 
#     bf(psc ~ exam_day + (1 + exam_day | user_id)) +
#     set_rescor(TRUE),  # To estimate the residual correlation between nsc and psc
#   data = d,
#   family = student(), 
#   prior = c(
#     prior(normal(0, 2.5), class = "b"),
#     prior(normal(0, 2.5), class = "Intercept")
#   ),
#   control = list(adapt_delta = 0.95, max_treedepth = 12),
#   seed = SEED
# )

fit_nsc <- brm(
  nsc ~ exam_day + (1 + exam_day | user_id/bysubj_day),
  data = d,
  family = gaussian(),
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept")
  ),
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = SEED
)
pp_check(fit_nsc)
summary(fit_nsc)

fit_psc <- brm(
  psc ~ exam_day + (1 + exam_day | user_id/bysubj_day),
  data = d,
  family = student(),
  prior = c(
    prior(normal(0, 2.5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept")
  ),
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = SEED
)
pp_check(fit_psc)
summary(fit_psc)


# exam cmdstan ----------------

# Create numeric indices for categorical variables if not already done
d$user_id_numeric <- as.numeric(as.factor(d$user_id))
d$day_numeric <- as.numeric(as.factor(d$day))
d$moment_numeric <- as.numeric(as.factor(d$time_window))  # Assuming time_window is the moment
d$exam_day_numeric <- as.numeric(as.factor(d$exam_day))

# Calculate the number of unique participants, days, and measurements
P <- length(unique(d$user_id_numeric))
D <- length(unique(d$day_numeric))
M <- length(unique(d$moment_numeric))

# Organize the data into arrays
CS <- d$CS
UCS <- d$UCS
neg_affect <- d$na_moment
decentering <- d$dec_moment
context_eval <- d$con_moment

d$exam_day_numeric <- as.numeric(as.factor(d$exam_day))

# measurement = as.numeric(as.factor(d$time_window)), # d$moment_numeric,
# CS = CS,
# UCS = UCS,
# neg_affect = neg_affect,
# decentering = decentering,
# context_eval = context_eval,

# Prepare the list for Stan

user_id_numeric <- as.numeric(as.factor(d$user_id))
day_numeric <- as.numeric(as.factor(d$bysubj_day))
moment_numeric <- as.numeric(as.factor(d$time_window))
exam_day_pre <- ifelse(d$exam_day == "pre", 1, 0)
exam_day_post <- ifelse(d$exam_day == "post", 1, 0)

stan_data <- list(
  N = nrow(d),
  J = length(unique(user_id_numeric)),
  D = length(unique(day_numeric)),
  M = length(unique(moment_numeric)),
  subj = user_id_numeric,
  day = day_numeric,
  meas = moment_numeric,
  psc = d$psc,
  nsc = d$nsc,
  exam_day_pre = exam_day_pre,
  exam_day_post = exam_day_post
)
str(stan_data)

# Compile the Stan model
stan_file <- here::here(
  "workflows", "stan", "exam_sc_4.stan"
)
mod <- cmdstan_model(stan_file)

# Sample from the posterior 
fit <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  adapt_delta = 0.95,  
  max_treedepth = 12, 
  seed = SEED
)


fit <- readRDS(here::here("data", "prep", "ema", "brms_fits", "fit_mod_exam_sc_4.RDS"))

# Extract posterior samples
# posterior_samples <- fit$draws()
# dim(posterior_samples)

# Extract beta coefficients (check the exact name in your model output)
# beta_pre_samples <- posterior_samples[,, "beta_pre"]

# Plot a histogram of the posterior draws with bayesplot 
draws <- fit$draws(format="df")
mcmc_hist(draws, pars="beta_pre") 
beta_pre_neg <- mean(draws$beta_pre < 0)
beta_pre_neg

mcmc_hist(draws, pars="beta_post") 
beta_post_pos <- mean(draws$beta_post > 0)
beta_post_pos

# Posterior predictive checks
y <- stan_data$nsc
stanfit <- rstan::read_stan_csv(fit$output_files())
y_rep <- as.matrix(stanfit, pars = "y_rep")
ppc_dens_overlay(y, y_rep[1:200, ]) + xlim(c(-30, 30))

loo(fit$draws("log_lik"))


params <- c("beta_pre", "beta_post")

temps <- fit$draws(format = "df") |>
  as_tibble() |>
  select(all_of(params))

mcmc_areas(temps) + xlab('')

names(mod$variables()$parameters)

fit$summary(names(mod$variables()$parameters))

fit$summary(c("beta_pre", "beta_post"))


# eof ---








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



