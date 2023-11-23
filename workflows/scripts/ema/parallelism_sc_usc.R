

d <- get_data_models_comparison()

d1 <- d |> 
  dplyr::select(
    psc, nsc, 
    neg_aff, dec,
    time_window, bysubj_day, user_id
  ) |> 
  mutate(
    nsc_rev = nsc * -1
  ) |> 
  dplyr::select(-nsc) |> 
  dplyr::rename(
    "ucs" = "nsc_rev",
    "sc" = "psc"
  )

long_df <- pivot_longer(
  d1, -c("neg_aff", "dec", "time_window", "bysubj_day", "user_id"), 
  names_to = "dimension", values_to = "ssc"
)

foo <- long_df |> 
  dplyr::select(-c(time_window, user_id, bysubj_day))

performance::check_outliers(foo)

bad_obs <- c(
  572, 580, 588, 589, 989, 2971, 2972, 2974, 2980, 2981, 2982, 2988, 3007, 3008, 7477,
  10992, 11503, 11869, 12756, 12770, 12788, 12794
)

# Specify the columns for which you want to set the values to NA
columns_to_na <- c("neg_aff", "dec", "ssc")

# Set the values to NA for the specified rows and columns
long_df[bad_obs, columns_to_na] <- NA

# Imputing missing data
imputed_data <- mice(long_df %>% dplyr::select(all_of(columns_to_na)),
                     m = 1, maxit = 50, method = "pmm", seed = 123
) %>%
  complete(1)

imputed_data$user_id <- long_df$user_id
imputed_data$bysubj_day <- long_df$bysubj_day
imputed_data$time_window <- long_df$time_window
imputed_data$dimension <- long_df$dimension

imputed_data$zna <- as.vector(scale(imputed_data$neg_aff))
imputed_data$zdec <- as.vector(scale(imputed_data$dec))
imputed_data$zssc <- as.vector(scale(imputed_data$ssc))

priors1 <- c(
  set_prior("normal(0, 2)", class = "b")
)

mod_parallel <- brm(
  zssc ~ dimension * (zdec + zna) +
    (1 | user_id) + # Random intercept for subject
    (1 + time_window | user_id:bysubj_day), # Random intercept and slope for time window within day within subject 
  data = imputed_data,
  prior = priors1,
  family = asym_laplace(),
  # control = list(adapt_delta = 0.99, max_treedepth = 20),
  backend = "cmdstanr",
  cores = 6,
  chains = 2,
  threads = threading(3),
  silent = 2,
  file = here::here("workflows", "scripts", "ema", "brms_fits", "mod_parallel.rds")
)
pp_check(mod)
loo_mod <- loo(mod)

mod_parallel_0 <- brm(
  zssc ~ dimension + (zdec + zna) +
    (1 | user_id) + # Random intercept for subject
    (1 + time_window | user_id:bysubj_day), # Random intercept and slope for time window within day within subject 
  data = imputed_data,
  prior = priors1,
  family = asym_laplace(),
  # control = list(adapt_delta = 0.99, max_treedepth = 20),
  backend = "cmdstanr",
  cores = 6,
  chains = 2,
  threads = threading(3),
  silent = 2,
  file = here::here("workflows", "scripts", "ema", "brms_fits", "mod_parallel_0.rds")
)
loo_mod_noint <- loo(mod_noint)

loo_compare(loo_mod, loo_mod_noint)
#          elpd_diff se_diff
# mod_noint    0.0       0.0 
# mod       -786.7      46.3 

bayes_R2(mod)
#     Estimate  Est.Error      Q2.5     Q97.5
# R2 0.7427189 0.01170741 0.7151978 0.7625236
bayes_R2(mod_noint)
#     Estimate   Est.Error     Q2.5     Q97.5
# R2 0.7664634 0.002552426 0.761222 0.7715766

pp_check(mod)

print(loo_mod)

summary(mod)
# Population-Level Effects: 
#                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept             0.46      0.03     0.40     0.52 1.00      922      899
# dimensionucs         -0.46      0.02    -0.49    -0.42 1.00      937      724
# zdec                  0.17      0.02     0.14     0.21 1.00      826      813
# zna                  -0.27      0.02    -0.31    -0.24 1.00      970      983
# dimensionucs:zdec     0.17      0.03     0.12     0.22 1.00     1003      954
# dimensionucs:zna     -0.07      0.02    -0.12    -0.02 1.00      918     1026

