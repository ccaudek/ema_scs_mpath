#' Functions for testing the Bipolar Continuum Hypothesis

get_data_bip_cont_test <- function(input_path, output_path) {
  library(dplyr)
  
  source(
    here::here("workflows", "scripts", "ema", "functions", "funs_ema_mpath.R")
  )
  
  temp <- readRDS(input_path)
  
  # Centering data
  d <- center3L(temp, neg_aff, user_id, bysubj_day) |>
    center3L(context, user_id, bysubj_day)  |>
    center3L(dec, user_id, bysubj_day)
  
  # Convert 'user_id', 'day', and 'moment' to numeric indices
  d$user_id_numeric <- as.numeric(as.factor(d$user_id))
  d$day_numeric <- as.numeric(as.factor(d$day))
  d$moment_numeric <- as.numeric(as.factor(d$time_window))
  
  # Get the number of unique participants, days, and measurements
  P <- length(unique(d$user_id_numeric))
  D <- length(unique(d$day_numeric))
  M <- length(unique(d$moment_numeric))  # Assuming this is 5 as per your description
  
  # Create a unique index for each observation combining participant, day, and measurement
  d$pd_index <- as.numeric(
    factor(paste(d$user_id_numeric, d$day_numeric, d$moment_numeric, sep = "_"))
  )
  
  d$CS <- as.vector(scale(d$psc))
  d$UCS <- as.vector(scale(d$nsc))
  
  d$na_moment <- as.vector(scale(d$neg_aff_Moment))
  d$dec_moment <- as.vector(scale(d$dec_Moment))
  d$con_moment <- as.vector(scale(d$context_Moment))
  
  # Prepare the list for Stan
  stan_data <- list(
    N = nrow(d),
    P = P,
    D = D,
    M = M,
    participant = d$user_id_numeric,
    day = d$day_numeric,
    measurement = d$moment_numeric,
    CS = d$CS,
    UCS = d$UCS,
    neg_affect = d$na_moment,
    decentering = d$dec_moment,
    context_eval = d$con_moment
  )
  
  saveRDS(stan_data, output_path)
}

# fit_mod_9_rnd_slopes_bip_cont_hyp() ------------------------------------------

fit_mod_9_rnd_slopes_bip_cont_hyp <- function(input_path, output_path) {
  library("dplyr")
  library("here")
  library("cmdstanr")
  
  SEED = 48927
  
  stan_data <- readRDS(input_path)
  
  # Compile the Stan model
  stan_file <- here::here(
    "workflows", "stan", "model_9_rnd_slopes.stan"
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
  
  fit$save_object(file = output_path)
}

