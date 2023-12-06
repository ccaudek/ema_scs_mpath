#' Functions for testing the effect of exam day on the SC and USC 
#' components of State Self-Compassion

get_stan_data_exam_state_self_comp <- function(input_path, output_path) {
  library("dplyr")
  
  d <- readRDS(input_path)
  
  user_id_numeric <- as.numeric(as.factor(d$user_id))
  day_numeric <- as.numeric(as.factor(d$bysubj_day))
  moment_numeric <- as.numeric(as.factor(d$time_window))
  exam_day_pre <- ifelse(
    d$exam_day == "pre", 1, ifelse(d$exam_day == "no_exam", -1, 0)
    )
  exam_day_post <- ifelse(
    d$exam_day == "post", 1, ifelse(d$exam_day == "no_exam", -1, 0)
  )
  
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
  
  saveRDS(stan_data, output_path)
}


# fit_mod_exam_sc_4() ------------------------------------------

fit_mod_exam_sc_4 <- function(input_path, output_path) {
  library("dplyr")
  library("here")
  library("cmdstanr")
  
  SEED = 48927
  
  stan_data <- readRDS(input_path)
  
  # Compile the Stan model
  stan_file <- here::here("workflows", "stan", "exam_sc_4.stan")
  mod <- cmdstan_model(stan_file)
  
  # Sample from the posterior 
  fit <- mod$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1500,
    adapt_delta = 0.99,  
    max_treedepth = 12, 
    seed = SEED
  )
  
  fit$save_object(file = output_path)
}

# fit_mod_exam_sc_4() ------------------------------------------

fit_mod_exam_usc_4 <- function(input_path, output_path) {
  library("dplyr")
  library("here")
  library("cmdstanr")
  
  SEED = 48927
  
  stan_data <- readRDS(input_path)
  
  # Compile the Stan model
  stan_file <- here::here("workflows", "stan", "exam_usc_4.stan")
  mod <- cmdstan_model(stan_file)
  
  # Sample from the posterior 
  fit <- mod$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1500,
    adapt_delta = 0.99,  
    max_treedepth = 12, 
    seed = SEED
  )
  
  fit$save_object(file = output_path)
}


# generate_plot_exam_self_comp() -----------------------------------------------

generate_plot_exam_self_comp <- function(
    input_path_1, input_path_2, output_path) {
  library("here")
  library("bayesplot")
  library("ggplot2")
  library("dplyr")
  library("tidyr")
  library("gridExtra")
  
  fit1 <- readRDS(here::here(input_path_1))
  fit2 <- readRDS(here::here(input_path_2))
  
  # Parameters to plot
  params <- c("beta_pre", "beta_post")
  
  # Extract and prepare data from fit1
  temps_fit1 <- fit1$draws(format = "df") |>
    as_tibble() |>
    select(all_of(params)) |>
    pivot_longer(cols = everything(), names_to = "parameter", values_to = "value") |>
    mutate(component = "CS")
  
  # Extract and prepare data from fit2
  temps_fit2 <- fit2$draws(format = "df") |>
    as_tibble() |>
    select(all_of(params)) |>
    pivot_longer(cols = everything(), names_to = "parameter", values_to = "value") |>
    mutate(component = "UCS")
  
  # Combine data
  combined_temps <- bind_rows(temps_fit1, temps_fit2)
  
  # Define color palette
  color_palette <- c("black", "gray")
  
  # Function to create combined plots with Bayesplot theme
  plot_func <- function(param) {
    ggplot(combined_temps %>% filter(parameter == param), aes(x = value, fill = component)) +
      geom_density(alpha = 0.6) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      # theme_minimal(base_family = "sans") +
      theme(legend.position = "top", 
            plot.title = element_text(size = 14, face = "bold")) +
      labs(title = paste("Posterior Distribution of", param),
           x = '',
           y = 'Density') +
      scale_fill_manual(values = color_palette) +
      guides(fill = guide_legend(override.aes = list(colour = color_palette)))
  }
  
  # Create plots for each parameter
  plot_beta_pre <- plot_func("beta_pre")
  plot_beta_post <- plot_func("beta_post")
  
  # Combine the plots
  combined_plot <- grid.arrange(plot_beta_pre, plot_beta_post, ncol = 1)
  
  # Save to PDF
  ggsave(
    here::here(output_path), 
    plot = combined_plot, 
    device = "pdf", 
    width = 8, 
    height = 6
  )
}


