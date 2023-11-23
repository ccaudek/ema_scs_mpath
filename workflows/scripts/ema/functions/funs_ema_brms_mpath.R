# Code for comparing brms models describing the effects of various predictors
# on the positive or negative components of State Self-Compassion.

#' get_data_models_comparison() ------------------------------------------------
#' 
#' @description
#' Get data for models' comparisons.
#' @return A data frame.
#' 
get_data_models_comparison <- function() {
  
  # Read raw data.
  d <- readRDS(here::here("data", "prep", "ema", "ema_data_3.RDS"))
  
  # Ensure the categorical variables are formatted as factors in R.
  d$bysubj_day <- factor(d$bysubj_day)
  d$user_id <- factor(d$user_id)
  d$time_window <- factor(d$time_window)
  d$exam_day <- factor(d$exam_day)
  
  # Add SCS scores
  scs_trait_tot_df <- rio::import(
    here::here("data", "prep", "quest_scales", "scs_scores.csv")
  )
  
  scs_trait_df <- scs_trait_tot_df |> 
    dplyr::select(user_id, scs_total_score)
  
  scs_trait_df$user_id <- gsub("_", "-", scs_trait_df$user_id)
  scs_trait_df$user_id <- gsub("(\\d{2})(\\d{2})", "\\2", scs_trait_df$user_id)
  # Remove duplicate rows based on user_id, keeping the first occurrence
  scs_trait_df <- scs_trait_df[!duplicated(scs_trait_df$user_id), ]
  
  d1 <- left_join(d, scs_trait_df, by = "user_id")
  
  # Check for outliers by considering the target variables
  column_names <- c("psc", "nsc", "context", "neg_aff", "dec")
  
  foo <- d1 |>
    dplyr::select(all_of(column_names))
  
  res <- performance::check_outliers(foo)
  attr_list <- attributes(res)
  outlier_count <- attr_list$outlier_count
  bad_row_indices <- outlier_count$all$Row
  # bad_row_indices
  
  # For the considered columns, replace the outliers with NAs
  d1[bad_row_indices, column_names] <- NA
  
  # Select variables for multiple imputation
  variables_for_mult_imp <- c(
    column_names, "bysubj_day", "user_id", "time_window", "exam_day", 
    "scs_total_score"
  )
  
  temp <- d1 |> 
    dplyr::select(all_of(variables_for_mult_imp))
  
  imputed_data <- mice(
    temp,
    m = 1, 
    maxit = 50, 
    printFlag = FALSE,
    seed = 123
  ) |> 
    complete(1)
  
  # Centering data
  centered_data <- center3L(imputed_data, neg_aff, user_id, bysubj_day)  |> 
    center3L(context, user_id, bysubj_day)  |> 
    center3L(dec, user_id, bysubj_day)
  
  # Standardize variables for modeling
  df <- centered_data |> 
    dplyr::mutate(
      scs_trait = as.vector(scale(scs_total_score)),
      na_moment = as.vector(scale(neg_aff_Moment)),
      na_day = as.vector(scale(neg_aff_Day)),
      na_person = as.vector(scale(neg_aff_Person)),
      dec_moment = as.vector(scale(dec_Moment)),
      dec_day = as.vector(scale(dec_Day)),
      dec_person = as.vector(scale(dec_Person)),
      con_moment = as.vector(scale(context_Moment)),
      con_day = as.vector(scale(context_Day)),
      con_person = as.vector(scale(context_Person)),
      state_cs = as.vector(scale(psc)),
      state_ucs = as.vector(scale(nsc))
    ) |> 
    dplyr::select(
      scs_trait, state_cs, state_ucs,
      na_moment, na_day, na_person, 
      dec_moment, dec_day, dec_person,
      con_moment, con_day, con_person,
      user_id, bysubj_day, time_window
    ) |> 
    dplyr::rename(
      day = bysubj_day,
      moment = time_window
    )
  
  return(df)
}


#' fit_and_compare_neg_ssc_models() --------------------------------------------
#' 
#' @description
#' The `fit_and_compare_neg_ssc_models` function fits and compares a series of 
#' Bayesian models to analyze negative state self-compassion (znsc). It 
#' sequentially fits ten `brms` models with increasing complexity, incorporating 
#' variables such as negative affect, context, and decision-making factors, 
#' along with random effects for individual users and day-to-day variations. 
#' Each model undergoes Leave-One-Out Cross-Validation (LOO) for assessment. T
#' he function concludes by comparing all models using LOO statistics to 
#' identify the model that best explains znsc, returning the LOO comparison 
#' results. 
#' @param data A data frame with the complete data set.
#' @return The function returns the result of the LOO model comparison, 
#' providing insights into which of the ten models is most effective at 
#' capturing the patterns in the znsc data.

fit_and_compare_neg_ssc_models <- function(data) {
  
  # Model 1: Establishes a baseline by modeling negative state self-compassion 
  # (znsc) with only an intercept. This basic model serves as a reference point 
  # to assess the impact of adding predictors and random effects.
  bmod1_nsc <- brm(
    znsc ~ 1,
    data = data,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod1_nsc <- loo(bmod1_nsc))
  
  # Model 2: Introduces random intercepts for individual users to account for 
  # variation in znsc across users. This model examines whether individual 
  # differences contribute significantly to znsc.
  bmod2_nsc <- brm(
    znsc ~ 1 + 
      (1 | user_id),
    data = data,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod2_nsc <- loo(bmod2_nsc))

  # Model 3: Adds random intercepts for bysubj_day alongside user_id. This model
  # explores whether day-to-day variations within subjects further explain
  # variations in znsc.
  bmod3_nsc <- brm(
    znsc ~ 1 + 
      (1 | user_id) + 
      (1 + time_window | user_id:bysubj_day), 
    data = data,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod3_nsc <- loo(bmod3_nsc))

  priors1 <- c(
    set_prior("normal(0, 2.5)", class = "b")
  )

  # Model 4: Incorporates the effects of negative affect (na_moment, na_day,
  # na_person) as fixed effects. This model tests the hypothesis that negative
  # affect influences znsc.
  bmod4_nsc <- brm(
    znsc ~ na_moment + na_day + na_person +
      (1 | user_id) + 
      (1 + time_window | user_id:bysubj_day), 
    data = data,
    prior = priors1,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod4_nsc <- loo(bmod4_nsc))

  # Model 5: Expands upon Model 4 by adding random slopes for na_moment and
  # na_day within users. It examines whether the impact of momentary and daily
  # negative affect on znsc varies across individuals.
  bmod5_nsc <- brm(
    znsc ~ na_moment + na_day + na_person +
      (1 + na_moment + na_day | user_id) + 
      (1 + time_window | user_id:bysubj_day), 
    data = data,
    prior = priors1,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod5_nsc <- loo(bmod5_nsc))

  # Model 6: Further extends Model 5 by including fixed effects for context
  # variables (context_moment, context_day, context_person). This model assesses
  # the combined influence of negative affect and context on znsc.
  bmod6_nsc <- brm(
    znsc ~ na_moment + na_day + na_person +
      context_moment + context_day + context_person +
      (1 + na_moment + na_day | user_id) + 
      (1 + time_window | user_id:bysubj_day), 
    data = data,
    prior = priors1,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod6_nsc <- loo(bmod6_nsc))

  # Model 7: Builds on Model 6 by adding random slopes for context_moment and
  # context_day within users. Investigates if the context's impact on znsc varies
  # by individual.
  bmod7_nsc <- brm(
    znsc ~ na_moment + na_day + na_person +
      context_moment + context_day + context_person +
    (1 + na_moment + na_day + context_moment + context_day | user_id) + 
    (1 + time_window | user_id:bysubj_day), 
    data = data,
    prior = priors1,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod7_nsc <- loo(bmod7_nsc))

  # Model 8: Integrates decision-making variables (dec_moment, dec_day,
  # dec_person) as fixed effects, along with the variables in Model 7. This model
  # evaluates how decision-making, context, and negative affect jointly influence
  # znsc.
  bmod8_nsc <- brm(
    znsc ~ dec_moment + dec_day + dec_person +
      na_moment + na_day + na_person +
      context_moment + context_day + context_person +
      (1 + na_moment + na_day + context_moment + context_day | user_id) + 
      (1 + time_window | user_id:bysubj_day), 
    data = data,
    prior = priors1,
    family = student(),
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    #silent = 2,
    file = here::here("workflows", "scripts", "ema", "brms_fits", "bmod8_nsc.rds")
  )
  (loo_bmod8_nsc <- loo(bmod8_nsc))

  # Model 9: Adds random slopes for decision-making variables (dec_moment,
  # dec_day) within users to the structure of Model 8. Explores individual
  # variability in how decision-making affects znsc.
  bmod9_usc <- brm(
    state_ucs ~ dec_moment + dec_day + dec_person +
      na_moment + na_day + na_person +
      con_moment + con_day + con_person +
      (1 + na_moment + na_day + con_moment + con_day + dec_moment + dec_day | user_id) + 
      (1 + moment | user_id:day), 
    data = data,
    prior = priors1,
    family = student(),
    control = list(adapt_delta = 0.99), # max_treedepth = 20
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2,
    file = here::here("workflows", "scripts", "ema", "brms_fits", "bmod9_nsc.rds")
  )
  (loo_bmod9_usc <- loo(bmod9_usc))

  # Model 10: Introduces zscs_trait as an additional fixed effect, alongside the
  # predictors in Model 9. This final model assesses the overall impact of trait
  # self-compassion, decision-making, context, and negative affect on znsc, along
  # with the individual variability in these effects.
  bmod10_nsc <- brm(
    znsc ~ zscs_trait +
      dec_moment + dec_day + dec_person +
      na_moment + na_day + na_person +
      (1 + na_moment + na_day + context_moment + context_day + dec_moment + dec_day | user_id) + 
      (1 + time_window | user_id:bysubj_day), 
    data = data,
    family = student(),
    prior = priors1,
    # control = list(adapt_delta = 0.99, max_treedepth = 20),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod10_nsc <- loo(bmod10_nsc))

  # Compare models using LOO
  loo_comparison <- loo_compare(
    loo_bmod1_nsc, loo_bmod2_nsc, loo_bmod3_nsc, loo_bmod4_nsc, loo_bmod5_nsc,
    loo_bmod6_nsc, loo_bmod7_nsc, loo_bmod8_nsc, loo_bmod9_nsc, loo_bmod10_nsc
  )
  
  return(loo_comparison)
}


#' fit_and_compare_pos_ssc_models() --------------------------------------------

fit_and_compare_pos_ssc_models <- function(data) {
  
  bmod1_psc <- brm(
    zpsc ~ 1,
    data = tot_df,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod1_psc <- loo(bmod1_psc))
  
  bmod2_psc <- brm(
    zpsc ~ 1 + (1 | user_id),
    data = tot_df,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod2_psc <- loo(bmod2_psc))
  
  bmod3_psc <- brm(
    zpsc ~ 1 + (1 | user_id) + (1 | bysubj_day),
    data = tot_df,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod3_psc <- loo(bmod3_psc))
  
  priors1 <- c(
    set_prior("normal(0, 2)", class = "b")
  )
  
  bmod4_psc <- brm(
    zpsc ~ na_moment + na_day + na_person +
      (1 | user_id) + (1 | bysubj_day),
    data = tot_df,
    prior = priors1,
    family = student(),
    control = list(adapt_delta = 0.99, max_treedepth = 20),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod4_psc <- loo(bmod4_psc))
  
  bmod5_psc <- brm(
    zpsc ~ na_moment + na_day + na_person +
      (1 + na_moment + na_day | user_id) + (1 | bysubj_day),
    data = tot_df,
    prior = priors1,
    family = student(),
    control = list(adapt_delta = 0.99, max_treedepth = 20),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod5_psc <- loo(bmod5_psc))
  
  bmod6_psc <- brm(
    zpsc ~ na_moment + na_day + na_person +
      context_moment + context_day + context_person + 
      (1 + na_moment + na_day | user_id) + (1 | bysubj_day),
    data = tot_df,
    prior = priors1,
    family = student(),
    control = list(adapt_delta = 0.99, max_treedepth = 20),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod6_psc <- loo(bmod6_psc))
  
  bmod7_psc <- brm(
    zpsc ~ na_moment + na_day + na_person +
      context_moment + context_day + context_person + 
      (1 + na_moment + na_day + context_moment + context_day | user_id) + 
      (1 | bysubj_day),
    data = tot_df,
    prior = priors1,
    family = student(),
    control = list(adapt_delta = 0.99, max_treedepth = 20),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod7_psc <- loo(bmod7_psc))
  
  bmod8_psc <- brm(
    zpsc ~ dec_moment + dec_day + dec_person +
      na_moment + na_day + na_person +
      context_moment + context_day + context_person + 
      (1 + na_moment + na_day + context_moment + context_day | user_id) + 
      (1 | bysubj_day),
    data = tot_df,
    prior = priors1,
    family = student(),
    control = list(adapt_delta = 0.99, max_treedepth = 20),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod8_psc <- loo(bmod8_psc)) # reloo = TRUE
  
  bmod9_psc <- brm(
    zpsc ~ dec_moment + dec_day + dec_person +
      na_moment + na_day + na_person +
      context_moment + context_day + context_person + 
      (1 + na_moment + na_day + context_moment + context_day + dec_moment + dec_day | user_id) + 
      (1 | bysubj_day),
    data = tot_df,
    prior = priors1,
    family = student(),
    control = list(adapt_delta = 0.99, max_treedepth = 20),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2,
    file = here::here("workflows", "scripts", "ema", "brms_fits", "bmod9_psc.rds")
  )
  (loo_bmod9_psc <- loo(bmod9_psc))
  # pp_check(bmod9_psc)
  
  bmod10_psc <- brm(
    zpsc ~ zscs_trait + 
      dec_moment + dec_day + dec_person +
      na_moment + na_day + na_person +
      context_moment + context_day + context_person + 
      (1 + na_moment + na_day + context_moment + context_day + dec_moment + dec_day | user_id) + 
      (1 | bysubj_day),
    data = tot_df,
    prior = priors1,
    family = student(),
    control = list(adapt_delta = 0.99, max_treedepth = 20),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  (loo_bmod10_psc <- loo(bmod10_psc))
  
  loo_comparison <- loo_compare(
    loo_bmod1_psc, loo_bmod2_psc, loo_bmod3_psc, loo_bmod4_psc, loo_bmod5_psc, 
    loo_bmod6_psc, loo_bmod7_psc, loo_bmod8_psc, loo_bmod9_psc, loo_bmod10_psc
  )
  
  return(loo_comparison)
}


#' get_stats_brms_param() ------------------------------------------------------
#' 
#' @description
#' Get posterior estimate of a model's parameter, together with 89% CI and
#' p(beta < 0) * 100.
#' @param model_path The complete path to a fitted brms object.
#' @param PARAM_NAME The name of the model's parameter.
#' @return A list. 
#' result[[1]] = param_name
#' result[[2]] = beta_mean
#' result[[3]] = ci_89
#' result[[4]] = prob_beta_less_than_zero
#' @example get_stats_brms_param(model_path, PARAM_NAME)
#' model_path <- here::here("workflows", "scripts", "ema", "brms_fits", "bmod9_nsc.rds")
#' PARAM_NAME <- "b_dec_moment" 
#' Note that you need to add "b_" to the name of the parameter printed by summary().
#' 
get_stats_brms_param <- function(model_path, PARAM_NAME) {
  fit <- readRDS(model_path)
  param_name <- PARAM_NAME
  
  posterior_samples <- posterior_samples(fit, pars = param_name)
  
  # Calculate mean
  beta_mean <- mean(posterior_samples[[param_name]])
  
  # Calculate the 89% credibility interval
  ci_89 <- bayestestR::ci(posterior_samples[[param_name]], ci = .89)
  
  # Calculate the probability that β is less than 0
  prob_beta_less_than_zero <- mean(posterior_samples[[param_name]] < 0) * 100
  
  result <- list()
  
  result[[1]] = param_name
  result[[2]] = beta_mean
  result[[3]] = ci_89
  result[[4]] = prob_beta_less_than_zero
  
  return(result)
}

#' plot_brms_coefficients() ----------------------------------------------------
#' 
#' @description
#' Creates a pdf ggplot plot of the coefficients of a brms model, with CIs.
#' @param brms_model A fitted brms model.
#' @param parameters_of_interest A character vector.
#' @param SUBTITLE A string.
#' @return Null
#' 
plot_brms_coefficients <- function(
    brms_model, parameters_of_interest = NULL, SUBTITLE) {
  # Check if parameters are specified, else use default
  if (is.null(parameters_of_interest)) {
    parameters_of_interest <- c(
      "na_moment", "na_day", "na_person",
      "dec_moment", "dec_day", "dec_person",
      "context_moment", "context_day", "context_person"
    )
  }
  
  library("bayesplot") 
  theme_set(bayesplot::theme_default(base_family = "sans"))

  # Summary of the model
  brms_summary <- summary(brms_model)

  # Check if parameters exist in the model
  available_params <- rownames(brms_summary$fixed)
  if (!all(parameters_of_interest %in% available_params)) {
    stop("Some parameters of interest are not found in the model summary.")
  }

  # Extracting summary statistics
  summary_stats <- brms_summary$fixed[parameters_of_interest, 1:4]

  # Prepare the data frame
  plot_data <- data.frame(
    Mean = summary_stats$Estimate,
    SD = summary_stats$`Est.Error`,
    Low = summary_stats$`l-95% CI`,
    High = summary_stats$`u-95% CI`,
    Parameter = parameters_of_interest
  )

  # Dynamically assign Category and Type based on parameter names
  # (This part can be customized as per your parameter naming conventions)
  plot_data$Category <- ifelse(grepl("na_", plot_data$Parameter), "Negative Affect",
    ifelse(grepl("dec_", plot_data$Parameter), "Decentering", "Context")
  )
  plot_data$Type <- gsub(".*(Moment|Day|Person).*", "\\1", plot_data$Parameter)

  # Create the plot
  final_plot <- ggplot(plot_data, aes(x = Type, y = Mean, ymin = Low, ymax = High)) +
    geom_pointrange(position = position_dodge(width = 0.4)) +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "") +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip() +
    facet_grid(~Category) +
    # theme_minimal() +
    labs(title = "State Self-Compassion", subtitle = SUBTITLE)

  ggsave(
    filename = "final_plot.pdf",
    plot = final_plot,
    width = 6,
    height = 5,
    units = "in"
  )
}


#' posterior_variance_estimates() ----------------------------------------------
#' 
#' @description
#' Generates a plot of the variance components of the random effects of the
#' final model
#' @param path_to_fitted_model The path to the file where the model is stored
#' @param FILE_NAME The path of the pdf file where the plot will be saved
#' @example posterior_variance_estimates(
#' here::here("workflows", "scripts", "ema", "brms_fits", "bmod9_nsc.rds"), 
#' here::here("doc", "figures", "var_comp.pdf")
#' )
posterior_variance_estimates <- function(path_to_fitted_model, FILE_NAME) {
  # Load required libraries
  library("bayesplot")
  library("dplyr")
  library("ggplot2")
  library("tidyr")
  theme_set(bayesplot::theme_default(base_family = "sans"))
  
  # Load the fitted model
  fit <- readRDS(path_to_fitted_model)
  
  # Extract posterior samples
  post_samples <- posterior_samples(fit, add_group_level_effects = TRUE)
  
  # Select only the relevant SD parameters and convert to long format
  sd_samples <- post_samples %>%
    select(matches("^sd_")) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "SD")
  
  # Simple labels for mapping
  label_mapping <- c(
    "sd_bysubj_day__Intercept" = "bysubj_day Intercept",
    "sd_user_id__context_day" = "context_day user_id",
    "sd_user_id__context_moment" = "context_moment user_id",
    "sd_user_id__dec_day" = "dec_day user_id",
    "sd_user_id__dec_moment" = "dec_moment user_id",
    "sd_user_id__Intercept" = "user_id Intercept",
    "sd_user_id__na_day" = "na_day user_id",
    "sd_user_id__na_moment" = "na_moment user_id"
  )
  
  # Apply the simple mapping
  sd_samples$Variable <- factor(sd_samples$Variable, levels = names(label_mapping))
  renamed_variables <- label_mapping[sd_samples$Variable]
  sd_samples$Variable <- renamed_variables
  
  # Colors for the plot (adjust color_scheme as needed)
  color_scheme <- c("blue", "red", "green", "orange", "purple", "brown", "pink", "yellow")
  
  # Create the density plot
  sd_plot <- ggplot(sd_samples, aes(x = SD, fill = Variable)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = color_scheme, labels = unique(renamed_variables)) +
    theme_classic() +
    scale_x_continuous("Standard Deviation (SD)", expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    ggtitle("Posterior Distributions of Variance Components") +
    theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
    labs(y = "Density")
  
  # Save the plot
  ggsave(
    filename = FILE_NAME,
    plot = sd_plot,
    width = 8,
    height = 4,
    units = "in"
  )
}


#' plot_cs_ucs_by_neg_aff_day() ------------------------------------------------
#' 
#' @param datapath The path to the data frame
#' @param FILE_NAME The path where the pdf file is saved
#' @example 
#' plot_cs_ucs_by_neg_aff_day(
#'   here::here("data", "prep", "ema", "ema_data_3.RDS"),
#'   here::here("doc", "figures", "cs_ucs_neg_aff_day.pdf")
#'   )
plot_cs_ucs_by_neg_aff_day <- function(datapath, FILE_NAME) {
  # Check for required columns in the data frame
  d3 <- readRDS(datapath)
  required_cols <- c("neg_aff", "psc", "nsc", "exam_day", "bysubj_day")
  if (!all(required_cols %in% names(d3))) {
    stop("Data frame does not contain all required columns")
  }
  
  # Load required libraries
  if (!require(wesanderson)) install.packages("wesanderson")
  library(wesanderson)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  cols <- wes_palette(n=4, name="Darjeeling2")
  
  # Compute quantiles and filter data
  quantiles <- quantile(d3$neg_aff, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
  d3$neg_aff_quintiles <- cut(d3$neg_aff, breaks = quantiles, include.lowest = TRUE, labels = FALSE)
  d3$is_exam_day <- ifelse(d3$exam_day == "no_exam", 0, 1)
  d3 <- d3 %>% filter(is_exam_day == 0)
  
  # Calculate means and standard errors
  d3_summary <- d3 %>%
    group_by(neg_aff_quintiles, bysubj_day) %>%
    summarise(
      mean_psc = mean(psc, na.rm = TRUE),
      se_psc = sd(psc, na.rm = TRUE) / sqrt(n()),
      mean_nsc = mean(nsc, na.rm = TRUE),
      se_nsc = sd(nsc, na.rm = TRUE) / sqrt(n())
    )
  
  # Reshape data for plotting
  d3_long <- d3_summary %>%
    pivot_longer(
      cols = starts_with("mean"),
      names_to = "variable",
      values_to = "mean"
    ) %>%
    mutate(
      error = case_when(
        variable == "mean_psc" ~ se_psc,
        variable == "mean_nsc" ~ se_nsc
      )
    )
  
  # Create the plot
  p <- ggplot(d3_long, aes(x = factor(neg_aff_quintiles), y = mean, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(
      aes(ymin = mean - error, ymax = mean + error),
      position = position_dodge(0.9),
      width = 0.25
    ) +
    geom_line(
      aes(group = interaction(variable, bysubj_day)), 
      position = position_dodge(0.9), 
      color = "lightgray",
      size = 1
    ) +
    facet_wrap(~bysubj_day) +
    labs(x = "Negative Affect Quintiles", y = "Mean Value") +
    scale_fill_manual(
      values = cols,
      labels = c("CS", "UCS")
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
  # Save the plot
  ggsave(
    filename = FILE_NAME,
    plot = p,
    width = 8,
    height = 8,
    units = "in"
  )
}


