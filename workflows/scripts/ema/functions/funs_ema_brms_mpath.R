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
    brms_model, parameters_of_interest = NULL, SUBTITLE,
    FILE_NAME) {
  # Check if parameters are specified, else use default
  if (is.null(parameters_of_interest)) {
    parameters_of_interest <- c(
      "na_moment", "na_day", "na_person",
      "dec_moment", "dec_day", "dec_person",
      "con_moment", "con_day", "con_person"
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
    filename = here::here("doc", "figures", FILE_NAME),
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
#'   here::here("workflows", "scripts", "ema", "brms_fits", "bmod9_nsc.rds"), 
#'   here::here("doc", "figures", "var_comp.pdf")
#' )
#' 
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
    "sd_user_id__Intercept" = "Variability in Baseline Task Performance across Individuals",
    "sd_user_id__na_moment" = "Variability in Moment-to-Moment Fluctuations of Daily Negative Affect",
    "sd_user_id__na_day" = "Variability in Negative Affect Across Different Days",
    "sd_user_id__dec_moment" = "Variability in Moment-to-Moment Fluctuations of Decentering",
    "sd_user_id__dec_day" = "Variability in Decentering Across Different Days",
    "sd_user_id:day__Intercept" = "Day-Specific Variability in Baseline Task Performance"
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
    theme(
      legend.title = element_blank(), 
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"  # Place legend at the bottom
    ) +
    labs(y = "Density") +
    guides(fill = guide_legend(nrow = 6, ncol = 1))  # Organize legend in 6 rows and 1 column

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
plot_cs_ucs_by_neg_aff_day <- function(d3, FILE_NAME) {
  # Check for required columns in the data frame
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


