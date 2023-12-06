#' process_exam_data() ---------------------------------------------------------
#' 
#' @description
#' This function pre-process the data for computing the difference in negative
#' affect before and after the exam.
#' @param exam_type It can be "first_exam" or "second_exam"
#' @return A data frame.
#' @example completed_data_first_exam <- process_exam_data("first_exam")
#' @example completed_data_second_exam <- process_exam_data("second_exam")
#' 
#' input_path <- "data/prep/ema/ema_data.rds"
#' exam_type <- "first_exam"
#' exam_type <- "second_exam"
#' output_path <- "data/prep/ema/data_first_exam.rds"
process_exam_data <- function(input_path, exam_type, output_path) {
  library("dplyr")
  library("here")
  
  # Read raw data
  data <- readRDS(here::here(input_path))
  
  # Process for EMA days before and after the exam
  exam_data <- data |> 
    filter(exam_day != "no_exam") |>
    group_by(user_id) |> 
    mutate(bysubj_day = dense_rank(day)) |> 
    ungroup() |> 
    select(user_id, day, exam_day, neg_aff) |>
    mutate(exam_day = factor(exam_day, levels = c("pre", "post")))
  
  # Separate the data based on exam type
  if (exam_type == "first_exam") {
    exam_specific_data <- exam_data[exam_data$day %in% c("2023-04-16", "2023-04-17"), ]
  } else if (exam_type == "second_exam") {
    exam_specific_data <- exam_data[exam_data$day %in% c("2023-05-21", "2023-05-22"), ]
  } else {
    stop("Invalid exam type specified.")
  }
  
  # Unique data for each user and exam day
  unique_exam_data <- exam_specific_data %>%
    group_by(user_id, exam_day) %>%
    slice(1) %>%
    ungroup()
  
  saveRDS(unique_exam_data, here::here(output_path))
}


#' compute_exam_effects_on_neg_aff() -------------------------------------------
#' @description
#' Compute the estimate of the negative affect difference before and after 
#' the exam.
#' 
#' @param data The data frame returned by the function process_exam_data()
#' @param exam_type Either "first_exam" or "second_exam"
#' @return A list:
#' (1) the regression coefficient for the difference between the pre and the
#' post negative affect (negative numbers indicate that negative affect has
#' decreased).
#' (2) the standard error
#' (3) the 95% credibility interval of beta
#' (4) the effect size
#' (5) the 95% credibility interval of the effect size

# input_path <- "data/prep/ema/data_first_exam.rds"
# exam_type <- "first_exam"
# exam_type <- "second_exam"
# output_path <- "data/prep/ema/res_neg_aff_on_first_exam.rds"

compute_exam_effects_on_neg_aff <- function(
    input_path, exam_type, output_path) {
  
  library("here")
  library("dplyr")
  library("brms")
  
  exam_data <- readRDS(here::here(input_path))
  
  # Internal function to compute effects
  compute_effects_internal <- function(model) {
    summary_model <- summary(model)
    pop_effects <- summary_model$fixed
    
    exam_daypost_estimate <- pop_effects["exam_daypost", "Estimate"]
    exam_daypost_error <- pop_effects["exam_daypost", "Est.Error"]
    exam_daypost_lower_ci <- pop_effects["exam_daypost", "l-95% CI"]
    exam_daypost_upper_ci <- pop_effects["exam_daypost", "u-95% CI"]
    
    effect_size_exam <- compute_effect_size(model, "exam_daypost")
    ci_effect_size_exam <- compute_effect_size_ci(model, "exam_daypost")
    
    return(list(
      estimate = exam_daypost_estimate,
      error = exam_daypost_error,
      ci = c(exam_daypost_lower_ci, exam_daypost_upper_ci),
      effect_size = effect_size_exam,
      effect_size_ci = ci_effect_size_exam
    ))
  }
  
  # Select model based on exam type
  if (exam_type == "first_exam") {
    model_formula <- neg_aff ~ 1 + exam_day + (1 + exam_day | user_id)
  } else if (exam_type == "second_exam") {
    model_formula <- neg_aff ~ 1 + exam_day + (1 | user_id)
  } else {
    stop("Invalid exam type specified.")
  }
  
  d <- exam_data
  d$neg_aff <- as.vector(scale(d$neg_aff))
  
  # Fit the Bayesian model
  model <- brm(
    formula = model_formula,
    data = d,
    family = asym_laplace(),
    prior = c(
      set_prior("normal(0, 2.5)", class = "b"),
      set_prior("normal(0, 1)", class = "sigma"),
      set_prior("normal(0, 0.2)", class = "quantile")
    ),
    # control = list(adapt_delta = 0.99),
    backend = "cmdstanr",
    chains = 3,
    cores = 6,
    threads = threading(2),
    silent = 2
  )
  
  res <- compute_effects_internal(model)
  saveRDS(res, output_path)
}



#' compute_effect_size() -------------------------------------------------------
#' @description
#' Compute effect size for a coefficient of a mixed-effect model computed with
#' brm()
#' @param brms_model
#' @param effect_name
#' @example: effect_size <- compute_effect_size(m1, "exam_daypost")

compute_effect_size <- function(brms_model, effect_name) {
  library("brms")
  library("posterior")
  
  # Estrai i campioni posteriori usando as_draws_df
  posterior_samples <- as_draws_df(brms_model)
  
  # Estrai le deviazioni standard degli effetti random e la deviazione standard dei residui
  sd_components <- posterior_samples %>%
    select(contains("sd_"), contains("sigma")) %>%
    summarise(across(everything(), ~sqrt(mean(.x^2))))
  
  # Calcola la deviazione standard totale come radice quadrata della somma dei quadrati delle SD
  total_sd <- sqrt(sum(sd_components^2, na.rm = TRUE))
  
  # Calcola l'effetto (ad esempio, il coefficiente per una variabile specifica)
  effect_mean <- mean(posterior_samples[[paste0("b_", effect_name)]])
  
  # Calcola la dimensione dell'effetto standardizzata
  effect_size <- effect_mean / total_sd
  
  return(effect_size)
}


#' compute_effect_size_ci() ----------------------------------------------------
#' @description
#' Compute credibility interval for the effect size.
#' @example effect_size_ci <- compute_effect_size_ci(m1, "exam_daypost")
#' 
compute_effect_size_ci <- function(brms_model, effect_name, probs = c(0.055, 0.945)) {
  # Estrai i campioni posteriori usando as_draws_df
  posterior_samples <- as_draws_df(brms_model)
  
  # Calcola la deviazione standard totale per ogni campione
  total_sd_samples <- apply(posterior_samples[, grep("sd_|sigma", colnames(posterior_samples))], 1, function(x) sqrt(sum(x^2)))
  
  # Calcola la dimensione dell'effetto per ogni campione
  effect_samples <- posterior_samples[[paste0("b_", effect_name)]] / total_sd_samples
  
  # Calcola l'intervallo di credibilità
  ci <- quantile(effect_samples, probs = probs)
  
  return(ci)
}

