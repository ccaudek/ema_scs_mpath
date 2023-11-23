# Code for the comparison of brms models describing the effects of various 
# predictors on the CS and UCS components of State Self-Compassion.

#' get_data_models_comparison() ------------------------------------------------
#' 
#' Obtain Data for Models Comparison
#'
#' This function processes raw EMA (Ecological Momentary Assessment) data 
#' for use in models comparison. It includes steps for reading the data, 
#' formatting variables, handling outliers, performing multiple imputation, 
#' and standardizing variables.
#'
#' The function performs the following steps:
#' 1. Reads raw EMA data from a specified RDS file.
#' 2. Formats certain variables as factors, suitable for categorical analysis.
#' 3. Imports and processes Self-Compassion Scale (SCS) scores, 
#'    merges them with EMA data, and handles user ID formatting and duplicates.
#' 4. Identifies and replaces outliers in specified target variables with NA.
#' 5. Performs multiple imputation on a selected set of variables to handle missing data.
#' 6. Centers and scales the data to prepare for modeling.
#'
#' @return A data frame ready for statistical modeling. This data frame includes:
#'    - Standardized scores for target psychological constructs (e.g., psc, nsc).
#'    - Momentary, daily, and personal variations of different psychological measures.
#'    - User ID, day, and time window information.
#' 
#' @examples
#' # To run the function and store the output in a variable:
#' processed_data <- get_data_models_comparison()
#'
get_data_for_model_comparisons <- function(d) {
  
  # Read raw data.
  # d <- readRDS(here::here("data", "prep", "ema", "ema_data_3.RDS"))
  
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
fit_and_compare_models <- function(data, dependent_var) {
  formula <- as.formula(
    paste(dependent_var, "~ na_person + dec_person + con_person +
                              na_moment + na_day +
                              dec_moment + dec_day +
                              con_moment + con_day")
  )
  
  #  This model serves as a baseline to assess the importance of adding random effects.
  mod_1 <- brm(
    formula = formula,
    data = data,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  loo_mod_1 <- loo(mod_1)
  
  # Model 2
  formula <- as.formula(
    paste(dependent_var, "~ na_person + dec_person + con_person +
            na_moment + na_day +
            dec_moment + dec_day +
            con_moment + con_day +  
            (1 | user_id)
          ")
  )
  
  mod_2 <- brm(
    formula,
    data = data,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  loo_mod_2 <- loo(mod_2)
  
  # Model 3
  formula <- as.formula(
    paste(dependent_var, "~ na_person + dec_person + con_person +
            na_moment + na_day +
            dec_moment + dec_day +
            con_moment + con_day +  
            (1 | user_id) + (1 | user_id:day)
          ")
  )
  
  mod_3 <- brm(
    formula,
    data = data,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  loo_mod_3 <- loo(mod_3)
  
  # Model 4
  
  formula <- as.formula(
    paste(dependent_var, "~ na_person + dec_person + con_person +
            na_moment + na_day +
            dec_moment + dec_day +
            con_moment + con_day +  
            (1 + na_moment + na_day | user_id) + (1 | user_id:day)
          ")
  )
  
  mod_4 <- brm(
    formula,
    data = data,
    family = student(),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  loo_mod_4 <- loo(mod_4)
  
  
  # Model 5
  
  formula <- as.formula(
    paste(dependent_var, "~ na_person + dec_person + con_person +
            na_moment + na_day +
            dec_moment + dec_day +
            con_moment + con_day +  
            (1 + na_moment + na_day + dec_moment + dec_day | user_id) + 
          (1 | user_id:day)
          ")
  )
  
  mod_5 <- brm(
    formula,
    data = data,
    family = student(),
    control = list(adapt_delta = 0.99),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  loo_mod_5 <- loo(mod_5)
  
  if (dependent_var == "state_ucs") {
    saveRDS(
      mod_5,
      here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_ucs.rds")
    )
  } else if (dependent_var == "state_cs") {
    saveRDS(
      mod_5,
      here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_cs.rds")
    )
  } else {
    stop("Unsupported dependent variable: ", dependent_var)
  }
  
  
  # Model 6
  
  formula <- as.formula(
    paste(dependent_var, "~ na_person + dec_person + con_person +
            na_moment + na_day +
            dec_moment + dec_day +
            con_moment + con_day +  
            (1 + na_moment + na_day + dec_moment + dec_day | user_id) + 
          (1 | user_id:day) + (1 | user_id:day:moment)
          ")
  )
  
  mod_6 <- brm(
    formula,
    data = data,
    family = student(),
    control = list(adapt_delta = 0.99),
    backend = "cmdstanr",
    cores = 6,
    chains = 2,
    threads = threading(3),
    silent = 2
  )
  loo_mod_6 <- loo(mod_6)
  
  # Compare models using LOO
  loo_comparison <- loo_compare(
    loo_mod_1, loo_mod_2, loo_mod_3, loo_mod_4, loo_mod_5, loo_mod_6
  )
  
  return(loo_comparison)
}



