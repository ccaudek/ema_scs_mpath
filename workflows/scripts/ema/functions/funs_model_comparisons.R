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
#' 1. Reads raw EMA data from a RDS file.
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
#' processed_data <- get_data_models_comparison(input_path, output_path)
#'
get_data_for_model_comparisons <- function(input_path, output_path) {
  suppressPackageStartupMessages({
    library("dplyr")
    library("performance")
    library("miceRanger")
  })

  source(
    here::here("workflows", "scripts", "ema", "functions", "funs_ema_mpath.R")
  )

  # Read EMA data.
  d <- readRDS(input_path)

  # Ensure the categorical variables are formatted as factors in R.
  d$bysubj_day <- factor(d$bysubj_day)
  d$user_id <- factor(d$user_id)
  d$time_window <- factor(d$time_window)
  d$exam_day <- factor(d$exam_day)

  # Add SCS scores
  scs_trait_tot_df <-
    read.csv(here::here("data", "prep", "quest_scales", "scs_scores.csv"))

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

  # Run multiple imputations
  imputed_data <- miceRanger(
    temp,
    seed = 123
  )

  dataList <- completeData(imputed_data)

  numeric_cols <- sapply(dataList[[1]], is.numeric)

  # Initialize a data frame to store the mean values
  # Convert to data.table if it's not already
  mean_data <- setDT(dataList[[1]])

  # Compute mean for numeric columns
  for (col in which(numeric_cols)) {
    # Extract the specific column from each dataframe and calculate rowMeans
    mean_data[, (col) := rowMeans(sapply(dataList, function(df) df[[col]]))]
  }

  non_numeric_cols <- names(mean_data)[!numeric_cols]
  mean_data[, (non_numeric_cols) := dataList[[1]][, ..non_numeric_cols]]

  # Centering data
  centered_data <- center3L(mean_data, neg_aff, user_id, bysubj_day) |>
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

  saveRDS(df, output_path)
}


#' #' fit_and_compare_neg_ssc_models() --------------------------------------------
#' #' 
#' #' @description
#' #' The `fit_and_compare_neg_ssc_models` function fits and compares a series of 
#' #' Bayesian models to analyze negative state self-compassion (znsc). It 
#' #' sequentially fits ten `brms` models with increasing complexity, incorporating 
#' #' variables such as negative affect, context, and decision-making factors, 
#' #' along with random effects for individual users and day-to-day variations. 
#' #' Each model undergoes Leave-One-Out Cross-Validation (LOO) for assessment. T
#' #' he function concludes by comparing all models using LOO statistics to 
#' #' identify the model that best explains znsc, returning the LOO comparison 
#' #' results. 
#' #' @param data A data frame with the complete data set.
#' #' @return The function returns the result of the LOO model comparison, 
#' #' providing insights into which of the ten models is most effective at 
#' #' capturing the patterns in the znsc data.
#' fit_and_compare_models <- function(input_path, dependent_var, output_path) {
#'   
#'   suppressPackageStartupMessages({
#'     library("dplyr")
#'     library("brms")
#'     library("loo")
#'     library("cmdstanr")
#'   })
#'   
#'   data <- readRDS(input_path)
#'   
#'   formula <- as.formula(
#'     paste(dependent_var, "~ na_person + dec_person + con_person +
#'                               na_moment + na_day +
#'                               dec_moment + dec_day +
#'                               con_moment + con_day")
#'   )
#'   
#'   #  This model serves as a baseline to assess the importance of adding random effects.
#'   mod_1 <- brm(
#'     formula = formula,
#'     data = data,
#'     family = student(),
#'     backend = "cmdstanr",
#'     cores = 8,
#'     chains = 4,
#'     threads = threading(2),
#'     silent = 2
#'   )
#'   loo_mod_1 <- loo(mod_1)
#'   
#'   # Model 2
#'   formula <- as.formula(
#'     paste(dependent_var, "~ na_person + dec_person + con_person +
#'             na_moment + na_day +
#'             dec_moment + dec_day +
#'             con_moment + con_day +  
#'             (1 | user_id)
#'           ")
#'   )
#'   
#'   mod_2 <- brm(
#'     formula,
#'     data = data,
#'     family = student(),
#'     backend = "cmdstanr",
#'     cores = 8,
#'     chains = 4,
#'     threads = threading(2),
#'     silent = 2
#'   )
#'   loo_mod_2 <- loo(mod_2)
#'   
#'   # Model 3
#'   formula <- as.formula(
#'     paste(dependent_var, "~ na_person + dec_person + con_person +
#'             na_moment + na_day +
#'             dec_moment + dec_day +
#'             con_moment + con_day +  
#'             (1 | user_id) + (1 | user_id:day)
#'           ")
#'   )
#'   
#'   mod_3 <- brm(
#'     formula,
#'     data = data,
#'     family = student(),
#'     backend = "cmdstanr",
#'     cores = 8,
#'     chains = 4,
#'     threads = threading(2),
#'     silent = 2
#'   )
#'   loo_mod_3 <- loo(mod_3)
#'   
#'   # Model 4
#'   
#'   formula <- as.formula(
#'     paste(dependent_var, "~ na_person + dec_person + con_person +
#'             na_moment + na_day +
#'             dec_moment + dec_day +
#'             con_moment + con_day +  
#'             (1 + na_moment + na_day | user_id) + (1 | user_id:day)
#'           ")
#'   )
#'   
#'   mod_4 <- brm(
#'     formula,
#'     data = data,
#'     family = student(),
#'     backend = "cmdstanr",
#'     cores = 8,
#'     chains = 4,
#'     threads = threading(2),
#'     silent = 2
#'   )
#'   loo_mod_4 <- loo(mod_4)
#'   
#'   
#'   # Model 5
#'   
#'   formula <- as.formula(
#'     paste(dependent_var, "~ na_person + dec_person + con_person +
#'             na_moment + na_day +
#'             dec_moment + dec_day +
#'             con_moment + con_day +  
#'             (1 + na_moment + na_day + dec_moment + dec_day | user_id) + 
#'           (1 | user_id:day)
#'           ")
#'   )
#'   
#'   mod_5 <- brm(
#'     formula,
#'     data = data,
#'     family = student(),
#'     control = list(adapt_delta = 0.99),
#'     backend = "cmdstanr",
#'     cores = 8,
#'     chains = 4,
#'     threads = threading(2),
#'     silent = 2
#'   )
#'   loo_mod_5 <- loo(mod_5)
#'   
#'   if (dependent_var == "state_ucs") {
#'     saveRDS(
#'       mod_5,
#'       here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_ucs.rds")
#'     )
#'   } else if (dependent_var == "state_cs") {
#'     saveRDS(
#'       mod_5,
#'       here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_cs.rds")
#'     )
#'   } else {
#'     stop("Unsupported dependent variable: ", dependent_var)
#'   }
#'   
#'   
#'   # Model 6
#'   
#'   formula <- as.formula(
#'     paste(dependent_var, "~ na_person + dec_person + con_person +
#'             na_moment + na_day +
#'             dec_moment + dec_day +
#'             con_moment + con_day +  
#'             (1 + na_moment + na_day + dec_moment + dec_day | user_id) + 
#'           (1 | user_id:day) + (1 | user_id:day:moment)
#'           ")
#'   )
#'   
#'   mod_6 <- brm(
#'     formula,
#'     data = data,
#'     family = student(),
#'     control = list(adapt_delta = 0.99),
#'     backend = "cmdstanr",
#'     cores = 8,
#'     chains = 4,
#'     threads = threading(2),
#'     silent = 2
#'   )
#'   loo_mod_6 <- loo(mod_6)
#'   
#'   # Compare models using LOO
#'   loo_comparison <- loo_compare(
#'     loo_mod_1, loo_mod_2, loo_mod_3, loo_mod_4, loo_mod_5, loo_mod_6
#'   )
#'   
#'   saveRDS(loo_comparison, output_path)
#' }


fit_and_compare_random_effects <- 
  function(input_path, dependent_var, output_path, testing_mode = FALSE) {

  suppressPackageStartupMessages({
    library("dplyr")
    library("brms")
    library("loo")
    library("cmdstanr")
  })
  
  testing_mode <- as.logical(testing_mode)
  
  data <- readRDS(input_path)
  
  # Function to build formula
  build_formula <- function(dep_var, additional_terms = "") {
    as.formula(paste(dep_var, "~ na_person + dec_person + con_person +
                      na_moment + na_day +
                      dec_moment + dec_day +
                      con_moment + con_day", additional_terms))
  }
  
  # Function to fit model
  fit_model <- function(formula, data, control_list = NULL, testing_mode) {
    if (testing_mode) {
      brm(
        formula = formula,
        data = data,
        family = student(),
        algorithm = "meanfield", # Variational Inference
        iter = 1000, # Fewer iterations for quicker results
        silent = 2
      )
    } else {
      brm(
        formula = formula,
        data = data,
        family = student(),
        backend = "cmdstanr",
        cores = 8,
        chains = 4,
        threads = threading(2),
        control = control_list,
        silent = 2
      )
    }
  }
  
  # Model fitting
  model_descriptions <- c(
    "Model 1: Basic Model",
    "Model 2: Add Random Effect for user_id",
    "Model 3: Add Random Effects for user_id and user_id:day",
    "Model 4: Add Random Slopes for na_moment, na_day on user_id",
    "Model 5: Add Random Slopes for na_moment, na_day, dec_moment, dec_day on user_id",
    "Model 6: Complex Random Effects Structure"
  )
  
  models <- list()
  models[[1]] <- fit_model(build_formula(dependent_var), data, NULL, testing_mode)
  
  models[[2]] <- fit_model(build_formula(dependent_var, "+ (1 | user_id)"), data, NULL, testing_mode)
  
  models[[3]] <- fit_model(build_formula(dependent_var, "+ (1 | user_id) + (1 | user_id:day)"), data, NULL, testing_mode)
  
  models[[4]] <- fit_model(build_formula(dependent_var, "+ (1 + na_moment + na_day | user_id) + (1 | user_id:day)"), data, NULL, testing_mode)
  
  control_list <- list(adapt_delta = 0.99)
  models[[5]] <- fit_model(build_formula(dependent_var, "+ (1 + na_moment + na_day + dec_moment + dec_day | user_id) + (1 | user_id:day)"), data, control_list, testing_mode)
  
  models[[6]] <- fit_model(build_formula(dependent_var, "+ (1 + na_moment + na_day + dec_moment + dec_day | user_id) + (1 | user_id:day) + (1 | user_id:day:moment)"), data, control_list, testing_mode)
  
  # LOO comparison
  loo_models <- lapply(models, loo)
  loo_comparison <- do.call(loo_compare, loo_models)
  
  # Combine LOO results with model descriptions
  loo_results_with_descriptions <- cbind(Model = model_descriptions, as.data.frame(loo_comparison))
  
  # Save the results
  saveRDS(loo_results_with_descriptions, output_path)
  }


#' fit_and_compare_fixed_effects() ---------------------------------------------
fit_and_compare_fixed_effects <- 
  function(input_path, dependent_var, output_path, testing_mode = TRUE) {
  suppressPackageStartupMessages({
    library("dplyr")
    library("brms")
    library("loo")
    library("cmdstanr")
  })
  
  testing_mode <- as.logical(testing_mode)
  data <- readRDS(input_path)
  
  # Function to build formula with varying fixed effects
  build_formula <- function(dep_var, fixed_effects) {
    as.formula(paste(dep_var, "~", fixed_effects, "+ (1 | user_id)"))
  }
  
  # Function to fit model
  fit_model <- function(formula, data, control_list = NULL, testing_mode) {
    if (testing_mode) {
      brm(
        formula = formula,
        data = data,
        family = student(),
        algorithm = "meanfield", # Variational Inference
        iter = 1000, # Fewer iterations for quicker results
        silent = 2
      )
    } else {
      brm(
        formula = formula,
        data = data,
        family = student(),
        backend = "cmdstanr",
        cores = 8,
        chains = 4,
        threads = threading(2),
        control = control_list,
        silent = 2
      )
    }
  }
  
  # Define different sets of fixed effects
  fixed_effects_models <- list(
    "Model 1: Full Fixed Effects" = "na_person + na_day + na_moment + dec_person + dec_day + dec_moment + con_person + con_day + con_moment",
    "Model 2: With na and dec" = "na_person + na_day + na_moment + dec_person + dec_day + dec_moment",
    "Model 3: With na and con" = "na_person + na_day + na_moment + con_person + con_day + con_moment",
    "Model 4: With dec and con" = "dec_person + dec_day + dec_moment + con_person + con_day + con_moment",
    "Model 5: Only na" = "na_person + na_day + na_moment",
    "Model 6: Only dec" = "dec_person + dec_day + dec_moment",
    "Model 7: Only con" = "con_person + con_day + con_moment"
  )
  
  # Model fitting
  models <- lapply(names(fixed_effects_models), function(model_name) {
    fixed_effects <- fixed_effects_models[[model_name]]
    formula <- build_formula(dependent_var, fixed_effects)
    fit_model(formula, data, NULL, testing_mode)
  })
  
  # LOO comparison
  loo_models <- lapply(models, loo)
  loo_comparison <- do.call(loo_compare, loo_models)
  
  # Combine LOO results with model descriptions
  loo_results_with_descriptions <- cbind(Model = names(fixed_effects_models), as.data.frame(loo_comparison))
  
  # Save the results
  saveRDS(loo_results_with_descriptions, output_path)
}




