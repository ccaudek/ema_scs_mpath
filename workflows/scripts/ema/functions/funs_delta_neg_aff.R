fit_delta_neg_aff_model <- function(input_path, dependent_var, output_path) {
  
  suppressPackageStartupMessages({
    library("dplyr")
    library("brms")
    library("miceRanger")
    library("cmdstanr")
  })
  
  d <- readRDS(input_path)
  
  d$exam_day <- factor(d$exam_day)
  
  # Select only the relevant variables, only for no_exam days
  d1 <- d |> 
    dplyr::filter(exam_day == "no_exam") |> 
    dplyr::select(
      user_id, bysubj_day, time_window,
      neg_aff, psc, nsc
    )
  
  user_ids <- distinct(d1, user_id)
  bysubj_days <- distinct(d1, bysubj_day)
  time_windows <- distinct(d1, time_window)
  
  complete_combinations <- expand.grid(
    user_id = user_ids$user_id,
    # bysubj_day = bysubj_days$bysubj_day,
    time_window = time_windows$time_window
  )
  
  complete_data <- left_join(
    complete_combinations, 
    d1, by = c("user_id",  "time_window"))
  
  imputed_data <- miceRanger(
    complete_data, 
    # verbose=FALSE,
    seed = 123
  )
  
  dataList <- completeData(imputed_data)
  
  # Assuming dataList is a list of data frames/data.tables and they all have the same structure
  numeric_cols <- sapply(dataList[[1]], is.numeric)
  
  # Initialize a data frame to store the mean values
  # Convert to data.table if it's not already
  mean_data <- setDT(dataList[[1]])
  
  # Compute mean for numeric columns
  for (col in which(numeric_cols)) {
    # Extract the specific column from each dataframe and calculate rowMeans
    mean_data[, (col) := rowMeans(sapply(dataList, function(df) df[[col]]))]
  }
  
  # For non-numeric columns, decide how to handle.
  # For example, you can keep the values from the first imputation:
  non_numeric_cols <- names(mean_data)[!numeric_cols]
  mean_data[, (non_numeric_cols) := dataList[[1]][, ..non_numeric_cols]]
  
  # Check the dimensions of the resulting data frame
  # dim(mean_data)  # Should be 6252 x 6
  
  # long_data <- mice::complete(imputed_data, action="long")
  
  d3 <- mean_data |> 
    group_by(user_id, bysubj_day, time_window) |> 
    summarize(neg_aff = mean(neg_aff, na.rm = TRUE),
              psc = mean(psc, na.rm = TRUE),
              nsc = mean(nsc, na.rm = TRUE)) |> 
    ungroup()
  
  d3 <- d3 |> 
    group_by(user_id, bysubj_day) |> 
    arrange(time_window) |> 
    mutate(delta_neg_aff = neg_aff - lag(neg_aff))
  
  # Setting delta_neg_aff to NA for time_window == 1
  d3 <- d3 |> 
    mutate(delta_neg_aff = ifelse(time_window == 1, NA, delta_neg_aff))
  
  # Check that delta_neg_aff is neg_aff(t+1) - neg_aff(t)
  # Consider only one subject
  if (0) {
    temp <- d3[d3$user_id == "ag-nu-03-07-30-860-f", ]
    temp1 <- temp |> 
      dplyr::select(delta_neg_aff, neg_aff, time_window, bysubj_day)
    
    temp1 |> 
      arrange(bysubj_day, time_window) |> 
      as.data.frame()
    rm(temp, temp1)
  }
  # Interpretation: positive values of delta_neg_aff indicate that negative
  # affect has increased from time t to time t+1; negative values indicate the
  # opposite. delta_neg_aff is set to NA for the first measurement of each day 
  # (time_window == 1).
  
  d4 <- d3[d3$time_window != 1, ] 
  d4 <- d4[!is.na(d4$delta_neg_aff), ]
  
  # Check if all combinations of user_id and bysubj_day have time_window 
  # levels 2, 3, 4, 5
  if(0) {
    # Define the required time_window levels
    required_time_windows <- c(2, 3, 4, 5)
    
    # Check if all combinations of user_id and bysubj_day have the required time_window levels
    check_results <- d4 |> 
      group_by(user_id, bysubj_day) |> 
      summarise(time_windows = list(unique(time_window)), .groups = 'drop') |> 
      mutate(has_all_levels = all(required_time_windows %in% unlist(time_windows)))
    
    check_results$has_all_levels |> mean()
  }
  # Test passed.
  
  # Standardize numeric variables.
  d5 <- d4
  d5$psc <- as.vector(scale(d4$psc))
  d5$nsc <- as.vector(scale(d4$nsc))
  d5$delta_neg_aff <- as.vector(scale(d4$delta_neg_aff))
  d5$bysubj_day <- as.vector(scale(d4$bysubj_day))
  d5$time_window <- as.vector(scale(d4$time_window))
  d5$neg_aff <- as.vector(scale(d4$neg_aff))
  
  # Generate name of the file that will be saved
  # filename = paste0(dependent_var, "_delta_neg_aff", ".RDS")
  
  formula <- as.formula(
    paste(dependent_var, "~ neg_aff + delta_neg_aff + time_window + 
      (1 + neg_aff + delta_neg_aff | user_id) +
      (1 | bysubj_day) + 
      (1 | bysubj_day:time_window) +
      ar(time = time_window, gr = user_id:bysubj_day, p = 1, cov = TRUE)")
  )
  
  mod <- brm(
    formula = formula,
    data = d5,
    family = student(),
    backend = "cmdstanr",
    silent = 2
  )
  
  saveRDS(mod, output_path)
}

