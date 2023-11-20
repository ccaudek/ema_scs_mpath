#' delete_first_row_xlsx() -----------------------------------------------------
#' 
delete_first_row_xlsx <- function(FOLDER_NAME) {
  library(readxl)
  library(writexl)
  library(fs)

  # Directory path where Excel files are located
  directory_path <- here("data", "raw", FOLDER_NAME)

  # List all Excel files in the directory
  excel_files <- dir_ls(directory_path, regexp = "\\.xlsx$")

  # Loop through each Excel file
  for (file in excel_files) {
    # Read the Excel file
    data <- read_excel(file, skip = 1)

    # Write the modified data back to the Excel file
    write_xlsx(data, path = file)

    # Print the processed file name
    cat("Deleted first row in:", basename(file), "\n")
  }
}


#' import_ema_data() -----------------------------------------------------------
#' 
import_ema_data <- function(input_folder, output_rds_path) {
  
  suppressPackageStartupMessages({
    library(psych)
    library(ggplot2)
    library(sjPlot)
    library(dplyr)
    library(rio)
    library(here)
    library(purrr)
    library(lubridate)
    library(readxl)
    library(janitor)
  })
  
  source(here::here("workflows", "scripts", "ema", "functions", "funs_ema_mpath.R"))
  
  complete_column_names <- c(
    "Date and time",
    "question_context (multipleChoice)",
    "question8scs (multipleChoice)",
    "question_satisfied (smiley)",
    "question_nervous (smiley)",
    "question_dec1 (multipleChoice)",
    "question_happiness (smiley)",
    "question5scs (multipleChoice)",
    "question2scs (multipleChoice)",
    "question_dec3 (multipleChoice)",
    "question_sad (smiley)",
    "question7scs (multipleChoice)",
    "question3scs (multipleChoice)",
    "question4scs (multipleChoice)",
    "question_dec2 (multipleChoice)",
    "question_dec4 (multipleChoice)",
    "question1scs (multipleChoice)",
    "question6scs (multipleChoice)",
    "question_emotion (multiSmiley)",
    "yesnoexam (yesno)",
    "momentary_emotion_yes (multiSmiley)",
    "exam_preparation (sliderNegPos)",
    "grade_exp (sliderNeutralPos)",
    "momentary_emotion_no (multiSmiley)",
    "emotion_valence_no (smiley)",
    "real_grade (sliderNeutralPos)",
    "emotion_valence_yes (smiley)"
  )
  
  FOLDER_NAME <- input_folder
  
  if (file.exists(here::here("data", "raw", FOLDER_NAME, "boop.txt"))) {
    delete_first_row_xlsx(FOLDER_NAME)
    file.remove(here::here("data", "raw", FOLDER_NAME, "boop.txt"))
  }
  
  dir <- here::here("data", "raw", FOLDER_NAME)
  file_names <- list.files(path = dir, full.names = TRUE)
  
  d_list <- lapply(file_names, function(file_name) {
    d <- rio::import(file_name, header = TRUE)
    
    missing_cols <- setdiff(complete_column_names, names(d))
    d[missing_cols] <- NA
    d <- d[complete_column_names]
    
    d$subj_code <- substr(file_name, 1, nchar(file_name) - 5)
    d$datetime <- strptime(d$`Date and time`, "%a, %d %b %Y %H:%M:%S")
    d$day <- ymd(substring(d$datetime, 1, 10))
    d$hour <- hour(d$datetime)
    d$minute <- minute(d$datetime)
    
    d$time_window <- case_when(
      d$hour >= 0 & d$hour < 12 ~ 1,
      d$hour >= 12 & d$hour < 16 ~ 2,
      d$hour >= 16 & d$hour < 19 ~ 3,
      d$hour >= 19 & d$hour < 21 ~ 4,
      d$hour >= 21 & d$hour < 24 ~ 5,
      TRUE ~ NA_integer_
    )
    
    d$calendar_day <- as.numeric(factor(d$day))
    
    d <- d %>%
      mutate(bysubj_day = dense_rank(calendar_day)) %>%
      select(-`Date and time`, -datetime) %>%
      clean_names()
    
    if ("momentary_emotion_no_multi_smiley" %in% names(d)) {
      d$momentary_emotion_no_multi_smiley <- as.character(d$momentary_emotion_no_multi_smiley)
    }
    
    if ("momentary_emotion_yes_multi_smiley" %in% names(d)) {
      d$momentary_emotion_yes_multi_smiley <- as.character(d$momentary_emotion_yes_multi_smiley)
    }
    
    d
  })
  
  combined_df <- bind_rows(d_list)
  
  saveRDS(combined_df, output_rds_path)
}


#' process_ema_data() ----------------------------------------------------------
#' 
process_ema_data <- function(input_rds_path, output_rds_path) {
  suppressPackageStartupMessages({
    library("psych")
    library("ggplot2")
    library("dplyr")
    library("rio")
    library("here")
    library("lubridate")
    library("tidyr")
  })
  
  d <- readRDS(input_rds_path)
  
  new_column_names <- c(
    "question_context_multiple_choice" = "context",
    "question8scs_multiple_choice" = "scs_neg_8",
    "question_satisfied_smiley" = "satisfied",
    "question_nervous_smiley" = "nervous",
    "question_dec1_multiple_choice" = "dec_1",
    "question_happiness_smiley" = "happy",
    "question5scs_multiple_choice" = "scs_neg_5",
    "question2scs_multiple_choice" = "scs_neg_2",
    "question_dec3_multiple_choice" = "dec_3",
    "question_sad_smiley" = "upset",
    "question7scs_multiple_choice" = "scs_pos_7",
    "question3scs_multiple_choice" = "scs_pos_3",
    "question4scs_multiple_choice" = "scs_neg_4",
    "question_dec2_multiple_choice" = "dec_2",
    "question_dec4_multiple_choice" = "dec_4",
    "question1scs_multiple_choice" = "scs_pos_1",
    "question6scs_multiple_choice" = "scs_pos_6",
    "question_emotion_multi_smiley" = "emotion_now",
    "yesnoexam_yesno" = "exam",
    "momentary_emotion_yes_multi_smiley" = "emotion_after_exam",
    "exam_preparation_slider_neg_pos" = "exam_preparation",
    "grade_exp_slider_neutral_pos" = "grade_exp",
    "momentary_emotion_no_multi_smiley" = "emotion_before_exam",
    "emotion_valence_no_smiley" = "emotion_valence_before_exam",
    "real_grade_slider_neutral_pos" = "real_grade",
    "emotion_valence_yes_smiley" = "emotion_valence_after_exam",
    "subj_code" = "user_id",
    "day" = "day",
    "hour" = "hour",
    "minute" = "minute",
    "time_window" = "time_window",
    "calendar_day" = "calendar_day",
    "bysubj_day" = "bysubj_day"
  )
  
  d <- d %>% rename_with(~new_column_names[.x], everything())
  
  levels_to_numeric <- c(
    "Molto spiacevole" = -2,
    "Spiacevole" = -1,
    "Né spiacevole né piacevole" = 0,
    "Piacevole" = 1,
    "Molto piacevole" = 2
  )
  
  d$context <- d$context %>% factor(levels = names(levels_to_numeric)) %>% as.numeric()
  
  levels2_to_numeric <- c(
    "Totalmente falso" = -3,
    "Moderatamente falso" = -2,
    "Leggermente falso" = -1,
    "Leggermente vero" = 1,
    "Moderatamente vero" = 2,
    "Totalmente vero" = 3
  )
  
  d <- d %>% 
    mutate(across(starts_with("scs_") | starts_with("dec_"), ~levels2_to_numeric[.x] %>% as.numeric()))
  
  d <- d %>% 
    separate(emotion_now, into = c("emo_now_1", "emo_now_2", "emo_now_3"), sep = ",\\s*", fill = "right", extra = "merge") %>%
    separate(emotion_after_exam, into = c("emo_after_exam_1", "emo_after_exam_2", "emo_after_exam_3"), sep = ",\\s*", fill = "right", extra = "merge") %>%
    separate(emotion_before_exam, into = c("emo_before_exam_1", "emo_before_exam_2", "emo_before_exam_3"), sep = ",\\s*", fill = "right", extra = "merge") %>%
    mutate(across(starts_with("emo_"), ~ifelse(.x == "-1", NA, .x)))
  
  # Remove sessions with a very small number of participants
  d <- d %>%
    group_by(user_id) %>%
    mutate(date_order = dense_rank(day)) %>%
    ungroup()
  
  d <- d |> 
    dplyr::filter(date_order < 20)
  
  d <- d |> 
    dplyr::select(-date_order)
  
  saveRDS(d, output_rds_path)
}


#' remove_wrong_days() ---------------------------------------------------------
#' 
#' @description
#' Remove the data corresponding to days in which participants were not meant
#' to use the app.
#' @param filepath The data frame after data wrangling
#' @return A data frame.
#' 
remove_wrong_days <- function(filepath_input, filepath_output) {
  library(dplyr)
  library(here)
  
  # Read raw data
  data <- readRDS(here::here(filepath_input))
  
  # Define project days
  project_days <- c(
    "2023-03-18", "2023-03-25", "2023-04-01", "2023-04-08", "2023-04-15",
    "2023-04-16", "2023-04-17", "2023-04-22", "2023-04-29", "2023-05-06",
    "2023-05-13", "2023-05-20", "2023-05-21", "2023-05-22", "2023-05-27", 
    "2023-06-03"
  )
  
  # Filter data for project days and compute new variables
  filtered_data <- data %>%
    filter(day %in% project_days) %>%
    mutate(
      neg_aff = upset + nervous - satisfied - happy,
      psc = scs_pos_1 + scs_pos_3 + scs_pos_6 + scs_pos_7,
      nsc = scs_neg_2 + scs_neg_4 + scs_neg_5 + scs_neg_8,
      dec = dec_1 + dec_3 - dec_2 - dec_4,
      exam_day = case_when(
        day == "2023-04-16" ~ "pre",
        day == "2023-04-17" ~ "post",
        day == "2023-05-21" ~ "pre",
        day == "2023-05-22" ~ "post",
        TRUE ~ "no_exam"
      )
    ) %>%
    filter(!is.na(psc) & !is.na(nsc) & !is.na(neg_aff)) %>%
    group_by(user_id) %>%
    mutate(bysubj_day = dense_rank(day)) %>%
    ungroup()
  
  saveRDS(filtered_data, filepath_output)
}


# calculate_average_compliance <- function(data) {
#   library(dplyr)
#   library(purrr)
#   
#   # Calculate compliance per user, per day
#   user_day_compliance <- data %>%
#     group_by(bysubj_day, user_id) %>%
#     summarize(
#       total_responses = n_distinct(time_window),
#       .groups = "drop"
#     )
#   
#   # Calculate average compliance and number of distinct participants for each bysubj_day
#   day_compliance <- user_day_compliance %>%
#     group_by(bysubj_day) %>%
#     summarize(
#       average_compliance = mean(total_responses / 5),  # Assuming 5 time windows per day
#       distinct_participants = n_distinct(user_id),
#       .groups = "drop"
#     )
#   
#   return(day_compliance)
# }


#' compute_effect_size() -------------------------------------------------------
#' @description
#' Compute effect size for a coefficient of a mixed-effect model computed with
#' brm()
#' @param brms_model
#' @param effect_name
#' @example: effect_size <- compute_effect_size(m1, "exam_daypost")

compute_effect_size <- function(brms_model, effect_name) {
  library(brms)
  library(posterior)
  
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
compute_effect_size_ci <- function(brms_model, effect_name, probs = c(0.025, 0.975)) {
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


# get_state_self_comp_piel_mpath() ---------------------------------------------

get_state_self_comp_piel_mpath <- function() {
  suppressPackageStartupMessages({
    library(tidyverse)
    library(here)
    library(psych)
    library(rio)
    library(semTools)
  })
  
  # Function to process each dataset
  process_data <- function(path) {
    readRDS(path) |>
      dplyr::relocate(any_of(c(starts_with("scs_neg"), starts_with("scs_pos")))) |>
      dplyr::select(user_id, starts_with("scs_")) |>
      mutate(across(where(is.numeric), ~ . - 4))
  }
  
  # Paths to data files
  piel_path <- 
    "~/_repositories/ema_scs_piel/data/prep/ema/ema_data_2.RDS"
  mpath_path <- here("data", "prep", "ema", "ema_data_3.RDS")
  
  # Process data for each dataset
  scs_items_piel <- process_data(piel_path)
  scs_items_mpath <- process_data(mpath_path)
  
  # Combine both data sets
  both_df <- bind_rows(scs_items_piel, scs_items_mpath)
  
  return(both_df)
}


# calculate_ssc_reliabilities() ------------------------------------------------
calculate_ssc_reliabilities <- function(both_df) {
  library(tidyverse)
  library(lavaan)
  library(semTools)

  process_ssc_data <- function(df, mcfa_model) {
    suppressWarnings({
      fit <- cfa(mcfa_model, data = df, cluster = "person")
      comp_rel <- compRelSEM(
        fit,
        obs.var = FALSE, config = c("f1"), shared = "f1"
      )
    })
    return(comp_rel)
  }

  # Define the multilevel SEM model
  mcfa_model <-
    "
    level: 1
    f1 =~ NA * i1 + l1 * i1 + l2 * i2 + l3 * i3 + l4 * i4
    i1 ~~ ev1w * i1
    i2 ~~ ev2w * i2
    i3 ~~ ev3w * i3
    i4 ~~ ev4w * i4
    f1 ~~ 1 * f1
    
    level: 2
    f1 =~ NA * i1 + l1 * i1 + l2 * i2 + l3 * i3 + l4 * i4
    # fixed residual variances
    i1 ~~ ev1b * i1
    i2 ~~    0 * i2
    i3 ~~ ev3b * i3
    i4 ~~ ev4b * i4
    f1 ~~ vf1b * f1
    
    # tilde omega values:
    tilomgb := (l1 + l2 + l3 + l4)^2 * vf1b /
      ((l1 + l2 + l3 + l4)^2 * vf1b + ev1b + 0 + ev3b + ev4b)
    tilomgw := (l1 + l2 + l3 + l4)^2 * 1 /
      ((l1 + l2 + l3 + l4)^2 * 1 + ev1w + ev2w + ev3w + ev4w)
    # score reliability:
    omg2l := (l1 + l2 + l3 + l4)^2 * (1 + vf1b) /
      ((l1 + l2 + l3 + l4)^2 * (1 + vf1b) + 
         ev1b + 0 + ev3b + ev4b + ev1w + ev2w + ev3w + ev4w)
    omgb := (l1 + l2 + l3 + l4)^2 * vf1b /
      ((l1 + l2 + l3 + l4)^2 * vf1b + ev1b + 0 + ev3b + ev4b + 
         (ev1w + ev2w + ev3w + ev4w + (l1 + l2 + l3 + l4)^2) / 25.1)
  "

  # Prepare data
  pos_ssc_df <- both_df %>% select(user_id, starts_with("scs_pos"))
  neg_ssc_df <- both_df %>% select(user_id, starts_with("scs_neg"))
  colnames(pos_ssc_df) <- c("person", "i1", "i2", "i3", "i4")
  colnames(neg_ssc_df) <- c("person", "i1", "i2", "i3", "i4")

  # Calculate reliabilities
  pos_reliabilities <- process_ssc_data(pos_ssc_df, mcfa_model)
  neg_reliabilities <- process_ssc_data(neg_ssc_df, mcfa_model)

  # Format and return results
  results <- list(
    positive = pos_reliabilities,
    negative = neg_reliabilities
  )

  return(results)
}


#' center3L() ------------------------------------------------------------------
#' 
center3L <- function(dataname, varname, idname, dayname) {
  within <- dataname %>%
    group_by({{ idname }}, {{ dayname }}) %>%
    mutate("{{varname}}_DddM" := mean({{ varname }}, na.rm = TRUE)) %>%
    mutate(DddM = mean({{ varname }}, na.rm = TRUE)) %>%
    mutate("{{varname}}_Moment" := {{ varname }} - mean({{ varname }}, na.rm = TRUE)) %>%
    ungroup()
  within2 <- dataname %>%
    group_by({{ idname }}, {{ dayname }}) %>%
    summarize(temp_dddm = mean({{ varname }}, na.rm = TRUE)) %>%
    summarize("{{varname}}_PppM" := mean(temp_dddm, na.rm = TRUE)) %>%
    ungroup()
  within3 <- dataname %>%
    group_by({{ idname }}, {{ dayname }}) %>%
    summarize(temp_dddm = mean({{ varname }}, na.rm = TRUE)) %>%
    summarize(PppM = mean(temp_dddm, na.rm = TRUE)) %>%
    ungroup()
  combinewithin <- merge(within2, within3)
  allwithin <- merge(within, combinewithin)
  allwithinc <- allwithin %>% 
    mutate("{{varname}}_Day" := DddM - PppM)
  between <- dataname %>%
    group_by({{ idname }}, {{ dayname }}) %>%
    summarize(temp_dddm = mean({{ varname }}, na.rm = TRUE)) %>%
    summarize(temp_pppm = mean(temp_dddm, na.rm = TRUE)) %>%
    summarize("{{varname}}_SssM" := mean(temp_pppm, na.rm = TRUE)) %>%
    ungroup()
  between2 <- dataname %>%
    group_by({{ idname }}, {{ dayname }}) %>%
    summarize(temp_dddm = mean({{ varname }}, na.rm = TRUE)) %>%
    summarize(temp_pppm = mean(temp_dddm, na.rm = TRUE)) %>%
    summarize(SssM = mean(temp_pppm, na.rm = TRUE)) %>%
    ungroup()
  between3 <- full_join(between, between2, by = character())
  combined <- full_join(allwithinc, between3, by = character())
  output <- combined %>% 
    mutate("{{varname}}_Person" := PppM - SssM)
  out <- select(output, -c(DddM, PppM, SssM))
  return(out)
}


#' process_exam_data() ---------------------------------------------------------
#' 
#' @description
#' This function pre-process the data for computing the difference in negative
#' affect before and after the exam.
#' @param exam_type It can be "first_exam" or "second_exam"
#' @return A data frame.
#' @example completed_data_first_exam <- process_exam_data("first_exam")
#' @example completed_data_second_exam <- process_exam_data("second_exam")

process_exam_data <- function(exam_type) {
  library(dplyr)
  library(mice)
  library(here)
  
  # Read raw data
  data <- readRDS(here::here("data", "prep", "ema", "ema_data_3.RDS"))
  
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
  
  # Perform multiple imputation
  imputed_model <- mice(unique_exam_data, m = 1, method = 'pmm', seed = 123)
  completed_data <- complete(imputed_model)
  
  return(completed_data)
}


#' compute_exam_effects_on_neg_aff() -------------------------------------------
#' @description
#' Compute the estimate of the negative affect difference before and after 
#' the exam.
#' 
#' @param data The data frame returned by the function process_exam_data()
#' @param exam_type Either "first_exam" or "secon_exam"
#' @return A list:
#' (1) the regression coefficient for the difference between the pre and the
#' post negative affect (negative numbers indicate that negative affect has
#' decreased).
#' (2) the standard error
#' (3) the 95% credibility interval of beta
#' (4) the effect size
#' (5) the 95% credibility interval of the effect size
compute_exam_effects_on_neg_aff <- function(exam_data, exam_type) {
  # Check if the data is empty
  if (nrow(exam_data) == 0) {
    stop("No data provided for the exam.")
  }
  
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
  
  # Fit the Bayesian model
  model <- brm(
    formula = model_formula,
    data = exam_data,
    family = asym_laplace(),
    prior = c(
      set_prior("normal(0, 200)", class = "b"),
      set_prior("normal(0, 100)", class = "sigma"),
      set_prior("normal(0, 0.2)", class = "quantile")
    ),
    iter = 10000,
    backend = "cmdstanr",
    chains = 3,
    cores = 6,
    threads = threading(2),
    silent = 2
  )
  
  return(compute_effects_internal(model))
}


#' get_estimates_neg_aff_difference_exam() -------------------------------------
#' 
#' @description
#' Returns the estimates of the negative affect difference pre-exam - post-exam
#' @param EXAM_TYPE Either "first_exam" or "second_exam"
#' @return A list
#' 
get_estimates_neg_aff_difference_exam <- function(EXAM_TYPE) {
  data = process_exam_data(EXAM_TYPE)
  compute_exam_effects_on_neg_aff(data, EXAM_TYPE)
}

#' calculate_compliance() ------------------------------------------------------
#' @description
#' Get the compliance in terms of the proportion of subjects (out of the total), 
#' who responded each notification day (compliance_rate), and the average
#' frequency of responses in each day (compliance_notification_frequeny_per_day)
#' @param filepath The path to the data to be analyzed (ema_data_3.RDS)
#' @return A list.
#' 
calculate_compliance <- function(filepath) {
  library(dplyr)
  library(here)
  
  # Read data
  alldata <- readRDS(here::here(filepath))
  
  # Calculate compliance rate
  n_per_day <- alldata %>%
    group_by(bysubj_day) %>%
    summarize(n = n_distinct(user_id)) %>%
    mutate(compliance_day = n / max(n)) %>%
    ungroup()
  
  compliance_rate <- mean(n_per_day$compliance_day)
  
  # Calculate compliance of notification frequency within each day
  days_exams <- c("2023-04-16", "2023-04-17", "2023-05-21", "2023-05-22")
  
  compliance_notification_frequeny_per_day <- alldata %>%
    filter(!day %in% days_exams) %>%
    group_by(user_id, bysubj_day) %>%
    summarize(n_responses = n_distinct(time_window)) %>%
    mutate(compliance_notification = n_responses / 5) %>%
    ungroup() %>%
    summarize(average_compliance = mean(compliance_notification)) %>%
    pull(average_compliance)
  
  # Return compliance rates
  output_list <- list(
    compliance_rate = compliance_rate,
    compliance_notification_frequeny_per_day = compliance_notification_frequeny_per_day
  )
  
  return(output_list)
}

#' calculate_min_max_neg_aff() -------------------------------------------------
#' 
calculate_min_max_neg_aff <- function(df) {
  df %>%
    filter(time_window == 5) %>%
    group_by(user_id) %>%
    summarize(
      min_neg_aff = min(neg_aff, na.rm = TRUE),
      max_neg_aff = max(neg_aff, na.rm = TRUE)
    ) %>%
    ungroup()
}


calculate_bysubj_mean_neg_aff_w5 <- function(df) {
  df %>%
    filter(time_window == 5) %>%
    group_by(user_id) %>%
    summarize(
      avg_neg_aff = mean(neg_aff, na.rm = TRUE)
    ) %>%
    ungroup()
}


#' compute_no_exam_effects_on_neg_aff() ----------------------------------------
#' @description
#' Compute the estimate of the negative affect difference between the average
#' negative affect in the no-exam days and the average negative affect either
#' in the pre-exam day or in the post-exam day.
#' 
#' @param data The data frame returned by the function 
#' gen_data_comparison_avg_pre_post_neg_aff
#' @param comparison_type Either "pre" or "post"
#' @return A list:
#' (1) the regression coefficient for the difference between the no-exam and the
#' (pre- o post-) exam negative affect.
#' (2) the standard error
#' (3) the 95% credibility interval of beta
#' (4) the effect size
#' (5) the 95% credibility interval of the effect size
compute_no_exam_effects_on_neg_aff <- function(data, comparison_type) {
  # Check if the data is empty
  if (nrow(data) == 0) {
    stop("No data provided for the exam.")
  }
  
  # Select model based on exam type
  if (comparison_type == "pre") {
    coef_name <- "exam_daypre_neg_aff"
  } else if (comparison_type == "post") {
    coef_name <- "exam_daypost_neg_aff"
  } else {
    stop("Invalid exam type specified.")
  }
  
  # Internal function to compute effects
  compute_effects_internal <- function(model) {
    summary_model <- summary(model)
    pop_effects <- summary_model$fixed
    
    exam_daypost_estimate <- pop_effects[coef_name, "Estimate"]
    exam_daypost_error <- pop_effects[coef_name, "Est.Error"]
    exam_daypost_lower_ci <- pop_effects[coef_name, "l-95% CI"]
    exam_daypost_upper_ci <- pop_effects[coef_name, "u-95% CI"]
    
    effect_size_exam <- compute_effect_size(model, coef_name)
    ci_effect_size_exam <- compute_effect_size_ci(model, coef_name)
    
    return(list(
      estimate = exam_daypost_estimate,
      error = exam_daypost_error,
      ci = c(exam_daypost_lower_ci, exam_daypost_upper_ci),
      effect_size = effect_size_exam,
      effect_size_ci = ci_effect_size_exam
    ))
  }
  
  model_formula <- neg_aff ~ 1 + exam_day + (1 | user_id)
  
  # Fit the Bayesian model
  model <- brm(
    formula = model_formula,
    data = data,
    family = student(),
    prior = c(
      set_prior("normal(0, 50)", class = "b"),
      set_prior("normal(0, 150)", class = "sigma")
      # set_prior("normal(0, 0.2)", class = "quantile")
    ),
    backend = "cmdstanr",
    chains = 3,
    cores = 6,
    threads = threading(2),
    silent = 2
  )
  
  return(compute_effects_internal(model))
}

#' gen_data_comparison_avg_pre_post_neg_aff() ----------------------------------
#' 
gen_data_comparison_avg_pre_post_neg_aff <- function(comparison_type) {
  
  # Read raw data.
  d <- readRDS(here::here("data", "prep", "ema", "ema_data_3.RDS"))
  
  no_exam_df <- d |> 
    dplyr::filter(exam_day == "no_exam")
  
  bysubj_avg_neg_aff_df <- calculate_bysubj_mean_neg_aff_w5(no_exam_df)
  
  exam_df <- d |> 
    dplyr::filter(exam_day != "no_exam") |> 
    dplyr::select(user_id, exam_day, neg_aff)
  
  exam_neg_aff_pre_df <- exam_df |> 
    dplyr::filter(exam_day == "pre") |> 
    group_by(user_id) |> 
    summarize(
      pre_neg_aff = mean(neg_aff)
    )
  
  exam_neg_aff_post_df <- exam_df |> 
    dplyr::filter(exam_day == "post") |> 
    group_by(user_id) |> 
    summarize(
      post_neg_aff = mean(neg_aff)
    )
  
  temp <- full_join(bysubj_avg_neg_aff_df, exam_neg_aff_pre_df, by = "user_id")
  
  bysubj_exam_neg_aff_df <- full_join(temp, exam_neg_aff_post_df, by = "user_id")
  
  # Select data based on comparison type
  if (comparison_type == "pre") {
    comp_avg_pre_df <- bysubj_exam_neg_aff_df |> 
      dplyr::select(-post_neg_aff) |> 
      pivot_longer(!user_id, names_to = "exam_day", values_to = "neg_aff")
    comp_avg_pre_df <- comp_avg_pre_df[complete.cases(comp_avg_pre_df), ]
    
    result <- comp_avg_pre_df
    
  } else if (comparison_type == "post") {
    comp_avg_post_df <- bysubj_exam_neg_aff_df |> 
      dplyr::select(-pre_neg_aff) |> 
      pivot_longer(!user_id, names_to = "exam_day", values_to = "neg_aff")
    comp_avg_post_df <- comp_avg_post_df[complete.cases(comp_avg_post_df), ]
    
    result <- comp_avg_post_df
  } else {
    stop("Invalid comparison type specified.")
  }
  
  return(result)
  
}




