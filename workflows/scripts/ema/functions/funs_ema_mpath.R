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
  
  saveRDS(d, output_rds_path)
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
  mpath_path <- here("data", "prep", "ema", "ema_data_2bis.RDS")
  
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



