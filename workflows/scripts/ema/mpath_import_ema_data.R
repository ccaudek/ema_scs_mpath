# Script name: import_ema_data.R
# Project: EMA self-compassion mpath
# Script purpose: Read individual EMA data files and save an RDS file.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Jun 15 04:48:21 CEST 2023
# Last Modified Date: Thu Jun 15 04:54:41 CEST 2023
#
# 👉 input:  here("data", "raw", "piel2022")
#    output: here("data", "prep", "ema", "ema_data_1.RDS")

# log <- file(snakemake@log[[1]], open="wt")
# sink(log)
# sink(log, type="message")

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

source(here::here("workflows", "scripts", "ema", "functions", 
                  "funs_ema_mpath.R"))

# All individual EMA files are stored in this directory.
# dir <- here::here("data", "raw", "mpath2023")

### This is a test with only correct files!!!!
### It must be replaced with the folder name containing all the data files,
### when all files will be corrected.
FOLDER_NAME <- "m_path_data_2023"

# The first row of each xlsx file is empyt. It must be deleted. But
# this operation must only be performed once. To insure that, I use
# the following trick.
if (file.exists(here::here("data", "raw", FOLDER_NAME, "boop.txt"))) {
  # Delete the first row of each xlsx file.
  delete_first_row_xlsx(FOLDER_NAME)
  file.remove(here::here("data", "raw", FOLDER_NAME, "boop.txt"))
}

# Folder with all raw data files.
dir <- here::here("data", "raw", FOLDER_NAME)

file_names <- list.files(path = dir, full.names = TRUE)
file_names <- as.character(list.files(path = dir))
n_files <- length(file_names)


d_list <- list()

for (index_file in 1:n_files) {
  
  d <-
    rio::import(
      here("data", "raw", FOLDER_NAME, file_names[index_file]),
      header = FALSE
    )
  
  # Get subject code
  d$subj_code <-
    substr(file_names[index_file], 1, nchar(file_names[index_file]) - 5)
  
  # Change columns' names.
  # d1 <- change_cols_names(d)
  # d <- d1
  
  # Get time window
  d$datetime <- strptime(d$`Date and time`, "%a, %d %b %Y %H:%M:%S")
  d$day <- ymd(substring(d$datetime, 1, 10))
  d$hour <- hour(d$datetime)
  d$minute <- minute(d$datetime)
  
  d$time_window <- character(length(d$datetime))
  
  for (i in seq_along(d$datetime)) {
    if (d$hour[i] >= 0 & d$hour[i] < 12) {
      d$time_window[i] <- 1 # "10-11"
    } else if (d$hour[i] >= 12 & d$hour[i] < 16) {
      d$time_window[i] <- 2 # "14-15"
    } else if (d$hour[i] >= 16 & d$hour[i] < 19) {
      d$time_window[i] <- 3 # "17-18"
    } else if (d$hour[i] >= 19 & d$hour[i] < 21) {
      d$time_window[i] <- 4 # "19-20"
    } else if (d$hour[i] >= 21 & d$hour[i] < 24) {
      d$time_window[i] <- 5 # "21-23"
    } else {
      d$time_window[i] <- "NA"
    }
  }
  # table(d$user_id, d$time_window)
  
  foo <- d$day |>
    as.numeric()
  d$calendar_day <- as.numeric(factor(foo))
  
  d1 <- d %>%
    mutate(bysubj_day = dense_rank(calendar_day)) 
  
  # data.frame(
  #   d1$user_id, d1$calendar_day, d1$day, d1$bysubj_day
  # )[1:200, ]
  
  d2 <- d1 |>
    dplyr::select(
      -c(`Date and time`, datetime)
    )
  
  d3 <- d2 %>%
    janitor::clean_names()
  
  if ("momentary_emotion_no_multi_smiley" %in% names(d3)) {
    d3$momentary_emotion_no_multi_smiley <- 
      as.character(d3$momentary_emotion_no_multi_smiley)
  }
  
  if ("momentary_emotion_yes_multi_smiley" %in% names(d3)) {
    d3$momentary_emotion_yes_multi_smiley <- 
      as.character(d3$momentary_emotion_yes_multi_smiley)
  }
  
  d_list[[index_file]] <- d3
}

# Get a vector of unique column names across all data frames
all_columns <- unique(unlist(lapply(d_list, names)))

# Create a new data frame with consistent columns and fill missing columns with NA
combined_df <- do.call(bind_rows, lapply(d_list, function(df) {
  missing_columns <- setdiff(all_columns, names(df))
  df[, missing_columns] <- NA
  return(df)
}))

# Save complete raw data
saveRDS(
  combined_df,
  # here::here("data", "prep", "ema", "ema_data_1.RDS")
  snakemake@output[["rds"]]
)

# eof ----
