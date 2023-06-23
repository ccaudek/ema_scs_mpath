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
})

source(here::here("workflows", "scripts", "ema", "functions", "funs_ema_mpath.R"))

# All individual EMA files are stored in this directory.
# dir <- here::here("data", "raw", "mpath2023")

### This is a test with only correct files!!!!
### It must be replaced with the folder name containing all the data files,
### when all files will be corrected.
FOLDER_NAME <- "files28"

# The first row of each xlsx file is empyt. It must be deleted. But
# this operation must only be performed once. To insure that, I use
# the following trick.
if (file.exists(here::here("data", "raw", FOLDER_NAME, "boop.txt"))) {
  # Delete the first row of each xlsx file.
  delete_first_row_xlsx()
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

  # print(dim(d))

  d_list[[index_file]] <- d
}

mydat <- do.call(rbind.data.frame, d_list)

# Change columns' names.
d <- change_cols_names(mydat)

# Get time window
d$datetime <- strptime(d$date_time, "%a, %d %b %Y %H:%M:%S")
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
  group_by(user_id) %>%
  mutate(bysubj_day = dense_rank(calendar_day)) |>
  ungroup()

# data.frame(
#   d1$user_id, d1$calendar_day, d1$day, d1$bysubj_day
# )[1:200, ]

d2 <- d1 |>
  dplyr::select(
    -c(date_time, datetime)
  )

# Save complete raw data
saveRDS(
  d2,
  # here::here("data", "prep", "ema", "ema_data_1.RDS")
  snakemake@output[["rds"]]
)

# eof ----
