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
  library("psych")
  library("ggplot2")
  library("dplyr")
  library("rio")
  library("here")
  library("lubridate")
  library("tidyr")
})

source(here::here("workflows", "scripts", "ema", "functions", "funs_ema_mpath.R"))


# Read complete raw data
d <- readRDS(
  # here::here("data", "prep", "ema", "ema_data_1.RDS")
  snakemake@input[["rds"]]
)

# Change columns names
d <- change_cols_names(d)


# Recode `context` -------------------------------------------------------------
# Define the levels you want to convert to numeric values
levels_to_numeric <- c(
  "Molto spiacevole" = -2,
  "Spiacevole" = -1,
  "Né spiacevole né piacevole" = 0,
  "Piacevole" = 1,
  "Molto piacevole" = 2
)

# Convert catch item to numeric values.
d$context <-
  apply(data.frame(d$context), 2, function(x) levels_to_numeric[x]) |>
  as.numeric()

# Recode `scs` -----------------------------------------------------------------

levels2_to_numeric <- c(
  "Totalmente falso" = -3,
  "Moderatamente falso" = -2,
  "Leggermente falso" = -1,
  "Leggermente vero" = 1,
  "Moderatamente vero" = 2,
  "Totalmente vero" = 3
)

# decentramento
# neg: dec_2, dec_4 (mancata capacità di decentramento)
# pos: dec_1, dec_3

temp <- d |>
  dplyr::select(dplyr::starts_with("scs_") | dplyr::starts_with("dec_"))


# Convert catch item to numeric values.
temp1 <-
  apply(temp, 2, function(x) levels2_to_numeric[x]) |>
  as.data.frame()

# Remove `scs_` and `dec_` items.
d1 <- d |>
  dplyr::select(-starts_with("scs_"), -starts_with("dec_"))
# Add numeric columns for `scs_` and `dec_`
d2 <- cbind(d1, temp1)

# Separate `present_emotion` into three columns.
d3 <- separate(
  d2,
  emotion_now,
  into = c("emo_now_1", "emo_now_2", "emo_now_3"),
  sep = ",", fill = "right", extra = "drop"
)

d3 <- separate(
  d3,
  emotion_after_exam,
  into = c("emo_after_exam_1", "emo_after_exam_2", "emo_after_exam_3"),
  sep = ",", fill = "right", extra = "drop"
)

d3$emo_after_exam_1 <- ifelse(
  d3$emo_after_exam_1 == "-1", NA, d3$emo_after_exam_1
)

d3 <- separate(
  d3,
  emotion_before_exam,
  into = c("emo_before_exam_1", "emo_before_exam_2", "emo_before_exam_3"),
  sep = ",", fill = "right", extra = "drop"
)

d3$emo_before_exam_1 <- ifelse(
  d3$emo_before_exam_1 == "-1", NA, d3$emo_before_exam_1
)

# Save RDS file.
saveRDS(
  d3,
  snakemake@output[["rds"]]
  # here::here("data", "prep", "ema", "ema_data_2.RDS")
)

# eof ----
