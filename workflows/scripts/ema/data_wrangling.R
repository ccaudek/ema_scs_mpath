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
  library(tidyr)
})

source(here::here("workflows", "scripts", "ema", "functions", "funs_ema_mpath.R"))


# Read complete raw data
d <- readRDS(
  here::here("data", "prep", "ema", "ema_data_1.RDS")
  # snakemake@input[["rds"]]
) 

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
  present_emotion, 
  into = c("present_emo_1", "present_emo_2", "present_emo_3"), 
  sep = ",", fill = "right", extra = "drop"
)

d3 <- separate(
  d3, 
  after_exam_emotion, 
  into = c("after_exam_emo_1", "after_exam_emo_2", "after_exam_emo_3"), 
  sep = ",", fill = "right", extra = "drop"
)

d3$after_exam_emo_1 <- ifelse(
  d3$after_exam_emo_1 == "-1", NA, d3$after_exam_emo_1
)

d3 <- separate(
  d3, 
  before_exam_emotion, 
  into = c("before_exam_emo_1", "before_exam_emo_2", "before_exam_emo_3"), 
  sep = ",", fill = "right", extra = "drop"
)
d3$before_exam_emo_1 <- ifelse(
  d3$before_exam_emo_1 == "-1", NA, d3$before_exam_emo_1
)

# Save RDS file.
saveRDS(
  d3,
  # snakemake@output[["rds"]]
  here::here("data", "prep", "ema", "ema_data_2.RDS")
)

# eof ----
