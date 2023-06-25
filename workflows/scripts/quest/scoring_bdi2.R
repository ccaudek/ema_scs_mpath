# BDI-2

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

bdi2_items <- rio::import(
  # here::here("data", "prep", "quest_scales", "bdi2_items.csv")
  snakemake@input[["bdi2_cols"]]
)

# Source bdi2.R on GitHub, which includes the function scoring_bdi2().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/bdi2.R"
)

bdi2_subscales <- scoring_bdi2(bdi2_items)

rio::export(
  bdi2_subscales,
  # here::here("data", "prep", "quest_scales", "bdi2_scores.csv")
  snakemake@output[["bdi2_scores"]]
)

# eof ----
