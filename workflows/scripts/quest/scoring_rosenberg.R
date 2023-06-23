# Rosenberg Self-Esteem Scale.

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

rosenberg_items <- rio::import(
  here::here("data", "prep", "quest_scales", "rosenberg_items.csv")
  # snakemake@input[["rosenberg_cols"]]
)

# Source rosenberg.R on GitHub, which includes the function scoring_rosenberg().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/rosenberg.R"
)

rses <- scoring_rosenberg(rosenberg_items)

rio::export(
  rses,
  # snakemake@output[["rosenberg_scores"]]
  here::here("data", "prep", "quest_scales", "rosenberg_scores.csv")
)

# eof ----
