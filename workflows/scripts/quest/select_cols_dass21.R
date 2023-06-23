# DASS-21

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("rio")
})

source(here::here("workflows", "scripts", "quest", "funs", "funs_quest.R"))

NITEMS <- 21

d <- rio::import(
  here::here("data", "prep", "quest_scales", "quest_ema_mpath.csv")
  # snakemake@input[["quest_data1"]]
)

# Add catch item to catch_items.csv file.
add_catch_item(d$user_id, d$cr2_1.x)

# Select 21 items
dass21_items <- d %>%
  select(user_id, starts_with("dass"))

# Rename columns
colnames(dass21_items) <- c("user_id", paste0("dass21_", 1:NITEMS))

# Save data
rio::export(
  dass21_items,
  here::here("data", "prep", "quest_scales", "dass21_items.csv")
  # snakemake@output[["dass21_cols"]]
)

# eof ----
