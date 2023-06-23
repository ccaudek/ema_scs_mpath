# EAT 26

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
})

source(here::here("workflows", "scripts", "funs", "funs_quest.R"))

d <- rio::import(
  here::here("data", "prep", "quest_scales", "quest_ema_mpath.csv")
  # snakemake@input[["quest_data1"]]
)

NITEMS <- 26

# Add catch item to catch_items.csv file.
add_catch_item(d$user_id, d$cr1_1)

# Select 26 items
eat26_items <- d %>%
  select(user_id, starts_with("eat"))

# Rename columns
colnames(eat26_items) <- c("user_id", paste0("eat26_", 1:NITEMS))

# Save data
rio::export(
  eat26_items,
  here::here("data", "prep", "quest_scales", "eat26_items.csv")
  # snakemake@output[["eat26_cols"]]
)

# eof ----
