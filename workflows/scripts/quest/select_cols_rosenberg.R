# Rosenberg Self-Esteem Scale

suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("rio")
})

source(here::here("workflows", "scripts", "quest", "funs", "funs_quest.R"))

NITEMS <- 10

d <- rio::import(
  # here::here("data", "prep", "quest_scales", "quest_ema_mpath.csv")
  snakemake@input[["quest_data"]]
)

# Add catch item to catch_items.csv file.
add_catch_item(d$user_id, d$cr4_1.y)

# Select 10 items
rosenberg_items <- d %>%
  select(user_id, starts_with("ros"))

# Rename columns
colnames(rosenberg_items) <- c("user_id", paste0("ros_", 1:NITEMS))

# Save data
rio::export(
  rosenberg_items,
  # here::here("data", "prep", "quest_scales", "rosenberg_items.csv")
  snakemake@output[["rosenberg_cols"]]
)

# eof ----
