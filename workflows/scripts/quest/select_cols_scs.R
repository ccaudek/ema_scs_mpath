# Self-Compassion Scale

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
})

source(here::here("workflows", "scripts", "quest", "funs", "funs_quest.R"))

d <- rio::import(
  # here::here("data", "prep", "quest_scales", "quest_ema_mpath.csv")
  snakemake@input[["quest_data"]]
  )

NITEMS <- 26

scs_items <- d |>
  dplyr::select("user_id", starts_with("scs")) |>
  # catch item
  dplyr::select(-"scs_25")

scs_items_names <- paste0("scs_", 1:NITEMS)
scs_items_names_plus_id <- c("user_id", scs_items_names)
colnames(scs_items) <- scs_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d$user_id, d$scs_25)

rio::export(
  scs_items,
  # here::here("data", "prep", "quest_scales", "scs_items.csv")
  snakemake@output[["scs_cols"]]
)

