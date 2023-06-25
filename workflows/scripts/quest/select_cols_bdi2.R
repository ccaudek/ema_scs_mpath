#' BDI-2
#' The BDI-II is scored by summing the ratings for the 21 items. Each item is
#' rated on a 4-point scale ranging from 0 to 3. The maximum total score is 63.
#' Special attention must be paid to the correct scoring of the Changes in
#' Sleeping Pattern (Item 16) and Changes in Appetite (Item 18) items. Each of
#' these items contains seven options rated, in order, 0, 1a, 1b, 2a, 2b, 3a,
#' 3b, to differentiate between increases and decreases in behavior or
#' motivation. If a higher rated option is chosen by the respondent, the
#' presence of an increase or decrease in either symptom should be clinically
#' noted for diagnostic purposes.

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("rio")
})

source(here::here("workflows", "scripts", "quest", "funs", "funs_quest.R"))

NITEMS <- 21

d <- rio::import(
  # here::here("data", "prep", "quest_scales", "quest_ema_mpath.csv")
  snakemake@input[["quest_data"]]
)

# Add catch item to catch_items.csv file.
add_catch_item(d$user_id, d$cr4_1.x)

# Select 21 items
bdi2_items <- d %>%
  select(user_id, starts_with("bdi"))

# Rename columns
colnames(bdi2_items) <- c("user_id", paste0("bdi2_", 1:NITEMS))

# The coding for the items (not reversed) is the following:
# True = 3; Somewhat true = 2; Somewhat false = 1; False = 0.
# To obtain this coding we must perform this transformation.

set1 <- c(
  "bdi2_1", "bdi2_2", "bdi2_3", "bdi2_4", "bdi2_5",
  "bdi2_6", "bdi2_7", "bdi2_8", "bdi2_9", "bdi2_10", "bdi2_11",
  "bdi2_12", "bdi2_13", "bdi2_14", "bdi2_15", "bdi2_17",
  "bdi2_19", "bdi2_20", "bdi2_21"
)

bdi2_items <- bdi2_items %>%
  mutate(across(set1, ~ case_when(
    . == 4 ~ 3,
    . == 3 ~ 2,
    . == 2 ~ 1,
    . == 1 ~ 0,
    TRUE ~ NA_integer_
  )))

set2 <- c("bdi2_16", "bdi2_18")

bdi2_items <- bdi2_items %>%
  mutate(across(set2, ~ case_when(
    . == 7 ~ 3,
    . == 6 ~ 3,
    . == 5 ~ 2,
    . == 4 ~ 2,
    . == 3 ~ 1,
    . == 2 ~ 1,
    . == 1 ~ 0,
    TRUE ~ NA_integer_
  )))

# Save data
rio::export(
  bdi2_items,
  # here::here("data", "prep", "quest_scales", "bdi2_items.csv")
  snakemake@output[["bdi2_cols"]]
)

# eof ----
