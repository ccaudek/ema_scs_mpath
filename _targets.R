# _targets.R #

# Created by use_targets()
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   <https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline> # nolint

# Loading package namespaces:
suppressPackageStartupMessages({
  library("here")
  library("targets")
  library("tarchetypes")
  library("tidyverse")
  library("tidyr")
  library("purrr")
  library("cmdstanr")
  library("brms")
  library("posterior")
  library("loo")
  library("lme4")
  library("mice")
  library("parallel")
  library("emmeans")
  library("quarto")
  library("bayesplot") 
  library("gridExtra")
  library("ggdist")
  library("viridis")
  library("psych")
  library("sjPlot")
  library("rio")
  library("lubridate")
  library("readxl")
  library("janitor")
  library("semTools")
  library("bayestestR")
})

theme_set(bayesplot::theme_default(base_family = "sans"))

# Set random seed for reproducibility:
SEED <- 48927 

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c(
    "here", "targets", "tarchetypes", "tidyverse", "tidyr", "purrr",
    "cmdstanr", "brms", "posterior", "loo", "mice", "parallel", "emmeans",
    "quarto", "bayesplot", "gridExtra", "ggdist", "effectsize", "rio",
    "sjstats", "sjPlot", "sjmisc", "viridis", "lubridate", "readxl",
    "janitor", "semTools", "bayestestR"
  ),
  format = "qs", # faster than rds
  seed = SEED
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Sourcing helper files:
list.files(
  here::here("workflows", "scripts", "ema", "functions"),
  pattern = "\\.R$", recursive = TRUE,
  full.names = TRUE
) %>%
  walk(source)


source("targets_lists/_targets_preprocessing.R")
# source("targets_lists/_targets_exam_pre_post.R")
# source("targets_lists/_targets_models_comparison.R")
# source("targets_lists/_targets_stats_coefs.R")
# source("targets_lists/_targets_figures_coefs.R") 
# source("targets_lists/_targets_report.R")



