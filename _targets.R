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
})

options(pillar.neg=FALSE)
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
    "janitor", "semTools"
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


# Replace the target list below with your own:
# Targets:
list(
  
  # Import EMA data
  tar_target(
    mpath_ema_data_raw,
    import_ema_data(
      input_folder = "m_path_data_2023", 
      output_rds_path = here("data", "prep", "ema", "ema_data_1.RDS")
    ),
    format = "file"
  ),

  # Data wrangling
  tar_target(
    clean_mpath_ema_data,
    process_ema_data(
      input_rds_path = here("data", "prep", "ema", "ema_data_1.RDS"),
      output_rds_path = here("data", "prep", "ema", "ema_data_2.RDS")
    ),
    format = "file"
  ),
  
  # Remove data deriving from responses in days not included in the project
  tar_target(
    removed_wrong_days_data,
    remove_wrong_days(
      filepath_input = here("data", "prep", "ema", "ema_data_2.RDS"),
      filepath_output = here("data", "prep", "ema", "ema_data_3.RDS")
    ),
    format = "file"
  ),
  
  # Get compliance
  tar_target(
    compliance_results,
    calculate_compliance(
      filepath = here("data", "prep", "ema", "ema_data_3.RDS")
    )
  ),

  # Get State Self Compassion data for both piel and mpath samples
  tar_target(
    state_self_comp_piel_mpath_data,
    get_state_self_comp_piel_mpath()
  ),

  # Compute multilevel reliabilities
  tar_target(
    reliabilities_sem,
    calculate_ssc_reliabilities(state_self_comp_piel_mpath_data)
  ),
  
  # Get estimate and effect size of negative affect pre-exam - post-exam
  # First exam
  tar_target(
    list_params_first_exam_neg_aff_difference,
    get_estimates_neg_aff_difference_exam("first_exam")
  ),
  
  # Get estimate and effect size of negative affect pre-exam - post-exam
  # Second exam
  tar_target(
    list_params_second_exam_neg_aff_difference,
    get_estimates_neg_aff_difference_exam("second_exam")
  ),

  # Create report.
  tar_quarto(
    name = report,
    path = here::here("doc", "ema_ssc_report.qmd")
  )

  # close list  
)







