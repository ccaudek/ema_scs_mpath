# _targets_models_comparison.R #

source(here:::here("workflows", "scripts", "ema", "functions", "funs_model_comparisons.R"))

# Targets:
list(
  
  tar_load(ema_data),
  
  # Get data for brms models' comparisons
  tar_target(
    data_for_models_comparison,
    get_data_for_model_comparisons(ema_data)
  ),

  # LOO model comparison for the UCS component of the State Self_Compassion
  tar_target(
    loo_comparison_state_ucs_models,
    fit_and_compare_models(data_for_models_comparison, "state_ucs")
  ),
  
  # LOO model comparison for the CS component of the State Self_Compassion
  tar_target(
    loo_comparison_state_cs_models,
    fit_and_compare_models(data_for_models_comparison, "state_cs")
  )

  # close list  
)
