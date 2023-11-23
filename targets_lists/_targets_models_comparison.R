# _targets_models_comparison.R #

# Targets:
list(
  
  # Get data for brms models' comparisons
  tar_target(
    data_for_models_comparison,
    get_data_models_comparison()
  ),

  # LOO model comparison for the negative component of the State Self_Compassion
  tar_target(
    loo_comparison_nssc_models,
    fit_and_compare_neg_ssc_models(data_for_models_comparison)
  ),
  
  # LOO model comparison for the positive component of the State Self_Compassion
  tar_target(
    loo_comparison_pssc_models,
    fit_and_compare_pos_ssc_models(data_for_models_comparison)
  )

  # close list  
)
