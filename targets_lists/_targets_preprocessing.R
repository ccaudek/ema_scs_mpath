# _targets_preprocessing.R #

source(here::here("workflows", "scripts", "ema", "functions", "funs_ema_mpath.R"))

# Targets:
list(

  # Import EMA data
  tar_target(
    mpath_ema_data_raw,
    import_ema_data(input_folder = "m_path_data_2023")
  ),

  # Data wrangling
  tar_target(
    clean_mpath_ema_data,
    process_ema_data(mpath_ema_data_raw)
  ),

  # Remove data deriving from responses in days not included in the project
  tar_target(
    ema_data,
    remove_wrong_days(clean_mpath_ema_data)
  ),

  # Get compliance
  tar_target(
    compliance_results,
    calculate_compliance(ema_data)
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
  )

  # close list
)

