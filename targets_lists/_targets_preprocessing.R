# _targets_preprocessing.R #


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
  )

  # close list  
)


