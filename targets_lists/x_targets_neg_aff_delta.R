
list(
  
  tar_load(ema_data),
  
  tar_target(
    mod_psc_delta_neg_aff,
    fit_delta_neg_aff_model(ema_data, "psc")
  )
  
)