# _targets_stats_coefs.R #

# Produces the ...

# Targets:
list(
  
  # State Self-Compassion, negative component, coef = b_dec_moment
  tar_target(
    stats_coef_dec_moment,
    get_stats_brms_param(
      here::here("workflows", "scripts", "ema", "brms_fits", "bmod9_nsc.rds"), 
      "b_dec_moment"
      )
  ),
  
  # State Self-Compassion, negative component, coef = b_dec_day
  tar_target(
    stats_coef_dec_day,
    get_stats_brms_param(
      here::here("workflows", "scripts", "ema", "brms_fits", "bmod9_nsc.rds"), 
      "b_dec_day"
    )
  ),
  
  # State Self-Compassion, negative component, coef = b_dec_person
  tar_target(
    stats_coef_dec_person,
    get_stats_brms_param(
      here::here("workflows", "scripts", "ema", "brms_fits", "bmod9_nsc.rds"), 
      "b_dec_person"
    )
  )
  
  # close list  
)


