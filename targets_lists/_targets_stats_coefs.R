# _targets_stats_coefs.R #

# Produces the ...

source(here:::here("workflows", "scripts", "ema", "functions", "funs_ema_brms_mpath.R"))


# Targets:
list(
  
  # State Self-Compassion, negative component, coef = b_dec_moment
  tar_target(
    stats_coef_dec_moment,
    get_stats_brms_param(
      here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_ucs.rds"), 
      "b_dec_moment"
      )
  ),
  
  # State Self-Compassion, negative component, coef = b_dec_day
  tar_target(
    stats_coef_dec_day,
    get_stats_brms_param(
      here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_ucs.rds"), 
      "b_na_day"
    )
  ),
  
  # State Self-Compassion, negative component, coef = b_dec_person
  tar_target(
    stats_coef_dec_person,
    get_stats_brms_param(
      here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_ucs.rds"), 
      "b_na_person"
    )
  ),
  
  # State Self-Compassion, negative component, coef = b_dec_moment
  tar_target(
    stats_coef_na_moment,
    get_stats_brms_param(
      here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_ucs.rds"), 
      "b_na_moment"
    )
  ),
  
  # State Self-Compassion, negative component, coef = b_dec_day
  tar_target(
    stats_coef_na_day,
    get_stats_brms_param(
      here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_ucs.rds"), 
      "b_dec_day"
    )
  ),
  
  # State Self-Compassion, negative component, coef = b_dec_person
  tar_target(
    stats_coef_na_person,
    get_stats_brms_param(
      here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_ucs.rds"), 
      "b_dec_person"
    )
  ),
  
  tar_target(
    stats_coef_con_day,
    get_stats_brms_param(
      here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_ucs.rds"), 
      "b_con_day"
    )
  ),
  
  # State Self-Compassion, negative component, coef = b_dec_person
  tar_target(
    stats_coef_con_person,
    get_stats_brms_param(
      here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_ucs.rds"), 
      "b_con_person"
    )
  ),
  
  # State Self-Compassion, negative component, coef = b_dec_moment
  tar_target(
    stats_coef_con_moment,
    get_stats_brms_param(
      here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_ucs.rds"), 
      "b_con_moment"
    )
  )
  
  # close list  
)


