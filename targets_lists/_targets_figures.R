# _targets_figures.R #

# Generates two pdf files, with the plot of the brms coefficients of the
# model that predicts the negative and the positive components of State
# Self-Compassion from the Moment, Day, and Person components of Negative
# Affect, Context, and Decentering.
# Generate additional figures.
# Requires "bmod9_nsc.rds" and "bmod9_psc.rds".

# Targets:
list(
  
  tar_load(ema_data),

  # State Self-Compassion - Negative component
  tar_target(
    plot_mod_5_state_ucs_coefs,
    plot_brms_coefficients(
      readRDS(here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_ucs.rds")),
      parameters_of_interest = NULL,
      "Negative Component",
      "plot_coefs_state_ucs.pdf"
    ),
    format = "file"
  ),

  # State Self-Compassion - Positive component
  tar_target(
    plot_mod_5_state_cs_coefs,
    plot_brms_coefficients(
      readRDS(here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_cs.rds")),
      parameters_of_interest = NULL,
      "Positive Component",
      "plot_coefs_state_cs.pdf"
    ),
    format = "file"
  ),

  # Variance components for the random effects of the model for CS
  tar_target(
    plot_var_comp_mod_5_cs,
    posterior_variance_estimates(
      here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_cs.rds"),
      here::here("doc", "figures", "var_comp_mod_5_cs.pdf")
    ),
    format = "file"
  ),

  # Variance components for the random effects of the model for UCS
  tar_target(
    plot_var_comp_mod_5_ucs,
    posterior_variance_estimates(
      here::here("workflows", "scripts", "ema", "brms_fits", "mod_5_state_ucs.rds"),
      here::here("doc", "figures", "var_comp_mod_5_ucs.pdf")
    ),
    format = "file"
  ),

  # Generate figure with CS and UCS components as a function of negative affect
  # for each of the 16 days.
  tar_target(
    plot_cs_ucs_by_neg_aff_day_figures,
    plot_cs_ucs_by_neg_aff_day(
      ema_data, 
      here::here("doc", "figures", "cs_ucs_neg_aff_day.pdf")
    ),
    format = "file"
    # close list
  )
)

