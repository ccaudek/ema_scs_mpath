""" 
EMA State Self-Compassion Mpath 2023 
"""


include: "workflows/rules/common.smk"


##### Target rules #####


rule all:
    input:
        # Get mpath EMA data
        "data/prep/ema/mpath_ema_data_raw.rds",
        "data/prep/ema/mpath_ema_data_clean.rds",
        "data/prep/ema/ema_data.rds",
        # Compute compliance
        "data/prep/ema/compliance_results.rds",
        # Compute multilevel reliabilities
        "data/prep/ema/combined_piel_mpath_ema_data.rds",
        "data/prep/ema/ssc_reliabilities.rds",
        # Analysis of the effect of sequential negative affect on State Self-Compassion
        "data/prep/ema/brms_fits/fit_psc_delta_neg_aff.rds",
        "data/prep/ema/brms_fits/fit_nsc_delta_neg_aff.rds",
        # Model comparisons for (person, day, moment) centered data 
        # Get data for model comparisons
        "data/prep/ema/data_for_model_comparisons.rds",
        # Compute LOO values: CS component
        "data/prep/ema/brms_fits/loo_psc_rand_eff_mod_comp.rds",
        "data/prep/ema/brms_fits/loo_psc_fixed_eff_mod_comp.rds",
        # Compute LOO values: UCS component
        "data/prep/ema/brms_fits/loo_nsc_rand_eff_mod_comp.rds",
        "data/prep/ema/brms_fits/loo_nsc_fixed_eff_mod_comp.rds",
        # Report
        "doc/ema_ssc_report.html",


##### Modules #####


include: "workflows/rules/import_and_wrangling_ema_data.smk"
include: "workflows/rules/compute_compliance.smk"
include: "workflows/rules/compute_multilevel_reliabilities.smk"
include: "workflows/rules/neg_aff_seq_change_on_state_self_comp.smk"
include: "workflows/rules/model_comparisons_center3L.smk"
include: "workflows/rules/report.smk"
include: "workflows/rules/logging_information.smk"
