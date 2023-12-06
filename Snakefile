""" 
EMA State Self-Compassion Mpath 2023 
"""


configfile: "config/config.yaml"


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
        # Testing the Bipolar Continuum Hypothesis
        "data/prep/ema/data_mod_9_rnd_slopes.rds",
        "data/prep/ema/brms_fits/fit_mod_9_rnd_slopes_bip_cont_hyp.RDS",
        # Effect of exam day on Negative Affect
        "data/prep/ema/data_first_exam.rds",
        "data/prep/ema/res_neg_aff_on_first_exam.rds",
        "data/prep/ema/res_neg_aff_on_second_exam.rds",
        # Effect of exam day on State Self-Compassion
        # "data/prep/ema/stan_data_exam_state_self_comp.rds",
        # "data/prep/ema/brms_fits/fit_mod_exam_sc_4.RDS",
        # "data/prep/ema/brms_fits/fit_mod_exam_usc_4.RDS",
        # "doc/figures/plot_mod_exam_ssc.pdf",
        # Report
        "doc/ema_ssc_report.html",


##### Modules #####


include: "workflows/rules/import_and_wrangling_ema_data.smk"
include: "workflows/rules/compute_compliance.smk"
include: "workflows/rules/compute_multilevel_reliabilities.smk"
include: "workflows/rules/neg_aff_seq_change_on_state_self_comp.smk"
include: "workflows/rules/model_comparisons_center3L.smk"
include: "workflows/rules/bipolar_cont_hyp_test.smk"
include: "workflows/rules/exam_day_state_self_comp.smk"
include: "workflows/rules/neg_aff_exam_pre_post.smk"
include: "workflows/rules/report.smk"
include: "workflows/rules/logging_information.smk"
