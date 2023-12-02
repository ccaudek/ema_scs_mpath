""" 
EMA State Self-Compassion Mpath 2023 
"""

import os
from pathlib import Path
from snakemake.utils import min_version


# Snake Version
min_version("5.7.1")


# Configuration file
# configfile: "config/config.yaml"


print(f"Current directory: {Path.cwd()}")
print(f"Home directory: {Path.home()}")


# %% All rule  ------------------------------------------------------


rule all:
    input:
        # ------------------- Get mpath EMA data
        "data/prep/ema/mpath_ema_data_raw.rds",
        "data/prep/ema/mpath_ema_data_clean.rds",
        "data/prep/ema/ema_data.rds",
        # ------------------- Compliance
        "data/prep/ema/compliance_results.rds",
        # ------------------- Multilevel reliabilities
        "data/prep/ema/combined_piel_mpath_ema_data.rds",
        "data/prep/ema/ssc_reliabilities.rds",
        # ------------------- Delta negative affect
        "data/prep/ema/brms_fits/fit_psc_delta_neg_aff.rds",
        "data/prep/ema/brms_fits/fit_nsc_delta_neg_aff.rds",
        # ------------------- Model comparisons
        # Get data for model comparisons
        "data/prep/ema/data_for_model_comparisons.rds",
        # Test run for computing LOO values
        # CS component
        "data/prep/ema/brms_fits/loo_psc_rand_eff_mod_comp.rds",
        "data/prep/ema/brms_fits/loo_psc_fixed_eff_mod_comp.rds",
        # UCS component
        "data/prep/ema/brms_fits/loo_nsc_rand_eff_mod_comp.rds",
        "data/prep/ema/brms_fits/loo_nsc_fixed_eff_mod_comp.rds",
        # ------------------- Report
        "doc/ema_ssc_report.html",


# %% ----- Import EMA data -----


rule import_ema_data:
    output:
        "data/prep/ema/mpath_ema_data_raw.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_ema_mpath.R"); 
        import_ema_data("m_path_data_2023", "{output}")'
        """


# %% ----- Data wrangling -----


rule process_mpath_ema_data:
    input:
        "data/prep/ema/mpath_ema_data_raw.rds",
    output:
        "data/prep/ema/mpath_ema_data_clean.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_ema_mpath.R");
        process_ema_data("{input}", "{output}")'
        """


# Remove wrong days
rule remove_wrong_days:
    input:
        "data/prep/ema/mpath_ema_data_clean.rds",
    output:
        "data/prep/ema/ema_data.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_ema_mpath.R");
        remove_wrong_days("{input}", "{output}")'
        """


# %% ----- Compute Compliance -----


rule get_compliance:
    input:
        "data/prep/ema/ema_data.rds",
    output:
        "data/prep/ema/compliance_results.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_ema_mpath.R");
        get_compliance("{input}", "{output}")'
        """


# %% ----- Compute multilevel reliabilities -----


# Get State Self-Compassion data for both piel and mpath projects
rule get_state_self_comp_piel_mpath:
    input:
        "data/prep/ema/ema_data.rds",
    output:
        "data/prep/ema/combined_piel_mpath_ema_data.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_ema_mpath.R");
        get_state_self_comp_piel_mpath("{input}", "{output}")'
        """


rule get_ssc_reliabilities:
    input:
        "data/prep/ema/combined_piel_mpath_ema_data.rds",
    output:
        "data/prep/ema/ssc_reliabilities.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_ema_mpath.R");
        get_ssc_reliabilities("{input}", "{output}")'
        """


# %% ----- Influence of sequential changes in Negative Affect on State Self-Compassion -----


# CS component of State Self-Compassion.
rule fit_psc_delta_neg_aff_model:
    input:
        data_path="data/prep/ema/ema_data.rds",
    params:
        dependent_var="psc",
    output:
        model_path="data/prep/ema/brms_fits/fit_psc_delta_neg_aff.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_delta_neg_aff.R");
        fit_delta_neg_aff_model("{input.data_path}", "{params.dependent_var}", "{output.model_path}")'
        """


# UCS component of State Self-Compassion.
rule fit_nsc_delta_neg_aff_model:
    input:
        data_path="data/prep/ema/ema_data.rds",
    params:
        dependent_var="nsc",
    output:
        model_path="data/prep/ema/brms_fits/fit_nsc_delta_neg_aff.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_delta_neg_aff.R");
        fit_delta_neg_aff_model("{input.data_path}", "{params.dependent_var}", "{output.model_path}")'
        """


# %% ----- Models' comparison -----


# Get data for brms models' comparisons
rule get_data_for_model_comparisons:
    input:
        "data/prep/ema/ema_data.rds",
    output:
        "data/prep/ema/data_for_model_comparisons.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_model_comparisons.R");
        get_data_for_model_comparisons("{input}", "{output}")'
        """


# LOO model comparison for the CS component of the State Self_Compassion
# Random-effect structure
rule fit_and_compare_random_effects_cs:
    input:
        data_path="data/prep/ema/data_for_model_comparisons.rds",
    params:
        dependent_var="state_cs",
        testing_mode="FALSE",  # Pass as a string
    output:
        model_path="data/prep/ema/brms_fits/loo_psc_rand_eff_mod_comp.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_model_comparisons.R");
        fit_and_compare_random_effects("{input.data_path}", "{params.dependent_var}", "{output.model_path}", {params.testing_mode})'
        """


# Fixed-effect structure
rule fit_and_compare_fixed_effects_cs:
    input:
        data_path="data/prep/ema/data_for_model_comparisons.rds",
    params:
        dependent_var="state_cs",
        testing_mode="FALSE",
    output:
        model_path="data/prep/ema/brms_fits/loo_psc_fixed_eff_mod_comp.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_model_comparisons.R");
        fit_and_compare_fixed_effects("{input.data_path}", "{params.dependent_var}", "{output.model_path}", {params.testing_mode})'
        """


# LOO model comparison for the UCS component of the State Self_Compassion
# Random-effect structure
rule fit_and_compare_random_effects_ucs:
    input:
        data_path="data/prep/ema/data_for_model_comparisons.rds",
    params:
        dependent_var="state_ucs",
        testing_mode="FALSE",
    output:
        model_path="data/prep/ema/brms_fits/loo_nsc_rand_eff_mod_comp.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_model_comparisons.R");
        fit_and_compare_random_effects("{input.data_path}", "{params.dependent_var}", "{output.model_path}", {params.testing_mode})'
        """


# Fixed-effect structure
rule fit_and_compare_fixed_effects_ucs:
    input:
        data_path="data/prep/ema/data_for_model_comparisons.rds",
    params:
        dependent_var="state_ucs",
        testing_mode="FALSE",
    output:
        model_path="data/prep/ema/brms_fits/loo_nsc_fixed_eff_mod_comp.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_model_comparisons.R");
        fit_and_compare_fixed_effects("{input.data_path}", "{params.dependent_var}", "{output.model_path}", {params.testing_mode})'
        """


# %% ----- Report -----


rule create_report:
    output:
        report="doc/ema_ssc_report.html",  # Quarto will automatically place the output here
    shell:
        """
        quarto render doc/ema_ssc_report.qmd --to html
        """


# # %% Read questionnaire data and save two CSV files -----------------
#
#
# rule read_quest_data:
#     output:
#         quest_data=config["quest_data"],
#     log:
#         "logs/read_quest_data.log",
#     script:
#         "workflows/scripts/quest/mpath_import_quest_data.R"
#

# # Select columns of the NATES questionnaire -------------------------
# rule select_cols_nates:
#     input:
#         quest_data1=config["quest_data1_clean"],
#     output:
#         nates_cols="data/prep/quest_scales/nates_items.csv",
#     log:
#         "logs/select_cols_nates.log",
#     script:
#         "workflows/scripts/quest/select_cols_nates.R"
#
#
# # Scoring of the NATES questionnaire.
# rule scoring_nates:
#     input:
#         nates_cols="data/prep/quest_scales/nates_items.csv",
#     output:
#         nates_score="data/prep/quest_scales/nates_scores.csv",
#     log:
#         "logs/scoring_nates.log",
#     script:
#         "workflows/scripts/quest/scoring_nates.R"
#
#
# include: "workflows/rules/closing_messages.smk"


# # %% DASS-21 rules --------------------------------------------------
#
#
# rule select_cols_dass21:
#     input:
#         quest_data=config["quest_data"],
#     output:
#         dass21_cols="data/prep/quest_scales/dass21_items.csv",
#     log:
#         "logs/select_cols_dass21.log",
#     script:
#         "workflows/scripts/quest/select_cols_dass21.R"
#
#
# # Scoring of the DASS-21 questionnaire.
# rule scoring_dass21:
#     input:
#         dass21_cols="data/prep/quest_scales/dass21_items.csv",
#     output:
#         dass21_scores="data/prep/quest_scales/dass21_scores.csv",
#     log:
#         "logs/scoring_dass21.log",
#     script:
#         "workflows/scripts/quest/scoring_dass21.R"
#
#
# # %% BDI-II rules ---------------------------------------------------
#
#
# rule select_cols_bdi2:
#     input:
#         quest_data=config["quest_data"],
#     output:
#         bdi2_cols="data/prep/quest_scales/bdi2_items.csv",
#     log:
#         "logs/select_cols_bdi2.log",
#     script:
#         "workflows/scripts/quest/select_cols_bdi2.R"
#
#
# # Scoring of the DASS-21 questionnaire.
# rule scoring_bdi2:
#     input:
#         bdi2_cols="data/prep/quest_scales/bdi2_items.csv",
#     output:
#         bdi2_scores="data/prep/quest_scales/bdi2_scores.csv",
#     log:
#         "logs/scoring_bdi2.log",
#     script:
#         "workflows/scripts/quest/scoring_bdi2.R"
#
#
# # Select columns of the DERS questionnaire --------------------------
# rule select_cols_ders:
#     input:
#         quest_data1=config["quest_data1_clean"],
#     output:
#         ders_cols="data/prep/quest_scales/ders_items.csv",
#     log:
#         "logs/select_cols_ders.log",
#     script:
#         "workflows/scripts/quest/select_cols_ders.R"
#
#
# # Scoring of the DERS questionnaire.
# rule scoring_ders:
#     input:
#         ders_cols="data/prep/quest_scales/ders_items.csv",
#     output:
#         ders_scores="data/prep/quest_scales/ders_scores.csv",
#     log:
#         "logs/scoring_ders.log",
#     script:
#         "workflows/scripts/quest/scoring_ders.R"
#
#
# # Select columns of the NEO-FFI-60-NEURO questionnaire --------------
# rule select_cols_neoffi60neuro:
#     input:
#         quest_data1=config["quest_data1_clean"],
#     output:
#         neoffi60neuro_cols="data/prep/quest_scales/neoffi60_neuro_items.csv",
#     log:
#         "logs/select_cols_neoffi60neuro.log",
#     script:
#         "workflows/scripts/quest/select_cols_neoffi60_neuro.R"
#
#
# # Scoring of the NEO-FFI-60-NEURO questionnaire.
# rule scoring_neoffi60neuro:
#     input:
#         neoffi60neuro_cols="data/prep/quest_scales/neoffi60_neuro_items.csv",
#     output:
#         neoffi60neuro_scores="data/prep/quest_scales/neoffi60_neuro_scores.csv",
#     log:
#         "logs/scoring_neoffi60neuro.log",
#     script:
# #         "workflows/scripts/quest/scoring_neoffi60neuro.R"
# #
#
# # %% Rosenberg SES rules --------------------------------------------
#
#
# rule select_cols_rosenberg:
#     input:
#         quest_data=config["quest_data"],
#     output:
#         rosenberg_cols="data/prep/quest_scales/rosenberg_items.csv",
#     log:
#         "logs/select_cols_rosenberg.log",
#     script:
#         "workflows/scripts/quest/select_cols_rosenberg.R"
#
#
# # Scoring of the Rosenberg questionnaire.
# rule scoring_rosenberg:
#     input:
#         rosenberg_cols="data/prep/quest_scales/rosenberg_items.csv",
#     output:
#         rosenberg_scores="data/prep/quest_scales/rosenberg_scores.csv",
#     log:
#         "logs/scoring_rosenberg.log",
#     script:
#         "workflows/scripts/quest/scoring_rosenberg.R"
#
#
# # Select columns of the RSCS questionnaire --------------------------
# rule select_cols_rscs:
#     input:
#         quest_data1=config["quest_data1_clean"],
#     output:
#         rscs_cols="data/prep/quest_scales/rscs_items.csv",
#     log:
#         "logs/select_cols_rscs.log",
#     script:
#         "workflows/scripts/quest/select_cols_rscs.R"
#
#
# # Scoring of the RSCS questionnaire.
# rule scoring_rscs:
#     input:
#         rscs_cols="data/prep/quest_scales/rscs_items.csv",
#     output:
#         rscs_scores="data/prep/quest_scales/rscs_scores.csv",
#     log:
#         "logs/scoring_rscs.log",
#     script:
#         "workflows/scripts/quest/scoring_rscs.R"
#
#
# # Select columns of the SCL90 questionnaire -------------------------
# rule select_cols_scl90:
#     input:
#         quest_data1=config["quest_data1_clean"],
#     output:
#         scl90_cols="data/prep/quest_scales/scl90_items.csv",
#     log:
#         "logs/select_cols_scl90.log",
#     script:
#         "workflows/scripts/quest/select_cols_scl90.R"
#
#
# # Scoring of the SCL90 questionnaire.
# rule scoring_scl90:
#     input:
#         scl90_cols="data/prep/quest_scales/scl90_items.csv",
#     output:
#         scl90_scores="data/prep/quest_scales/scl90_scores.csv",
#     log:
#         "logs/scoring_scl90.log",
#     script:
#         "workflows/scripts/quest/scoring_scl90.R"
#
# # Select columns of the SCS questionnaire ---------------------------
#
#
# rule select_cols_scs:
#     input:
#         quest_data=config["quest_data"],
#     output:
#         scs_cols="data/prep/quest_scales/scs_items.csv",
#     log:
#         "logs/select_cols_scs.log",
#     script:
#         "workflows/scripts/quest/select_cols_scs.R"
#
#
# # Scoring of the SCS questionnaire.
# rule scoring_scs:
#     input:
#         scs_cols="data/prep/quest_scales/scs_items.csv",
#     output:
#         scs_scores="data/prep/quest_scales/scs_scores.csv",
#     log:
#         "logs/scoring_scs.log",
#     script:
#         "workflows/scripts/quest/scoring_scs.R"
#
# %% logging information --------------------------------------------
onstart:
    shell('echo -e "running\t`date +%Y-%m-%d" "%H:%M`" > pipeline_status.txt')


onsuccess:
    shell('echo -e "success\t`date +%Y-%m-%d" "%H:%M`" > pipeline_status.txt')
    print("Workflow finished, no error")


onerror:
    shell('echo -e "error\t`date +%Y-%m-%d" "%H:%M`" > pipeline_status.txt'),
    print("An error occurred")
