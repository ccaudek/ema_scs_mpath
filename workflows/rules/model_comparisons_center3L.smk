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
