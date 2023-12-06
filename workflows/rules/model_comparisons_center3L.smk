# Models' comparison
#
# Several models are compared with LOO (for creating a table similar to Table 1
# of Scelza et al., 2019).
# 1. Model selection according to the random-effect structure. The 'maximal'
# fixed-effect structure is paired with a progressively more complex random-effect
# structure. LOO comparison is used for selecting the best random-effect structure.
# 2. Having selected the best random-effect structure, a further model comparison
# procedure is performed, so as to determine the best random-effects supported by
# the data (person, day, moment x negative affect, decentering, context).


# Get data for brms models' comparisons.
rule get_data_for_model_comparisons:
    input:
        config["ema_data"],
    output:
        "data/prep/ema/data_for_model_comparisons.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_model_comparisons.R");
        get_data_for_model_comparisons("{input}", "{output}")'
        """


# Random-effects: LOO model comparisons for the CS component of SSC.
rule fit_and_compare_random_effects_cs:
    input:
        data_path="data/prep/ema/data_for_model_comparisons.rds",
    params:
        dependent_var="state_cs",
        testing_mode="FALSE",
    output:
        model_path=protected("data/prep/ema/brms_fits/loo_psc_rand_eff_mod_comp.rds"),
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_model_comparisons.R");
        fit_and_compare_random_effects("{input.data_path}", "{params.dependent_var}", "{output.model_path}", {params.testing_mode})'
        """


# Fixed-effect: LOO model comparisons for the CS component of SSC.
rule fit_and_compare_fixed_effects_cs:
    input:
        data_path="data/prep/ema/data_for_model_comparisons.rds",
    params:
        dependent_var="state_cs",
        testing_mode="FALSE",
    output:
        model_path=protected("data/prep/ema/brms_fits/loo_psc_fixed_eff_mod_comp.rds"),
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_model_comparisons.R");
        fit_and_compare_fixed_effects("{input.data_path}", "{params.dependent_var}", "{output.model_path}", {params.testing_mode})'
        """


# Random-effects: LOO model comparison for the UCS component of SSC.
rule fit_and_compare_random_effects_ucs:
    input:
        data_path="data/prep/ema/data_for_model_comparisons.rds",
    params:
        dependent_var="state_ucs",
        testing_mode="FALSE",
    output:
        model_path=protected("data/prep/ema/brms_fits/loo_nsc_rand_eff_mod_comp.rds"),
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_model_comparisons.R");
        fit_and_compare_random_effects("{input.data_path}", "{params.dependent_var}", "{output.model_path}", {params.testing_mode})'
        """


# Fixed-effects: LOO model comparison for the UCS component of SSC.
rule fit_and_compare_fixed_effects_ucs:
    input:
        data_path="data/prep/ema/data_for_model_comparisons.rds",
    params:
        dependent_var="state_ucs",
        testing_mode="FALSE",
    output:
        model_path=protected("data/prep/ema/brms_fits/loo_nsc_fixed_eff_mod_comp.rds"),
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_model_comparisons.R");
        fit_and_compare_fixed_effects("{input.data_path}", "{params.dependent_var}", "{output.model_path}", {params.testing_mode})'
        """
