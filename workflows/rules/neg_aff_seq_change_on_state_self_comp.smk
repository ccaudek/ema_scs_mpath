# %% ----- Influence of sequential changes in Negative Affect on State Self-Compassion -----


# CS component of State Self-Compassion.
rule fit_psc_delta_neg_aff_model:
    input:
        data_path=config["ema_data"],
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
        data_path=config["ema_data"],
    params:
        dependent_var="nsc",
    output:
        model_path="data/prep/ema/brms_fits/fit_nsc_delta_neg_aff.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_delta_neg_aff.R");
        fit_delta_neg_aff_model("{input.data_path}", "{params.dependent_var}", "{output.model_path}")'
        """
