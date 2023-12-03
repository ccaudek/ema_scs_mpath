# %% ----- Compute compliance -----


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
