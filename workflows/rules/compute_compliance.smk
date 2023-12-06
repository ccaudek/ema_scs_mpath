# %% ----- Compute compliance -----


rule get_compliance:
    input:
        config["ema_data"],
    output:
        "data/prep/ema/compliance_results.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_ema_mpath.R");
        get_compliance("{input}", "{output}")'
        """
