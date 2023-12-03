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
