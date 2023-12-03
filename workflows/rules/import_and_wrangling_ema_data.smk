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
