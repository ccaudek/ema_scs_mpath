# %% ----- Test the Bipolar Continuum Hypothesis -----


rule get_data_mod_9_rnd_slopes_bip_cont_hyp:
    input:
        config["ema_data"],
    output:
        "data/prep/ema/data_mod_9_rnd_slopes.rds",
    shell:
        """
        Rscript -e 'source("workflows/scripts/ema/functions/funs_bipolar_cont_hyp.R");
        get_data_bip_cont_test("{input}", "{output}")'
        """


# rule fit_mod_9_rnd_slopes_bip_cont_hyp:
#     input:
#         "data/prep/ema/data_mod_9_rnd_slopes.rds",
#     output:
#         "data/prep/ema/brms_fits/fit_mod_9_rnd_slopes_bip_cont_hyp.RDS",
#     shell:
#         """
#         Rscript -e 'source("workflows/scripts/ema/functions/funs_bipolar_cont_hyp.R");
#         fit_mod_9_rnd_slopes_bip_cont_hyp("{input}", "{output}")'
#         """
