# Effect of exam on the CS and UCS components of State Self-Compassion.
#
# The variation of the CS and UCS components of SSC as a consequence of the manipulation
# of exam (presence/absence of the exam, in different days) can be interpreted as a causal
# effect. The manipulation of the exam "context" was under the control of the experimenter.


# Get the data for the following analyses.
rule get_stan_data_exam_state_self_comp:
    input:
        data=config["ema_data"],
        script="workflows/scripts/ema/functions/funs_exam_state_self_comp.R",
    output:
        "data/prep/ema/stan_data_exam_state_self_comp.rds",
    shell:
        """
        Rscript -e 'source("{input.script}");
        get_stan_data_exam_state_self_comp("{input.data}", "{output}")'
        """


# The model includes two dummy variables which code the difference in the SC level
# between the no-exam days and 'pre' (the day before the exam) or 'post' (the
# evening after the exam).
rule fit_mod_exam_sc_4:
    input:
        data="data/prep/ema/stan_data_exam_state_self_comp.rds",
        script="workflows/scripts/ema/functions/funs_exam_state_self_comp.R",
    output:
        protected("data/prep/ema/brms_fits/fit_mod_exam_sc_4.RDS"),
    shell:
        """
        Rscript -e 'source("{input.script}");
        fit_mod_exam_sc_4("{input.data}", "{output}")'
        """


# The model includes two dummy variables which code the difference in the USC level
# between the no-exam days and 'pre' (the day before the exam) or 'post' (the
# evening after the exam).
rule fit_mod_exam_usc_4:
    input:
        data="data/prep/ema/stan_data_exam_state_self_comp.rds",
        script="workflows/scripts/ema/functions/funs_exam_state_self_comp.R",
    output:
        protected("data/prep/ema/brms_fits/fit_mod_exam_usc_4.RDS"),
    shell:
        """
        Rscript -e 'source("{input.script}");
        fit_mod_exam_usc_4("{input.data}", "{output}")'
        """


# Save a pdf file with the plot with the posterior distributions of the coefficients
# of the two dummy variables, for both the 'pre' and 'post' differences.
rule generate_plot_exam_self_comp:
    input:
        mod_sc="data/prep/ema/brms_fits/fit_mod_exam_sc_4.RDS",
        mod_usc="data/prep/ema/brms_fits/fit_mod_exam_usc_4.RDS",
        script="workflows/scripts/ema/functions/funs_exam_state_self_comp.R",
    output:
        "doc/figures/plot_mod_exam_ssc.pdf",
    shell:
        """
        Rscript -e 'source("{input.script}");
        generate_plot_exam_self_comp("{input.mod_sc}", "{input.mod_usc}", "{output}")'
        """
