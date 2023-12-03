# %% ----- Report -----


rule create_report:
    input:
        "doc/ema_ssc_report.qmd",
    output:
        report="doc/ema_ssc_report.html",  # Quarto will automatically place the output here
    shell:
        """
        quarto render doc/ema_ssc_report.qmd --to html
        """
