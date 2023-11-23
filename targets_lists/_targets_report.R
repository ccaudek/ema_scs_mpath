# _targets_report.R #

# Targets:
list(
  
  # Create report.
  tar_quarto(
    name = report,
    path = here::here("doc", "ema_ssc_report.qmd")
  )
  
  # close list  
)


