# Script name: mpath_reliability.R
# Project: EMA mpath 2023
# Script purpose: compute multilevel reliability
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Jun 27 06:51:35 2023
# Last Modified Date: Tue Jun 27 06:51:35 2023
#
# 👉 
#
# https://www.rdocumentation.org/packages/psych/versions/2.3.6/topics/multilevel.reliability
# https://github.com/simsem/semTools/issues/106
# https://github.com/marklhc/mcfa_reliability_supp/blob/master/multilevel_alpha.R
#
# https://github.com/marklhc/mcfa_reliability_supp/blob/master/compare_semTools.md

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(psych)
  library(rio)
  library(semTools)
})

# Piel --------------------------------------------------------------

d_piel <- readRDS(
  "/Users/corrado/_repositories/ema_scs_piel/data/prep/ema/ema_data_2.RDS"
) |>
  dplyr::relocate(
    any_of(c(
      starts_with("scs_neg"),
      starts_with("scs_pos")
    ))
  )  

scs_items_piel <- d_piel |> 
  dplyr::select(user_id, all_of(starts_with("scs_")))

# Recode response levels so that they vary between -3 and +3.
scs_items_piel[, sapply(scs_items_piel, is.numeric)] <- 
  scs_items_piel[, sapply(scs_items_piel, is.numeric)] - 4


# M-path ------------------------------------------------------------

d_mpath <- readRDS(
  here::here(
    "data", "prep", "ema", "ema_data_2.RDS"
  )
) |>
  dplyr::relocate(
    any_of(c(
      starts_with("scs_neg"),
      starts_with("scs_pos")
    ))
  )         

scs_items_mpath <- d_mpath |> 
  dplyr::select(user_id, all_of(starts_with("scs_")))


# Combine both data sets ----------------------------------------------------

both_df <- bind_rows(scs_items_piel, scs_items_mpath)


item_names <- c(
  "person", "i1", "i2", "i3", "i4", "i5", "i6", "i7", "i8"
)

colnames(both_df) <- item_names

# Convert Person to integers.
# scs_items$person_int <- NA
# scs_items$person_int <- match(scs_items$person, unique(scs_items$person))
# scs_items$person <- match(scs_items$person, unique(scs_items$person))


# ------------------------------------------------------------

mcfa11 <- 
'level: 1
     f1 =~ NA * i1 + l1 * i1 + l2 * i2 + l3 * i3 + l4 * i4
     i1 ~~ ev1w * i1
     i2 ~~ ev2w * i2
     i3 ~~ ev3w * i3
     i4 ~~ ev4w * i4
     f1 ~~ 1 * f1

   level: 2
     f1 =~ NA * i1 + l1 * i1 + l2 * i2 + l3 * i3 + l4 * i4
     # fixed residual variances
     i1 ~~ ev1b * i1
     i2 ~~    0 * i2
     i3 ~~ ev3b * i3
     i4 ~~ ev4b * i4
     f1 ~~ vf1b * f1

   # tilde omega values:
   tilomgb := (l1 + l2 + l3 + l4)^2 * vf1b /
              ((l1 + l2 + l3 + l4)^2 * vf1b + ev1b + 0 + ev3b + ev4b)
   tilomgw := (l1 + l2 + l3 + l4)^2 * 1 /
              ((l1 + l2 + l3 + l4)^2 * 1 + ev1w + ev2w + ev3w + ev4w)
   # score reliability:
   omg2l := (l1 + l2 + l3 + l4)^2 * (1 + vf1b) /
            ((l1 + l2 + l3 + l4)^2 * (1 + vf1b) + 
             ev1b + 0 + ev3b + ev4b + ev1w + ev2w + ev3w + ev4w)
   omgb := (l1 + l2 + l3 + l4)^2 * vf1b /
           ((l1 + l2 + l3 + l4)^2 * vf1b + ev1b + 0 + ev3b + ev4b + 
            (ev1w + ev2w + ev3w + ev4w + (l1 + l2 + l3 + l4)^2) / 25.1)
'

mcfa11_fit <- cfa(mcfa11, data = both_df, cluster = "person")
summary(mcfa11_fit)

comp_rel <- compRelSEM(
  mcfa11_fit,
  obs.var = FALSE,
  config = c("f1"), 
  shared = "f1"
)
comp_rel

# scs_pos
#
# $config
# $config$f1
# omega_W  omega_2L 
# 0.6263006 0.7903851 
# 
# 
# $shared
# $shared$f1
# omega_B       IRR 
# 0.8203389 0.9935517 

# scs_neg
#
# $config
# $config$f1
# omega_W  omega_2L 
# 0.6805850 0.8261527 
# 
# 
# $shared
# $shared$f1
# omega_B       IRR 
# 0.8849397 0.9852958 

# An interrater reliability (IRR) coefficient is also returned, quantifying 
# generalizability across rater/sampling-error only


# eof ----

