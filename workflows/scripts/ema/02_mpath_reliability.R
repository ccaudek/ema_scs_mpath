# Script name: 02_mpath_reliability.R
# Project: EMA mpath 2023
# Script purpose: Compute multilevel reliability of State Self-Compassion
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Jun 27 06:51:35 2023
# Last Modified Date: Fri Nov 17 16:30:33 2023
#
# https://www.rdocumentation.org/packages/psych/versions/2.3.6/topics/multilevel.reliability
# https://github.com/simsem/semTools/issues/106
# https://github.com/marklhc/mcfa_reliability_supp/blob/master/multilevel_alpha.R
# https://github.com/marklhc/mcfa_reliability_supp/blob/master/compare_semTools.md

suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("psych")
  library("rio")
  library("semTools")
})


# Read data Piel

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

names(scs_items_piel)
# [1] "user_id"   "scs_neg_2" "scs_neg_4" "scs_neg_5" "scs_neg_8" "scs_pos_1" "scs_pos_3"
# [8] "scs_pos_6" "scs_pos_7"

# Read data M-path
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

names(scs_items_mpath)
# [1] "user_id"   "scs_neg_5" "scs_neg_8" "scs_neg_4" "scs_neg_2" "scs_pos_1" "scs_pos_3"
# [8] "scs_pos_6" "scs_pos_7"

# Combine both data sets
both_df <- bind_rows(scs_items_piel, scs_items_mpath)
# [1] "user_id"   "scs_neg_2" "scs_neg_4" "scs_neg_5" "scs_neg_8" "scs_pos_1" "scs_pos_3"
# [8] "scs_pos_6" "scs_pos_7"


length(unique(both_df$user_id))
# [1] 495

# Multilevel SEM analysis ------------------------------------------------------

# Selecting the user_id column and all columns that start with 'scs_pos'
pos_ssc_df <- both_df %>% select(user_id, starts_with("scs_pos"))
# Selecting the user_id column and all columns that start with 'scs_neg'
neg_ssc_df <- both_df %>% select(user_id, starts_with("scs_neg"))

# Change columns names 
item_names <- c("person", "i1", "i2", "i3", "i4")
colnames(pos_ssc_df) <- item_names
colnames(neg_ssc_df) <- item_names


# Multilevel SEM model
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

pos_fit <- cfa(mcfa11, data = pos_ssc_df, cluster = "person")
summary(pos_fit)

comp_rel <- semTools::compRelSEM(
  pos_fit,
  obs.var = FALSE,
  config = c("f1"), 
  shared = "f1"
)
comp_rel

# Positive State Self-Compassion 
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


# Negative State Self-Compassion
neg_fit <- cfa(mcfa11, data = neg_ssc_df, cluster = "person")
summary(neg_fit)

comp_rel <- semTools::compRelSEM(
  neg_fit,
  obs.var = FALSE,
  config = c("f1"), 
  shared = "f1"
)
# comp_rel
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

# An inter-rater reliability (IRR) coefficient is also returned, quantifying 
# generalizability across rater/sampling-error only


#' In our multilevel analysis of State Self-Compassion, we observed significant 
#' reliability indices for both the positive and negative components. Focusing 
#' first on the positive aspect, the within-subject reliability 
#' (\(\tilde{\omega}^w\)) was recorded at 0.626. This figure indicates a 
#' moderate level of consistency in individual responses across various 
#' measurement instances. In contrast, the between-subject reliability 
#' (\(\tilde{\omega}^b\)), crucial for differentiating between individuals, was 
#' found to be 0.820. This higher value underscores the measure's effectiveness 
#' in capturing stable individual differences in self-compassion.
#' Moreover, the composite reliability of the overall positive score 
#' (\(\omega^{2L}\)), which integrates both within and between subject 
#' variabilities, was calculated to be 0.79. This aligns with the framework 
#' proposed by Lai (2021) and confirms the measure's reliability in a 
#' comprehensive manner.
#' Turning to the negative component of State Self-Compassion, the 
#' within-subject reliability (\(\tilde{\omega}^w\)) was slightly higher at 
#' 0.68, while the between-subject reliability (\(\tilde{\omega}^b\)) further 
#' increased to 0.88. The composite reliability for the overall negative 
#' component score (\(\omega^{2L}\)) was determined to be 0.83. These values, 
#' particularly the between-subject reliabilities exceeding the anticipated 
#' benchmark of approximately 0.8, emphasize the robustness of the measure.
#' The lower within-subject reliabilities for both components, while perhaps 
#' initially surprising, are understandable upon closer examination. These 
#' values reflect not only measurement error but also the inherent variability 
#' of State Self-Compassion as a dynamic construct, influenced by changing 
#' circumstances and internal states. Therefore, the lower within-subject 
#' reliabilities underscore the measure's sensitivity to these temporal changes 
#' within individuals.
#' In summary, the multilevel reliabilities of the State Self-Compassion measure, 
#' with robust between-subject reliability and lower, yet insightful, 
#' within-subject reliability, demonstrate its overall soundness and 
#' applicability in the present study. The composite reliability figures further 
#' confirm the measure's effectiveness in comprehensively capturing the 
#' construct of State Self-Compassion across different levels of analysis.


# eof ----

