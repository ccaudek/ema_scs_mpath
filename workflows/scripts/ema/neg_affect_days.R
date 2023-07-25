#' Script name: mpath_exam_na.R
#' Project: EMA SC mpath
#' Script purpose: lmer analyses
#' @author: Corrado Caudek <corrado.caudek@unifi.it>
#' Date Created: Wed Jun 28 06:00:22 2023
#' Last Modified Date: Wed Jun 28 06:00:22 2023
#'
#' 👉 
#' PURPOSE: Contextual factors were assessed with three methods:
#' 1. Current mood.
#' 2. Pleasantness/unpleasantness of the most salient previously occurred event.
#' 3. Level of attachment/detachment to the current situation.
#'
#' Statistical analyses
#' 
#' - For exam-independent days:
#' 1. Within-person (within single day, across weeks) and between-person 
#' effects of the three contextual factors on the PSC and NSC components.
#' 2. Interactions effects with SCS.
#' 
#' For pre-post exam days:
#' 1. ?

# EMA notification dates.
#
# 2023/04/17 Prova in itinere (1)
# 2023/05/22 Prova in itinere (2)
# 2023/05/23 Presentazione dei progetti di gruppo
# 2023/05/24 Presentazione dei progetti di gruppo

# [1] "2023-03-16" "2023-03-18" "2023-03-19" "2023-03-20" "2023-03-25" "2023-03-26"
# [7] "2023-03-27" "2023-04-01" "2023-04-02" "2023-04-08" "2023-04-15" "2023-04-16"
# [13] "2023-04-17" "2023-04-18" "2023-04-19" "2023-04-20" "2023-04-22" "2023-04-29"
# [19] "2023-04-30" "2023-05-01" "2023-05-06" "2023-05-13" "2023-05-20" "2023-05-21"
# [25] "2023-05-22" "2023-05-23" "2023-05-24" "2023-05-25" "2023-05-26" "2023-05-27"
# [31] "2023-06-02" "2023-06-03"

suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("rio")
  # library("multilevelTools")
  library("lmerTest")
  # library("JWileymisc") # testDistribution()
  # library("extraoperators") # %!in%
  library("sjPlot") # plot_model()
  library("performance")
  # library(jtools)
  library("glmmTMB")
  library("mlim")
  library("MuMIn")
  # library("sjPlot")
  # library(effects)
  # library(readxl)
  # library(mousetrap)
  # library(misty)
  # library(report)
  # library(jtools)
  # library(interactions)
})

source(
  here::here(
    "workflows", "scripts", "ema", "functions", "funs_ema_mpath.R"
  )
)

# Read raw data.
d <- readRDS(here::here("data", "prep", "ema", "ema_data_2.RDS"))

d <- d |>
  mutate(
    neg_aff = upset + nervous - satisfied - happy,
    psc = scs_pos_1 + scs_pos_3 + scs_pos_6 + scs_pos_7,
    nsc = scs_neg_2 + scs_neg_4 + scs_neg_5 + scs_neg_8,
    dec = dec_1 + dec_3 - dec_2 - dec_4
  ) 

d |>
  dplyr::select(psc, nsc, dec) |>
  na.omit() |>
  cor() |>
  round(2)

d$exam_day <- case_when(
  d$day == "2023-04-16" ~ "pre",
  d$day == "2023-04-17" ~ "post",
  d$day == "2023-05-21" ~ "pre",
  d$day == "2023-05-22" ~ "post",
  d$day == "2023-05-23" ~ "post",
  d$day == "2023-05-24" ~ "post",
  d$day == "2023-05-25" ~ "post",
  d$day == "2023-05-26" ~ "post",
  .default = "no_exam"
)

# Remove NAs on SC.
temp1 <- d[!(is.na(d$psc) | is.na(d$nsc) | is.na(d$neg_aff)), ]

wrong_days <- c(
  "2023-03-16", "2023-03-19", "2023-03-20", "2023-03-26",
  "2023-03-27", "2023-04-02", "2023-04-18", "2023-04-19",
  "2023-04-20", "2023-04-30", "2023-05-01", "2023-06-02"
)

temp2 <- temp1[!(temp1$day %in% wrong_days), ]

unique(temp2$day)
# [1] "2023-03-25" "2023-04-01" "2023-04-08" "2023-04-15" "2023-04-22" "2023-04-29"
# [7] "2023-05-06" "2023-05-13" "2023-05-20" "2023-05-27" "2023-06-03" "2023-03-18"

length(unique(temp2$user_id))

# temp2 |> 
#   group_by(day) |> 
#   summarize(
#     n = n_distinct(user_id)
#   )

temp3 <- temp2 %>%
  group_by(user_id) |> 
  mutate(bysubj_day = dense_rank(day)) |> 
  ungroup()

# boo <- temp3 |> 
#   group_by(foo) |> 
#   summarize(
#     n = n_distinct(user_id)
#   )

temp4 <- temp3 |> 
  dplyr::filter(bysubj_day < 11)

foo <- temp4 |>
  dplyr::select(
    psc, nsc, context, neg_aff, dec
  )

check_outliers(foo)

bad_obs <- c(218, 222, 334, 335, 339, 556, 751, 760, 1120, 1127,
             1130, 1364, 1600, 1614, 2502, 2510, 2521, 2681, 2801, 
             3396, 3947, 4076, 4267, 4685, 4764)

temp5 <- temp4[-bad_obs, ]

exam_df_1 <-  center3L(temp5, neg_aff, user_id, bysubj_day)
exam_df_2 <-  center3L(exam_df_1, context, user_id, bysubj_day)
exam_df <-  center3L(exam_df_2, dec, user_id, bysubj_day)

rm(temp, temp1, temp2, temp3, temp4, temp5)

# Check compliance

temp <- exam_df |> 
  group_by(bysubj_day) |> 
  summarize(
    nid = n_distinct(user_id), 
    n = n()
  ) 

# Compliance: on how many days on average the participants responded?
mean(temp$nid) / length(unique(exam_df$user_id))
# [1] 0.7908778

# For the days in which participants responded, on which proportion of time-
# window they responded?
mean((temp$n / (temp$nid*1)))
# [1] 1.0

exam_df$exam_day <- factor(exam_df$exam_day)
exam_df$exam_day <- relevel(exam_df$exam_day, ref = "no_exam")

hist(exam_df$neg_aff)

exam_df$zneg_aff <- scale(exam_df$neg_aff) |> as.numeric()

# transform neg_aff to approximate gaussianity
exam_df$zneg_aff <- exam_df$zneg_aff + 5.7
bc <- MASS::boxcox(zneg_aff ~ exam_day, data=exam_df)
(lambda <- bc$x[which.max(bc$y)])
lambda <- 0.5454545
exam_df$yp <- (exam_df$zneg_aff^lambda-1)/lambda
hist(exam_df$yp)


library(robustlmm)

m <- rlmer(
  yp ~ exam_day + (exam_day | user_id),
  exam_df
)
summary(m)

variance_yes <- exam_df %>%
  filter(exam_day == "no_exam") %>%
  group_by(user_id) %>%
  summarise(variance = var(neg_aff, na.rm=T))
variance_yes$exam <- "yes"

mean(variance_yes$variance)
# 5879.22

variance_no <- exam_df %>%
  filter(exam_day != "no_exam") %>%
  group_by(user_id) %>%
  summarise(variance = var(neg_aff, na.rm=T))
variance_yes$exam <- "no"

mean(variance_no$variance)
# 10640.62

variance_no <- variance_no[!is.na(variance_no$variance), ]

variance_df <- left_join(variance_no, variance_yes, by = "user_id")

hist(variance_df$variance)

library(PairedData)

levene.Var.test(
  variance_df$variance.x, variance_df$variance.y
)
# Levene paired test for scale comparison
# 
# data:  variance_df$variance.x and variance_df$variance.y
# t = 7.3265, df = 143, p-value = 1.588e-11
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   4603.139 8004.777
# sample estimates:
#   mean difference 
# 6303.958 




# Brown-Forsythe test
# The Brown-Forsythe test is a robust alternative to Levene's test and is 
# suitable when the assumption of normality is not met or when the data 
# contains outliers.
car::leveneTest(
  variance ~ exam,
    var.equal = FALSE,
  blocks = user_id,
    data=variance_df)


# Brown-Forsythe test with blocking
brown_forsythe_with_blocking_test_result <- 
  leveneTest(c(group1_variances, group2_variances),
  group = rep(c("Group 1", "Group 2"), each = 5),
                                                       blocks = blocks)


