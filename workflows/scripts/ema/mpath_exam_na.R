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

temp <- d |> 
  dplyr::filter(exam_day != "no_exam")

# Remove NAs on SC.
temp1 <- temp[!(is.na(temp$psc) | is.na(temp$nsc) | is.na(temp$neg_aff)), ]

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

bad_obs <- c(38, 891)

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



hist(exam_df$neg_aff)










# recode negative affect
exam_df$na_moment <- 
  (exam_df$neg_aff_Moment - mean(exam_df$neg_aff_Moment, na.rm= T)) /
  sd(exam_df$neg_aff_Moment, na.rm= T)

exam_df$na_day <- 
  (exam_df$neg_aff_Day - mean(exam_df$neg_aff_Day, na.rm= T)) /
  sd(exam_df$neg_aff_Day, na.rm= T)

exam_df$na_person <- 
  (exam_df$neg_aff_Person - mean(exam_df$neg_aff_Person, na.rm= T)) /
  sd(exam_df$neg_aff_Person, na.rm= T)

# recode decentering abilities
exam_df$dec_moment <- 
  (exam_df$dec_Moment - mean(exam_df$dec_Moment, na.rm= T)) /
  sd(exam_df$dec_Moment, na.rm= T)

exam_df$dec_day <- 
  (exam_df$dec_Day - mean(exam_df$dec_Day, na.rm= T)) /
  sd(exam_df$dec_Day, na.rm= T)

exam_df$dec_person <- 
  (exam_df$dec_Person - mean(exam_df$dec_Person, na.rm= T)) /
  sd(exam_df$dec_Person, na.rm= T)

# recode event pleasantness
exam_df$cntx_moment <- 
  (exam_df$context_Moment - mean(exam_df$context_Moment, na.rm= T)) /
  sd(exam_df$context_Moment, na.rm= T)

exam_df$cntx_day <- 
  (exam_df$context_Day - mean(exam_df$context_Day, na.rm= T)) /
  sd(exam_df$context_Day, na.rm= T)

exam_df$cntx_person <- 
  (exam_df$context_Person - mean(exam_df$context_Person, na.rm= T)) /
  sd(exam_df$context_Person, na.rm= T)

# state self-compassion
exam_df$spsc <- exam_df$psc + 12.1
bc <- MASS::boxcox(spsc ~ na_moment * na_day + na_person, data=exam_df)
(lambda <- bc$x[which.max(bc$y)])
lambda <- 1.272727
exam_df$yp <- (exam_df$spsc^lambda-1)/lambda

plot(density(exam_df$yp))
cor(exam_df$yp, exam_df$psc)

exam_df$zpsc <- 
  (exam_df$yp - mean(exam_df$yp, na.rm= T)) /
  sd(exam_df$yp, na.rm= T)

cor(exam_df$zpsc, exam_df$psc)
# [1] 0.997446
plot(density(exam_df$zpsc))

exam_df$znsc <- 
  (exam_df$nsc - mean(exam_df$nsc, na.rm= T)) /
  sd(exam_df$nsc, na.rm= T)

exam_df$zdec <- 
  (exam_df$dec - mean(exam_df$dec, na.rm= T)) /
  sd(exam_df$dec, na.rm= T)

exam_df$zcntx <- 
  (exam_df$context - mean(exam_df$context, na.rm= T)) /
  sd(exam_df$context, na.rm= T)

# Negative State Self-Compassion and Negative Affect ------------------------------------

# mod_nsc <- lmer(
#   znsc ~ zdec + zcntx + na_moment + na_day + na_person +
#     (1 + zdec + zcntx + na_moment + na_day | user_id),
#   data = exam_df,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )

with(
  exam_df,
  cor(neg_aff, na_person)
)

exam_df$zneg_aff <- scale(exam_df$neg_aff) |> 
  as.numeric()

mod_nsc <- lmer(
  znsc ~ na_day + na_person +
    (1 + na_day | user_id),
  data = exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_nsc)

MuMIn::r.squaredGLMM(mod_nsc)
#            R2m       R2c
# [1,] 0.3486749 0.7633387

summary(mod_nsc)

# Check assumptions.

# Check for normality
res <- residuals(mod_nsc)
lattice::qqmath(res)

# Check for linearity
plot(residuals(mod_nsc), exam_df$znsc) 

# Check for homoscedasticity
# Check normality RE
sjPlot::plot_model(mod_nsc, type='diag')

sjPlot::tab_model(mod_nsc, title = "Negative State Self-Compassion")

# Positive State Self-Compassion and Negative Affect ------------------------------------

mod_psc <- lmer(
  zpsc ~ na_day + na_person +
    (1 + na_day | user_id),
  data = exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_psc)

MuMIn::r.squaredGLMM(mod_psc)
#    R2m      R2c
# 0.2664119 0.7288218

summary(mod_psc)

# Check assumptions.

# Check for VIF
car::vif(mod_psc)

# Check for normality
res <- residuals(mod_psc)
lattice::qqmath(res)

# Check for linearity
plot(residuals(mod_psc), exam_df$zpsc) 

# Check for homoscedasticity
# Check normality RE
sjPlot::plot_model(mod_psc, type='diag')

sjPlot::tab_model(mod_psc, title = "Positive State Self-Compassion")


# Negative State Self-Compassion and Decentering abilities ----------------

mod_dec_nsc <- lmer(
  znsc ~ dec_moment + dec_day + dec_person +
    (1 + dec_moment + dec_day | user_id),
  data = exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_dec_nsc)

MuMIn::r.squaredGLMM(mod_dec_nsc)
#      R2m       R2c
#   0.3322188 0.7290525

summary(mod_dec_nsc)

# Check assumptions.

# Check for normality
res <- residuals(mod_dec_nsc)
lattice::qqmath(res)

# Check for linearity
plot(residuals(mod_dec_nsc), exam_df$znsc) 

# Check for homoscedasticity
# Check normality RE
sjPlot::plot_model(mod_dec_nsc, type='diag')

sjPlot::tab_model(mod_dec_nsc)

# Positive State Self-Compassion and Decentering abilities ------------------------------------

mod_dec_psc <- lmer(
  zpsc ~ dec_moment + dec_day + dec_person +
    (1 + dec_moment + dec_day | user_id),
  data = exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_dec_psc)

MuMIn::r.squaredGLMM(mod_dec_psc)
#    R2m      R2c
# 0.3008028 0.692333

summary(mod_dec_psc)

# Check assumptions.

# Check for VIF
car::vif(mod_dec_psc)

# Check for normality
res <- residuals(mod_dec_psc)
lattice::qqmath(res)

# Check for linearity
plot(residuals(mod_dec_psc), exam_df$zpsc) 

# Check for homoscedasticity
# Check normality RE
sjPlot::plot_model(mod_dec_psc, type='diag')

sjPlot::tab_model(mod_dec_psc, title = "Positive State Self-Compassion")

# Negative State Self-Compassion and Event Pleasantness ------------------------------------

mod_cntx_nsc <- lmer(
  znsc ~ cntx_moment + cntx_day + cntx_person +
    (1 + cntx_moment + cntx_day | user_id),
  data = exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_cntx_nsc)

MuMIn::r.squaredGLMM(mod_cntx_nsc)
#     R2m       R2c
# 0.1082713 0.6449944

summary(mod_cntx_nsc)

# Check assumptions.

# Check for VIF
car::vif(mod_cntx_nsc)

# Check for normality
res <- residuals(mod_cntx_nsc)
lattice::qqmath(res)

# Check for linearity
plot(residuals(mod_cntx_nsc), exam_df$zpsc) 

# Check for homoscedasticity
# Check normality RE
sjPlot::plot_model(mod_cntx_nsc, type='diag')

sjPlot::tab_model(mod_cntx_nsc, title = "Negative State Self-Compassion")

# Positive State Self-Compassion and Event Pleasantness ------------------------------------

mod_cntx_psc <- lmer(
  zpsc ~ cntx_moment + cntx_day + cntx_person +
    (1 + cntx_moment + cntx_day | user_id),
  data = exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_cntx_psc)

MuMIn::r.squaredGLMM(mod_cntx_psc)
#    R2m       R2c
# 0.126428 0.6139421

summary(mod_cntx_psc)

# Check assumptions.

# Check for VIF
car::vif(mod_cntx_psc)

# Check for normality
res <- residuals(mod_cntx_psc)
lattice::qqmath(res)

# Check for linearity
plot(residuals(mod_cntx_psc), exam_df$zpsc) 

# Check for homoscedasticity
# Check normality RE
sjPlot::plot_model(mod_cntx_psc, type='diag')

sjPlot::tab_model(mod_cntx_psc, title = "Positive State Self-Compassion")

# Add SCS data ------------------------------------------------------

# Correct codes
exam_df$user_id[exam_df$user_id == "ch-va-0-04-08-010-f"] <- 
  "ch-va-03-04-08-010-f"
exam_df$user_id[exam_df$user_id == "au-vi-04-0117-529-f"] <- 
  "au-vi-04-01-17-529-f"
exam_df$user_id[exam_df$user_id == "sa-su-03-09-08-17-m"] <- 
  "sa-su-03-09-08-137-m"
exam_df$user_id[exam_df$user_id == "va-na-02-06-15--180-f"] <- 
  "va-na-02-06-15-180-f"
exam_df$user_id[exam_df$user_id == "sa_pe_02_12_08_963_f"] <- 
  "sa-pe-02-12-08-963-f"

# Change year format
# Transformation function
transform_string <- function(string) {
  # Extract the first couple of numerical characters
  num_str <- substr(string, start = 7, stop = 8)
  
  # Check if the numerical characters are less than 5
  if (as.numeric(num_str) < 5) {
    # Use the first transformation pattern
    transformed_string <- gsub("-(\\d{2})-(\\d{2})-", "-20\\1-\\2-", string)
  } else {
    # Use the second transformation pattern
    transformed_string <- gsub("-(\\d{2})-(\\d{2})-", "-19\\1-\\2-", string)
  }
  
  return(transformed_string)
}


exam_df$user_id <- sapply(exam_df$user_id, transform_string)

# Replace dashes with underscores
exam_df$user_id <- gsub("-", "_", exam_df$user_id)


# Import SCS data
scs_scores_df <- rio::import(
  here::here(
    "data", "prep", "quest_scales", "scs_scores.csv"
  )
)

# intersect(
#   scs_scores_df$user_id, exam_df$user_id
# )

dat <- left_join(exam_df, scs_scores_df, by = "user_id")

# Imputation
# temp = mice(dat, seed = 500) 
# data_imp = complete(temp, 1)

temp <- dat |> 
  select_if(is.numeric)

MLIM <- mlim(temp, m=1, seed = 2022, tuning_time = 180) 

dat$scs_total_score <- MLIM$scs_total_score

mod3_psc <- lmer(
  zpsc ~ scs_total_score * (na_moment + na_day + na_person) +
    (1 + na_moment + na_day | user_id),
  data = dat,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod3_psc)
MuMIn::r.squaredGLMM(mod3_psc)

car::vif(mod3_psc)
summary(mod3_psc)
sjPlot::tab_model(mod3_psc, title = "")

mod3_nsc <- lmer(
  znsc ~ scs_total_score * (na_moment + na_day + na_person) +
    (1 + na_moment + na_day | user_id),
  data = dat,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod3_nsc)
MuMIn::r.squaredGLMM(mod3_nsc)

car::vif(mod3_nsc)
summary(mod3_nsc)
sjPlot::tab_model(mod3_nsc, title = "")

# negative scs and decentering abilities

mod4_dec_psc <- lmer(
  zpsc ~ scs_total_score * (dec_moment + dec_day + dec_person) +
    (1 + dec_moment + dec_day | user_id),
  data = dat,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod4_dec_psc)
MuMIn::r.squaredGLMM(mod4_dec_psc)

car::vif(mod4_dec_psc)
summary(mod4_dec_psc)
sjPlot::tab_model(mod4_dec_psc, title = "")

# negative scs and decentering abilities 

mod4_dec_nsc <- lmer(
  znsc ~ scs_total_score * (dec_moment + dec_day + dec_person) +
    (1 + dec_moment + dec_day | user_id),
  data = dat,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod4_dec_nsc)
MuMIn::r.squaredGLMM(mod4_dec_nsc)

car::vif(mod4_dec_nsc)
summary(mod4_dec_nsc)
sjPlot::tab_model(mod4_dec_nsc, title = "")

# positive scs and event pleasantness

mod5_cntx_psc <- lmer(
  zpsc ~ scs_total_score * (cntx_moment + cntx_day + cntx_person) +
    (1 + cntx_moment + cntx_day | user_id),
  data = dat,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod5_cntx_psc)
MuMIn::r.squaredGLMM(mod5_cntx_psc)

car::vif(mod5_cntx_psc)
summary(mod5_cntx_psc)
sjPlot::tab_model(mod5_cntx_psc, title = "")

# negative scs and event pleasantness

mod5_cntx_nsc <- lmer(
  znsc ~ scs_total_score * (cntx_moment + cntx_day + cntx_person) +
    (1 + cntx_moment + cntx_day | user_id),
  data = dat,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod5_cntx_nsc)
MuMIn::r.squaredGLMM(mod5_cntx_nsc)

car::vif(mod5_cntx_nsc)
summary(mod5_cntx_nsc)
sjPlot::tab_model(mod5_cntx_nsc, title = "")


# bysubj_psc <- dat |> 
#   group_by(user_id) |> 
#   summarize(
#     zpsc = mean(zpsc),
#     scs_total_score = mean(scs_total_score)
#   )
# 
# fm <- lm(
#   zpsc ~ scale(scs_total_score),
#   data = bysubj_psc
# )
# summary(fm)
# 

# eof ----

# 
# with(exam_df,
#   cbind(zdec, zcntx, nervous, upset, happy, satisfied)
# ) |> cor()
# 
# 
# 
# 
# m1 <- lmer(
#   zpsc ~ znsc * (zdec + zcntx + na_moment + na_day + na_person) +
#     (1 + znsc + (zdec + zcntx + na_moment + na_day + na_person) | user_id),
#   data = exam_df,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# 
# m2 <- lmer(
#   znsc ~ zpsc * (zdec + zcntx + na_moment + na_day + na_person) +
#     (1 + zpsc + (zdec + zcntx + na_moment + na_day + na_person) | user_id),
#   data = exam_df,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# 
# exam_df <-  center3L(exam_df, zcntx, user_id, bysubj_day)
# 
# 
# m1 <- lmer(
#   zpsc ~ zcntx_Moment + zcntx_Day + zcntx_Person + 
#     (1 + zcntx_Moment + zcntx_Day + zcntx_Person | user_id),
#   data = exam_df,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# 
# m2 <- lmer(
#   znsc ~ zcntx_Moment + zcntx_Day + zcntx_Person + 
#     (1 + zcntx_Moment + zcntx_Day + zcntx_Person | user_id),
#   data = exam_df,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# 
# 
# exam_df <-  center3L(exam_df, zdec, user_id, bysubj_day)
# 
# 
# mod1 <- lmer(
#   zpsc ~ zdec_Moment + zdec_Day + zdec_Person + 
#     (1 + zdec_Moment + zdec_Day + zdec_Person | user_id),
#   data = exam_df,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# 
# mod2 <- lmer(
#   znsc ~ zdec_Moment + zdec_Day + zdec_Person + 
#     (1 + zdec_Moment + zdec_Day + zdec_Person | user_id),
#   data = exam_df,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# 
# 
# 
# 
# ################
# 
# 
# 
# exam_df$sc_comb <- exam_df$psc + exam_df$nsc
# hist(exam_df$sc_comb)
# 
# 
# m1 <- lmer(
#   sc_comb ~ na_moment + na_day + na_person + 
#     (1 + na_moment + na_day + na_person | user_id),
#   data = exam_df,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# summary(m1)
# 
# reportMLM(m1)
# 
