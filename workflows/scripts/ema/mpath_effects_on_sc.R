

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
  # library("sjPlot")
  # library(effects)
  # library(readxl)
  # library(mousetrap)
  # library(misty)
  # library(report)
  # library(jtools)
  # library(interactions)
})


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
  dplyr::filter(exam_day == "no_exam")

# Remove NAs on SC.
temp1 <- temp[!(is.na(temp$psc) | is.na(temp$nsc) | is.na(temp$neg_aff)), ]

temp2 <- temp1 |>
  dplyr::select(
    psc, nsc, context, neg_aff, dec
  )

check_outliers(temp2)

bad_obs <- c(
  270, 418, 419, 423, 426, 431, 435, 487, 722, 948,
  957, 1382, 1387, 1390, 1399, 1623, 1631, 1687, 1969, 
  1991, 2352, 3114, 3120, 3131, 3349, 3430, 3484, 3515, 
  4209, 4215, 4898, 5070, 5836, 5934, 5956
)

temp3 <- temp1[-bad_obs, ]

no_exam_df <-  center3L(temp3, neg_aff, user_id, bysubj_day)


no_exam_df$na_moment <- 
  (no_exam_df$neg_aff_Moment - mean(no_exam_df$neg_aff_Moment, na.rm= T)) /
  sd(no_exam_df$neg_aff_Moment, na.rm= T)

no_exam_df$na_day <- 
  (no_exam_df$neg_aff_Day - mean(no_exam_df$neg_aff_Day, na.rm= T)) /
  sd(no_exam_df$neg_aff_Day, na.rm= T)

no_exam_df$na_person <- 
  (no_exam_df$neg_aff_Person - mean(no_exam_df$neg_aff_Person, na.rm= T)) /
  sd(no_exam_df$neg_aff_Person, na.rm= T)


no_exam_df$spsc <- no_exam_df$psc + 12.1
bc <- MASS::boxcox(spsc ~ na_moment * na_day + na_person, data=no_exam_df)
(lambda <- bc$x[which.max(bc$y)])
lambda <- 1.232323
no_exam_df$yp <- (no_exam_df$spsc^lambda-1)/lambda

plot(density(no_exam_df$yp))
cor(no_exam_df$yp, no_exam_df$psc)

no_exam_df$zpsc <- 
  (no_exam_df$yp - mean(no_exam_df$yp, na.rm= T)) /
  sd(no_exam_df$yp, na.rm= T)

cor(no_exam_df$zpsc, no_exam_df$psc)
# [1] 0.9983688
plot(density(no_exam_df$zpsc))

no_exam_df$znsc <- 
  (no_exam_df$nsc - mean(no_exam_df$nsc, na.rm= T)) /
  sd(no_exam_df$nsc, na.rm= T)

no_exam_df$zdec <- 
  (no_exam_df$dec - mean(no_exam_df$dec, na.rm= T)) /
  sd(no_exam_df$dec, na.rm= T)

no_exam_df$zcntx <- 
  (no_exam_df$context - mean(no_exam_df$context, na.rm= T)) /
  sd(no_exam_df$context, na.rm= T)

# Negative State Self-Compassion ------------------------------------

mod_nsc <- lmer(
  znsc ~ zdec + zcntx + na_moment + na_day + na_person +
    (1 + zdec + zcntx + na_moment + na_day | user_id),
  data = no_exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_nsc)

MuMIn::r.squaredGLMM(mod_nsc)

summary(mod_nsc)

# Check assumptions.

# Check normality
res <- residuals(mod_nsc)
lattice::qqmath(res)

# Check for linearity
plot(residuals(mod_nsc), no_exam_df$znsc) 

# Check for homoscedasticity
# Check normality RE
sjPlot::plot_model(mod_nsc, type='diag')


# Positive State Self-Compassion ------------------------------------

mod_psc <- lmer(
  zpsc ~ zdec + zcntx + na_moment + na_day + na_person +
    (1 + zdec + zcntx + na_moment + na_day | user_id),
  data = no_exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_psc)

MuMIn::r.squaredGLMM(mod_psc)

summary(mod_psc)

# Check assumptions.

# Check for VIF
car::vif(mod_psc)

# Check for normality
res <- residuals(mod_psc)
lattice::qqmath(res)

# Check for linearity
plot(residuals(mod_psc), no_exam_df$zpsc) 

# Check for homoscedasticity
# Check normality RE
sjPlot::plot_model(mod_psc, type='diag')


# Add SCS data ------------------------------------------------------

# Change year format
foo <- gsub("-(\\d{2})-(\\d{2})-", "-20\\1-\\2-", no_exam_df$user_id)

# Replace dashes with underscores
modified_strings <- gsub("-", "_", foo)

no_exam_df$user_id <- modified_strings


scs_scores_df <- rio::import(
  here::here(
    "data", "prep", "quest_scales", "scs_scores.csv"
  )
)

# intersect(
#   scs_scores_df$user_id, no_exam_df$user_id
# )

dat <- left_join(no_exam_df, scs_scores_df, by = "user_id")


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

mod4_psc <- lmer(
  znsc ~ scs_total_score * (na_moment + na_day + na_person) +
    (1 + na_moment + na_day | user_id),
  data = dat,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod4_psc)
MuMIn::r.squaredGLMM(mod3_psc)

car::vif(mod4_psc)



# eof ----


with(no_exam_df,
  cbind(zdec, zcntx, nervous, upset, happy, satisfied)
) |> cor()




m1 <- lmer(
  zpsc ~ znsc * (zdec + zcntx + na_moment + na_day + na_person) +
    (1 + znsc + (zdec + zcntx + na_moment + na_day + na_person) | user_id),
  data = no_exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

m2 <- lmer(
  znsc ~ zpsc * (zdec + zcntx + na_moment + na_day + na_person) +
    (1 + zpsc + (zdec + zcntx + na_moment + na_day + na_person) | user_id),
  data = no_exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

no_exam_df <-  center3L(no_exam_df, zcntx, user_id, bysubj_day)


m1 <- lmer(
  zpsc ~ zcntx_Moment + zcntx_Day + zcntx_Person + 
    (1 + zcntx_Moment + zcntx_Day + zcntx_Person | user_id),
  data = no_exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

m2 <- lmer(
  znsc ~ zcntx_Moment + zcntx_Day + zcntx_Person + 
    (1 + zcntx_Moment + zcntx_Day + zcntx_Person | user_id),
  data = no_exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)


no_exam_df <-  center3L(no_exam_df, zdec, user_id, bysubj_day)


mod1 <- lmer(
  zpsc ~ zdec_Moment + zdec_Day + zdec_Person + 
    (1 + zdec_Moment + zdec_Day + zdec_Person | user_id),
  data = no_exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

mod2 <- lmer(
  znsc ~ zdec_Moment + zdec_Day + zdec_Person + 
    (1 + zdec_Moment + zdec_Day + zdec_Person | user_id),
  data = no_exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)
