#' Script name: mpath_effects_on_sc.R
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
  dplyr::filter(exam_day == "no_exam")

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

bad_obs <- c(
  245, 373, 374, 378, 381, 386, 640, 846, 855, 1247,
  1252, 1255, 1264, 1458, 1514, 1776, 1794, 2123, 2807, 
  2813, 2824, 3013, 3142, 3173, 3796, 3802, 4419, 4571, 
  5257, 5350, 5372
)

temp5 <- temp4[-bad_obs, ]

no_exam_df <-  center3L(temp5, neg_aff, user_id, bysubj_day)

rm(temp, temp1, temp2, temp3, temp4, temp5)

# Check compliance

temp <- no_exam_df |> 
  group_by(bysubj_day) |> 
  summarize(
    nid = n_distinct(user_id), 
    n = n()
  ) 

# Compliance: on how many days on average the participants responded?
mean(temp$nid) / length(unique(no_exam_df$user_id))
# [1] 0.908284

# For the days in which participants responded, on which proportion of time-
# window they responded?
mean((temp$n / (temp$nid*5)))
# [1] 0.7335109

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
# [1] 0.9984466
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

# Check for normality
res <- residuals(mod_nsc)
lattice::qqmath(res)

# Check for linearity
plot(residuals(mod_nsc), no_exam_df$znsc) 

# Check for homoscedasticity
# Check normality RE
sjPlot::plot_model(mod_nsc, type='diag')


# Positive State Self-Compassion ------------------------------------

mod_psc <- lmer(
  zpsc ~ na_moment + na_day + na_person +
    (1 + na_moment + na_day | user_id),
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

# Correct codes
no_exam_df$user_id[no_exam_df$user_id == "ch-va-0-04-08-010-f"] <- 
  "ch-va-03-04-08-010-f"
no_exam_df$user_id[no_exam_df$user_id == "au-vi-04-0117-529-f"] <- 
  "au-vi-04-01-17-529-f"
no_exam_df$user_id[no_exam_df$user_id == "sa-su-03-09-08-17-m"] <- 
  "sa-su-03-09-08-137-m"
no_exam_df$user_id[no_exam_df$user_id == "va-na-02-06-15--180-f"] <- 
  "va-na-02-06-15-180-f"
no_exam_df$user_id[no_exam_df$user_id == "sa_pe_02_12_08_963_f"] <- 
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


no_exam_df$user_id <- sapply(no_exam_df$user_id, transform_string)

# Replace dashes with underscores
no_exam_df$user_id <- gsub("-", "_", no_exam_df$user_id)


# Import SCS data
scs_scores_df <- rio::import(
  here::here(
    "data", "prep", "quest_scales", "scs_scores.csv"
  )
)

# intersect(
#   scs_scores_df$user_id, no_exam_df$user_id
# )

dat <- left_join(no_exam_df, scs_scores_df, by = "user_id")

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

mod3_nsc <- lmer(
  znsc ~ scs_total_score * (na_moment + na_day + na_person) +
    (1 + na_moment + na_day | user_id),
  data = data_imp,
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



bysubj_psc <- dat |> 
  group_by(user_id) |> 
  summarize(
    zpsc = mean(zpsc),
    scs_total_score = mean(scs_total_score)
  )

fm <- lm(
  zpsc ~ scale(scs_total_score),
  data = bysubj_psc
)
summary(fm)



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




################



no_exam_df$sc_comb <- no_exam_df$psc + no_exam_df$nsc
hist(no_exam_df$sc_comb)


m1 <- lmer(
  sc_comb ~ na_moment + na_day + na_person + 
    (1 + na_moment + na_day + na_person | user_id),
  data = no_exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)
summary(m1)

reportMLM(m1)

