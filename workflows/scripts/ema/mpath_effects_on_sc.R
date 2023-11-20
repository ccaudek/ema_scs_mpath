#' Script name: mpath_effects_on_sc.R
#' Project: EMA SC mpath
#' Script purpose: lmer analyses
#' @author: Corrado Caudek <corrado.caudek@unifi.it>
#' Date Created: Wed Jun 28 06:00:22 2023
#' Last Modified Date: Sun Nov 19 04:14:08 2023
#'
#' PURPOSE: Contextual factors were assessed with three methods:
#' 1. Current mood.
#' 2. Pleasantness/unpleasantness of the most salient previously occurred event.
#' 3. Level of attachment/detachment to the current situation.
#' 

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
  library("brms")
  # library("multilevelTools")
  library("lmerTest")
  # library("JWileymisc") # testDistribution()
  # library("extraoperators") # %!in%
  library("sjPlot") # plot_model()
  library("performance")
  library("mice")
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

d <- d %>%
  group_by(user_id) %>%
  mutate(date_order = dense_rank(day)) %>%
  ungroup()

table(d$date_order)
#   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21 
# 649 621 588 510 407 297 369 537 518 508 442 342 169 169 170 209 241 292 172  12  10 

d <- d |> 
  dplyr::filter(date_order < 12)

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

temp6 <- center3L(temp5, neg_aff, user_id, bysubj_day)

no_exam_df <- center3L(temp6, context, user_id, bysubj_day)

rm(temp, temp1, temp2, temp3, temp4, temp5, temp6)

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

# Add SCS scores

scs_trait_tot_df <- rio::import(
  here::here(
    "data", "prep", "quest_scales", "scs_scores.csv"
  )
)

scs_trait_df <- scs_trait_tot_df |> 
  dplyr::select(user_id, scs_total_score)

scs_trait_df$user_id <- gsub("_", "-", scs_trait_df$user_id)

scs_trait_df$user_id <- gsub("(\\d{2})(\\d{2})", "\\2", scs_trait_df$user_id)

# Remove duplicate rows based on user_id, keeping the first occurrence
scs_trait_df <- scs_trait_df[!duplicated(scs_trait_df$user_id), ]

no_exam2_df <- left_join(no_exam_df, scs_trait_df, by = "user_id")

selected_cols <- c(
  "scs_total_score", "neg_aff_Moment", "neg_aff_Day", "neg_aff_Person", 
  "context_Moment", "context_Day", "context_Person", "bysubj_day", "dec"
)

imputed_data <- mice(
  no_exam2_df %>% dplyr::select(all_of(selected_cols)), 
    m = 1, 
    maxit = 50, 
    method = "pmm", 
    seed = 123
  )

temp <- complete(imputed_data, 1)

no_exam2_df$scs_total_score <- temp$scs_total_score

no_exam2_df$zscs_trait <- as.vector(scale(no_exam2_df$scs_total_score))

no_exam_df <- no_exam2_df

no_exam_df$zdec <- as.vector(scale(no_exam_df$dec))

no_exam_df$na_moment <- as.vector(scale(no_exam_df$neg_aff_Moment))
no_exam_df$na_day <- as.vector(scale(no_exam_df$neg_aff_Day))
no_exam_df$na_person <- scale(no_exam_df$neg_aff_Person)

no_exam_df$context_moment <- as.vector(scale(no_exam_df$context_Moment))
no_exam_df$context_day <- as.vector(scale(no_exam_df$context_Day))
no_exam_df$context_person <- as.vector(scale(no_exam_df$context_Person))

no_exam_df$znsc <- as.vector(scale(no_exam_df$nsc))
no_exam_df$zpsc <- as.vector(scale(no_exam_df$psc))


# Negative State Self-Compassion -----------------------------------------------

get_prior(
  znsc ~ zdec + context_moment + context_day + context_person +
    na_moment + na_day + na_person +
    (1 + zdec + context_moment + context_day + na_moment + na_day | user_id),
  data = no_exam_df
)

bmod1_nsc <- brm(
  znsc ~ 1,
  data = no_exam_df,
  iter = 2000,
  backend = "cmdstanr",
  cores = 8,
  chains = 2,
  threads = threading(4),
  silent = 2
)
loo_bmod1_nsc <- loo(bmod1_nsc)

bmod2_nsc <- brm(
  znsc ~ 1 + (1 | user_id),
  data = no_exam_df,
  # control = list(adapt_delta = 0.99, max_treedepth = 20),
  iter = 5000,
  backend = "cmdstanr",
  cores = 8,
  chains = 2,
  threads = threading(4),
  silent = 2
)
loo_bmod2_nsc <- loo(bmod2_nsc)

bmod3_nsc <- brm(
  znsc ~ 1 + (1 | user_id) + (1 | bysubj_day),
  data = no_exam_df,
  # control = list(adapt_delta = 0.99, max_treedepth = 20),
  iter = 5000,
  backend = "cmdstanr",
  cores = 8,
  chains = 2,
  threads = threading(4),
  silent = 2
)
loo_bmod3_nsc <- loo(bmod3_nsc)

priors1 <- c(
  set_prior("normal(0, 2)", class = "b")
)

bmod4_nsc <- brm(
  znsc ~ na_moment + na_day + na_person +
    (1 | user_id) + (1 | bysubj_day),
  data = no_exam_df,
  # control = list(adapt_delta = 0.99, max_treedepth = 20),
  prior = priors1,
  iter = 5000,
  backend = "cmdstanr",
  cores = 8,
  chains = 2,
  threads = threading(4),
  silent = 2
)
loo_bmod4_nsc <- loo(bmod4_nsc)

bmod5_nsc <- brm(
  znsc ~ na_moment + na_day + na_person +
    (1 + na_moment + na_day | user_id) + (1 | bysubj_day),
  data = no_exam_df,
  prior = priors1,
  iter = 15000,
  backend = "cmdstanr",
  cores = 8,
  chains = 2,
  threads = threading(4),
  silent = 2
)
loo_bmod5_nsc <- loo(bmod5_nsc)

bmod6_nsc <- brm(
  znsc ~ na_moment + na_day + na_person +
    context_moment + context_day + context_person + 
    (1 + na_moment + na_day | user_id) + (1 | bysubj_day),
  data = no_exam_df,
  # control = list(adapt_delta = 0.99, max_treedepth = 20),
  prior = priors1,
  iter = 15000,
  backend = "cmdstanr",
  cores = 8,
  chains = 2,
  threads = threading(4),
  silent = 2
)
loo_bmod6_nsc <- loo(bmod6_nsc)

bmod7_nsc <- brm(
  znsc ~ na_moment + na_day + na_person +
    context_moment + context_day + context_person + 
    (1 + na_moment + na_day + context_moment + context_day | user_id) + 
    (1 | bysubj_day),
  data = no_exam_df,
  # control = list(adapt_delta = 0.99, max_treedepth = 20),
  prior = priors1,
  iter = 15000,
  backend = "cmdstanr",
  cores = 8,
  chains = 2,
  threads = threading(4),
  silent = 2
)
loo_bmod7_nsc <- loo(bmod7_nsc)

bmod8_nsc <- brm(
  znsc ~ zdec + na_moment + na_day + na_person +
    context_moment + context_day + context_person + 
    (1 + na_moment + na_day + context_moment + context_day | user_id) + 
    (1 | bysubj_day),
  data = no_exam_df,
  # control = list(adapt_delta = 0.99, max_treedepth = 20),
  prior = priors1,
  iter = 15000,
  backend = "cmdstanr",
  cores = 8,
  chains = 2,
  threads = threading(4),
  silent = 2
)
loo_bmod8_nsc <- loo(bmod8_nsc)

bmod9_nsc <- brm(
  znsc ~ zscs_trait + zdec + na_moment + na_day + na_person +
    context_moment + context_day + context_person + 
    (1 + na_moment + na_day + context_moment + context_day | user_id) + 
    (1 | bysubj_day),
  data = no_exam_df,
  # control = list(adapt_delta = 0.99, max_treedepth = 20),
  prior = priors1,
  iter = 15000,
  backend = "cmdstanr",
  cores = 8,
  chains = 2,
  threads = threading(4),
  silent = 2
)
loo_bmod9_nsc <- loo(bmod9_nsc)

loo_compare(loo_bmod8_nsc, loo_bmod9_nsc)

loo_compare(
  loo_bmod1_nsc, loo_bmod2_nsc, loo_bmod3_nsc, loo_bmod4_nsc, loo_bmod5_nsc, 
  loo_bmod6_nsc, loo_bmod7_nsc, loo_bmod8_nsc
)
#           elpd_diff se_diff
# bmod8_nsc     0.0       0.0
# bmod7_nsc  -343.5      32.8
# bmod6_nsc  -433.7      35.8
# bmod5_nsc  -437.6      36.0
# bmod4_nsc  -780.3      44.1
# bmod3_nsc -1762.9      65.5
# bmod2_nsc -1768.9      65.6
# bmod1_nsc -4046.4      75.0

###### fatto fino a qui!  ------------------------------Wed Nov 15 19:23:21 2023



bmod_nsc <- brm(
  znsc ~ zdec + context_moment + context_day + context_person + 
    na_moment + na_day + na_person +
    (1 + zdec + context_moment + context_day + na_moment + na_day | user_id),
  data = no_exam_df,
  # control = list(adapt_delta = 0.99, max_treedepth = 20),
  prior = priors1,
  iter = 15000,
  backend = "cmdstanr",
  cores = 8,
  chains = 2,
  threads = threading(4),
  silent = 2
)

pp_check(bmod_nsc)
summary(bmod_nsc)
(loo_bmod_nsc <- loo(bmod_nsc))
performance::r2_bayes(bmod_nsc)
parameters::standardize_parameters(bmod_nsc)

marginal_effects(bmod_nsc, "na_person")


bmod_0_nsc <- brm(
  znsc ~ zdec + zcntx + (1 + zdec + zcntx | user_id),
  data = no_exam_df,
  # control = list(adapt_delta = 0.9, max_treedepth = 20),
  backend = "cmdstanr",
  threads = threading(2),
  silent = 2
)

(loo_bmod_0_nsc <- loo(bmod_0_nsc))

loo_compare(loo_bmod_nsc, loo_bmod_0_nsc)


# Positive State Self-Compassion ------------------------------------

mod_psc <- lmer(
  zpsc ~ zdec + context_moment + context_day + context_person + 
    na_moment + na_day + na_person +
    (1 + zdec + context_moment + context_day + context_person + 
       na_moment + na_day | user_id),
  data = no_exam_df,
  REML = TRUE,
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
  data = dat,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod3_psc)
MuMIn::r.squaredGLMM(mod3_psc)
car::vif(mod3_psc)

reportMLM(mod3_nsc)
MuMIn::r.squaredGLMM(mod3_nsc)
car::vif(mod3_nsc)

cor.test(dat$scs_total_score, dat$zpsc)


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

