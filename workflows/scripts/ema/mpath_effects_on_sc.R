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
d <- readRDS(here::here("data", "prep", "ema", "ema_data_3.RDS"))

# Ensure the categorical variables are formatted as factors in R.
d$bysubj_day <- factor(d$bysubj_day)
d$user_id <- factor(d$user_id)
d$time_window <- factor(d$time_window)
d$exam_day <- factor(d$exam_day)

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

d1 <- left_join(d, scs_trait_df, by = "user_id")

# Check for outliers by considering the target variables
column_names <- c("psc", "nsc", "context", "neg_aff", "dec")

foo <- d1 |>
  dplyr::select(all_of(column_names))

res <- performance::check_outliers(foo)
attr_list <- attributes(res)
outlier_count <- attr_list$outlier_count
bad_row_indices <- outlier_count$all$Row
# bad_row_indices

# For the considered columns, replace the outliers with NAs
d1[bad_row_indices, column_names] <- NA

# Perform multiple imputation

# Select variables for multiple imputation
variables_for_mult_imp <- c(
  column_names, "bysubj_day", "user_id", "time_window", "exam_day", 
  "scs_total_score"
  )

temp <- d1 |> 
  dplyr::select(all_of(variables_for_mult_imp))

imputed_data <- mice(
  temp,
  m = 1, 
  maxit = 50, 
  # printFlag = FALSE,
  seed = 123
) |> 
  complete(1)

# Centering data
centered_data <- center3L(imputed_data, neg_aff, user_id, bysubj_day)  |> 
  center3L(context, user_id, bysubj_day)  |> 
  center3L(dec, user_id, bysubj_day)

df <- centered_data |> 
  dplyr::mutate(
    scs_trait = as.vector(scale(scs_total_score)),
    na_moment = as.vector(scale(neg_aff_Moment)),
    na_day = as.vector(scale(neg_aff_Day)),
    na_person = as.vector(scale(neg_aff_Person)),
    dec_moment = as.vector(scale(dec_Moment)),
    dec_day = as.vector(scale(dec_Day)),
    dec_person = as.vector(scale(dec_Person)),
    con_moment = as.vector(scale(context_Moment)),
    con_day = as.vector(scale(context_Day)),
    con_person = as.vector(scale(context_Person)),
    state_cs = as.vector(scale(psc)),
    state_ucs = as.vector(scale(nsc))
  ) |> 
  dplyr::select(
      scs_trait, state_cs, state_ucs,
      na_moment, na_day, na_person, 
      dec_moment, dec_day, dec_person,
      con_moment, con_day, con_person,
      user_id, bysubj_day, time_window
      ) |> 
  dplyr::rename(
    day = bysubj_day,
    moment = time_window
  )


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

