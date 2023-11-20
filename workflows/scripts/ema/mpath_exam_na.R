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
  library("brms")
  library("lmerTest")
  # library("JWileymisc") # testDistribution()
  # library("extraoperators") # %!in%
  library("sjPlot") # plot_model()
  library("performance")
  # library(jtools)
  library("glmmTMB")
  library("mlim")
  library("MuMIn")
  library("sjPlot")
  # library(effects)
  library("loo")
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
temp <- d[!(is.na(d$psc) | is.na(d$nsc) | is.na(d$neg_aff)), ]

wrong_days <- c(
  "2023-03-16", "2023-03-19", "2023-03-20", "2023-03-26",
  "2023-03-27", "2023-04-02", "2023-04-18", "2023-04-19",
  "2023-04-20", "2023-04-30", "2023-05-01", "2023-06-02",
  "2023-05-26"
)

alldata <- temp[!(temp$day %in% wrong_days), ]

unique(alldata$day)
# [1] "2023-03-25" "2023-04-01" "2023-04-08" "2023-04-15" "2023-04-16" "2023-04-17" "2023-04-22"
# [8] "2023-04-29" "2023-05-06" "2023-05-13" "2023-05-20" "2023-05-21" "2023-05-22" "2023-05-23"
# [15] "2023-05-24" "2023-05-25" "2023-05-27" "2023-06-03" "2023-03-18"

n_per_day <- alldata |>
  group_by(day) |>
  summarize(
    n = n_distinct(user_id)
  )
n_per_day

# Compliance of participants per day
mean(n_per_day$n) / max(n_per_day$n)





# Only EMA dats before and after the day of the exam
temp2 <-  alldata|> 
  dplyr::filter(exam_day != "no_exam")

temp3 <- temp2 %>%
  group_by(user_id) |> 
  mutate(bysubj_day = dense_rank(day)) |> 
  ungroup()

table(temp3$day, temp3$exam_day)
#            post pre
# 2023-04-16    0 146
# 2023-04-17  142   0
# 2023-05-21    0 128
# 2023-05-22  142   0
# 2023-05-23  138   0
# 2023-05-24  120   0
# 2023-05-25  128   0

# follow_up_days <- c("2023-05-22", "2023-05-23", "2023-05-24", "2023-05-25")
# followup_df <- temp3[temp3$day %in% follow_up_days, ]
# 
# followup_df |> 
#   group_by(day) |> 
#   summarize(
#     avg_neg_aff = mean(neg_aff, na.rm = TRUE)
#   )
# day        avg_neg_aff
# <date>           <dbl>
# 1 2023-05-22       -54.9
# 2 2023-05-23       -58.3
# 3 2023-05-24       -42.4
# 4 2023-05-25       -66.3
# There is not an obvious decrease in the days in negative affect in the days
# after the second exam. In the following two days there is a third exam, with
# varying dates for different groups of students.

# Check compliance
nrow(temp3) / (7 * 166) 
# [1] 0.8123924

# Remove outliers
foo <- temp3 |>
  dplyr::select(
    psc, nsc, context, neg_aff, dec
  )

check_outliers(foo)

bad_obs <- c(38, 791, 887)

temp5 <- temp3[-bad_obs, ]






# Centering
exam_df_1 <-  center3L(alldata, neg_aff, user_id, bysubj_day)
exam_df_2 <-  center3L(exam_df_1, context, user_id, bysubj_day)
exam_df <-  center3L(exam_df_2, dec, user_id, bysubj_day)

rm(temp, temp1, temp2, temp3, temp4, temp5)



# recode negative affect
exam_df$na_day <- 
  (exam_df$neg_aff_Day - mean(exam_df$neg_aff_Day, na.rm= T)) /
  sd(exam_df$neg_aff_Day, na.rm= T)

exam_df$na_person <- 
  (exam_df$neg_aff_Person - mean(exam_df$neg_aff_Person, na.rm= T)) /
  sd(exam_df$neg_aff_Person, na.rm= T)

# recode decentering abilities
exam_df$dec_day <- 
  (exam_df$dec_Day - mean(exam_df$dec_Day, na.rm= T)) /
  sd(exam_df$dec_Day, na.rm= T)

exam_df$dec_person <- 
  (exam_df$dec_Person - mean(exam_df$dec_Person, na.rm= T)) /
  sd(exam_df$dec_Person, na.rm= T)

# recode event pleasantness
exam_df$cntx_day <- 
  (exam_df$context_Day - mean(exam_df$context_Day, na.rm= T)) /
  sd(exam_df$context_Day, na.rm= T)

exam_df$cntx_person <- 
  (exam_df$context_Person - mean(exam_df$context_Person, na.rm= T)) /
  sd(exam_df$context_Person, na.rm= T)


# state self-compassion
exam_df$zpsc <- 
  (exam_df$psc - mean(exam_df$psc, na.rm= T)) /
  sd(exam_df$psc, na.rm= T)
plot(density(exam_df$zpsc))

exam_df$znsc <- 
  (exam_df$nsc - mean(exam_df$nsc, na.rm= T)) /
  sd(exam_df$nsc, na.rm= T)
plot(density(exam_df$znsc))


# Analysis of Covariance -------------------------------------------------------

sc <- (c(-exam_df$znsc, exam_df$zpsc))
valence <- 
  c(
    rep("n", length(exam_df$znsc)), rep("p", length(exam_df$zpsc))
  ) |> 
  as.factor()
na_day <- c(exam_df$na_day, exam_df$na_day)
na_person <- c(exam_df$na_person, exam_df$na_person)
cntx_day <- c(exam_df$cntx_day, exam_df$cntx_day)
cntx_person <- c(exam_df$cntx_person, exam_df$cntx_person)
dec_day <- c(exam_df$dec_day, exam_df$dec_day)
dec_person <- c(exam_df$dec_person, exam_df$dec_person)
user_id <- exam_df$user_id

mydat <- data.frame(
  sc, valence, 
  na_day, na_person, 
  cntx_day, cntx_person, 
  dec_day, dec_person,
  user_id
)

contrasts(mydat$valence) = contr.sum(2) * -1 # so that "n" is coded as -1
contrasts(mydat$valence)


mod_ancov <- brm(
  sc ~ valence *
    (na_day + na_person + cntx_day + cntx_person) +
    (1 + valence + na_day + cntx_day | user_id),
  data = mydat,
  family = asym_laplace(),
  algorithm = "meanfield",
  prior = c(
    prior(normal(0, 5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept")
  )
)

pp_check(mod_ancov)
summary(mod_ancov)
marginal_effects(mod_ancov, "dec_day:valence")
bayes_R2(mod_ancov)
# Estimate   Est.Error      Q2.5     Q97.5
# R2 0.7455381 0.007081896 0.7309843 0.7583213
r2_bayes(mod_ancov)

loo_1 <- loo(mod_ancov)
plot(loo_1)

tab_model(mod_ancov)



# Model without interaction ----------------------------------------------------

mod2_ancov <- brm(
  sc ~ valence +
    (na_day + na_person + cntx_day + cntx_person) +
    (1 + valence + na_day + cntx_day | user_id),
  data = mydat,
  family = asym_laplace(),
  algorithm = "meanfield",
  prior = c(
    prior(normal(0, 5), class = "b"),
    prior(normal(0, 2.5), class = "Intercept")
  )
)

loo_2 <- loo(mod2_ancov)

comp <- loo_compare(loo_1, loo_2)
print(comp, digits = 2)
#           elpd_diff se_diff
# mod2_ancov   0.00      0.00 
# mod_ancov  -35.55     10.08 


# Pre/post NA ------------------------------------------------------------------

exam_df$exam_day <- factor(exam_df$exam_day)

exam_df$na <- scale(exam_df$neg_aff) |> as.numeric()

exam_df |> 
  group_by(exam_day) |> 
  summarize(
    ngaff = mean(na, na.rm = TRUE),
    std = sd(na, na.rm = TRUE)
  )

exam_df %>%
  ggplot( aes(x=neg_aff, fill=exam_day)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity')

model <- brm(na ~ exam_day + (1 | user_id) + (1 | user_id:day), data = exam_df)


mod_prepost <- brm(
  na ~ exam_day + (1 | user_id) + (1 | user_id:day),
  data = exam_df,
  family = asym_laplace(),
  algorithm = "meanfield",
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept")
  )
)
summary(mod_prepost)

pp_check(mod_prepost)


delta_t <-
  posterior_samples(mod_prepost, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 3:5, .funs = funs(.^2) ) %>%
  mutate(delta = `b_exam_daypre` / sqrt(rowSums(.[3:5]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
#      2.5%       50%     97.5% 
# 0.6318748 0.7730177 0.9108119 


# ------------------------------------------------------------------------------

alldata$na <- scale(alldata$neg_aff) |> as.numeric()

alldata |> 
  group_by(exam_day) |> 
  summarize(
    ngaff = mean(na, na.rm = TRUE),
    std = sd(na, na.rm = TRUE), 
    n = n()
  )


mod_day <- brm(
  na ~ exam_day + (1 | user_id) + (1 | user_id:day),
  data = alldata,
  family = asym_laplace(),
  algorithm = "meanfield",
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept")
  )
)
summary(mod_day)

pp_check(mod_day)


delta_t <-
  posterior_samples(mod_day, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 4:6, .funs = funs(.^2) ) %>%
  mutate(delta = `b_exam_daypost` / sqrt(rowSums(.[4:6]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
#       2.5%        50%      97.5% 
# -0.2577485 -0.2071659 -0.1544859 

delta_t <-
  posterior_samples(mod_day, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 4:6, .funs = funs(.^2) ) %>%
  mutate(delta = `b_exam_daypre` / sqrt(rowSums(.[4:6]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
#      2.5%       50%     97.5% 
# 0.5166054 0.6215429 0.7235328 