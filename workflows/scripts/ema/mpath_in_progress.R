# Preliminary analysis of the effect of context on negative affect.
#
# https://cran.r-project.org/web/packages/multilevelTools/vignettes/lmer-vignette.html

suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("rio")
  library("multilevelTools")
  library("lmerTest")
  library("JWileymisc") # testDistribution()
  library("extraoperators") # %!in%
  library("sjPlot") # plot_model()
  library("performance")
  library(jtools)
  library("sjPlot")
  library(effects)
  library(readxl)
  library(mousetrap)
  library(misty)
  library(lmerTest) 
  library(report)
  library(jtools)
  library(interactions)
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

d$bysubj_day_f <- factor(d$bysubj_day)
d$time_window_f <- factor(d$time_window)

bad_ids <- c(
  418, 419, 423, 426, 431, 435, 487, 948, 957,
  2120, 2354, 3098, 3116, 3122, 3133, 3134, 3351, 3432, 3517, 4211, 5072,
  5953, 5975, 5976
)

piel_clean_df <- d[-bad_ids, ]

piel_clean_df |> 
  group_by(bysubj_day) |> 
  summarize(
    n = n_distinct(user_id)
  ) |> 
  as.data.frame()


piel_clean_df$exam_day <- case_when(
  piel_clean_df$day == "2023-04-16" ~ "pre",
  piel_clean_df$day == "2023-04-17" ~ "post",
  piel_clean_df$day == "2023-05-21" ~ "pre",
  piel_clean_df$day == "2023-05-22" ~ "post",
  piel_clean_df$day == "2023-05-23" ~ "post",
  piel_clean_df$day == "2023-05-24" ~ "post",
  piel_clean_df$day == "2023-05-25" ~ "post",
  piel_clean_df$day == "2023-05-26" ~ "post",
  .default = "no_exam"
)

temp <- piel_clean_df |> 
  dplyr::filter(exam_day == "no_exam")

no_exam_df <-  center3L(temp, neg_aff, user_id, bysubj_day)

strict_control <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12
))

no_exam_df$na_moment <- 
  (no_exam_df$neg_aff_Moment - mean(no_exam_df$neg_aff_Moment, na.rm= T)) /
  sd(no_exam_df$neg_aff_Moment, na.rm= T)

no_exam_df$na_day <- 
  (no_exam_df$neg_aff_Day - mean(no_exam_df$neg_aff_Day, na.rm= T)) /
  sd(no_exam_df$neg_aff_Day, na.rm= T)

no_exam_df$na_person <- 
  (no_exam_df$neg_aff_Person - mean(no_exam_df$neg_aff_Person, na.rm= T)) /
  sd(no_exam_df$neg_aff_Person, na.rm= T)

no_exam_df$znsc <- 
  (no_exam_df$nsc - mean(no_exam_df$nsc, na.rm= T)) /
  sd(no_exam_df$nsc, na.rm= T)

no_exam_df$zpsc <- 
  (no_exam_df$psc - mean(no_exam_df$psc, na.rm= T)) /
  sd(no_exam_df$psc, na.rm= T)

mod_piel_nsc <- lmer(
  znsc ~ na_moment * na_day + na_person +
    (1 + na_moment * na_day | user_id),
  data = no_exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)
summary(mod_piel_nsc)

no_exam_df <- no_exam_df[!is.na(no_exam_df$psc), ]

# mod_piel_psc <- rlmer(
#   zpsc ~ na_moment * na_day + na_person +
#     (1 + na_moment * na_day | user_id),
#   data = no_exam_df,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
summary(mod_piel_nsc)

reportMLM(mod_piel_nsc)

reportMLM(mod_piel_psc)

lattice::qqmath(mod_piel_psc)

mod_piel_psc <- glmmTMB(
  zpsc ~ na_moment * na_day + na_person +
    (1 + na_moment * na_day | user_id),
  data = no_exam_df, 
  family = tweedie()
  )
summary(mod_piel_psc)

reportMLM(mod_piel_psc)

lattice::qqmath(mod_piel_psc)

res <- no_exam_df$zpsc - fitted(mod_piel_psc)
lattice::qqmath(res)


library(MASS)

no_exam_df$spsc <- no_exam_df$psc + 12.1
bc <- boxcox(spsc ~ na_moment * na_day + na_person, data=no_exam_df)
(lambda <- bc$x[which.max(bc$y)])

lambda <- 1.232323
no_exam_df$y <- (no_exam_df$spsc^lambda-1)/lambda

no_exam_df$znsc <- (no_exam_df$y - mean(no_exam_df$y)) / sd(no_exam_df$y) 

no_exam_df$zcntx <- (no_exam_df$context - mean(no_exam_df$context)) / 
  sd(no_exam_df$context) 

no_exam_df$zdec <- (no_exam_df$dec - mean(no_exam_df$dec)) / 
  sd(no_exam_df$dec) 

mod_piel_nsc <- lmer(
  znsc ~ zdec + zcntx + (na_moment + na_day) + na_person +
    (1 + zdec + zcntx + (na_moment + na_day) | user_id),
  data = no_exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)
summary(mod_piel_nsc)

reportMLM(mod_piel_nsc)

interact_plot(model = mod_piel_nsc, pred = na_moment, modx = zcntx)




res <- residuals(mod_piel_nsc)



plot_model(mod_piel_nsc, "eff", "zcntx") 
plot_model(mod_piel_nsc, "eff", "na_moment") 
plot_model(mod_piel_nsc, "eff", "na_day") 
plot_model(mod_piel_nsc, "eff", "na_person") 

MuMIn::r.squaredGLMM(mod_piel_nsc)


res <- residuals(mod_piel_nsc)
lattice::qqmath(res)
#Check for linearity
plot(jitter(residuals(mod_piel_nsc)), jitter(no_exam_df$znsc))

# Check normality
lattice::qqmath(res)
plot_model(mod_piel_nsc, type='diag')







#######################
#######################




mod1 <- brm(
  zpsc ~ na_moment * na_day + na_person +
    (1 + na_moment * na_day | user_id),
  data = no_exam_df,
  family = skew_normal(),
  algorithm = "meanfield"
  # backend = "cmdstanr"
)
pp_check(mod1)
summary(mod1)
bayes_R2(mod1)

#' The ICC is a measure of the proportion of variance that is between people
#' versus the total variance (i.e., variance between people and variance
#' within persons). multilevelTools provides a function, iccMixed() to
#' estimate ICCs based on mixed effects / multilevel models. The following
#' code does this for negative affect. The output is the ICC for the row
#' named user_id. An ICC of 1 indicates that 100% of all variance exists
#' between people, which would mean that 0% of variance exists within person,
#' indicating that people have identical scores every time they are assessed.
#' Conversely an ICC of 0 would indicate that everyone’s average was identical
#' and 100% of the variance exists within person. For negative affect, we can
#' see the ICCs fall between 0 and 1, indicating that some variance is between
#' people (i.e., individuals have different average levels of negative affect
#' and stress) but also that some variance is within person, meaning that
#' people’s negative affect fluctuates or vary within a person across the
#' day and the 10-days of the study.

iccMixed(
  dv = "psc",
  id = "user_id",
  data = d
)


d$exam_day <- case_when(
  d$day == "2023-04-16" ~ "pre",
  d$day == "2023-04-17" ~ "post",
  .default = NA
)


d$exam_day <- case_when(
  d$day == "2023-05-21" ~ "pre",
  d$day == "2023-05-22" ~ "post",
  d$day == "2023-05-23" ~ "post",
  d$day == "2023-05-24" ~ "post",
  d$day == "2023-05-25" ~ "post",
  d$day == "2023-05-26" ~ "post",
  .default = NA
)


days_without_time_window <- c(
  "2023-04-16", "2023-04-17",
  "2023-05-21", "2023-05-22", "2023-05-23", "2023-05-24", "2023-05-25", "2023-05-26"
)

piel_df <- d[!(d$day %in% days_without_time_window), ]

strict_control <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12
))

mydat <- piel_df |>
  dplyr::select(
    psc, nsc, context, neg_aff
  )

set.seed(1234)
imp <- mice(mydat, method = "norm.predict", m = 1)
data_imp <- complete(imp)
check_outliers(data_imp)

bad_ids <- c(
  418, 419, 423, 426, 431, 435, 487, 948, 957,
  2120, 2354, 3098, 3116, 3122, 3133, 3134, 3351, 3432, 3517, 4211, 5072,
  5953, 5975, 5976
)

piel_clean_df <- piel_df[-bad_ids, ]

mod_piel_1 <- lmer(
  psc ~ context * neg_aff +
    (1 + context + neg_aff | user_id) +
    (1 | user_id:bysubj_day) +
    (1 | user_id:bysubj_day:time_window),
  data = piel_clean_df,
  control = strict_control
)
summary(mod_piel_1)

mod_piel_2 <- lmer(
  nsc ~ context * neg_aff +
    (1 + context + neg_aff | user_id) +
    (1 | user_id:bysubj_day) +
    (1 | user_id:bysubj_day:time_window),
  data = piel_df,
  control = strict_control
)
summary(mod_piel_2)

hist(piel_df$psc)

piel_df$zpsc <- (piel_df$psc - mean(piel_df$psc, na.rm = T)) / sd(piel_df$psc, na.rm = T)
piel_df$znsc <- (piel_df$nsc - mean(piel_df$nsc, na.rm = T)) / sd(piel_df$nsc, na.rm = T)
piel_df$zcontext <- (piel_df$context - mean(piel_df$context, na.rm = T)) / sd(piel_df$context, na.rm = T)
piel_df$zna <- (piel_df$neg_aff - mean(piel_df$neg_aff, na.rm = T)) / sd(piel_df$neg_aff, na.rm = T)

mod1 <- brm(
  znsc ~ zcontext * zna +
    (1 + zcontext + zna | user_id) +
    (1 | user_id:bysubj_day) +
    (1 | user_id:bysubj_day:time_window),
  data = piel_df,
  algorithm = "meanfield"
  # backend = "cmdstanr"
)
pp_check(mod1)
summary(mod1)
bayes_R2(mod1)

mod2 <- brm(
  zpsc ~ zcontext * zna +
    (1 + zcontext + zna | user_id) +
    (1 | user_id:bysubj_day) +
    (1 | user_id:bysubj_day:time_window),
  data = piel_df,
  cores = 4,
  backend = "cmdstanr"
)
loo2 <- loo(mod2)
plot(loo2)

pp_check(mod2)
summary(mod2)
bayes_R2(mod2)

mod2a <- brm(
  zpsc ~ zna +
    (1 + zna | user_id) +
    (1 | user_id:bysubj_day) +
    (1 | user_id:bysubj_day:time_window),
  data = piel_df,
  cores = 4,
  backend = "cmdstanr"
)
loo2a <- loo(mod2a)

mod02 <- brm(
  zpsc ~ 1 +
    (1 | user_id) +
    (1 | user_id:bysubj_day) +
    (1 | user_id:bysubj_day:time_window),
  data = piel_df,
  cores = 4,
  backend = "cmdstanr"
)
loo02 <- loo(mod02)

loo_compare(loo2, loo2a)
loo_compare(loo2, loo02)




mod2a <- brm(
  zpsc ~ zna +
    (1 + zna | user_id) +
    (1 | user_id:bysubj_day) +
    (1 | user_id:bysubj_day:time_window),
  data = piel_df,
  algorithm = "meanfield"
  # backend = "cmdstanr"
)



temp <- d[!(is.na(d$exam_day)), ]

temp$emo <- coalesce(temp$emo_before_exam_1, temp$emo_after_exam_1)
temp$emo_intensity <-
  coalesce(temp$emotion_intensity_before_exam, temp$emotion_intensity_after_exam)

temp$exam_day <- if_else(temp$exam_day == "pre", -1, 1)

temp$sc_bip <- (12 - temp$psc) - (12 - temp$nsc)

temp$r_nsc <- (12 - temp$nsc)


m0 <- lmer(
  sc_bip ~ 1 + (1 | user_id),
  temp
)
summary(m0)

m1 <- lmer(
  r_nsc ~ (context + dec + neg_aff) * exam_day + (1 | user_id),
  temp
)
summary(m1)

-0.018260 + c(-1, 1) * 2 * 0.002211
# -0.022682 -0.013838
-0.025968 + c(-1, 1) * 2 * 0.002628
# -0.031224 -0.020712

temp |>
  group_by(exam_day) |>
  summarize(
    na = mean(neg_aff, na.rm = T),
    psc = mean(psc, na.rm = T),
    nsc = mean(nsc, na.rm = T),
  )

temp$exam_day_dummy <- if_else(
  temp$exam_day == -1, 0, 1
)

m1 <- lmer(
  psc ~ (emo_intensity) * exam_day_dummy + (1 | user_id),
  temp
)
summary(m1)



m1 <- lmer(
  nsc ~ (dec + neg_aff) * exam_day + (1 | user_id),
  temp
)
summary(m1)

fm1 <- lmer(
  sc_bip ~ neg_aff * exam_day + (1 + neg_aff || user_id),
  temp
)
summary(fm1)

tapply(temp$sc_bip, temp$exam_day, mean)
tapply(temp$neg_aff, temp$exam_day, mean)


plot_model(fm1, "eff", "neg_aff", "exam_day")

d |>
  group_by(
    calendar_day,
    emo_before_exam_1
  ) |>
  summarize(
    na = mean(neg_aff, na.mr = T)
  )






#' For multilevel data, it is helpful to examine between and within person
#' aspects of a variable separately. multilevelTools makes this easy using
#' the meanDecompose() function. This is important as, for example, if on
#' 11 of 12 days, someone has a negative affect score of 5, and then one
#' day a score of 1, the score of 1 may be an extreme value, for that person
#' even though it is common for the rest of the participants in the study.
#' meanDecompose() returns a list with X values at different levels, here
#' by ID and the residuals, which in this case are within person effects.

tmp <- meanDecompose(neg_aff ~ user_id, data = d)
str(tmp, nchar.max = 30)

#' We make plots of the distributions using testDistribution(), which
#' defaults to testing against a normal distribution, which is a common
#' default and in our case appropriate for linear mixed effects / multilevel
#' models.

plot(
  testDistribution(tmp[["neg_aff by user_id"]]$X,
    extremevalues = "theoretical", ev.perc = .001
  ),
  varlab = "Between Person Negative Affect"
)

plot(
  testDistribution(tmp[["neg_aff by residual"]]$X,
    extremevalues = "theoretical", ev.perc = .001
  ),
  varlab = "Within Person Negative Affect"
)

strict_control <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12
))

d$psc_c <- d$psc - mean(d$psc, na.rm = TRUE)
d$nsc_c <- d$nsc - mean(d$nsc, na.rm = TRUE)

# scl90_df <- rio::import(
#   here::here(
#     "data", "prep", "quest_scales", "scl90_scores.csv"
#   )
# )

temp <- left_join(d, scl90_df, by = "user_id")

formula <- psc_c ~ (scl90_somatization + scl90_osbsess_comp + scl90_interp_sens +
  scl90_depression + scl90_anxiety + scl90_anger_hostility +
  scl90_phobic_anxiety + scl90_paranoid_ideation +
  scl90_psychoticism + scl90_psychoticism + scl90_sleep_disorder) *
  (context + neg_aff) +
  (1 + context + neg_aff | user_id) +
  (1 | user_id:bysubj_day) + (1 | user_id:bysubj_day:time_window)

mod <- lmer(formula, data = temp, control = strict_control)
summary(mod)



mod <- lmer(
  psc_c ~ (scl90_somatization + scl90_osbsess_comp + scl90_interp_sens +
    scl90_depression + scl90_anxiety + scl90_anger_hostility +
    scl90_phobic_anxiety + scl90_paranoid_ideation +
    scl90_psychoticism + scl90_psychoticism + scl90_sleep_disorder) +
    neg_aff + context +
    (1 + context + neg_aff | user_id),
  data = temp,
  control = strict_control
)

MuMIn::r.squaredGLMM(mod)

temp <- d
temp$zpsc <- (temp$psc - mean(temp$psc, na.rm = T)) / sd(temp$psc, na.rm = T)
temp$znsc <- (temp$nsc - mean(temp$nsc, na.rm = T)) / sd(temp$nsc, na.rm = T)
temp$znegaff <- (temp$neg_aff - mean(temp$neg_aff, na.rm = T)) / sd(temp$neg_aff, na.rm = T)
temp$zdec <- (temp$dec - mean(temp$dec, na.rm = T)) / sd(temp$dec, na.rm = T)


m <- brm(
  znsc ~ znegaff + zdec +
    (1 + znegaff + zdec | user_id) +
    (1 | user_id:bysubj_day) +
    (1 | user_id:bysubj_day:time_window),
  data = temp,
  backend = "cmdstanr"
)


m2 <- lmer(
  znsc ~ znegaff + zdec +
    (1 + znegaff + zdec || user_id) +
    (1 | user_id:bysubj_day) +
    (1 | user_id:bysubj_day:time_window),
  data = temp,
  control = strict_control
)

MuMIn::r.squaredGLMM(m2)
summary(m2)

# Try removing bad_ids
bad_ids <- rio::import(here("data", "prep", "ema", "bad_ids.csv"))

d1 <- d |>
  dplyr::filter(!(user_id %in% bad_ids$bad_ids))

m2a <- lmer(
  psc_c ~ context + neg_aff +
    (1 + context + neg_aff | user_id) +
    (1 | user_id:bysubj_day) +
    (1 | user_id:bysubj_day:time_window),
  data = d1,
  control = strict_control
)
MuMIn::r.squaredGLMM(m2a)
summary(m2a)








d$id <- as.numeric(factor(as.character(d$user_id)))
temp <- d[d$id < 21, ]

mod1 <- lmer(
  psc_c ~ 1 + neg_aff + time_window_f + day_f +
    (1 + neg_aff + time_window_f + day_f | user_id),
  data = temp
)

mod1 <- lmer(
  psc_c ~ context + neg_aff +
    (1 + context + neg_aff | user_id) + (context + neg_aff | day_f) +
    (context + neg_aff | time_window_f),
  data = d
)


#' (1 + context + neg_aff | user_id): This specifies the random effects
#' part of the model formula. It includes random intercepts (1) and random
#' slopes for context and neg_aff variables, nested within the user_id
#' variable. This allows the intercept and slopes to vary across different
#' subjects.
#' (1 | user_id:day): This includes a random intercept for the interaction
#' of user_id and day. It captures the correlation of repeated measurements
#' within the same subject across different days.
#' (1 | user_id:day:time_window): This includes a random intercept for the
#' interaction of user_id, day, and time_window. It captures the correlation
#' of repeated measurements within the same subject across different days
#' and time windows.

bmod_1 <- brm(
  prior = c(
    prior(normal(0, 2), class = b)
  ),
  psc_c ~ context * neg_aff +
    (1 + context + neg_aff | user_id) +
    (1 | user_id:bysubj_day) +
    (1 | user_id:bysubj_day:time_window),
  data = d,
  init = 0.1,
  # algorithm = "meanfield" # do not use cmdstan
  backend = "cmdstanr"
)

pp_check(bmod_1)
summary(bmod_1)
conditional_effects(bmod_1, "context")
conditional_effects(bmod_1, "neg_aff")
bayes_R2(bmod_1)



bmod_1a <- brm(
  prior = c(
    prior(normal(0, 2), class = b)
  ),
  psc_c ~ context * neg_aff +
    (1 + context + neg_aff | user_id) +
    (1 | user_id:bysubj_day) +
    (1 | user_id:bysubj_day:time_window),
  data = d,
  init = 0.1,
  # algorithm = "meanfield" # do not use cmdstan
  backend = "cmdstanr"
)

pp_check(bmod_1a)
summary(bmod_1a)
conditional_effects(bmod_1a, "context")
conditional_effects(bmod_1a, "neg_aff")
bayes_R2(bmod_1a)









delta_t <-
  # extracting posterior samples from bmod1
  posterior_samples(bmod_1, pars = c("^b_", "sd_", "sigma")) %>% # taking the square of each variance component
  mutate_at(.vars = 5:8, .funs = funs(.^2)) %>%
  # dividing the slope estimate by the square root of the sum of # all variance components
  mutate(delta = b_context / sqrt(rowSums(.[5:8])))

c(
  quantile(delta_t$delta, .025),
  mean(delta_t$delta),
  quantile(delta_t$delta, .975)
)
#       2.5%                 97.5%
# 0.02126986 0.04448138 0.06912467

delta_t <-
  # extracting posterior samples from bmod5
  posterior_samples(bmod_1, pars = c("^b_", "sd_", "sigma")) %>% # taking the square of each variance component
  mutate_at(.vars = 5:8, .funs = funs(.^2)) %>%
  # dividing the slope estimate by the square root of the sum of # all variance components
  mutate(delta = b_neg_aff / sqrt(rowSums(.[5:8])))

c(
  quantile(delta_t$delta, .025),
  mean(delta_t$delta),
  quantile(delta_t$delta, .975)
)
#       2.5%                 97.5%
# -0.1566128 -0.1442089 -0.1317828


# bmod_1 <- brm(
#   prior = c(
#     prior(normal(0, 2), class = b)
#   ),
#   psc_c ~ context * neg_aff +
#     (1 + context + neg_aff | user_id) +
#     (1 | user_id:day) +
#     (1 | user_id:day:time_window),
#   data = d,
#   # algorithm = "meanfield" # do not use cmdstan
#   backend = "cmdstanr"
# )

conditional_effects(bmod_2, "context")
conditional_effects(bmod_2, "neg_aff")
bayes_R2(bmod_2)

m1x <- lm(psc ~ neg_aff, data = d)

md <- modelDiagnostics(m1x, ev.perc = .001)
plot(md, ask = FALSE, nrow = 4, ncol = 3)

mvextreme <- subset(
  md$extremeValues,
  EffectType == "Multivariate Random Effect user_id"
)
head(mvextreme)

unique(mvextreme$user_id)

strict_control <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12
))

m1a <- update(
  m1,
  data = subset(d, user_id %!in% unique(mvextreme$user_id)),
  control = strict_control
)

md <- modelDiagnostics(m1a, ev.perc = .001)
plot(md, ask = FALSE, ncol = 4, nrow = 3)

modelPerformance(m1a)

summary(m1a)


bmod_2 <- brm(
  prior = c(
    prior(normal(0, 2), class = b)
  ),
  nsc_c ~ context * neg_aff +
    (1 + context + neg_aff | user_id),
  data = d,
  init = 0.1,
  algorithm = "meanfield" # do not use cmdstan
  # backend = "cmdstanr"
)
pp_check(bmod_2)
summary(bmod_2)

delta_t <-
  # extracting posterior samples from bmod5
  posterior_samples(bmod_2, pars = c("^b_", "sd_", "sigma")) %>% # taking the square of each variance component
  mutate_at(.vars = 5:8, .funs = funs(.^2)) %>%
  # dividing the slope estimate by the square root of the sum of # all variance components
  mutate(delta = b_neg_aff / sqrt(rowSums(.[5:8])))

c(
  quantile(delta_t$delta, .025),
  mean(delta_t$delta),
  quantile(delta_t$delta, .975)
)


m3 <- lmer(
  nsc ~ context * neg_aff +
    (1 + context + neg_aff | user_id) +
    (1 | user_id:day) +
    (1 | user_id:day:time_window),
  data = d,
  control = strict_control
)

md <- modelDiagnostics(m3, ev.perc = .001)
plot(md, ask = FALSE, nrow = 4, ncol = 3)

mvextreme <- subset(
  md$extremeValues,
  EffectType == "Multivariate Random Effect user_id"
)
head(mvextreme)

unique(mvextreme$user_id)

m3a <- update(
  m3,
  data = subset(d, user_id %!in% unique(mvextreme$user_id)),
  control = strict_control
)

md <- modelDiagnostics(m3a, ev.perc = .001)
plot(md, ask = FALSE, ncol = 4, nrow = 3)

modelPerformance(m3a)

summary(m3a)

plot_model(m1a, type = "eff", terms = c("neg_aff_c", "context_c")) # + ylim(9, 20)
plot_model(m3, type = "eff", terms = c("neg_aff", "context")) # + ylim(9, 20)
