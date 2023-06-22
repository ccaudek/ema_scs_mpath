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
})

d <- readRDS(here::here("data", "prep", "ema", "ema_data_2.RDS"))

d <- d |>
  mutate(
    neg_aff = upset + nervous - satisfied - happy,
    psc = scs_pos_1 + scs_pos_3 + scs_pos_6 + scs_pos_7,
    nsc = scs_neg_2 + scs_neg_4 + scs_neg_5 + scs_neg_8,
    dec = dec_1 + dec_3 - dec_2 - dec_4
  ) 

cor(cbind(d$psc, d$nsc, d$dec))

d$bysubj_day_f <- factor(d$bysubj_day)
d$time_window_f <- factor(d$time_window)

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
  dv = "nsc",
  id = "user_id",
  data = d
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

d$psc_c <- d$psc - mean(d$psc)
d$nsc_c <- d$nsc - mean(d$nsc)

scl90_df <- rio::import(
  here::here(
    "data", "prep", "quest_scales", "scl90_scores.csv"
  )
)

temp <- left_join(d, scl90_df, by="user_id")

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


m2 <- lmer(
  psc_c ~ context + neg_aff + dec +
    (1 + context + neg_aff + dec | user_id) +
    (1 | user_id:bysubj_day) +
    (1 | user_id:bysubj_day:time_window),
  data = d,
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
  mutate_at(.vars = 5:8, .funs = funs(.^2) ) %>%
  # dividing the slope estimate by the square root of the sum of # all variance components
  mutate(delta = b_context / sqrt(rowSums(.[5:8]) ) )

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
  mutate_at(.vars = 5:8, .funs = funs(.^2) ) %>%
  # dividing the slope estimate by the square root of the sum of # all variance components
  mutate(delta = b_neg_aff / sqrt(rowSums(.[5:8]) ) )

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
  mutate_at(.vars = 5:8, .funs = funs(.^2) ) %>%
  # dividing the slope estimate by the square root of the sum of # all variance components
  mutate(delta = b_neg_aff / sqrt(rowSums(.[5:8]) ) )

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
