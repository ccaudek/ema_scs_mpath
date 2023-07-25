mpath_data <- no_exam_df

y <- (c(mpath_data$zpsc, -mpath_data$znsc))

valence <- c(rep("p", length(mpath_data$zpsc)), rep("n", length(mpath_data$znsc)))
na_moment <- c(mpath_data$na_moment, mpath_data$na_moment)
na_day <- c(mpath_data$na_day, mpath_data$na_day)
na_person <- c(mpath_data$na_person, mpath_data$na_person)
user_id <- mpath_data$user_id

mydat1 <- data.frame(
  y, valence, na_moment, na_day, na_person, user_id
)

mod_na <- lmer(
  y ~ valence * (na_moment + na_day + na_person) +
    (1 + valence + (na_moment + na_day) | user_id),
  data = mydat1,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

summary(mod_na)





y <- (c(mpath_data$zpsc, -mpath_data$znsc))
valence <- c(rep("p", length(mpath_data$zpsc)), rep("n", length(mpath_data$znsc)))
cntx_moment <- c(mpath_data$cntx_moment, mpath_data$cntx_moment)
cntx_day <- c(mpath_data$cntx_day, mpath_data$cntx_day)
cntx_person <- c(mpath_data$cntx_person, mpath_data$cntx_person)
user_id <- mpath_data$user_id

mydat2 <- data.frame(
  y, valence, cntx_moment, cntx_day, cntx_person, user_id
)

mod_cntx <- lmer(
  y ~ valence * (cntx_moment + cntx_day + cntx_person) +
    (1 + valence + (cntx_moment + cntx_day) | user_id),
  data = mydat2,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

summary(mod_cntx)

MuMIn::r.squaredGLMM(mod_cntx)

mod2_cntx <- lmer(
  y ~ valence + (cntx_moment + cntx_day + cntx_person) +
    (1 + valence + (cntx_moment + cntx_day) | user_id),
  data = mydat2,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

MuMIn::r.squaredGLMM(mod2_cntx)
