

exam_df <- d |> 
  dplyr::filter(exam_day != "no_exam")

exam_df$exam_day <- factor(exam_df$exam_day)
exam_df$exam_day <- relevel(exam_df$exam_day, ref = "pre")

tapply(exam_df$neg_aff, exam_df$exam_day, mean)

exam_df$zna <- as.vector(scale(exam_df$neg_aff))
exam_df$zpsc <- as.vector(scale(exam_df$psc))
exam_df$znsc <- as.vector(scale(exam_df$nsc))


mod_na_exam <- lmer(
  zna ~ exam_day +
    (1 + exam_day | user_id),
  data = exam_df,
  REML = TRUE,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_na_exam)


mod_nsc <- lmer(
  nsc ~ exam_day * zna +
    (1 + exam_day * zna | user_id),
  data = exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_nsc)

mod_psc <- lmer(
  psc ~ exam_day * zna +
    (1 + exam_day * zna | user_id),
  data = exam_df,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_psc)

temp <- exam_df |> 
  dplyr::select(
    user_id, zna, zpsc, znsc, exam_day
  )

# Reversed Negative SC
temp$znsc_r <- temp$znsc * -1

temp$znsc <- NULL

exam_long_df <- temp |> 
  pivot_longer(
    cols = c(zpsc, znsc_r),
    names_to = "valence",
    values_to = "values"
)


fm <- lmer(
  values ~ valence * zna * exam_day +
    (valence * zna * exam_day | user_id),
  data = exam_long_df,
  REML = TRUE,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

summary(fm)

plot_model(fm, "pred", terms = c("zna", "valence"))

##########

bad_days <- c("2023-05-23","2023-05-24","2023-05-25","2023-05-26")

temp1 <- temp[!(temp$day %in% bad_days), ]

d1 <- temp1 |> 
  dplyr::select(
    user_id, exam_day, neg_aff, real_grade, grade_exp, psc, nsc
  )

d1_pre <- d1 |> 
  dplyr::filter(exam_day == "pre") |> 
  dplyr::select(user_id, neg_aff, grade_exp, psc, nsc) |> 
  group_by(user_id) |> 
  summarize(
    grade_pre = mean(grade_exp, na.rm = T),
    na_pre = mean(neg_aff, na.rm = T),
    psc_pre = mean(psc, na.rm = T),
    nsc_pre = mean(nsc, na.rm = T)
    
  )

d1_post <- d1 |> 
  dplyr::filter(exam_day == "post") |> 
  dplyr::select(user_id, neg_aff, real_grade, psc, nsc) |> 
  group_by(user_id) |> 
  summarize(
    grade_post = mean(real_grade, na.rm = T),
    na_post = mean(neg_aff, na.rm = T),
    psc_post = mean(psc, na.rm = T),
    nsc_post = mean(nsc, na.rm = T)
  )

d_pre_post <- inner_join(d1_pre, d1_post, by = "user_id")

d_pre_post$grade_diff <- d_pre_post$grade_post - d_pre_post$grade_pre
d_pre_post$grade_post <- NULL
d_pre_post$grade_pre <- NULL


m1 <- lm(nsc_post ~ grade_diff + (na_pre + na_post), data = d_pre_post)
summary(m1)

m2 <- lm(psc_post ~ grade_diff * (na_pre + na_post), data = d_pre_post)
summary(m2)
