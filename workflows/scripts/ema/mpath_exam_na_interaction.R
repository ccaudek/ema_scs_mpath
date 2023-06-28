

temp <- d |> 
  dplyr::filter(exam_day != "no_exam")


# temp1 <- temp %>%
#   group_by(user_id) |> 
#   mutate(bysubj_day = dense_rank(day)) |> 
#   ungroup()
# 
# boo <- temp1 |>
#   group_by(bysubj_day) |>
#   summarize(
#     n = n_distinct(user_id)
#   )


mod_nsc <- lmer(
  nsc ~ exam_day*neg_aff +
    (1 + exam_day*neg_aff | user_id),
  data = temp1,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_nsc)

mod_psc <- lmer(
  psc ~ exam_day*neg_aff +
    (1 + exam_day*neg_aff | user_id),
  data = temp1,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_psc)

temp1 |> 
  group_by(exam_day) |> 
  summarize(
    y = mean(psc)
  )

MuMIn::r.squaredGLMM(mod_psc)



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
