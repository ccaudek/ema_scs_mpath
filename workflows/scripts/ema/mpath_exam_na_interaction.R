

# eof ----------


# d1 <- d |> 
#   dplyr::select(
#     psc, nsc, 
#     na_moment, na_day, na_person,
#     dec_moment, dec_day, dec_person,
#     time_window, bysubj_day, user_id
#   ) |> 
#   mutate(
#     nsc_rev = nsc * -1
#   ) |> 
#   dplyr::select(-nsc) |> 
#   dplyr::rename(
#     "ucs" = "nsc_rev",
#     "sc" = "psc"
#   )
# 
# 
# long_df <- pivot_longer(
#   d1, -c("na_moment", "na_day", "na_person",
#         "dec_moment", "dec_day", "dec_person",
#         "time_window", "bysubj_day", "user_id"), 
#   names_to = "dimension", values_to = "ssc"
# )
# 
# foo <- long_df |> 
#   dplyr::select(-c(time_window, user_id, bysubj_day))
# 
# performance::check_outliers(foo)
# 
# bad_obs <- c(
#   177, 178, 371, 372, 569, 570, 571, 572, 573, 574, 575, 576, 577, 578, 579, 580, 581,
#   583, 584, 585, 586, 587, 588, 589, 590, 593, 594, 972, 981, 982, 1013, 1014, 1017, 1018, 2047, 2048, 2050, 2434,
#   2941, 2942, 2951, 2952, 2953, 2954, 2955, 2956, 2959, 2960, 2963, 2964, 2966, 2969, 2970, 2971, 2972, 2979, 2980,
#   2981, 2982, 2983, 2984, 2985, 2986, 2987, 2988, 2995, 3005, 3007, 3008, 3199, 3200, 3257, 3258, 3269, 3270, 3398,
#   3463, 3464, 3987, 3988, 3989, 3990, 4057, 4063, 4210, 4465, 4483, 4484, 5063, 5585, 5647, 5648, 5741, 5742, 5841,
#   5842, 6237, 6238, 6319, 6320, 6321, 6322, 6323, 6324, 6325, 6326, 6339, 6341, 6350, 6356, 6381, 6382, 6383, 6384,
#   6387, 6388, 6949, 7175, 7477, 7478, 7533, 7534, 7717, 7718, 7865, 7866, 9073, 9728, 9759, 9760, 10859, 10860,
#   10960, 11503, 11504, 11869, 11999, 12003, 12125, 12126, 12571, 12572, 12756, 12765, 12766, 12770, 12788, 13217,
#   13218, 13229, 13230, 13305
# )
# 
# 
# # Specify the columns for which you want to set the values to NA
# columns_to_na <- c("na_moment", "na_day", "na_person", "dec_moment", "dec_day", "dec_person", "ssc")
# 
# # Set the values to NA for the specified rows and columns
# long_df[bad_obs, columns_to_na] <- NA
# 
# # Imputing missing data
# imputed_data <- mice(long_df %>% dplyr::select(all_of(columns_to_na)),
#                      m = 1, maxit = 50, method = "pmm", seed = 123
# ) %>%
#   complete(1)
# 
# imputed_data$user_id <- long_df$user_id
# imputed_data$bysubj_day <- long_df$bysubj_day
# imputed_data$time_window <- long_df$time_window
# imputed_data$dimension <- long_df$dimension
# 
# imputed_data$zna_moment <- as.vector(scale(imputed_data$na_moment))
# imputed_data$zna_day <- as.vector(scale(imputed_data$na_day))
# imputed_data$zna_person <- as.vector(scale(imputed_data$na_person))
# imputed_data$zdec_moment <- as.vector(scale(imputed_data$dec_moment))
# imputed_data$zdec_day <- as.vector(scale(imputed_data$dec_day))
# imputed_data$zdec_person <- as.vector(scale(imputed_data$dec_person))
# imputed_data$zssc <- as.vector(scale(imputed_data$ssc))

# priors1 <- c(
#   set_prior("normal(0, 2)", class = "b")
# )

# mod <- brm(
#   zssc ~ dimension * 
#     (zdec_moment + zdec_day + zdec_person + zna_moment + zna_day + zna_person) +
#     (dimension * (zdec_moment + zdec_day + zdec_person + zna_moment + zna_day + zna_person) | user_id) + 
#     (1 | bysubj_day) + (1 | time_window),
#   data = imputed_data,
#   prior = priors1,
#   family = student(),
#   # control = list(adapt_delta = 0.99, max_treedepth = 20),
#   backend = "cmdstanr",
#   iter = 100,
#   cores = 6,
#   chains = 2,
#   threads = threading(3),
#   silent = 2,
#   # file = here::here("workflows", "scripts", "ema", "brms_fits", "file.rds")
# )






 








pre_post_df <- pre_post_df |> 
  dplyr::select(user_id, day, exam_day, neg_aff)

e1_df <- pre_post_df[pre_post_df$day %in% c("2023-04-16", "2023-04-17"), ]
e2_df <- pre_post_df[pre_post_df$day %in% c("2023-05-21", "2023-05-22"), ]

e1_df_unique <- e1_df %>%
  group_by(user_id, exam_day) %>%
  slice(1) %>%
  ungroup()

e2_df_unique <- e2_df %>%
  group_by(user_id, exam_day) %>%
  slice(1) %>%
  ungroup()

e1_df_wide <- e1_df_unique %>%
  dplyr::select(-day) |> 
  pivot_wider(names_from = "exam_day", values_from = "neg_aff")

e2_df_wide <- e2_df_unique %>%
  dplyr::select(-day) |> 
  pivot_wider(names_from = "exam_day", values_from = "neg_aff")

# Performing multiple imputation
mice1_mod <- mice(e1_df_wide, m = 1, method = 'pmm', seed = 123) 
mice2_mod <- mice(e2_df_wide, m = 1, method = 'pmm', seed = 123) 
completed_data1 <- complete(mice1_mod)
completed_data2 <- complete(mice2_mod)

e1e2_df <- rbind(completed_data1, completed_data2)

e1e2_df <- e1e2_df |> 
  mutate(
    neg_aff_diff = pre - post
  ) 

e1e2_df$pre_c <- as.vector(scale(e1e2_df$pre, scale = FALSE, center = TRUE))

hist(e1e2_df$neg_aff_diff)

mod <- lmer(
  neg_aff_diff ~ pre_c +
    (1 | user_id),
  data = e1e2_df,
  REML = TRUE,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)
summary(mod)

e1e2_df$zneg_aff_diff <- e1e2_df$neg_aff_diff / 200
hist(e1e2_df$zneg_aff_diff)

e1e2_df$zpre_c <- e1e2_df$pre_c / 100
hist(e1e2_df$zpre_c)

mod1 <- brm(
  zneg_aff_diff ~ 1 + zpre_c +
    (1 + zpre_c | user_id),
  prior = set_prior("normal(0, 2)", class = "b"),
  data = e1e2_df,
  family = student(),
  backend = "cmdstanr",
  chains = 3, cores = 6,
  threads = threading(2),
  silent = 2
)

pp_check(mod1)
(loo1 <- loo(mod1))
plot(loo1)

summary(mod1)

parameters::standardize_parameters(mod1, method = "posthoc")

e1_df_unique |> 
  group_by(exam_day) |> 
  summarize(
    na = mean(neg_aff)
  )


fm <- lmer(
  neg_aff ~ 1 + exam_day + (1 | user_id),
  data = e2_df_unique
)
fixef(fm)

m2 <- brm(
  neg_aff ~ 1 + exam_day + (1 | user_id),
  data = e2_df_unique, 
  family = skew_normal(),
  prior = c(
    set_prior("normal(0, 100)", class = "b"),
    set_prior("normal(0, 100)", class = "sigma")
  ),
  iter = 10000,
  backend= "cmdstanr",
  chains = 3,
  cores = 6,
  threads = threading(2)
)
pp_check(m2)
summary(m2)


# Estrai i parametri del modello
posterior <- as.data.frame(m2)

# Calcola il d di Cohen
# Differenza delle medie (effetto di time_point) diviso per la deviazione standard residua
d_cohen <- posterior$b_exam_day / posterior$sigma

# Calcola la media e l'intervallo di credibilità del d di Cohen
d_cohen_mean <- mean(d_cohen)
d_cohen_ci <- quantile(d_cohen, probs = c(0.025, 0.975))

list(mean = d_cohen_mean, ci = d_cohen_ci)






















# Preparing the dataset for imputation
impute_data <- exam_diff_df %>% 
  select(user_id, exam_occasion, neg_aff_pre, neg_aff_post, neg_aff_diff)

# Performing multiple imputation
mice_mod <- mice(impute_data, m = 5, method = 'pmm', seed = 123) # adjust m for the number of imputations

# Calculating neg_aff_diff for each imputed dataset
completed_data <- complete(mice_mod, "long", include = TRUE)
completed_data$neg_aff_diff <- completed_data$neg_aff_post - completed_data$.imp

# Aggregating the results
final_data <- completed_data %>%
  group_by(user_id, exam_occasion, .imp) %>%
  summarize(
    neg_aff_pre = first(neg_aff_pre),
    neg_aff_post = first(neg_aff_post),
    neg_aff_diff = mean(neg_aff_diff),
    .groups = "drop"
  )

# Averaging over all imputations
average_data <- final_data %>%
  group_by(user_id, exam_occasion) %>%
  summarize(
    neg_aff_diff_avg = mean(neg_aff_diff),
    .groups = "drop"
  )

average_data














exam_diff_df$exam_occasion <- factor(exam_diff_df$exam_occasion)
summary(exam_diff_df)

length(unique(exam_diff_df$user_id))



tapply(exam_diff_df$neg_aff_diff, exam_diff_df$exam_occasion, mean)

exam_diff_df$zna_diff <- as.vector(scale(exam_diff_df$neg_aff_diff))
exam_diff_df$na_pre_c <- as.vector(scale(exam_diff_df$neg_aff_pre, center = TRUE, scale = FALSE))
exam_diff_df$exam_occasion <- factor(exam_diff_df$exam_occasion)

hist(exam_diff_df$zna)

fm <- lm(
  neg_aff_diff ~ 1 + na_pre_c,
  data = exam_diff_df
)
summary(fm)



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
