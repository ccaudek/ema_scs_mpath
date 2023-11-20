
# Read raw data.
d <- readRDS(here::here("data", "prep", "ema", "ema_data_2.RDS"))

d <- d |>
  mutate(
    neg_aff = upset + nervous - satisfied - happy,
    psc = scs_pos_1 + scs_pos_3 + scs_pos_6 + scs_pos_7,
    nsc = scs_neg_2 + scs_neg_4 + scs_neg_5 + scs_neg_8,
    dec = dec_1 + dec_3 - dec_2 - dec_4
  ) 

# d |>
#   dplyr::select(psc, nsc, dec) |>
#   na.omit() |>
#   cor() |>
#   round(2)

d$exam_day <- case_when(
  d$day == "2023-04-16" ~ "pre",
  d$day == "2023-04-17" ~ "post",
  d$day == "2023-05-21" ~ "pre",
  d$day == "2023-05-22" ~ "post",
  # d$day == "2023-05-23" ~ "post",
  # d$day == "2023-05-24" ~ "post",
  # d$day == "2023-05-25" ~ "post",
  # d$day == "2023-05-26" ~ "post",
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


# Only EMA days before and after the day of the exam
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


# Check compliance
nrow(temp3) / (4 * length(unique(temp3$user_id))) 
# [1] 0.8123924

# Remove outliers
foo <- temp3 |>
  dplyr::select(
    psc, nsc, context, neg_aff, dec
  )

performance::check_outliers(foo)

bad_obs <- c(38, 791, 887)

exam_df <- temp3[-bad_obs, ]


exam_df$exam_day <- factor(exam_df$exam_day)
exam_df$exam_day <- relevel(exam_df$exam_day, ref = "pre")


followup_days <- c("2023-05-23", "2023-05-24", "2023-05-25")
pre_post_df <- exam_df[!exam_df$day %in% followup_days, ]

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
