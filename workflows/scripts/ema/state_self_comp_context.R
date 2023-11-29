#' This script explores the relation between the context and State 
#' Self-Compassion. The context is operationalized as a change in the affective 
#' state: delta_neg_aff = neg_aff(t+1) - neg_aff(t). So, in each day, we have
#' 4 delta_neg_aff, if there is perfect compliance. 
#' 
#' To deal with the problems of the missing responses to notification, multiple
#' imputation is used to have a complete matrix of notification-responses.
#'
#' The relationship between state self-compassion and contextual factors can 
#' be highlighted by observing how different types of events (e.g., minor vs. 
#' major stressors, personal failures, life changes) impact the self-compassion 
#' trajectory. For instance, individuals with higher trait self-compassion might 
#' show a quicker recovery in their state self-compassion following adversity, 
#' or they might not experience as significant a drop in the first place.

library(lmtest)

tar_load(ema_data)

d <- ema_data

d$exam_day <- factor(d$exam_day)

#' Select only the relevant variables, only for no_exam days
d1 <- d |> 
  dplyr::filter(exam_day == "no_exam") |> 
  dplyr::select(
    user_id, bysubj_day, time_window,
    neg_aff, psc, nsc
  )

user_ids <- distinct(d1, user_id)
bysubj_days <- distinct(d1, bysubj_day)
time_windows <- distinct(d1, time_window)

complete_combinations <- expand.grid(
  user_id = user_ids$user_id,
  # bysubj_day = bysubj_days$bysubj_day,
  time_window = time_windows$time_window
  )

complete_data <- left_join(
  complete_combinations, 
  d1, by = c("user_id",  "time_window"))

imputed_data <- mice(complete_data, m = 50, method = 'pmm', seed = 123)

long_data <- mice::complete(imputed_data, action="long")

d3 <- long_data %>%
  group_by(user_id, bysubj_day, time_window) %>%
  summarize(neg_aff = mean(neg_aff, na.rm = TRUE),
            psc = mean(psc, na.rm = TRUE),
            nsc = mean(nsc, na.rm = TRUE)) |> 
  ungroup()

d3 <- d3 %>%
  group_by(user_id, bysubj_day) %>%
  arrange(time_window) %>%
  mutate(delta_neg_aff = neg_aff - lag(neg_aff))

# Setting delta_neg_aff to NA for time_window == 1
d3 <- d3 %>%
  mutate(delta_neg_aff = ifelse(time_window == 1, NA, delta_neg_aff))

# Check that delta_neg_aff is neg_aff(t+1) - neg_aff(t)
# Consider only one subject
if (0) {
  temp <- d3[d3$user_id == "ag-nu-03-07-30-860-f", ]
  temp1 <- temp |> 
    dplyr::select(delta_neg_aff, neg_aff, time_window, bysubj_day)
  
  temp1 |> 
    arrange(bysubj_day, time_window) |> 
    as.data.frame()
  rm(temp, temp1)
}
# Interpretation: positive values of delta_neg_aff indicate that negative
# affect has increased from time t to time t+1; negative values indicate the
# opposite. delta_neg_aff is set to NA for the first measurement of each day 
# (time_window == 1).

d4 <- d3[d3$time_window != 1, ] 
d4 <- d4[!is.na(d4$delta_neg_aff), ]

# Check if all combinations of user_id and bysubj_day have time_window 
# levels 2, 3, 4, 5
if(0) {
  # Define the required time_window levels
  required_time_windows <- c(2, 3, 4, 5)
  
  # Check if all combinations of user_id and bysubj_day have the required time_window levels
  check_results <- d4 %>%
    group_by(user_id, bysubj_day) %>%
    summarise(time_windows = list(unique(time_window)), .groups = 'drop') %>%
    mutate(has_all_levels = all(required_time_windows %in% unlist(time_windows)))
  
  check_results$has_all_levels |> mean()
}
# Test passed.

# Standardize numeric variables.
d5 <- d4
d5$psc <- as.vector(scale(d4$psc))
d5$nsc <- as.vector(scale(d4$nsc))
d5$delta_neg_aff <- as.vector(scale(d4$delta_neg_aff))
d5$bysubj_day <- as.vector(scale(d4$bysubj_day))
d5$time_window <- as.vector(scale(d4$time_window))
d5$neg_aff <- as.vector(scale(d4$neg_aff))

#' UCS component of State Self-Compassion
#' 
#' This model assumes that the baseline level of psc and its change over time 
#' (due to delta_neg_aff and the passage of time) might vary across individuals 
#' (user_id) and possibly across different days (bysubj_day).

formula_nsc <- bf(
    nsc ~ neg_aff + delta_neg_aff + time_window + 
      (1 + neg_aff + delta_neg_aff | user_id) +
      (1 | bysubj_day) + 
      (1 | bysubj_day:time_window) +
      ar(time = time_window, gr = user_id:bysubj_day, p = 1, cov = TRUE)
  )

fm3 <- brm(
  formula_nsc,
  family = student(),
  data = d5,
  backend = "cmdstanr",
  cores = 8,
  chains = 2,
  # threads = threading(4),
  silent = 2
)
pp_check(fm3) + xlim(-5, 5)
summary(fm3)
performance::r2_bayes(fm3)
(loo_fm3 <- loo(fm3))
conditional_effects(fm3, "delta_neg_aff")

#' CS component of State Self-Compassion

formula_psc <- bf(
  psc ~ neg_aff + delta_neg_aff + time_window + 
    (1 + neg_aff + delta_neg_aff | user_id) +
    (1 | bysubj_day) + 
    (1 | bysubj_day:time_window) +
    ar(time = time_window, gr = user_id:bysubj_day, p = 1, cov = TRUE)
)

fm4 <- brm(
  formula_psc,
  family = student(),
  data = d5,
  backend = "cmdstanr",
  cores = 8,
  chains = 2,
  # threads = threading(4),
  silent = 2
)
pp_check(fm4) + xlim(-5, 5)
summary(fm4)
performance::r2_bayes(fm4)
(loo_fm4 <- loo(fm4))
conditional_effects(fm4, "delta_neg_aff")

tab_model(fm4)

#' Granger Causality Test

granger_result1 <- grangertest(
  psc ~ delta_neg_aff, order = 1, 
  data = d5
  )
print(granger_result1)

granger_result2 <- grangertest(
  delta_neg_aff ~ psc, order = 1, 
  data = d5
)
print(granger_result2)

granger_result3 <- grangertest(
  nsc ~ delta_neg_aff, order = 1, 
  data = d5
)
print(granger_result3)

granger_result4 <- grangertest(
  delta_neg_aff ~ nsc, order = 1, 
  data = d5
)
print(granger_result4)

#' Given that there's only one measurement point per day before and after the 
#' exam, you won't be able to calculate delta negative affect (delta_neg_aff) 
#' for these days. However, you can still explore the contextual effect of 
#' these high-stress exam days on the use of positive (psc) and negative (nsc) 
#' components of state self-compassion. Here's an approach to consider:
#' Direct Comparison of Exam Days to Other Days:
#' Since delta_neg_aff cannot be computed for the exam days, focus on comparing 
#' the levels of psc and nsc on these days directly with other days. 
#' This comparison can help determine if there's a significant difference in 
#' self-compassion components on high-stress days (exam days) compared to 
#' regular days.

only_exam_days_df <- ema_data |> 
  dplyr::filter(exam_day != "no_exam")

only_exam_days_df$exam_day <- factor(only_exam_days_df$exam_day)

only_exam_days_df |> 
  group_by(exam_day) |> 
  summarize(
    psc = mean(psc, trim = 0.1),
    nsc = mean(nsc, trim = 0.1)
  )

only_exam_days_df$zpsc <- as.vector(scale(only_exam_days_df$psc))
only_exam_days_df$znsc <- as.vector(scale(only_exam_days_df$nsc))

# grades <- grade_df |> 
#   dplyr::select(user_id, grade_diff)

# only_exam_days_grades_df <- left_join(only_exam_days_df, grades, by = "user_id")

model_psc <- brm(
  zpsc ~ exam_day + (1 + exam_day | user_id),
  data = only_exam_days_df,
  family = student(),
  cores = 8,
  chains = 4,
  threads = threading(2),
  control = list(adapt_delta = 0.99),
  silent = 2
)
pp_check(model_psc)
(loo_model_psc <- loo(model_psc))
summary(model_psc)
conditional_effects(model_psc, "exam_day")
performance::r2_bayes(model_psc)


model_nsc <- brm(
  znsc ~ exam_day + (1 + exam_day | user_id),
  data = only_exam_days_df,
  family = student(),
  cores = 8,
  chains = 4,
  threads = threading(2),
  control = list(adapt_delta = 0.99),
  silent = 2
)
pp_check(model_nsc)
summary(model_nsc)
(loo_model_nsc <- loo(model_nsc))
conditional_effects(model_nsc, "exam_day")
performance::r2_bayes(model_nsc)

report(model_nsc)


# Generate LaTeX table
library(kableExtra)

# Extract model summary
model_summary <- summary(model_nsc)

# Create a data frame for the fixed effects (you might need to adjust this based on your model's structure)
fixed_effects <- data.frame(
  Term = rownames(model_summary$fixed),
  Estimate = model_summary$fixed[, "Estimate"],
  StdErr = model_summary$fixed[, "Est.Error"],
  CI.Lower = model_summary$fixed[, "l-95% CI"],
  CI.Upper = model_summary$fixed[, "u-95% CI"]
)


# Function to format to 3 decimal points
format_three_decimals <- function(x) formatC(x, format = "f", digits = 3)

# Apply formatting
fixed_effects[, -1] <- apply(fixed_effects[, -1], 2, format_three_decimals)

# Generate LaTeX table
latex_table <- kable(fixed_effects, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "scale_down"))



# eof -----



























#' All other days vs pre

d6 <- d |> 
  dplyr::filter(exam_day != "post")

d7 <- d6 |> 
  group_by(user_id, exam_day) |> 
  summarize(
    psc = mean(psc, na.rm = TRUE),
    nsc = mean(nsc, na.rm = TRUE)
  ) |> 
  ungroup()

d7$exam_day <- factor(d7$exam_day)
tapply(d7$psc, d7$exam_day, mean, na.rm = TRUE)
tapply(d7$nsc, d7$exam_day, mean, na.rm = TRUE)


formula_exam_nsc <- bf(
  nsc ~ exam_day + 
    (1 + exam_day | user_id)
  )

mod1 <- brm(
  formula_exam_nsc,
  family = student(),
  data = d7,
  backend = "cmdstanr",
  cores = 8,
  chains = 4,
  threads = threading(2),
  silent = 2
)

pp_check(mod1) + xlim(-5, 5)
summary(mod1)
performance::r2_bayes(mod1)
(loo_mod1 <- loo(mod1))
conditional_effects(mod1, "exam_day")


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


exam_days_df <- d |> 
  dplyr::filter(exam_day != "no_exam") 


new_exam_days_df <- exam_days_df %>%
  mutate(
    combined_grade = coalesce(grade_exp, real_grade)
  ) %>%
  select(user_id, day, combined_grade, exam_day, neg_aff, psc, nsc)

# Assuming new_exam_days_df is your dataframe
grade_difference_df <- new_exam_days_df %>%
  group_by(user_id) %>%
  summarize(
    grade_diff = combined_grade[exam_day == "post"] - combined_grade[exam_day == "pre"],
    .groups = 'drop'
  ) |> 
  group_by(user_id) |> 
  summarize(grade_diff = mean(grade_diff))


grade_df <- left_join(exam_days_df, grade_difference_df, by = "user_id") |> 
  dplyr::select(user_id, day, exam_day, neg_aff, grade_diff, psc, nsc) |> 
  dplyr::filter(exam_day == "post") |> 
  group_by(user_id) |> 
  summarize(
    neg_aff = mean(neg_aff),
    grade_diff = mean(grade_diff),
    psc = mean(psc),
    nsc = mean(nsc)
  )

m1 <- lm(nsc ~ grade_diff, grade_df)
summary(m1)

################################################################################

bdi2 <- rio::import(
  here::here("data", "prep", "quest_scales", "bdi2_scores.csv")
) %>%
  distinct(user_id, .keep_all = TRUE)

dass21 <- rio::import(
  here::here("data", "prep", "quest_scales", "dass21_scores.csv")
) %>%
  distinct(user_id, .keep_all = TRUE)

rosenberg <- rio::import(
  here::here("data", "prep", "quest_scales", "rosenberg_scores.csv")
) %>%
  distinct(user_id, .keep_all = TRUE)

scs <- rio::import(
  here::here("data", "prep", "quest_scales", "scs_scores.csv")
) |> 
  dplyr::select(user_id, scs_total_score) %>%
  distinct(user_id, .keep_all = TRUE)

q1 <- full_join(bdi2, dass21, by = "user_id")
q2 <- full_join(q1, rosenberg, by = "user_id")
quest_df <- full_join(q2, scs, by = "user_id")


library(tidyLPA)

quest_df %>%
  select(-user_id) %>%
  estimate_profiles(1:3, 
                    variances = c("equal", "varying"),
                    covariances = c("zero", "varying")) %>%
  compare_solutions(statistics = c("AIC", "BIC"))

quest_df %>%
  select(-user_id) %>%
  scale() %>%
  estimate_profiles(2) %>% 
  plot_profiles()

m2 <- quest_df %>%
  select(-user_id) %>%
  scale() %>%
  estimate_profiles(2) 

get_estimates(m2)
get_data(m2)

class_vect <- get_data(m2)$Class

class_df <- data.frame(
  user_id = bdi2$user_id, class = class_vect
)

class_df$group <- ifelse(class_df$class == 1, "less_resilient", "more_resilient")
table(class_df$group)

class_df$class <- NULL

# class_df <- class_df %>%
#   mutate(user_id = str_replace_all(user_id, "_", "-"))


class_df$user_id <- gsub("_", "-", class_df$user_id)
class_df$user_id <- gsub("(\\d{2})(\\d{2})", "\\2", class_df$user_id)


#######################

grade2_df <- inner_join(class_df, grade_df, by = "user_id")

mod1 <- lm(nsc ~ neg_aff * group + grade_diff, grade2_df)
summary(mod1)


