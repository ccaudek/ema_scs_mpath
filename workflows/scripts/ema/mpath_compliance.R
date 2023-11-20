


# Read raw data.
d <- readRDS(here::here("data", "prep", "ema", "ema_data_2.RDS"))

project_days <- c(
  "2023-03-18",
  "2023-03-25",
  "2023-04-01",
  "2023-04-08",
  "2023-04-15",
  "2023-04-16",
  "2023-04-17",
  "2023-04-22",
  "2023-04-29",
  "2023-05-06",
  "2023-05-13",
  "2023-05-20",
  "2023-05-21",
  "2023-05-22",
  # "2023-05-23",
  # "2023-05-24",
  # "2023-05-25",
  "2023-05-27",
  "2023-06-03"
)

d <- d[d$day %in% project_days, ]

d <- d |>
  mutate(
    neg_aff = upset + nervous - satisfied - happy,
    psc = scs_pos_1 + scs_pos_3 + scs_pos_6 + scs_pos_7,
    nsc = scs_neg_2 + scs_neg_4 + scs_neg_5 + scs_neg_8,
    dec = dec_1 + dec_3 - dec_2 - dec_4
  ) 

d$exam_day <- case_when(
  d$day == "2023-04-16" ~ "pre",
  d$day == "2023-04-17" ~ "post",
  d$day == "2023-05-21" ~ "pre",
  d$day == "2023-05-22" ~ "post",
  .default = "no_exam"
)

# Remove NAs on SC.
alldata <- d[!(is.na(d$psc) | is.na(d$nsc) | is.na(d$neg_aff)), ]


# # Test notifications -- not part of the project
# wrong_days <- c(
#   "2023-03-16", "2023-03-19", "2023-03-20", "2023-03-26",
#   "2023-03-27", "2023-04-02", "2023-04-18", "2023-04-19",
#   "2023-04-20", "2023-04-30", "2023-05-01", "2023-06-02",
#   "2023-05-26"
# )
# 
# 
# alldata <- temp[!(temp$day %in% wrong_days), ]

alldata <- alldata |> 
  group_by(user_id) |> 
  mutate(bysubj_day = dense_rank(day)) |> 
  ungroup()



# Compliance rate (average proportion of participants for each day in which
# a notification was received)

n_per_day <- alldata |>
  group_by(bysubj_day) |>
  summarize(
    n = n_distinct(user_id)
  )

n_per_day <- n_per_day |> 
  mutate(
    compliance_day = n / max(n_per_day$n)
  )
n_per_day$compliance_day |> mean()
# [1] 0.8224852


# Compliance of notification frequency within each day

# Only one notifiation in these days
days_exams <- c(
  "2023-04-16",
  "2023-04-17",
  "2023-05-21",
  "2023-05-22"
)

temp <- alldata[!(alldata$day %in% days_exams), ] |> 
  # dplyr::filter(bysubj_day!= 13 & bysubj_day!= 14 & bysubj_day!= 15 & 
  #                 bysubj_day!= 16 & bysubj_day!= 17) |>
  group_by(user_id, bysubj_day) |> 
  summarize(
    n_responses = n_distinct(time_window)
  ) |> 
  mutate(
    compliance_notification = n_responses / 5
  ) |> 
  ungroup()

temp$compliance_notification |> mean()
# [1] 0.7236905







