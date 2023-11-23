
#  This model serves as a baseline to assess the importance of adding random effects.
model_1 <- brm(
  state_ucs ~ na_person + dec_person + con_person + # Between-subject variables
    na_moment + na_day + # Within-subject variables (day and moment)
    dec_moment + dec_day +
    con_moment + con_day,
  data = dat, 
  family = student(),
  backend = "cmdstanr",
  cores = 6,
  chains = 2,
  threads = threading(3),
  silent = 2
  )

model_2 <- brm(
  state_ucs ~ na_person + dec_person + con_person + # Between-subject variables
    na_moment + na_day + # Within-subject variables (day and moment)
    dec_moment + dec_day +
    con_moment + con_day +  
    (1 | user_id),
  data = dat, 
  family = student(),
  backend = "cmdstanr",
  cores = 6,
  chains = 2,
  threads = threading(3),
  silent = 2
)

model_3 <- brm(
  state_ucs ~ na_person + dec_person + con_person + # Between-subject variables
    na_moment + na_day + # Within-subject variables (day and moment)
    dec_moment + dec_day +
    con_moment + con_day + 
    (1 | user_id) + (1 | user_id:day),
  data = dat, 
  family = student(),
  backend = "cmdstanr",
  cores = 6,
  chains = 2,
  threads = threading(3),
  silent = 2
)

model_4 <- brm(
  state_ucs ~ na_person + dec_person + con_person + # Between-subject variables
    na_moment + na_day + # Within-subject variables (day and moment)
    dec_moment + dec_day +
    con_moment + con_day + 
    (1 + na_moment + na_day | user_id) + (1 | user_id:day),
  data = dat, 
  family = student(),
  backend = "cmdstanr",
  cores = 6,
  chains = 2,
  threads = threading(3),
  silent = 2
)

model_5 <- brm(
  state_ucs ~ na_person + dec_person + con_person + # Between-subject variables
    na_moment + na_day + # Within-subject variables (day and moment)
    dec_moment + dec_day +
    con_moment + con_day + 
    (1 + na_moment + na_day + dec_moment + dec_day | user_id) + 
    (1 | user_id:day),
  data = dat, 
  family = student(),
  control = list(adapt_delta = 0.99),
  backend = "cmdstanr",
  cores = 6,
  chains = 2,
  threads = threading(3),
  silent = 2
)
loo_model_5 <- loo(model_5)


model_6 <- brm(
  state_ucs ~ na_person + dec_person + con_person + # Between-subject variables
    na_moment + na_day + # Within-subject variables (day and moment)
    dec_moment + dec_day +
    con_moment + con_day + 
    (1 + na_moment + na_day + dec_moment + dec_day | user_id) + 
    (1 | user_id:day) + 
    (1 | user_id:day:moment),
  data = dat, 
  family = student(),
  control = list(adapt_delta = 0.99),
  backend = "cmdstanr",
  cores = 6,
  chains = 2,
  threads = threading(3),
  silent = 2
)
loo_model_6 <- loo(model_6)

model_7 <- brm(
  state_ucs ~ na_person + dec_person + con_person + # Between-subject variables
    na_moment + na_day + # Within-subject variables (day and moment)
    dec_moment + dec_day +
    con_moment + con_day + 
    (1 + na_moment + na_day + dec_moment + dec_day + con_moment + con_day | user_id) + 
    (1 | user_id:day),
  data = dat, 
  family = student(),
  control = list(adapt_delta = 0.99),
  backend = "cmdstanr",
  cores = 6,
  chains = 2,
  threads = threading(3),
  silent = 2
)
loo_model_7 <- loo(model_7)




model_7 <- brm(
  state_ucs ~ na_person + dec_person + con_person + # Between-subject variables
    na_moment + na_day + # Within-subject variables (day and moment)
    dec_moment + dec_day +
    con_moment + con_day + 
    (1 + na_moment + na_day + dec_moment + dec_day | user_id) + 
    (1 | user_id:day:moment),
  data = dat, 
  family = student(),
  control = list(adapt_delta = 0.99),
  backend = "cmdstanr",
  cores = 6,
  chains = 2,
  threads = threading(3),
  silent = 2
)



loo_model_5 <- loo(model_5)
loo_model_6 <- loo(model_6)