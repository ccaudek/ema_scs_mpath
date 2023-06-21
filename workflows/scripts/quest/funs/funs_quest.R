# Add item to catch_items.csv --------------------------------------------------

add_catch_item <- function(user_id, catch_item) {
  file_path <- here::here("data", "prep", "quest_scales", "catch_items.csv")
  
  if (file.exists(file_path)) {
    old_dat <- rio::import(file_path)
    new_dat <- data.frame(user_id, catch_item)
    both_dat <- dplyr::full_join(old_dat, new_dat, by = "user_id")
    rio::export(both_dat, file_path)
  } else {
    new_dat <- data.frame(user_id, catch_item)
    rio::export(new_dat, file_path)
  }
}

