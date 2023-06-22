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



# gen_subj_name() ---------------------------------------------------------


#' @description 
#' generate subject code
#' @return data.frame.
#' @param data.frame.
gen_user_id <- function(d) {
  
  library("stringi")
  
  d$nome <- tolower(d$nome_1)
  d$nome <- ifelse(d$nome == "andrea", "an", d$nome)
  d$nome <- ifelse(d$nome == "margherita", "ma", d$nome)
  
  d$cognome <- tolower(d$cognome_1)
  d$cognome <- ifelse(d$cognome == "nibbi", "ni", d$cognome)
  d$cognome <- ifelse(d$cognome == "santarelli", "sa", d$cognome)
  
  d$mese_c <- ifelse(
    d$mese_1 < 10, 
    stri_join("0", as.character(d$mese_1), sep=''), 
    as.character(d$mese_1)
  )
  
  d$giorno_c <- ifelse(
    d$giorno_1 < 10, 
    stri_join("0", as.character(d$giorno_1), sep=''), 
    as.character(d$giorno_1)
  )
  
  d$cellulare_c <- ifelse(
    d$cellulare_1 < 100, 
    stri_join("0", as.character(d$cellulare_1), sep=''), 
    as.character(d$cellulare_1)
  )
  d$cellulare_c <- ifelse(
    d$cellulare_c == "3920674393", "393", d$cellulare_c
  )
  d$cellulare_c <- ifelse(
    d$cellulare_c == "01", "001", d$cellulare_c
  )
  d$cellulare_c <- ifelse(
    d$cellulare_c == "03", "003", d$cellulare_c
  )
  
  d$sex <- ifelse(
    d$sesso_1 == 1, "f",
    ifelse(d$sesso_1 == 2, "m", NA)
  )
  
  d$user_id <- tolower(
    stri_join(d$nome, d$cognome, d$anno_1, 
              d$mese_c, d$giorno_c, d$cellulare_c, d$sex, 
              sep='_')
  )
  
  d
}


