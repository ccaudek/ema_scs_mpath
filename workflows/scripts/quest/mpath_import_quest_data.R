# Script name: mpath_import_quest_data.R
# Project: EMA self-compassion MPATH 2023
# Script purpose: import and clean questionnaire data
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Jun 22 21:08:03 2023
# Last Modified Date: Thu Jun 22 21:08:03 2023
#
# 👉

suppressPackageStartupMessages({
  library("rio")
  library("dplyr")
})

source(here::here("workflows", "scripts", "quest", "funs", "funs_quest.R"))

# Iterate over each file using a loop, import the data, and perform the
# necessary data transformations. Store the DataFrames in a list called
# data_frames, with the index i representing the corresponding file.
file_names <-
  c("quest1.xlsx", "quest2.xlsx", "quest3.xlsx", "quest4.xlsx")

data_frames <- list()

for (i in seq_along(file_names)) {
  file_name <- file_names[i]
  file_path <- here::here("data", "raw", "quest2023", file_name)
  data <- rio::import(file_path)
  data <- data %>%
    dplyr::select(-TIME_start, -TIME_end) %>%
    dplyr::rename(time = TIME_total) %>%
    dplyr::select(-participant)

  # Reconstruct user_id
  data <- gen_user_id(data)

  # Find the index of "time" column
  time_col_index <- which(names(data) == "time")

  if (length(time_col_index) > 0) {
    # Generate new column name
    new_time_col_name <- paste0("time_", i)
    # Update column name
    colnames(data)[time_col_index] <- new_time_col_name
  }

  # Remove unecessary variables
  data <- data |>
    dplyr::select(
      -c(
        nome_1,
        cognome_1,
        anno_1,
        mese_1,
        giorno_1,
        cellulare_1,
        sesso_1,
        nome,
        cognome,
        mese_c,
        giorno_c,
        cellulare_c,
        sex
      )
    )

  data_frames[[i]] <- data
}

# Merge the data frames using the inner_join function in a loop. Starting
# with the first data frame (data_frames[[1]]), iterate over the remaining
# DataFrames and perform inner joins based on the "num_cell_tot_1" column.
quest_dat <- data_frames[[1]]

for (i in 2:length(data_frames)) {
  quest_dat <- inner_join(quest_dat, data_frames[[i]], by = "user_id")
}

names(quest_dat)

quest_dat$sex <- ifelse(quest_dat$Genere_1 == 1, "f", "m")
quest_dat$sex <- NULL
quest_dat$Nazionalita_1 <- NULL

# Remove "kg" and convert to numbers
quest_dat$weight_now <-
  as.numeric(gsub(" kg", "", gsub("kg", "", quest_dat$Peso_attuale_1)))
quest_dat$Peso_attuale_1 <- NULL

quest_dat$Peso_alto_1 <-
  ifelse(quest_dat$Peso_alto_1 == "48,5", "48.5", quest_dat$Peso_alto_1)
quest_dat$Peso_alto_1 <-
  ifelse(quest_dat$Peso_alto_1 == "Boh", "63.85", quest_dat$Peso_alto_1)
quest_dat$weight_high <-
  as.numeric(gsub(" kg", "", gsub("kg", "", quest_dat$Peso_alto_1)))
quest_dat$Peso_alto_1 <- NULL

quest_dat$Peso_basso_1 <- ifelse(quest_dat$Peso_basso_1 == "Boh",
  "56.5",
  quest_dat$Peso_basso_1
)
quest_dat$weight_low <-
  as.numeric(gsub(" kg", "", gsub("kg", "", quest_dat$Peso_basso_1)))
quest_dat$Peso_basso_1 <- NULL

quest_dat$Peso_ideale_1 <-
  ifelse(quest_dat$Peso_ideale_1 == "almeno 5 kg in meno", "63",
    quest_dat$Peso_ideale_1
  )
quest_dat$Peso_ideale_1 <- ifelse(quest_dat$Peso_ideale_1 == "Il mio", "53",
  quest_dat$Peso_ideale_1
)
quest_dat$Peso_ideale_1 <- ifelse(quest_dat$Peso_ideale_1 == "53/54", "53.5",
  quest_dat$Peso_ideale_1
)
quest_dat$weight_ideal <-
  as.numeric(gsub(" kg", "", gsub("kg", "", quest_dat$Peso_ideale_1)))
quest_dat$Peso_ideale_1 <- NULL

quest_dat$Peso_dieta_1 <- ifelse(quest_dat$Peso_dieta_1 == "4-5 kg", "4.5",
  quest_dat$Peso_dieta_1
)
quest_dat$Peso_dieta_1 <- ifelse(quest_dat$Peso_dieta_1 == "7/8 kili", "7.5",
  quest_dat$Peso_dieta_1
)

# Function to convert strings to numbers while handling non-numeric elements
convert_to_number <- function(x) {
  converted <-
    as.numeric(gsub("[^-0-9.]+", "", x)) # Remove non-numeric characters except "-" and "."
  converted[is.na(converted)] <- 0 # Replace NA values with 0
  return(converted)
}

quest_dat$weight_diet <- convert_to_number(quest_dat$Peso_dieta_1)

quest_dat$weight_diet[8] <- 0
quest_dat$weight_diet[10] <- 0
quest_dat$weight_diet[11] <- 5
quest_dat$weight_diet[13] <- 10
quest_dat$weight_diet[29] <- 10
quest_dat$weight_diet[50] <- 0

quest_dat$Peso_dieta_1 <- NULL

# data.frame(
#   weight_diet = quest_dat$weight_diet, weight_now=quest_dat$weight_now,
#   h=quest_dat$Altezza_1, sex=quest_dat$Genere_1
# )

# Convert heights to numeric vector
quest_dat$height <-
  as.numeric(gsub("[^0-9.]+", "", quest_dat$Altezza_1))
quest_dat$Altezza_1 <- NULL
quest_dat$height <-
  ifelse(quest_dat$height < 10, quest_dat$height * 100, quest_dat$height)

quest_dat <- quest_dat |>
  dplyr::rename(
    age = Eta_1,
    marital_status = Stato_civile_1,
    education = Scolarita_1,
    job = Occupazione_1,
    father_job = Professione_del_padre_1,
    mother_job = Professione_della_madre_1,
    test_time = time_1
  )

quest_dat$age <- ifelse(quest_dat$age == "Venti", 20, quest_dat$age)
quest_dat$age <- as.numeric(gsub("[^0-9]+", "", quest_dat$age))

quest_dat$bmi <- quest_dat$weight_now / (quest_dat$height / 100)^2

quest_dat$sex <- ifelse(quest_dat$Genere_1 == 2, "f", "m")
quest_dat$Genere_1 <- NULL

# Remove unnecessary variables.
quest_dat <- quest_dat |>
  dplyr::select(-Dichiara_1, -Dichiara_2)

quest_dat <- quest_dat |>
  dplyr::relocate(user_id)

# Save csv file.
rio::export(
  quest_dat,
  # here::here("data", "prep", "quest_scales", "quest_ema_mpath.csv")
  snakemake@output[["quest_data"]]
)

# eof ----
