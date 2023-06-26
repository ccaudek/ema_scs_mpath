delete_first_row_xlsx <- function(FOLDER_NAME) {
  
  library(readxl)
  library(writexl)
  library(fs)
  
  # Directory path where Excel files are located
  directory_path <- here("data", "raw", FOLDER_NAME)
  
  # List all Excel files in the directory
  excel_files <- dir_ls(directory_path, regexp = "\\.xlsx$")
  
  # Loop through each Excel file
  for (file in excel_files) {
    # Read the Excel file
    data <- read_excel(file, skip = 1)
    
    # Get the original column names
    # original_column_names <- readxl::col_names(readxl::read_excel(file, n_max = 1))
    
    # Set the original column names to the modified data
    # colnames(data) <- original_column_names
    
    # Write the modified data back to the Excel file
    write_xlsx(data, path = file)
    
    # Print the processed file name
    cat("Deleted first row in:", basename(file), "\n")
  }
  
}


# Change the columns' names of the file with the raw data.
change_cols_names <- function(d) {
  
  # col_names <- c(
  #   "date_time", "context",
  #   "scs_pos_1", "happy",
  #   "dec_2", "dec_4",
  #   "scs_neg_5", "scs_pos_3",
  #   "satisfied", "scs_neg_8",
  #   "dec_3", "scs_neg_4",
  #   "dec_1", "scs_neg_2",
  #   "nervous", "scs_pos_6",
  #   "upset", "scs_pos_7",
  #   "present_emotion", "exam",
  #   "after_exam_emotion", "exam_preparation",
  #   "grade_exp", "before_exam_emotion",
  #   "before_exam_emotion_intensity", "real_grade",
  #   "after_exam_emotion_intensity", "user_id"
  # )
  
  col_names <- c(
    "context", "scs_pos_1",
    "happy", "dec_2",
    "dec_4", "scs_neg_5",
    "scs_pos_3", "satisfied",
    "scs_neg_8", "dec_3",
    "scs_neg_4", "dec_1",
    "scs_neg_2", "nervous",
    "scs_pos_6", "upset",
    "scs_pos_7", "emotion_now",
    "exam", "emotion_after_exam",
    "exam_preparation", "grade_exp",
    "emotion_before_exam", "emotion_intensity_before_exam",
    "real_grade", "emotion_intensity_after_exam",
    "user_id", "day",
    "hour", "minute",
    "time_window", "calendar_day",
    "bysubj_day"
  )
  
  names(d) <- col_names
  
  return(d)
}
