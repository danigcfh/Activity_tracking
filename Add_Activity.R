library(tidyverse)
library(readxl)
library(writexl)

#Load data
Activities <- read_excel("Activities.xlsx")

#Create function to add activity 
add_activity <- function(data, day = Sys.Date(), topic, activity, difficulty, Sub_category_1 = NA, Sub_category_2 = NA, comment = NA) {
  # Ensure day is a Date object
  if (!inherits(day, "Date")) {
    day <- as.Date(day, format = "%Y-%m-%d")
  } 
  
  # Ensure all inputs are vectors of the same length
  n <- max(length(topic), length(activity), length(difficulty), length(Sub_category_1), length(Sub_category_2), length(comment))
  topic <- rep(topic, length.out = n)
  activity <- rep(activity, length.out = n)
  difficulty <- rep(difficulty, length.out = n)
  Sub_category_1 <- rep(Sub_category_1, length.out = n)
  Sub_category_2 <- rep(Sub_category_2, length.out = n)
  comment <- rep(comment, length.out = n)
  
  # Create a dataframe for new activities
  new_rows <- data.frame(
    Day = rep(day, n),
    Topic = topic,
    Activity = activity,
    Difficulty = difficulty,
    Sub_category_1 = Sub_category_1,
    Sub_category_2 = Sub_category_2,
    Comment = comment,
    stringsAsFactors = FALSE
  )
  
  # Use anti_join to filter out duplicates
  new_rows_filtered <- new_rows %>%
    anti_join(data, by = c("Day", "Activity"))
  
  if (nrow(new_rows_filtered) == 0) {
    message("No new activities to add (all are duplicates).")
    return(data)
  }
  
  # Combine the filtered new activities with the existing data
  updated_data <- bind_rows(data, new_rows_filtered)
  message("New activities added.")
  return(updated_data)
}


#current topics: "Self-care"     "House"         "Administratif" "Thèse"        
# "Cours"         "Others"        "FEI"              

new_data <- add_activity(Activities,
                         topic = c("Administratif", "Administratif", "Thèse","Others" ),
                         activity = c("Data Analysis for survey École Doctorale",
                           "Conseil École Doctorale",
                           "Attend seminar: mediatization of Gaza war",
                           "Train back"), 
                         difficulty = c(3,3,2,2))

# Export the dataframe to an Excel file
write_xlsx(new_data, "Activities.xlsx")

# Confirm the file is saved
print("File 'Activities.xlsx' saved in the working directory.")


