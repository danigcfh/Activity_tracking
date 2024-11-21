library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)


#Load data
Mood <- read_excel("Mood.xlsx")
str(Mood)

add_mood <- function(data, sleep, anxiety, mood, exercise, exercise_intensity = 1, comment = NA,   day = Sys.Date()) {
  # Ensure day is a Date object
  if (!inherits(day, "Date")) {
    day <- as.Date(day, format = "%Y-%m-%d")
  } 
  
  # Check if the day already exists in the dataset
  existing_row <- which(data$Day == day)
  
  # Create a new row based on input
  new_row <- data.frame(
    Day = day,
    Sleep = sleep,
    Anxiety = anxiety,
    Mood = mood,
    Exercise = exercise,
    Exercise_intensity = exercise_intensity,
    Comment = comment,
    stringsAsFactors = FALSE
  )
  
  #delete old row
  if (length(existing_row)>0){
    data <- data[-existing_row, ]
  }
  
  
  # Add the new row to the existing dataset
  updated_data <- rbind(data, new_row)
  
  return(updated_data)
}


new_data <-add_mood(Mood,
                    day = "2024-11-20",
                    sleep = 3,  
                    anxiety = 3, 
                    mood = 0, 
                    exercise = 51, 
                    exercise_intensity = 1, 
                    comment = "Had a spike in anxiety towards the end of the day, pretty sure I was dissociating all day and then it dropped sudenly, I didn't feel particularly bad otherwise so I'm putting neutral mood" ) #input variables as needed


# Export the dataframe to an Excel file
write_xlsx(new_data, "Mood.xlsx")




