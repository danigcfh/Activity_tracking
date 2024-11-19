library(tidyverse)
library(readxl)
library(writexl)


#Create excel for activity tracking 
Activities_tracking <- data.frame(
  Day = numeric() ,
  Topic = character(),# personnally divided by house, self care, administrative stuff, class, research, etc
  Activity = character(),# description of activity
  Difficulty = numeric(),# 1-5
  Sub_category_1 = character(),# further description if needed
  Sub_category_2 = character(),# further description if needed
  Sleep	= numeric(),# 1-5
  Anxiety	= numeric(), #1-5
  Mood = numeric(),# -5 - 5
  Comment = numeric()
)

# Export the dataframe to an Excel file
write_xlsx(Activities_tracking, "Activities.xlsx")

#Create excel for daily analytics

Analytics <- data.frame(
  Day = as.Date(character()),  # Explicitly set as Date
  Activities = integer(),
  Weighted_difficulty = numeric(),
  Anxiety = numeric(),
  Mood = numeric(),
  Sleep = numeric(),
  Exercise = numeric(),
  Comments = character(),
  stringsAsFactors = FALSE
)


# Save results to an Excel file
write_xlsx(Analytics, "Day_Analytics.xlsx")
