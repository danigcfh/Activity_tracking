library(tidyverse)
library(readxl)
library(writexl)

#This work © 2024 by Daniela González is licensed under CC BY-NC-SA 4.0


#Create excel for activity tracking 
Activities_tracking <- data.frame(
  Day = numeric() ,
  Topic = character(),# personnally divided by house, self care, administrative stuff, class, research, etc
  Activity = character(),# description of activity
  Difficulty = numeric(),# 1-5
  Sub_Category_1 = character(),# further description if needed
  Sub_Category_2 = character(),# further description if needed
  Comment = numeric()
)

# Export the dataframe to an Excel file
write_xlsx(Activities_tracking, "Activities.xlsx")

#Create excel for daily analytics

Analytics <- data.frame(
  Day = as.Date(character()),  # Explicitly set as Date
  Activities = integer(),
  Base_difficulty = numeric(),
  Factor_difficulty = numeric(),
  Weighted_difficulty = numeric(),
  Anxiety = numeric(),
  Mood = numeric(),
  Health = numeric(),
  Sleep = numeric(),
  Exercise_Low = numeric(),# in minutes
  Exercise_High= numeric(),
  Exercise_weighted = numeric(),
  Main_topic = character(),
  Main_topic_Activities = numeric(),
  Aggregated_difficulty = numeric(),
  Comment = character(),
  Needs_Update = logical(),
  stringsAsFactors = FALSE
)


# Save results to an Excel file
write_xlsx(Analytics, "Day_Analytics.xlsx")

#Create excel for topic analytics

Analytics_per_topic <- data.frame(
    Topic = character(),
    Day = as.Date(character()),  # Explicitly set as Date
    Activities = integer(),
    Aggregated_difficulty = numeric(),
    Day_start = as.Date(character()),  
    Day_end = as.Date(character()),
    Details = character(),
    Comments = character(),
    stringsAsFactors = FALSE
)


# Save results to an Excel file
write_xlsx(Analytics_per_topic, "Topic_Analytics.xlsx")



#to do tasks tracking
To_do <- data.frame(
  Priority = numeric(),
  Urgency = numeric(),
  Importance = numeric(),
  Topic = character(),
  Activity = character(),
  Difficulty = numeric(),
  Due_date = numeric(),
  Recommended_date = numeric(),
  Estimated_time = numeric(),
  Dependencies = logical(),
  Sub_category_1 = character(),
  Sub_category_2 = character(),
  Comment = character(),
  Last_updated = numeric() ,
  stringsAsFactors = FALSE
)

# Export the dataframe to an Excel file
write_xlsx(To_do, "To_do.xlsx")

