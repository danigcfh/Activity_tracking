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
  Comment = numeric()
)

# Export the dataframe to an Excel file
write_xlsx(Activities_tracking, "Activities.xlsx")

#Mood tracking
Mood_tracking <- data.frame(
  Day = numeric() ,
  Sleep	= numeric(),# 1-5
  Anxiety	= numeric(), #1-5
  Mood = numeric(),# -5 - 5
  Exercise = numeric(),# in minutes
  Exercise_intensity = logical(),
  Comment = numeric()
)
# Export the dataframe to an Excel file
write_xlsx(Mood_tracking, "Mood.xlsx")

#Create excel for daily analytics

Analytics <- data.frame(
  Day = as.Date(character()),  # Explicitly set as Date
  Activities = integer(),
  Weighted_difficulty = numeric(),
  Anxiety = numeric(),
  Mood = numeric(),
  Sleep = numeric(),
  Exercise_weighted = numeric(),
  Main_topic = character(),
  Main_topic_Activities = numeric(),
  Aggregated_difficulty = numeric(),
  Comment = character(),
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

