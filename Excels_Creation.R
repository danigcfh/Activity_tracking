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
  Sleep	= numeric(),# -1, 5
  
  #Scale for Sleep
  # 5: Perfectly rested – Indicates optimal sleep with no noticeable impact on functionality.
  # 4: Minor disturbances – Sleep quality is slightly impaired, but it has minimal effect on functionality the next day.
  # 3: Wakes up tired – Sleep quality is compromised, leading to limited but noticeable difficulty in functioning the next day.
  # 2: Major disturbances – Sleep disruptions significantly affect how well one can perform tasks the next day.
  # 1: Significant impairment – This rating implies very poor sleep quality that greatly impairs performance.
  # -1: No sleep, need an entire day to recover – Reflects extreme impairment where sleep deprivation results in a complete inability to perform tasks and necessitates an entire recovery period.
  
  Anxiety	= numeric(), # 1-5
  
  #Scale for anxiety
  # To take into account, single events can push anxiety number up even if the day overall is a lesser score
  # 10: Panic attack
  # 5: High level Anxiety sustained all the day
  # 4: Sustained low level anxiety all day or semi-regular high level anxiety
  # 3: semi-regular low level anxiety
  # 2: Mostly relaxed, seldom activated
  # 1: perfect relaxation
  
  Mood = numeric(),# -5 - 5
  
  
  #Scale for Mood
  # 5: Upbeat, confident, and full of energy. 
  # 4: Cheerful and motivated. 
  # 3: Happy, lively
  # 2: Happy, chill
  # 1: Content, at ease
  # 0: neutral
  # -1: Manageable emotional discomfort
  # -2: Frustrated or mildly discouraged. 
  # -3: Sad, unmotivated, make slightly harder to make decisions
  # -4: Depressive, emotionally drained, it's significantly harder to focus or take decisions
  # -5: Deep depression, overwhelmed and hopeless, no motivation, energy or concentration and/or panic attack during the day, impossible to focus or take decisions
  
  
  
  Healt = numeric(),# 0 - -5
  #Scale for health
  # 0: normal, no health problems, individual baseline
  # -1: Slight physical discomfort: might affect focus but manageable
  # -2: Mild illness or injury : distracting and noticeable
  # -3: Moderate illness or injury : hard to ignore, interrupts some activities
  # -4: Severe illness or injury: prevents doing daily activities
  # -5: Extreme physical impairment: as bad as it can be 
  
  
  Exercise = numeric(),# in minutes
  Exercise_Intensity = logical(),
  Comment = numeric()
)


# Export the dataframe to an Excel file
write_xlsx(Mood_tracking, "Mood.xlsx")

#Create excel for daily analytics

Analytics <- data.frame(
  Day = as.Date(character()),  # Explicitly set as Date
  Activities = integer(),
  Base_difficulty = numeric(),
  Factor_difficylty = numeric(),
  Weighted_difficulty = numeric(),
  Anxiety = numeric(),
  Mood = numeric(),
  Health = numeric(),
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

