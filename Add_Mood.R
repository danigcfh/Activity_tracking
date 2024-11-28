library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)

#This work © 2024 by Daniela González is licensed under CC BY-NC-SA 4.0


#Load data
Per_day <- read_excel("Day_Analytics.xlsx")

View(Mood)

#add mood
new_data <-add_mood(data = Per_day,
                    day = Sys.Date()-1,
                    sleep = 4,  
                    anxiety = 2, 
                    mood = 2, 
                    health = 0,
                    exercise_low = 48, 
                    exercise_high = NA, 
                    comment = "pretty good day overall, always exhausting to go to paris but got to actually enter the CEMTI this time")

#Scale for anxiety
# To take into account, single events can push anxiety number up even if the day overall is a lesser score
# 10: Panic attack
# 5: High level Anxiety sustained all the day
# 4: Sustained low level anxiety (annoying but not debilitating) all day or mixed with occasional high level anxiety
# 3: semi-regular low level anxiety
# 2: Mostly relaxed, seldom activated
# 1: perfect relaxation

#Scale for Sleep
# 5: Perfectly rested – Indicates optimal sleep with no noticeable impact on functionality.
# 4: Minor disturbances – Sleep quality is slightly impaired, but it has minimal effect on functionality the next day.
# 3: Wakes up tired – Sleep quality is compromised, leading to limited but noticeable difficulty in functioning the next day.
# 2: Major disturbances – Sleep disruptions significantly affect how well one can perform tasks the next day.
# 1: Significant impairment – This rating implies very poor sleep quality that greatly impairs performance.
# -1: No sleep, need an entire day to recover – Reflects extreme impairment where sleep deprivation results in a complete inability to perform tasks and necessitates an entire recovery period.


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


#Scale for health
# 0: normal, no health problems, individual baseline
# -1: Slight physical discomfort: might affect focus but manageable
# -2: Mild illness or injury : distracting and noticeable
# -3: Moderate illness or injury : hard to ignore, interrupts some activities
# -4: Severe illness or injury: prevents doing daily activities
# -5: Extreme physical impairment: as bad as it can be 

View(new_data)
# Export the dataframe to an Excel file
write_xlsx(new_data, "Day_Analytics.xlsx")




