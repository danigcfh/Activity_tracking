library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)

#This work © 2024 by Daniela González is licensed under CC BY-NC-SA 4.0

#Load data
To_do <- read_excel("To_do.xlsx")

#add mood
new_data <-add_to_do(data = NA,
                     due_date = To_do$Due_date,
                     topic = To_do$Topic, 
                     activity = To_do$Activity, 
                     difficulty  = To_do$Difficulty, 
                     estimated_time = To_do$Estimated_time, 
                     importance  = To_do$Importance, 
                     dependencies  = To_do$Dependencies, 
                     Sub_category_1  = To_do$Sub_category_1, 
                     Sub_category_2  = To_do$Sub_category_2, 
                     comment = To_do$Comment
)
                     

# Export the dataframe to an Excel file
write_xlsx(new_data, "To_do.xlsx")


#Urgency scale
#5: Immediate : has to be done as soon as possible
#4: High : to be done within the next week at most
#3: Medium : To be done in 2 weeks at most
#2: Low : can wait more than 2 weeks
#1: Irrelevant: if it's not done, no serious repercussions

#Importance scale
#1: minimal impact on goals or responsabilities
#2: low impact, relevant but not directly tied to goals
#3: Moderate, not critical in the short term and/or routine, needed for smooth functioning
#4: High impact:  for achieving significant goals or maintaining key commitments.
#5: Critical impact: essential and tied directly to major goals, responsibilities, or deadlines, could lead to important consequences

#Difficulty scale
# 1: Minimal effort required
# 2: light effort: straightforward but may demand some cognitive or physical investment
# 3: Moderate: require a balanced amount of focus, mental effort, or time
# 4: Challenging: demand significant concentration, time, or physical energy
# 5: Tasks that are mentally or physically exhausting, requiring extreme concentration, effort, or time

