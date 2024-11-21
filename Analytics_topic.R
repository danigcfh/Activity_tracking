library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)

# Load data
Activities <- read_excel("Activities.xlsx")
Mood <- read_excel("Mood.xlsx")

# Load all sheets from the Excel file into a list
Per_topic <- read_excel("Topic_Analytics.xlsx")
Per_topic$Topic <- as.character(Per_topic$Topic )
Per_topic$Day <- as.Date(Per_topic$Topic, format = "%d/%m/%Y" )

# Identify topics and days with new or updated activities
affected_activity_topics <- Activities %>%
  group_by(Topic) %>%
  summarise(Activities_count = n(), .groups = "drop") %>%
  left_join(
    Per_topic %>%
      group_by(Topic) %>%
      summarise(Old_count = sum(Activities, na.rm = TRUE), .groups = "drop"),
    by = "Topic"
  ) %>%
  filter(is.na(Old_count) | Activities_count != Old_count) %>%
  pull(Topic)

affected_activity_days <- Activities %>%
  group_by(Day) %>%
  summarise(Activities_count = n(), .groups = "drop") %>%
  left_join(
    Per_topic %>%
      group_by(Day) %>%
      summarise(Old_count = sum(Activities, na.rm = TRUE), .groups = "drop"),
    by = "Day"
  ) %>%
  filter(is.na(Old_count) | Activities_count != Old_count) %>%
  pull(Day)

# Subset activities for identified topics and days
new_activity <- Activities %>%
  filter(Topic %in% affected_activity_topics| Day %in% affected_activity_days)

#Initialize df for results
results <- data.frame(
  Topic = character(),
  Day = as.Date(character(), format = "%d/%m/%Y"),  # Explicitly set as Date
  Activities = integer(),
  Aggregated_difficulty = numeric(),
  Day_start = as.Date(character(), format = "%d/%m/%Y"),
  Day_end =as.Date(character(), format = "%d/%m/%Y"),
  Details = character(),
  Comments = character(),
  stringsAsFactors = FALSE
)

# Process updates for each affected topic
for (topic in unique(new_activity$Topic)) {
  # Get the sheet data for the topic
  existing_data <- subset(Per_topic, Topic == topic)
  
  # Subset activities for the topic
  activity_topic <- new_activity %>% filter(Topic == topic)
  
  # Compute new analytics per day
  daily_analytics <- activity_topic %>%
    group_by(Day) %>%
    summarise(
      Activities = n(),
      Aggregated_difficulty = sum(Difficulty, na.rm = TRUE),
      Details = paste(Activity[!is.na(Activity)], collapse = "/"),
      Comment = paste(Comment[!is.na(Comment)], collapse = "/")
    )
  
  #Set analytics per day
  row_days <- data.frame(
    Topic = topic,
    Day = daily_analytics$Day,  
    Activities = daily_analytics$Activities,
    Aggregated_difficulty = daily_analytics$Aggregated_difficulty,
    Day_start = NA,  
    Day_end = NA,
    Details = daily_analytics$Details,
    Comments = daily_analytics$Comment,
    stringsAsFactors = FALSE
  )
  
  #Set analytics for the entire topic
  row_general_summary <- data.frame(
    Topic = topic,
    Day = NA,  
    Activities = sum(row_days$Activities),
    Aggregated_difficulty = sum(row_days$Aggregated_difficulty),
    Day_start = as.Date(min(row_days$Day), format = "%d/%m/%Y"),  
    Day_end = as.Date(max(row_days$Day), format = "%d/%m/%Y"),
    Details = NA,
    Comments = NA,
    stringsAsFactors = FALSE
  )
  
  # update results
  results <- rbind(results, row_days)
  results$Day_start <- as.Date(  results$Day_start, format = "%d/%m/%Y")
  results$Day_end <- as.Date(  results$Day_end, format = "%d/%m/%Y")
  results <- rbind(results, row_general_summary)
  
}

View(results)

#update initial df

Per_topic <- rbind(Per_topic, results)


#Save df
write_xlsx(Per_topic, "Topic_Analytics.xlsx")

