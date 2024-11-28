library(tidyverse)
library(readxl)
library(writexl)

#This work © 2024 by Daniela González is licensed under CC BY-NC-SA 4.0


# Load data
Activities <- read_excel("Activities.xlsx")
Per_day <- read_excel("Day_Analytics.xlsx")


# Ensure date coherence  day 

Activities$Day <- as.Date(Activities$Day, format = "%d/%m/%Y")
Per_day$Day <- as.Date(Per_day$Day, format = "%d/%m/%Y")

#avoid duplicates
Per_day <- Per_day %>% distinct()
Activities <- Activities %>% distinct()

# Exclude old data explicitly
# Identify days with added or changed activities
activity_number <- Activities %>%
  group_by(Day) %>%
  summarise(Activities_count = n())  # Count activities per day

# Initialize a vector to store days needing reanalysis as a Date vector
days <- as.Date(character())

# Identify days with new or updated activities
affected_activity_days <- Activities %>%
  group_by(Day) %>%
  summarise(Activities_count = n()) %>%
  left_join(Per_day %>% select(Day, Activities), by = "Day") %>%
  filter(is.na(Activities) | Activities_count != Activities) %>%
  pull(Day)

# Identify days with new or updated mood data
affected_mood_days <- Per_day %>%
  filter(Needs_Update) %>%
  pull(Day)


# Combine all affected dates
days <- unique(c(affected_activity_days, affected_mood_days))


# Subset activities for the identified days
new_activity <- subset(Activities, Day %in% days)
new_mood <- subset(Per_day, Day %in% days)


# Create an empty data frame for results
results <- data.frame(
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

# Iterate over each day and calculate metrics
for (day in days) {
  # Filter data for the current day
  activity_day <- subset(new_activity, Day == day)
  mood_day <- subset(new_mood, Day == day)
  
  # Extract metrics
  Difficulties <- activity_day$Difficulty
  Day <- as.Date(day, format = "%d/%m/%Y")  # Ensure the day is a Date type
  numActivities <- length(activity_day$Activity[!is.na(activity_day$Difficulty)])  # Count valid difficulties

  # Use default values for Anxiety, Sleep, and Comment from Mood df
  Anxiety <- ifelse(nrow(mood_day) > 0, mood_day$Anxiety, NA)
  Mood <- ifelse(nrow(mood_day) > 0, mood_day$Mood, NA)
  Health <- ifelse(nrow(mood_day) > 0, mood_day$Health, NA)
  Sleep <- ifelse(nrow(mood_day) > 0, mood_day$Sleep, NA)
  Comment <- ifelse(nrow(mood_day) > 0, mood_day$Comment, NA)
  
  #Retrieve exercise data
  Exercise_Low <- ifelse(nrow(mood_day) > 0, mood_day$Exercise_Low, NA)
  Exercise_High <- ifelse(nrow(mood_day) > 0, mood_day$Exercise_High, NA)
  Exercise <- ifelse(nrow(mood_day) > 0, mood_day$Exercise_weighted, NA)
  
  #Calculate weighted difficulty
  Difficulty_calcs <- weighted_sum(Difficulties, Sleep, Mood, Health)
  
  Base_difficulty <-  Difficulty_calcs[1]
  Factor_difficulty <-  Difficulty_calcs[2]
  Weighted_difficulty <-  Difficulty_calcs[3]

  # Identify main topic
  topics <- unique(activity_day$Topic)
  
  # Calculate the weighted difficulty for each topic
  topic_summary <- activity_day %>%
    group_by(Topic) %>%
    summarise(
      Topic_count = n(),  # Count of activities for each topic
      Total_difficulty = sum(Difficulty, na.rm = TRUE)  # Sum of difficulties for each topic
    ) %>%
    arrange(desc(Total_difficulty))  # Sort by the weighted difficulty
  
  # Select the topic with the highest weighted difficulty
  Main_topic <- topic_summary$Topic[1]
  Aggregated_difficulty<- topic_summary$Total_difficulty[1]
  Main_topic_Activities<- topic_summary$Topic_count[1]
  
  # Create a new row
  row <- data.frame(
    Day = Day,  # Ensure the Day column is of Date type
    Activities = numActivities,
    Base_difficulty = Base_difficulty,
    Factor_difficulty = Factor_difficulty,
    Weighted_difficulty = Weighted_difficulty,
    Anxiety = Anxiety,
    Mood = Mood,
    Sleep = Sleep,
    Health = Health,
    Exercise_Low = Exercise_Low,
    Exercise_High= Exercise_High,
    Exercise_weighted = Exercise,
    Main_topic = Main_topic,
    Main_topic_Activities = Main_topic_Activities,
    Aggregated_difficulty = Aggregated_difficulty,
    Comment = Comment,
    Needs_Update = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Append to results
  results <- bind_rows(results, row)
}

# Ensure the Comment column is of type character in both data frames
Per_day$Comment <- as.character(Per_day$Comment)
results$Comment <- as.character(results$Comment)
Per_day$Main_topic <- as.character(Per_day$Main_topic)
results$Main_topic <- as.character(results$Main_topic)

# Combine with existing analytics
combined_results <- bind_rows(Per_day, results)

# Ensure the 'Day' column is of type Date and remove duplicates
final_results <- combined_results %>%
  group_by(Day) %>%
  slice_max(order_by = row_number(), n = 1) %>%  # Retain only the last entry for each day
  ungroup()

# View and save results
View(final_results)

# Save results to an Excel file
write_xlsx(final_results, "Day_Analytics.xlsx")
