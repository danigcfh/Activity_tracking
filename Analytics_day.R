library(tidyverse)
library(readxl)
library(writexl)

# Load data
Activities <- read_excel("Activities.xlsx")
Per_day <- read_excel("Day_Analytics.xlsx")
Mood <- read_excel("Mood.xlsx")

# Ensure date coherence  day 

Activities$Day <- as.Date(Activities$Day, format = "%d/%m/%Y")
Per_day$Day <- as.Date(Per_day$Day, format = "%d/%m/%Y")
Mood$Day <- as.Date(Mood$Day, format = "%d/%m/%Y")



#avoid duplicates
Per_day <- Per_day %>% distinct()
Activities <- Activities %>% distinct()
Mood <- Mood %>% distinct()


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
affected_mood_days <- Mood %>%
  left_join(Per_day, by = "Day", suffix = c("_Mood", "_PerDay")) %>%
  filter(
    is.na(Sleep_Mood) != is.na(Sleep_PerDay) | Sleep_Mood != Sleep_PerDay |
      is.na(Anxiety_Mood) != is.na(Anxiety_PerDay) | Anxiety_Mood != Anxiety_PerDay |
      is.na(Mood_Mood) != is.na(Mood_PerDay) | Mood_Mood != Mood_PerDay |
      is.na(Comment_Mood) != is.na(Comment_PerDay) | Comment_Mood != Comment_PerDay
  ) %>%
  pull(Day)

# Combine all affected dates
days <- unique(c(affected_activity_days, affected_mood_days))


# Subset activities for the identified days
new_activity <- subset(Activities, Day %in% days)
new_mood <- subset(Mood, Day %in% days)

# Initialize a weighted sum function
weighted_sum <- function(difficulties, sleep, mood, health) {
  weights <- 1 + 0.1 * (seq_along(difficulties) - 1)  # Increasing weights
  base_difficulty <- sum(difficulties * weights, na.rm = TRUE)  # Handle NA values
  
  # Adjust difficulty based on sleep
  sleep_factor <- ifelse(is.na(sleep), 1, 1 + (5 - sleep) * 0.1)
  
  # Adjust difficulty based on mood
  mood_factor <- ifelse(is.na(mood), 1, 1 + -mood / 20)  # Mood in range -5 to 5
  
  # Adjust difficulty based on health
  health_factor <- ifelse(
    is.na(health), 
    1, 
    ifelse(health >= 0, 1 + health / 50, 1 + health / 10)
  )  # Positive health has lower impact, negative health has stronger
  
  # Combine all factors
  return(base_difficulty * sleep_factor * mood_factor*health_factor)
}


# Create an empty dataframe for results
results <- data.frame(
  Day = as.Date(character()),  # Explicitly set as Date
  Activities = integer(),
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

# Iterate over each day and calculate metrics
for (day in days) {
  # Filter data for the current day
  activity_day <- subset(new_activity, Day == day)
  mood_day <- subset(new_mood, Day == day)
  
  # Extract metrics
  Difficulties <- activity_day$Difficulty
  Day <- as.Date(day, format = "%d/%m/%Y")  # Ensure the day is a Date type
  Activities <- length(activity_day$Activity[!is.na(activity_day$Difficulty)])  # Count valid difficulties

  # Use default values for Anxiety, Sleep, and Comment from Mood df
  Anxiety <- ifelse(nrow(mood_day) > 0, mood_day$Anxiety, NA)
  Mood <- ifelse(nrow(mood_day) > 0, mood_day$Mood, NA)
  Health <- ifelse(nrow(mood_day) > 0, mood_day$Health, NA)
  Sleep <- ifelse(nrow(mood_day) > 0, mood_day$Sleep, NA)
  Comment <- ifelse(nrow(mood_day) > 0, mood_day$Comment, NA)
  
  #Calculate weighted difficulty
  Weighted_difficulty <- weighted_sum(Difficulties, Sleep, Mood, Health)
  
  # Check for "Exercise" in activities
  Exercise_intensity <- ifelse(nrow(mood_day) > 0, mood_day$Exercise_intensity, NA)
  Exercise_base <- ifelse(nrow(mood_day) > 0, mood_day$Exercise, NA)
  Exercise <- ifelse(!is.na(Exercise_base), Exercise_base * Exercise_intensity, NA)
  
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
    Activities = Activities,
    Weighted_difficulty = Weighted_difficulty,
    Anxiety = Anxiety,
    Mood = Mood,
    Sleep = Sleep,
    Health = Health,
    Exercise_weighted = Exercise,
    Main_topic = Main_topic,
    Main_topic_Activities = Main_topic_Activities,
    Aggregated_difficulty = Aggregated_difficulty,
    Comment = Comment,
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
