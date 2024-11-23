library(dplyr)

# Functions

# Function to add activity to existing df (avoids duplicates by day and activity)
add_activity <- function(data = NA, day = Sys.Date(), topic, activity, difficulty, Sub_category_1 = NA, Sub_category_2 = NA, comment = NA) {
  # Ensure day is a Date object
  if (!inherits(day, "Date")) {
    day <- as.Date(day, format = "%Y-%m-%d")
  } 
  
  # Ensure all inputs are vectors of the same length
  n <- max(length(topic), length(activity), length(difficulty), length(Sub_category_1), length(Sub_category_2), length(comment))
  topic <- rep(topic, length.out = n)
  activity <- rep(activity, length.out = n)
  difficulty <- rep(difficulty, length.out = n)
  Sub_category_1 <- rep(Sub_category_1, length.out = n)
  Sub_category_2 <- rep(Sub_category_2, length.out = n)
  comment <- rep(comment, length.out = n)
  
  # Create a dataframe for new activities
  new_rows <- data.frame(
    Day = rep(day, n),
    Topic = topic,
    Activity = activity,
    Difficulty = difficulty,
    Sub_category_1 = Sub_category_1,
    Sub_category_2 = Sub_category_2,
    Comment = comment,
    stringsAsFactors = FALSE
  )
  
  # Use anti_join to filter out duplicates
  new_rows_filtered <- new_rows %>%
    anti_join(data, by = c("Day", "Activity"))
  
  if (nrow(new_rows_filtered) == 0) {
    message("No new activities to add (all are duplicates).")
    return(data)
  }
  
  # Combine the filtered new activities with the existing data
  updated_data <- bind_rows(data, new_rows_filtered)
  message("New activities added.")
  return(updated_data)
}

# Function to add mood to existing df (avoids duplicates by date)
add_mood <- function(data = NA, sleep, anxiety, mood, health, exercise, exercise_intensity = 1, comment = NA, day = Sys.Date()) {
  # Ensure day is a Date object
  if (!inherits(day, "Date")) {
    day <- as.Date(day, format = "%Y-%m-%d")
  }
  
  # Create a new row based on input
  new_row <- data.frame(
    Day = day,
    Sleep = sleep,
    Anxiety = anxiety,
    Mood = mood,
    Health = health,
    Exercise = exercise,
    Exercise_intensity = exercise_intensity,
    Comment = comment,
    stringsAsFactors = FALSE
  )
  
  # Check if the day already exists in the dataset
  existing_row <- which(data$Day == day)
  
  # Remove the old row if it exists
  if (length(existing_row) > 0) {
    data <- data[-existing_row, ]
  }
  
  # Add the new row to the existing dataset
  updated_data <- rbind(data, new_row)
  
  message("Mood entry added/updated.")
  return(updated_data)
}


# Initialize a weighted sum function
weighted_sum <- function(difficulties, sleep, mood, health) {
  weights <- 1 + 0.05 * (seq_along(difficulties) - 1)  # Increasing weights
  base_difficulty <- sum(difficulties * weights, na.rm = TRUE)  # Handle NA values
  
  # Adjust difficulty based on sleep
  sleep_factor <- ifelse(
    is.na(sleep), 
    1, 
    1 + (5 - sleep) * 0.1 + ifelse(sleep < 0, abs(sleep), 0)
  )
  
  #Scale
  # 5: Perfectly rested – Indicates optimal sleep with no noticeable impact on functionality.
  # 4: Minor disturbances – Sleep quality is slightly impaired, but it has minimal effect on functionality the next day.
  # 3: Wakes up tired – Sleep quality is compromised, leading to a noticeable difficulty in functioning the next day.
  # 2: Major disturbances – Sleep disruptions significantly affect how well one can perform tasks the next day.
  # 1: Significant impairment – This rating implies very poor sleep quality that greatly impairs performance.
  # -1: No sleep, need an entire day to recover – Reflects extreme impairment where sleep deprivation results in a complete inability to perform tasks and necessitates an entire recovery period.
  
  # Adjust difficulty based on mood
  mood_factor <- ifelse(
    is.na(mood), 
    1, 
    ifelse(mood > 0, 1 - mood/20, 1 + (mood^2) / 25)
  )  # Positive mood has lower impact, negative mood has stronger, exponential impact
  
  #Scale
  # 5: Upbeat, confident, and full of energy. 
  # 4: Cheerful and motivated. 
  # 3: Happy, lively
  # 2: Happy, chill
  # 1: Content, at ease
  # 0: neutral
  # -1: Manageable emotional discomfort
  # -2: Frustrated or mildly discouraged. 
  # -3: Sad, unmotivated
  # -4: Depressive, emotionally drained and hard to focus
  # -5: Deep depression, overwhelmed and hopeless, no motivation, energy or concentration
  
  # Adjust difficulty based on health
  health_factor <- ifelse(
    is.na(health), 
    1, 
    ifelse(health > 0, 1 - health / 50, 1 + (health^2) / 25)
  )  # Positive health has lower impact, negative health has stronger, exponential impact
  
  #Scale
  # 0: normal, no health problems, individual baseline
  # -1: Slight physical discomfort: might affect focus but manageable
  # -2: Mild illness or injury : distracting and noticeable
  # -3: Moderate illness or injury : hard to ignore, interrupts some activities
  # -4: Severe illness or injury: prevents doing daily activities
  # -5: Extreme physical impairment: as bad as it can be 
  
  overall_factor <- sleep_factor * mood_factor*health_factor
  Weighted_difficulty <- base_difficulty * sleep_factor * mood_factor*health_factor
  # Combine all factors
  return(c(base_difficulty, overall_factor, Weighted_difficulty))
}

