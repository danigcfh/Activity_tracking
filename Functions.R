library(dplyr)
#This work © 2024 by Daniela González is licensed under CC BY-NC-SA 4.0


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
  Sub_Category_1 <- rep(Sub_category_1, length.out = n)
  Sub_Category_2 <- rep(Sub_category_2, length.out = n)
  comment <- rep(comment, length.out = n)
  
  # Create a dataframe for new activities
  new_rows <- data.frame(
    Day = rep(as.Date(day), n),
    Topic = topic,
    Activity = activity,
    Difficulty = difficulty,
    Sub_Category_1 = Sub_Category_1,
    Sub_Category_2 = Sub_Category_2,
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
add_mood <- function(data = NA, sleep, anxiety, mood, health, exercise_low, exercise_high, comment = NA, day = Sys.Date(), Needs_Update = TRUE) {
  # Ensure day is a Date object
  if (!inherits(day, "Date")) {
    day <- as.Date(day, format = "%Y-%m-%d")
  }
  
  Exercise_weighted <- sum(c(exercise_low, exercise_high) * c(1, 2), na.rm = TRUE)  # Low exercise gets weight 1, High exercise gets weight 2
  
  # Create a new row based on input
  new_row <- data.frame(
    Day = as.Date(day),
    Sleep = sleep,
    Anxiety = anxiety,
    Mood = mood,
    Health = health,
    Exercise_Low = exercise_low,# in minutes
    Exercise_High= exercise_high,
    Exercise_weighted = Exercise_weighted,  # Correct weighted value here
    Comment = comment,
    Needs_Update = Needs_Update,
    stringsAsFactors = FALSE
  )
  
  # Ensure that we only keep columns that exist in the original dataframe
  new_row <- new_row[, colnames(new_row) %in% colnames(data), drop = FALSE]
  
  # Check if the day already exists in the dataset
  existing_row <- which(data$Day == day)
  
  if (length(existing_row) > 0) {
    # If the day already exists, compare values to check for updates
    old_row <- data[existing_row, colnames(new_row), drop = FALSE]
    if (!all.equal(new_row, old_row, ignore.row.order = TRUE)) {
      # If any value is different, set needs_update to TRUE
      new_row$Needs_Update <- TRUE
    }
    # Update the existing row with the new data (only for matching columns)
    data[existing_row, colnames(new_row)] <- new_row
  } else {
    # If the day does not exist, add the new row (only matching columns)
    data <- bind_rows(data, new_row)
  }
  
  message("Mood entry added/updated.")
  return(data)
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


# Helper function to enforce column classes
enforce_column_classes <- function(data, reference_df) {
  for (col in colnames(reference_df)) {
    if (col %in% colnames(data)) {
      # Cast the column to the reference type
      data[[col]] <- as(data[[col]], class(reference_df[[col]]))
    } else {
      # Add missing columns with default values
      data[[col]] <- reference_df[[col]]
    }
  }
  # Remove extra columns not in the reference
  data <- data[colnames(reference_df)]
  return(data)
}

# Define a function to calculate urgency
calculate_urgency <- function(Recommended_date, dependencies) {
  # Calculate days remaining
  days_remaining <- as.numeric(Recommended_date - Sys.Date())
  
  # Adjust for dependencies (e.g., add +3 urgency if there are dependencies)
  dependency_adjustment <- ifelse(dependencies == TRUE, 3, 0)
  
  # Combine the days remaining with dependencies adjustment
  urgency_score <- days_remaining - dependency_adjustment
  
  # Apply the urgency scale
  urgency <- case_when(
    is.na(Recommended_date) ~ 1,  # Default to 1 if Recommended_date is NA
    urgency_score <= 0 ~ 5,  # Immediate: already overdue or due today
    urgency_score <= 7 ~ 4,  # High: due within a week
    urgency_score <= 14 ~ 3, # Medium: due within two weeks
    TRUE ~ 2 
  )
  return(urgency)
}

calculate_recommended_date <- function(due_date, estimated_time, importance) {
  # Ensure due_date is a Date object
  due_date <- as.Date(due_date, format = "%Y-%m-%d")
  
  # Calculate the recommended date using case_when
  recommended_date <- dplyr::case_when(
    is.na(due_date) & estimated_time <= 1 ~ Sys.Date(),
    is.na(due_date) & importance == 5 ~ Sys.Date() + 7,
    is.na(due_date) & importance %in% c(3, 4) ~ Sys.Date() + 14,
    !is.na(due_date) & estimated_time <= 1 ~ due_date,
    !is.na(due_date) & estimated_time <= 3 ~ due_date - 1,
    !is.na(due_date) & estimated_time <= 5 ~ due_date - 2,
    !is.na(due_date) ~ due_date - 3,
    TRUE ~ NA_Date_  # Default to NA if none of the conditions match
  )
  
  return(as.Date(recommended_date, format = "%Y-%m-%d"))
}

add_to_do <- function(data = NULL, due_date, topic, activity, difficulty, 
                      estimated_time, importance, dependencies = FALSE, 
                      Sub_category_1 = NA, 
                      Sub_category_2 = NA, comment = NA) {
  
  # Ensure due_date and recommended_date are Date objects
  due_date <- as.Date(due_date, format = "%Y-%m-%d")
  
  # Calculate recommended date
  recommended_date <- as.Date(calculate_recommended_date(due_date, estimated_time, importance), format = "%Y-%m-%d")
  
  # Calculate urgency
  urgency <- calculate_urgency(recommended_date, dependencies)
  
  # Calculate priority
  priority <- urgency + importance + difficulty
  
  # Create a new row
  new_rows <- data.frame(
    Priority = as.numeric(priority),
    Urgency = as.numeric(urgency),
    Importance = as.numeric(importance),
    Topic = as.character(topic),
    Activity = as.character(activity),
    Difficulty = as.numeric(difficulty),
    Due_date = as.Date(due_date),
    Recommended_date = as.Date(recommended_date),
    Estimated_time = as.numeric(estimated_time),
    Dependencies = as.logical(dependencies),
    Sub_category_1 = as.character(Sub_category_1),
    Sub_category_2 = as.character(Sub_category_2),
    Comment = as.character(comment),
    Last_updated = as.Date(Sys.Date()),
    stringsAsFactors = FALSE
  )
  
  # Define the expected template for `data`
  default_to_do_df <- data.frame(
    Priority = numeric(),
    Urgency = numeric(),
    Importance = numeric(),
    Topic = character(),
    Activity = character(),
    Difficulty = numeric(),
    Due_date = as.Date(character(), format = "%Y-%m-%d"),
    Recommended_date = as.Date(character(), format = "%Y-%m-%d"),
    Estimated_time = numeric(),
    Dependencies = logical(),
    Sub_category_1 = character(),
    Sub_category_2 = character(),
    Comment = character(),
    Last_updated = as.Date(numeric(), format = "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )
  
  # Handle cases where `data` is NULL, empty, or misinterpreted
  if (is.null(data) || nrow(data) == 0 || !all(names(data) %in% names(template))) {
    message("Input data is empty, NULL, or incorrectly formatted. Initializing with correct structure.")
    data <- default_to_do_df
  } else {
    # Ensure `data` conforms to the template's structure
    data <- data %>%
      dplyr::mutate(across(everything(), ~ replace_na(as(.x, class(template[[cur_column()]])), NA)))
  }
  
  # Check for duplicates using anti_join
  new_rows_filtered <- new_rows %>%
    dplyr::anti_join(data, by = c("Due_date", "Activity"))
  
  if (nrow(new_rows_filtered) == 0) {
    message("No new activities to add (all are duplicates).")
    return(data)
  }
  
  # Combine new activities with the existing data
  data <- dplyr::bind_rows(data, new_rows_filtered)
  
  # Sort the data by Due_date
  data <- data %>%
    dplyr::arrange(Due_date)
  
  message("New activities added.")
  return(data)
}



# Function to remove an activity from the data frame
remove_activity <- function(data, day = NA, activity = NA, topic = NA) {
  # Ensure day is a Date object if provided
  if (!is.na(day) && !inherits(day, "Date")) {
    day <- as.Date(day, format = "%Y-%m-%d")
  }
  
  # Filter out rows matching the provided parameters
  filtered_data <- data %>%
    filter(
      !( (is.na(day) | Day == day) &  # Match Day if provided
           (is.na(activity) | Activity == activity) &  # Match Activity if provided
           (is.na(topic) | Topic == topic) )  # Match Topic if provided
    )
  
  # Check if any rows were removed
  if (nrow(filtered_data) == nrow(data)) {
    message("No matching activities found to remove.")
  } else {
    message("Activities removed.")
  }
  
  return(filtered_data)
}

