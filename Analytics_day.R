library(tidyverse)
library(readxl)
library(writexl)

# Load data
Activities <- read_excel("Activities.xlsx")
Per_day <- read_excel("Day_Analytics.xlsx")

# Ensure date coherence
Activities$Day <- as.Date(Activities$Day)
Per_day$Day <- as.Date(Per_day$Day)

# Exclude old data
new_data <- Activities %>%
  filter(!Day %in% Per_day$Day)

# Initialize a weighted sum function
weighted_sum <- function(difficulties) {
  weights <- 1 + 0.1 * (seq_along(difficulties) - 1)  # Increasing weights
  sum(difficulties * weights)
}

# Create analytics
results <- new_data %>%
  group_by(Day) %>%
  summarize(
    Activities = n(),
    Weighted_difficulty = weighted_sum(Difficulty),
    Anxiety_levels = "",  # Placeholder for future input
    Comments = ""         # Placeholder for future input
  )

# Combine with existing analytics
combined_results <- bind_rows(Per_day, results)

# Save results to an Excel file
write_xlsx(combined_results, "Day_Analytics.xlsx")
