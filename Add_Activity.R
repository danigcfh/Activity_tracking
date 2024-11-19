library(tidyverse)

# Install required packages if not already installed
if (!require("readxl")) install.packages("readxl")
if (!require("writexl")) install.packages("writexl")

# Create the dataframe
data <- data.frame(
  Jour = c(12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14),
  Thème = c("House", "House", "House", "Self-care", "Administratif", "Thèse", "Thèse", "Thèse", "Cours", 
            "Administratif", "Administratif", "Thèse", "Thèse", "Thèse", "Cours", "Others", "Others", 
            "Thèse", "Thèse", "Thèse", "Thèse", "Thèse"),
  Activité = c(
    "Take trash out", "Go get mail and set up heater", "Water plants",
    "Cook and put dishes in the dishwasher", "Mail: reading and answering",
    "Send article proposal", "Integrate reviews in article proposal", 
    "read 1 chapter social media book", "Grade 2 reading assignment", 
    "Emails: reading and answering", "Confirm conference attendance",
    "Meeting with new PhD candidate", 
    "Streamline LDA visualization and start coding for emotion analysis", 
    "Attend seminar: Content Moderation in a Historic Election Year", 
    "Grade 1 reading assignment", "Take train to Paris", "Train back to lille", 
    "Congreso alienación parental", 
    "LDA models: emotion analysis and distribution + topic distribution", 
    "Visualization of all 3 matrices", "Streamline code and export relevant data", 
    "ran Hclust"
  ),
  Difficulté = c(1, 2, 1, 2, 2, 2, 4, 3, 3, 2, 1, 3, 5, 3, 3, 2, 2, 4, 5, 4, 5, 4),
  Sous_catégorie_1 = c("", "", "", "", "", "Publication", "Publication", "Bibliography", "", "", "", "Lab Life", 
                       "Quanti", "Seminar", "", "", "", "Terrain", "Quanti", "Quanti", "Quanti", "Quanti"),
  Sous_catégorie_2 = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "")
)

# Export the dataframe to an Excel file
write_xlsx(data, "Activities.xlsx")

# Confirm the file is saved
print("File 'Activities.xlsx' saved in the working directory.")


