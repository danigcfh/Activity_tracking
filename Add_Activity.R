library(tidyverse)
library(readxl)
library(writexl)
#This work © 2024 by Daniela González is licensed under CC BY-NC-SA 4.0


#Load data
Activities <- read_excel("Activities.xlsx")
View(Activities)

#current topics: "Self-care"      "House"          "Administratif" 
# "PhD Work"       "Others"         "FEI"         "AppDevelopment"    

#Current sub-categories:  "Thèse" "Cours"

new_data <- add_activity(data = Activities,
                         day = Sys.Date()-1,
                         topic = c("Others",
                                   "Administratif",
                                   "PhD Work",
                                   "Administratif",
                                   "PhD Work",
                                   "PhD Work",
                                   "AppDevelopment",
                                   "AppDevelopment",
                                   "Self-care",
                                   "Others",
                                   "AppDevelopment"
                                   ),
                         activity = c("Train to Paris",
                           "Make doctor's appointments, confirm optique schedule",
                           "Read 1 article guerre culturelle, guerre de mots",
                           "Schedule trains next week",
                           "Séminaire CEMTI",
                           "Seminaire classe inversée: pedagogic tips",
                           "personal project: Debug delete buttons",
                           "personal project: Integration of to_do to sever function",
                           "Therapy",
                           "Train back home",
                           "update readme and create shiny.io account"
                           ), 
                         difficulty = c(2, 2, 3, 1, 4, 5, 3, 4, 4, 2, 2),
                         Sub_category_1 = c(NA,NA,
                                            "Thèse", NA, "Thèse", "Cours",
                                            NA,NA, NA, NA, NA
                                            ),
                         Sub_category_2 = c(NA,NA,
                                            "Bibliography", NA, "Seminar", "Seminar",
                                            NA,NA, NA, NA, NA
                         )
                         )

difficulties <- c(2, 2, 3, 1, 4, 5, 3, 4, 4, 2, 2)


View(new_data)
#Difficulty scale
# 5: Tasks that are mentally or physically exhausting, requiring extreme concentration, effort, or time
# 4: Challenging: demand significant concentration, time, or physical energy
# 3: Moderate: require a balanced amount of focus, mental effort, or time
# 2: light effort: straightforward but may demand some cognitive or physical investment
# 1: Minimal effort required


# Export the dataframe to an Excel file
write_xlsx(new_data, "Activities.xlsx")

# Confirm the file is saved
print("File 'Activities.xlsx' saved in the working directory.")


