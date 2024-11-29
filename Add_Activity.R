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
                         topic = c("AppDevelopment",
                                   "PhD Work",
                                   "Self-care",
                                   "Administratif",
                                   "PhD Work",
                                   "House",
                                   "House",
                                   "AppDevelopment",
                                   "House",
                                   "Self-care",
                                   "House"
                                   ),
                         activity = c("Finish defining and debugin all functions related to add_to_do ",
                           "reunion to get personalized pedagogy support",
                           "update diary/mood tracking",
                           "Emails",
                           "lab life: answer texts, get up to day with ongoing events",
                           "install temperature regulators in greenhouse",
                           "House maintenance: dishes, triage and remove big cardboard boxes",
                           "Integrate to do list to UI",
                           "Cook",
                           "Shower",
                           "Fold laundry"
                           ), 
                         difficulty = c(4, 5, 1, 2, 2, 2, 3, 3, 2, 1, 1),
                         Sub_category_1 = c(NA,"Cours",
                                            NA, NA, NA, "Lab life",
                                            NA,NA, NA, NA, NA
                                            )
                         )


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


