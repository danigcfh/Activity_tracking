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
                                   "AppDevelopment",
                                   "PhD Work",
                                   "PhD Work",
                                   "PhD Work",
                                   "PhD Work",
                                   "Others",
                                   "House",
                                   "Administratif"
                                   ),
                         activity = c("Train to Paris",
                           "Budgeting for the month",
                           "Pass to do list to r",
                           "Lecture Guerre Culturelle, Guerre de Mots et recherche bibliographique associée",
                           "Vie labo: échanges avec les autres doctorants",
                           "Give class panorama SIC",
                           "Update class files",
                           "Train back",
                           "House chores: dishes, mail",
                           "email"
                           ), 
                         difficulty = c(2,1,3,2,1,5,1,2,2,2),
                         Sub_category_1 = c(NA,NA,
                                            NA,"Thèse","Thèse", "Cours",  "Cours",
                                            NA,NA, NA
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


