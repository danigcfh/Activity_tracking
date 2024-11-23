library(tidyverse)
library(readxl)
library(writexl)

#Load data
Activities <- read_excel("Activities.xlsx")

#current topics: "Self-care"     "House"         "Administratif" "Thèse"        
# "Cours"         "Others"        "FEI"              

new_data <- add_activity(Activities,
                         topic = c("House",
                                   "Others",
                                   "Administratif",
                                   "Others",
                                   "Others",
                                   "Others",
                                   "Others"),
                         activity = c("Dishes",
                           "Out shopping with Lea",
                           "Cancel/manage subscriptions and mail",
                           "Develop initial Interface ",
                           "Develop initial Server",
                           "Add graphs and interactive summaries",
                           "app Mise en page in different pages with custom info"
                           ), 
                         difficulty = c(2,4,1,5,5,5,4),
                         Sub_category_1 = c(NA,NA,NA,"Personnal project",
                                            "Personnal project",
                                            "Personnal project","Personnal project"
                                            )
                         )

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


