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
                                   "Others",
                                   "",
                                   "",
                                   "",
                                   "",
                                   "",
                                   "",
                                   "" ),
                         activity = c("Dishes",
                           "Out shopping with Lea",
                           "upgrade tracking algo",
                           "",
                           "",
                           "",
                           "",
                           "",
                           "",
                           ""), 
                         difficulty = c(2,4,4,,,,,,,),
                         Sub_category_1 = c(NA,NA,"Personnal project",NA,NA,NA, NA,NA,NA,NA)
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


