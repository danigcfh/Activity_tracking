library(tidyverse)
library(readxl)
library(writexl)

#Load data
Activities <- read_excel("Activities.xlsx")

#current topics: "Self-care"     "House"         "Administratif" "Thèse"        
# "Cours"         "Others"        "FEI"         "AppDevelopment"     

new_data <- add_activity(data = Activities,
                         topic = c("Administratif",
                                   "House",
                                   "House",
                                   "House",
                                   "AppDevelopment",
                                   "AppDevelopment",
                                   "AppDevelopment",
                                   "AppDevelopment",
                                   "AppDevelopment",
                                   "AppDevelopment"),
                         activity = c("Appointments and train reservation",
                           "Groceries",
                           "Plant upkeep",
                           "Cook",
                           "Conditional access to other tabs and Create initial tab ",
                           "Create reactive buttons for both downloading blank templates and uploading relevant files",
                           "Debug uploaded files: ensure column and type consistency accross files ",
                           "create reactive Uis and personalize data input options",
                           "Ensure date consistency acroos files ",
                           "create reactive button for updating files or downloading raw data"
                           ), 
                         difficulty = c(1,3,2,2,3,4,4,4,2,3),
                         Sub_category_1 = c(NA,NA,NA, NA, "Personnal project",
                                            "Personnal project",
                                            "Personnal project","Personnal project",
                                            "Personnal project","Personnal project"
                                            )
                         )


new_data$Sub_Category_1[new_data$Sub_Category_1=="Personnal project"] <- NA

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


