This work © 2024 by Daniela González is licensed under CC BY-NC-SA 4.0 

## Activity Tracking Scripts

### Description
This repository contains three R scripts designed for tracking daily activities and analytics, including difficulty, mood, sleep, and exercise, to help analyze personal productivity and well-being.
It's a personal initiative to track my daily activities, mood and how these things interact.

### Support this project
You can support this project so it remains free and accessible.
Paypal: paypal.me/DaniGCFH

### Files
1. **`Create_Files.R`**  
   Creates two Excel files:
   - `Activities.xlsx`: Tracks individual activities and their attributes.
   - `Day_Analytics.xlsx`: Summarizes daily metrics.
   
2. **`Add_Activity.R`**  
   Updates `Activities.xlsx` by appending new rows for daily activities.

3. **`Daily_Analytics.R`**  
   Processes the data in `Activities.xlsx` and `Mood.xlsx`to calculate daily metrics, including:
   - Number of activities
   - Weighted difficulty based on activity, mood, and sleep. With the current parameters, the ideal range for daily weighted difficulty would be a score of around 20-30 to ensure a manageable balance between productivity and well-being.
   - Summary of anxiety, mood, and exercise metrics.

4. **`Topic_Analytics.R`**  
  Processes the data in `Activities.xlsx` to provide a summary of activities done per day and topic, including:
  - Number of activities per day and in aggregate
  - Aggregated difficulty (not considering weights)

### Usage
1. **Setup**
   - Run `Create_Files.R` first to initialize the Excel files.
   - Customize column values based on your needs.
   - Warning: this script should only run once, as the scripts created this way will be then used and updated with the following scripts

2. **Adding Activities**
  - Use `Add_Activity.R` to input daily activities. For this purpose, the function add_activity is created. It's possible to add multiple activities by using c()
     ```R
add_activity <- function(data, day = Sys.Date(), topic, activity, difficulty, Sub_category_1 = NA, Sub_category_2 = NA, comment = NA) 
     ```
  - Code was updated to be able to retroactively add activities to previous days

3. **Adding Moods**
  - Use `Add_Moody.R` to input daily mood metrics and exercise. Only one entry per day is permitted. For this purpose, the function add_mood is created
     ```R
add_mood <- function(data, sleep, anxiety, mood, exercise, exercise_intensity = 1, comment = NA,   day = Sys.Date()) {
     ```
  - Code was updated to be able to retroactively add activities to previous days

4. **Daily Analytics**
   - Run `Daily_Analytics.R` to calculate daily summaries and export the updated `Day_Analytics.xlsx`.
   - The weighted metric takes into account mood, sleep, number of activities and their difficulties from both mood and activities xlsx

4. **Topic Analytics**
   - Run `Topic_Analytics.R` to calculate how many activities per topic per day are performed
   - The aggregated difficulty as well as any details and comments added to the activities will be summarized in the output

This is an ongoing project. It will probably be updated for weekly and monthly summaries in the future. 


### License and Usage Guidelines
This repository is licensed under CC BY-NC-SA 4.0, which ensures that the code is free to use, share, and modify under the following terms:
1. **What You Can Do**
   - Use for Non-Commercial Purposes: You may freely use this code for research, education, or personal projects that do not involve commercial activity.
   - Modify and Adapt: You are encouraged to modify and adapt the code to suit your own purposes.
   - Share Your Modifications: If you modify or build upon this code, you must share your contributions under the same license (CC BY-NC-SA 4.0).
   - Credit the Author: When using or sharing this code, please provide proper attribution, including:
      - My name as the original author.
      - A link to this repository.
      - An  indication of any changes you made.
2. **What You Cannot Do**
   - Commercial Use: You may not use this code, or any derivative works, for commercial purposes without explicit permission.
      - This includes selling, licensing, or otherwise monetizing the code or its derivatives.
      - Re-License Under Incompatible Terms: Derivative works must also remain open and licensed under CC BY-NC-SA 4.0.

### My Philosophy
My work reflects the principles of open science and public accessibility. I believe that knowledge and science should remain open and free, avoiding commercialization that could restrict access or benefit only private actors. By sharing this work under these terms, I aim to support collaborative research and equitable access to scientific tools.





