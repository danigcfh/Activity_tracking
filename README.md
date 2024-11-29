This work © 2024 by Daniela González is licensed under CC BY-NC-SA 4.0 

## Activity Tracking Scripts

### Description
This repository contains three R scripts designed for tracking daily activities and analytics, including difficulty, mood, sleep, and exercise, to help analyze personal productivity and well-being.
It's a personal initiative to track my daily activities, mood and how these things interact.

### Support this project
You can support this project so it remains free and accessible.
Paypal: paypal.me/DaniGCFH


### app.r : Integrated Activity Tracker
  - Shiny app script for an application that summarizes everything that has been developped in this repository as a single interactive and intuitive app.


1. **Initial tab: Upload Files**

  - To use the app, create and download the template excels in the first tab. Then upload them to their relevant fields in order to manage the data. 
  - No data is kept in the app: it allows the calculation, management and visualization of data from the uploaded excel files.
  - *Important: do not change columns names or order in the excel files, these are needed for the app to run*
  - Once the relevant files have been uploaded, the rest of the app will be accessible

2. **Manage Data**
  - Allows to add or delete any activity or daily evaluation from the relevant dataframes. Direct editing of existing data is not supported, if an error is made, it's possible to delete the erroneous data and then add a corrected version. 
  - Allows to upload the new data to the original excels if desired, or to save them into a new excel. 

3. **Raw Data**
  - Displays in table form the tables of all 3 files (Activity tracking, daily evaluation and to do list)
  - Allows filtering the data by date and within the parameters of each table.
  - Allows access to automatically calculated values such as priority and recommended date for to do list. 

4. **Summary**
  - Displays graphic visualization to track different metrics regarding activity and mood (only for past activities stored in activities or daily analytics)
  - Allows filtering the data by date and dynamically shows the parameters selected

### Other Files
These files worked as the base to inspire the app, a testing ground of sorts. 
I suggest to use these scripts as a base to personalize the kind of data and functions. 

1. **`Create_Files.R`**  
   Creates two Excel files:
   - `Activities.xlsx`: Tracks individual activities and their attributes.
   - `Day_Analytics.xlsx`: Summarizes daily metrics.
   - `To_do.xlsx`: makes a to do list with activities and their attributes

   
2. **`Functions.R`**  
   Defines functions for
   - Adding activity and mood to their respective excel files
   - Calculating difficulty levels in aggregate, a modulating factor from sleep, mood and health scores, and the overall weighted difficulty
   - Calculations for priority and recommended date for tasks in to do file
   - Adding tasks to to do file and filling missing values with calculated ones
   
3. **`Add_Activity.R, Add_Mood.R, Add_to_do.R`**  
   Updates relevant excel file by adding new information using the functions described previously 

4. **`Daily_Analytics.R`**  
   Processes the data in `Activities.xlsx` and `Mood.xlsx`to calculate daily metrics, including:
   - Number of activities
   - Weighted difficulty based on activity, mood, and sleep. With the current parameters, the ideal range for daily weighted difficulty would be a score of around 20-30 to ensure a manageable balance between productivity and well-being.
   - Summary of anxiety, mood, and exercise metrics.

5. **`Topic_Analytics.R`**  
  Processes the data in `Activities.xlsx` to provide a summary of activities done per day and topic, including:
  - Number of activities per day and in aggregate
  - Aggregated difficulty (not considering weights)

### Usage
1. **Setup**
   - Run `Create_Files.R` first to initialize the Excel files.
   - Warning: this script should only run once, as the scripts created this way will be then used and updated with the following scripts

2. **Initialize functions**
  - Use `Functions.R` to initialize all the functions needed for the next scripts 

3. **Adding Activities**
  - Use `Add_Activity.R` to input daily activities. For this purpose, the function add_activity is created. It's possible to add multiple activities by using c()
     ```R
     add_activity (data, day = Sys.Date(), topic, activity, difficulty, Sub_category_1 = NA, Sub_category_2 = NA, comment = NA)
     
  - Code was updated to be able to retroactively add activities to previous days
```

4. **Adding Moods**
  - Use `Add_Moody.R` to input daily mood metrics and exercise. Only one entry per day is permitted. For this purpose, the function add_mood is created
     ```R
     add_mood (data, sleep, anxiety, mood, exercise, exercise_intensity = 1, comment = NA,   day = Sys.Date()) {

  - Code was updated to be able to retroactively add activities to previous days
```

4. **Daily Analytics**
   - Fill and run `Daily_Analytics.R` to calculate daily summaries and export the updated `Day_Analytics.xlsx`.
   - The weighted metric takes into account mood, sleep, number of activities and their difficulties from both mood and activities xlsx

5. **Topic Analytics**
   - Fill and run `Topic_Analytics.R` to calculate how many activities per topic per day are performed
   - The aggregated difficulty as well as any details and comments added to the activities will be summarized in the output

6. **To do list**
   - Run  `Add_to_do.R` to add new activity to the to do list.

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





