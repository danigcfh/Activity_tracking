library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)
library(writexl)
library(bslib)
library(shinyjs)


# Helper functions (Assume add_activity and add_mood are defined in Functions.R)
source("Functions.R")

# Define UI
ui <- navbarPage(
  title = "Integrated Activity Tracker",
  theme = bs_theme(version = 5, bootswatch = "flatly"),  # Optional theme
  # Introductory Tab for File Upload
  tabPanel("Upload Files",
           sidebarLayout(
             sidebarPanel(
               h4("Upload Required Files"),
               fileInput("activities_file", "Upload Activities File", 
                         accept = c(".xlsx")),
               fileInput("mood_file", "Upload Mood File", 
                         accept = c(".xlsx")),
               fileInput("Day_Analytics", "Upload Daily Summary File", 
                         accept = c(".xlsx")),
               actionButton("submit_files", "Submit Files"),
               div(id = "file_notice", style = "color: red; font-weight: bold;")  # Placeholder for custom notification
             ),
             mainPanel(
               h4("Instructions"),
               p("Please upload the required files to proceed. Ensure the files are in the correct format. If you have not created your files, select `Create files`"),
               actionButton("create_files", "Create and download template files"),
               uiOutput("show_download")  # Placeholder for download button
             )
           )
  ),
  
  # Add conditional access to other tabs
  tabPanel("Add Data", value = "add_data", uiOutput("add_data_ui")),
  tabPanel("Raw Data", value = "raw_data", uiOutput("raw_data_ui")),
  tabPanel("Summary", value = "summary", uiOutput("summary_ui"))
)


server <- function(input, output, session) {
  # Reactive values to track upload status and file paths
  upload_status <- reactiveValues(
    activities_uploaded = FALSE,
    mood_uploaded = FALSE,
    day_analytics_uploaded = FALSE,
    activities_path = NULL,
    mood_path = NULL,
    day_analytics_path = NULL
  )
  # Default DataFrames
  default_activities_df <- data.frame(
    Day = as.Date(character(), format = "%Y-%m-%d"),
    Topic = character(),
    Activity = character(),
    Difficulty = numeric(),
    Sub_Category_1 = character(),
    Sub_Category_2 = character(),
    Comment = character(),
    stringsAsFactors = FALSE
  )
  
  default_mood_df <- data.frame(
    Day = as.Date(character(), format = "%Y-%m-%d"),
    Sleep = numeric(),
    Anxiety = numeric(),
    Mood = numeric(),
    Health = numeric(),
    Exercise = numeric(),
    Exercise_Intensity = character(),
    Comment = character(),
    stringsAsFactors = FALSE
  )
  
  default_day_analytics_df <- data.frame(
    Day = as.Date(character(), format = "%Y-%m-%d"),
    Activities = numeric(),
    Base_difficulty = numeric(),
    Factor_difficulty = numeric(),
    Weighted_difficulty = numeric(),
    Anxiety = numeric(),
    Mood = numeric(),
    Health = numeric(),
    Sleep = numeric(),
    Exercise_weighted = numeric(),
    Main_topic = character(),
    Main_topic_Activities = numeric(),
    Aggregated_difficulty = numeric(),
    Comment = character(),
    stringsAsFactors = FALSE
  )
  
  # Reactive value to store file details
  file_info <- reactiveVal(list())
  
  
  # Download Handler
  output$download_templates <- downloadHandler(
    filename = function() {
      paste("template_files_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Temporary directory
      temp_dir <- tempdir()
      
      # Define paths for each file within the temporary directory
      activities_path <- file.path(temp_dir, "Activities.xlsx")
      mood_path <- file.path(temp_dir, "Mood.xlsx")
      day_analytics_path <- file.path(temp_dir, "Day_Analytics.xlsx")
      
      # Write the default data to temporary files
      write_xlsx(default_activities_df, activities_path)
      write_xlsx(default_mood_df, mood_path)
      write_xlsx(default_day_analytics_df, day_analytics_path)
      
      # Create the ZIP file without directory paths
      old_dir <- setwd(temp_dir)  # Change working directory to the temp directory
      on.exit(setwd(old_dir))    # Ensure we revert to the original directory after
      
      zip(file, c("Activities.xlsx", "Mood.xlsx", "Day_Analytics.xlsx"))  # Use file names only
    }
  )
  
  # Show download button after file creation
  observeEvent(input$create_files, {
    showNotification("Templates are ready for download!", type = "message")
    output$show_download <- renderUI({
      downloadButton("download_templates", "Download Template Files")
    })
  })
  
  # Monitor file uploads
  observeEvent(input$activities_file, {
    req(input$activities_file)
    upload_status$activities_uploaded <- TRUE
    upload_status$activities_path <- input$activities_file$datapath
    showNotification("Activities file uploaded successfully!", type = "message")
  })
  
  observeEvent(input$mood_file, {
    req(input$mood_file)
    upload_status$mood_uploaded <- TRUE
    upload_status$mood_path <- input$mood_file$datapath
    showNotification("Mood file uploaded successfully!", type = "message")
  })
  
  # Monitor file uploads
  observeEvent(input$Day_Analytics, {
    req(input$Day_Analytics)
    upload_status$day_analytics_uploaded <- TRUE
    upload_status$day_analytics_path <- input$Day_Analytics$datapath
    showNotification("Daily Summary file uploaded successfully!", type = "message")
  })
  
  # Capture file paths when files are uploaded
  observeEvent(input$submit_files, {
    # Read the uploaded files
    activities_file <- readxl::read_xlsx(input$activities_file$datapath)
    moods_file <- readxl::read_xlsx(input$mood_file$datapath)
    day_analytics_file <- readxl::read_xlsx(input$Day_Analytics$datapath)
    
    # Function to check if a data frame has the correct columns and types
    check_file_compatibility <- function(df, default_df) {
      # Check if the required columns are present
      required_columns <- colnames(default_df)
      file_columns <- colnames(df)
      
      # Check if all required columns are present
      missing_columns <- setdiff(required_columns, file_columns)
      extra_columns <- setdiff(file_columns, required_columns)
      
      # Check if the data types of columns match
      column_types_match <- all(sapply(df, class) == sapply(default_df, class))
      
      if (length(missing_columns) > 0) {
        return(paste("Missing columns: ", paste(missing_columns, collapse = ", ")))
      }
      
      if (length(extra_columns) > 0) {
        return(paste("Extra columns: ", paste(extra_columns, collapse = ", ")))
      }
      
      return(NULL)  # No issues
    }
    
    # Check compatibility for each file
    activities_check <- check_file_compatibility(activities_file, default_activities_df)
    moods_check <- check_file_compatibility(moods_file, default_mood_df)
    day_analytics_check <- check_file_compatibility(day_analytics_file, default_day_analytics_df)
    
    # If any file has issues, show an error message
    if (!is.null(activities_check)) {
      showNotification(paste("Activities file error: ", activities_check), type = "error")
    } else if (!is.null(moods_check)) {
      showNotification(paste("Moods file error: ", moods_check), type = "error")
    } else if (!is.null(day_analytics_check)) {
      showNotification(paste("Day Analytics file error: ", day_analytics_check), type = "error")
    } else {
      # Store file details (temporary paths and original names)
      file_info(list(
        activities = list(
          path = input$activities_file$datapath,
          name = input$activities_file$name
        ),
        moods = list(
          path = input$mood_file$datapath,
          name = input$mood_file$name
        ),
        day_analytics = list(
          path = input$Day_Analytics$datapath,
          name = input$Day_Analytics$name
        )
      )
      )
      
      showNotification("Files uploaded successfully! You can now continue to the other tabs", type = "message")
    }
  })
  
  # Helper function to enforce column classes
  enforce_column_classes <- function(data, reference_df) {
    for (col in colnames(reference_df)) {
      if (col %in% colnames(data)) {
        # Cast the column to the reference type
        data[[col]] <- as(data[[col]], class(reference_df[[col]]))
      } else {
        # Add missing columns with default values
        data[[col]] <- reference_df[[col]]
      }
    }
    # Remove extra columns not in the reference
    data <- data[colnames(reference_df)]
    return(data)
  }
  
  # Reactive data loading
  activities_data <- reactive({
    req(upload_status$activities_path)
    tryCatch({
      raw_data <- read_excel(upload_status$activities_path)
      validate(
        need("Day" %in% colnames(raw_data), "'Day' column is missing.")
      )
      raw_data <- raw_data %>% mutate(Day = as.Date(Day, format = "%Y-%m-%d"))
      enforce_column_classes(raw_data, default_activities_df)
    }, error = function(e) {
      showNotification(paste("Error reading Activities file:", e$message), type = "error")
      return(default_activities_df)
    })
  })
  
  # Define a reactiveVal for the editable activities
  activities <- reactiveVal()
  
  # Initialize the reactiveVal with the loaded data when the app starts
  observe({
    activities(activities_data())
  })
  
  moods_data <- reactive({
    req(upload_status$mood_path)
    tryCatch({
      raw_data <- read_excel(upload_status$mood_path)
      validate(
        need("Day" %in% colnames(raw_data), "'Day' column is missing.")
      )
      raw_data <- raw_data %>% mutate(Day = as.Date(Day, format = "%Y-%m-%d"))
      enforce_column_classes(raw_data, default_mood_df)
    }, error = function(e) {
      showNotification(paste("Error reading Mood file:", e$message), type = "error")
      return(default_mood_df)
    })
  })
  
  
  # Define a reactiveVal for the editable activities
  moods <- reactiveVal()
  
  # Initialize the reactiveVal with the loaded data when the app starts
  observe({
    moods(moods_data())
  })
  
  
  per_day_data <- reactive({
    req(upload_status$day_analytics_path)
    tryCatch({
      raw_data <- read_excel(upload_status$day_analytics_path)
      validate(
        need("Day" %in% colnames(raw_data), "'Day' column is missing.")
      )
      raw_data <- raw_data %>% mutate(Day = as.Date(Day, format = "%Y-%m-%d"))
      enforce_column_classes(raw_data, default_day_analytics_df)
    }, error = function(e) {
      showNotification(paste("Error reading Daily Analytics file:", e$message), type = "error")
      return(default_day_analytics_df)
    })
  })
  
  
  # Define a reactiveVal for the editable activities
  per_day <- reactiveVal()
  
  # Initialize the reactiveVal with the loaded data when the app starts
  observe({
    per_day(per_day_data())
  })
  
  # Global initialization for topics and sub-categories
  topics <- c("No predefined categories", "Other (Add new)")
  sub_cat1 <- c("No predefined categories", "Other (Add new)")
  sub_cat2 <- c("No predefined categories", "Other (Add new)")
  
  # UI for conditional tabs
  output$add_data_ui <- renderUI({
    if (!is.null(activities()) && nrow(activities()) > 0) {
      # Get unique topics and sub-categories from activities() if it is not empty
      topics <<- unique(activities()$Topic)
      sub_cat1 <<- unique(activities()$Sub_Category1)
      sub_cat2 <<- unique(activities()$Sub_Category2)
    }
    # Add the "Other (Add new)" option to topics and sub-categories
    topics <<- c(topics, "Other (Add new)", "No predefined categories")
    sub_cat1 <<- c(sub_cat1, "Other (Add new)", "No predefined categories")
    sub_cat2 <<- c(sub_cat2, "Other (Add new)", "No predefined categories")
    
    sidebarLayout(
      mainPanel(
        fluidRow(
          # Left Column: Add Activity
          column(6,
                 h4("Add Activity"),
                 dateInput("date", "Date:", value = Sys.Date()),
                 selectInput("topic", "Topic:", choices = topics),
                 textInput("activity", "Activity:"),
                 sliderInput("difficulty", "Difficulty:", min = 1, max = 5, value = 3),
                 selectInput("sub_cat1", "Sub-Category 1:", choices = sub_cat1, selected = "No predefined categories"),
                 selectInput("sub_cat2", "Sub-Category 2:", choices = sub_cat2, selected = "No predefined categories"),
                 textAreaInput("comment", "Comments:"),
                 actionButton("add_activity", "Add Activity")
          ),
          # Right Column: Add Mood
          column(6,
                 h4("Add daily evaluation"),
                 dateInput("mood_date", "Date:", value = Sys.Date()),
                 sliderInput("sleep", "Sleep:", min = -1, max = 5, value = 2),
                 sliderInput("anxiety", "Anxiety:", min = 1, max = 5, value = 2),
                 sliderInput("mood", "Mood:", min = -10, max = 10, value = 0),
                 sliderInput("health", "Health:", min = -5, max = 1, value = 0),
                 numericInput("exercise", "Exercise Duration (minutes):", value = 0),
                 selectInput("exercise_intensity", "Exercise Intensity:", c("Low", "High")),
                 textAreaInput("mood_comment", "Comments:"),
                 actionButton("add_mood", "Add Mood")
          )
        )
      ),
      sidebarPanel(
        h4("Add your activity or daily evaluation data using the forms."),
        actionButton("update_files", "Update Files")
      )
    )
  })
  
  # Observe event when the user selects "Other (Add new)" for Topic
  observeEvent(input$topic, {
    if (input$topic == "Other (Add new)") {
      showModal(modalDialog(
        title = "Add New Topic",
        textInput("new_topic", "Enter new topic:"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_new_topic", "Save")
        )
      ))
    } else if (input$topic == "No predefined categories") {
      # Automatically set Topic to NA when "No predefined categories" is selected
      updateSelectInput(session, "topic", selected = "")
    }
  })
  
  # Observe event when the user selects "Other (Add new)" for Sub-Category 1
  observeEvent(input$sub_cat1, {
    if (input$sub_cat1 == "Other (Add new)") {
      showModal(modalDialog(
        title = "Add New Sub-Category 1",
        textInput("new_sub_cat1", "Enter new sub-category 1:"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_new_sub_cat1", "Save")
        )
      ))
    } else if (input$sub_cat1 == "No predefined categories") {
      # Automatically set Sub-Category 1 to NA when "No predefined categories" is selected
      updateSelectInput(session, "sub_cat1", selected = "")
    }
  })
  
  # Observe event when the user selects "Other (Add new)" for Sub-Category 2
  observeEvent(input$sub_cat2, {
    if (input$sub_cat2 == "Other (Add new)") {
      showModal(modalDialog(
        title = "Add New Sub-Category 2",
        textInput("new_sub_cat2", "Enter new sub-category 2:"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_new_sub_cat2", "Save")
        )
      ))
    } else if (input$sub_cat2 == "No predefined categories") {
      # Automatically set Sub-Category 2 to NA when "No predefined categories" is selected
      updateSelectInput(session, "sub_cat2", selected = "")
    }
  })
  
  # Update activities with the new topic when saved
  observeEvent(input$save_new_topic, {
    new_topic <- input$new_topic
    if (new_topic != "") {
      # Add the new topic to the topics list globally
      topics <<- c(topics, new_topic, "Other (Add new)")  # Update topics with the new one
      # Update the dropdown menu with the new topic
      updateSelectInput(session, "topic", choices = topics, selected = new_topic)  # Set new topic as selected
      removeModal()  # Close the modal
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please enter a valid topic.",
        easyClose = TRUE
      ))
    }
  })
  
  # Update sub-category 1 with the new sub-category when saved
  observeEvent(input$save_new_sub_cat1, {
    new_sub_cat1 <- input$new_sub_cat1
    if (new_sub_cat1 != "") {
      # Add the new sub-category 1 to the sub_cat1 list globally
      sub_cat1 <<- c(sub_cat1, new_sub_cat1, "Other (Add new)")  # Update sub_cat1 with the new one
      # Update the dropdown menu with the new sub-category 1
      updateSelectInput(session, "sub_cat1", choices = sub_cat1, selected = new_sub_cat1)  # Set new sub-category 1 as selected
      removeModal()  # Close the modal
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please enter a valid sub-category 1.",
        easyClose = TRUE
      ))
    }
  })
  
  # Update sub-category 2 with the new sub-category when saved
  observeEvent(input$save_new_sub_cat2, {
    new_sub_cat2 <- input$new_sub_cat2
    if (new_sub_cat2 != "") {
      # Add the new sub-category 2 to the sub_cat2 list globally
      sub_cat2 <<- c(sub_cat2, new_sub_cat2, "Other (Add new)")  # Update sub_cat2 with the new one
      # Update the dropdown menu with the new sub-category 2
      updateSelectInput(session, "sub_cat2", choices = sub_cat2, selected = new_sub_cat2)  # Set new sub-category 2 as selected
      removeModal()  # Close the modal
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please enter a valid sub-category 2.",
        easyClose = TRUE
      ))
    }
  })
  
  output$raw_data_ui <- renderUI({
    if (!is.null(activities()) && !is.null(per_day())) {
      sidebarLayout(
        sidebarPanel(
          h4("View Recorded Data"),
          dateRangeInput("date_range_raw", "Filter by Date Range:",
                         start = Sys.Date() - 30, end = Sys.Date()),
          # Dynamically rendered dropdown inputs based on selected tab
          uiOutput("variables_ui_1"),
          actionButton("refresh", "Refresh Summary"),
          downloadButton("download_data", "Download Raw Data")
        ),
        mainPanel(
          tabsetPanel(
            id = "raw_data_tabs",
            tabPanel("Activities", DTOutput("activities_table")),
            tabPanel("Daily Summary", DTOutput("summary_table"))
          ),
          # Debug outputs
          verbatimTextOutput("debug_summary_data"),  # To show data structure or sample
          verbatimTextOutput("debug_structure"),
          verbatimTextOutput("date_range_start"),
          verbatimTextOutput("date_range_end")
          
        )
      )
    } else {
      h4("Please upload the required files to access this tab.")
    }
  })
  
  output$summary_ui <- renderUI({
    if (!is.null(activities()) && !is.null(per_day())) {
      sidebarLayout(
        sidebarPanel(
          dateRangeInput("date_range", "Filter by Date Range:",
                         start = Sys.Date() - 30, end = Sys.Date()),
          
          # Dynamically rendered checkbox input based on selected tab
          uiOutput("variables_ui_2"),
          
          actionButton("refresh", "Refresh Summary")
        ),
        mainPanel(
          tabsetPanel(id = "summary_tabset",  # Ensure the id is set for tabset Panel
                      tabPanel("Tracking Mood", plotOutput("summary_plot_mood")),
                      tabPanel("Tracking Activities", plotOutput("summary_plot_activities"))
          )
        )
      )
    } else {
      h4("Please upload the required files to access this tab.")
    }
  }
  )
  

  
  # Add activity
  observeEvent(input$add_activity, {
    new_data <- data.frame(
      Day = as.Date(input$date, format = "%Y-%m-%d"),
      Topic = input$topic,
      Activity = input$activity,
      Difficulty = as.numeric(input$difficulty),
      Sub_Category_1 = input$sub_cat1,
      Sub_Category_2 = input$sub_cat2,
      Comment = input$comment,
      stringsAsFactors = FALSE
    )
    activities(rbind(activities(), new_data))  # Add new data to the existing activities dataframe
    showNotification("Activity added successfully!", type = "message")
  })
  
  # Add mood
  observeEvent(input$add_mood, {
    new_data <- add_mood(
      data = moods(),
      day = as.Date(input$mood_date, format = "%Y-%m-%d"),
      sleep = input$sleep,
      anxiety = input$anxiety,
      mood = input$mood,
      health = input$health,
      exercise = input$exercise,
      exercise_intensity = input$exercise_intensity,
      comment = input$mood_comment
    )
    moods(new_data)
    showNotification("Daily evaluation added successfully!", type = "message")
  })
  
  # Final results processing for summary
  # Reactive processing logic for final results
  final_results <- reactive({
    Activities <- activities()
    Mood <- moods()
    Per_day <- per_day()
    
    # Ensure date coherence
    Activities$Day <- as.Date(Activities$Day, format = "%Y-%m-%d")
    Mood$Day <- as.Date(Mood$Day, format = "%Y-%m-%d")
    Per_day$Day <- as.Date(Per_day$Day, format = "%Y-%m-%d")
    
    # Avoid duplicates
    Activities <- distinct(Activities)
    Mood <- distinct(Mood)
    Per_day <- distinct(Per_day)
    
    # Ensure date is handled properly
    print(str(Activities$Day))
    print(str(Mood$Day))
    print(str(Per_day$Day))
    
    # Identify days with new or updated activities and moods
    activity_number <- Activities %>%
      group_by(Day) %>%
      summarise(Activities_count = n())
    
    affected_activity_days <- Activities %>%
      group_by(Day) %>%
      summarise(Activities_count = n()) %>%
      left_join(Per_day %>% select(Day, Activities), by = "Day") %>%
      filter(is.na(Activities) | Activities_count != Activities) %>%
      pull(Day)
    
    affected_mood_days <- Mood %>%
      left_join(Per_day, by = "Day", suffix = c("_Mood", "_PerDay")) %>%
      filter(
        is.na(Sleep_Mood) != is.na(Sleep_PerDay) | Sleep_Mood != Sleep_PerDay |
          is.na(Anxiety_Mood) != is.na(Anxiety_PerDay) | Anxiety_Mood != Anxiety_PerDay |
          is.na(Mood_Mood) != is.na(Mood_PerDay) | Mood_Mood != Mood_PerDay |
          is.na(Comment_Mood) != is.na(Comment_PerDay) | Comment_Mood != Comment_PerDay
      ) %>%
      pull(Day)
    
    # Combine affected days
    days <- unique(c(affected_activity_days, affected_mood_days))
    
    # Subset the data for affected days
    new_activity <- subset(Activities, Day %in% days)
    new_mood <- subset(Mood, Day %in% days)
    
    # Create an empty data frame for the results
    results <- data.frame(
      Day = as.Date(character(), format = "%Y-%m-%d"),
      Activities = integer(),
      Base_difficulty = numeric(),
      Factor_difficulty = numeric(),
      Weighted_difficulty = numeric(),
      Anxiety = numeric(),
      Mood = numeric(),
      Health = numeric(),
      Sleep = numeric(),
      Exercise_weighted = numeric(),
      Main_topic = character(),
      Main_topic_Activities = numeric(),
      Aggregated_difficulty = numeric(),
      Comment = character(),
      stringsAsFactors = FALSE
    )
    
    # Iterate over each affected day and calculate the metrics
    for (day in days) {
      activity_day <- subset(new_activity, Day == day)
      mood_day <- subset(new_mood, Day == day)
      
      # Check if mood_day is empty and assign NA if true
      if (nrow(mood_day) == 0) {
        Anxiety <- NA
        Mood <- NA
        Health <- NA
        Sleep <- NA
        Comment <- NA
        Exercise <- NA
      } else {
        # Extract and calculate metrics for non-empty mood_day
        Anxiety <- mood_day$Anxiety
        Mood <- mood_day$Mood
        Health <- mood_day$Health
        Sleep <- mood_day$Sleep
        Comment <- mood_day$Comment
      }
      
      # Extract and calculate activity-related metrics
      Difficulties <- activity_day$Difficulty
      numActivities <- length(activity_day$Activity[!is.na(activity_day$Difficulty)])
      
      # Calculate weighted difficulty (assume weighted_sum is defined elsewhere)
      Difficulty_calcs <- weighted_sum(Difficulties, Sleep, Mood, Health)
      
      Base_difficulty <- Difficulty_calcs[1]
      Factor_difficulty <- Difficulty_calcs[2]
      Weighted_difficulty <- Difficulty_calcs[3]
      
      Exercise <- ifelse(!nrow(mood_day) == 0, ifelse(mood_day$Exercise_Intensity == "High", mood_day$Exercise*2, mood_day$Exercise), NA)
      
      # Identify the main topic
      topic_summary <- activity_day %>%
        group_by(Topic) %>%
        summarise(
          Topic_count = n(),
          Total_difficulty = sum(Difficulty, na.rm = TRUE)
        ) %>%
        arrange(desc(Total_difficulty))
      
      Main_topic <- ifelse(nrow(topic_summary) > 0, topic_summary$Topic[1], NA)
      Aggregated_difficulty <- ifelse(nrow(topic_summary) > 0, topic_summary$Total_difficulty[1], NA)
      Main_topic_Activities <- ifelse(nrow(topic_summary) > 0, topic_summary$Topic_count[1], NA)
      
      # Append the results
      row <- data.frame(
        Day = as.Date(day, format = "%Y-%m-%d"),
        Activities = numActivities,
        Base_difficulty = Base_difficulty,
        Factor_difficulty = Factor_difficulty,
        Weighted_difficulty = Weighted_difficulty,
        Anxiety = Anxiety,
        Mood = Mood,
        Health = Health,
        Sleep = Sleep,
        Exercise_weighted = Exercise,
        Main_topic = Main_topic,
        Main_topic_Activities = Main_topic_Activities,
        Aggregated_difficulty = Aggregated_difficulty,
        Comment = Comment,
        stringsAsFactors = FALSE
      )
      
      results <- bind_rows(results, row)
    }
    
    # Return the results
    return(results)
  })
  
  observeEvent(input$update_files, {
    # Show modal dialog with options to update files
    showModal(modalDialog(
      title = "Choose Files to Update",
      easyClose = TRUE,
      footer = NULL,  # Remove footer (optional)
      tagList(
        actionButton("update_activities", "Update Activities File"),
        actionButton("update_moods", "Update Mood File"),
        actionButton("update_day_analytics", "Update Day Analytics File")
      )
    ))
  })
  
  observeEvent(input$update_activities, {
    # Choose file for activities
    activities_path <- file.choose()
    if (activities_path != "") {
      write_xlsx(activities(), activities_path)
      showNotification("Activities file updated successfully!", type = "message")
      removeModal()  # Close the modal after selecting the file
    } else {
      showNotification("No file selected for Activities! Please choose a file.", type = "error")
    }
  })
  
  observeEvent(input$update_moods, {
    # Choose file for moods
    moods_path <- file.choose()
    if (moods_path != "") {
      write_xlsx(moods(), moods_path)
      showNotification("Mood file updated successfully!", type = "message")
      removeModal()  # Close the modal after selecting the file
    } else {
      showNotification("No file selected for Mood! Please choose a file.", type = "error")
    }
  })
  
  observeEvent(input$update_day_analytics, {
    # Choose file for day analytics
    day_analytics_path <- file.choose()
    if (day_analytics_path != "") {
      write_xlsx(final_results(), day_analytics_path)
      showNotification("Day Analytics file updated successfully!", type = "message")
      removeModal()  # Close the modal after selecting the file
    } else {
      showNotification("No file selected for Day Analytics! Please choose a file.", type = "error")
    }
  })
  
  # Inspect the data returned by final_results()
  output$debug_summary_data <- renderPrint({
    head(final_results())  # Print the first few rows of the data
  })
  output$debug_structure <- renderPrint({
    str(final_results())  # Shows structure of the dataset
  })
  output$date_range_start <- renderPrint({
    str(as.Date(input$date_range_raw[1], format = "%Y-%m-%d"))  # Shows structure of the dataset
  })
  output$date_range_end <- renderPrint({
    str(as.Date(input$date_range_raw[2], format = "%Y-%m-%d"))  # Shows structure of the dataset
  })
  

  # Define a list of custom ranges for each variable
  custom_ranges <- list(
    "Difficulty" = c(1, 5), 
    "Anxiety" = c(1, 5),  
    "Mood" = c(-5, 5),         
    "Health" = c(-10, 10),       
    "Sleep" = c(-1, 5),
    "Activities" = c(1, 20)  # Default range (but this will be overridden in Daily Summary)
  )
  
  # Render dynamic UI for variable selection (filtering)
  output$variables_ui_1 <- renderUI({
    active_tab <- input$raw_data_tabs
    columns_for_activities <- c("Topic", "Difficulty", "Sub_category_1", "Sub_category_2")
    columns_for_summary <- c("Activities", "Anxiety", "Mood", "Health", "Sleep", "Main_topic")
    
    if (active_tab == "Activities") {
      activities_data <- activities()
      dropdown_ui <- lapply(columns_for_activities, function(colname) {
        if (colname %in% names(activities_data)) {
          if (is.numeric(activities_data[[colname]])) {
            # Use custom range for sliders for non-Activities columns
            range_values <- custom_ranges[[colname]]
            
            sliderInput(paste0(colname, "_filter"), 
                        label = paste("Select", colname), 
                        min = range_values[1],
                        max = range_values[2],
                        value = c(range_values[1], range_values[2]),
                        step = 1)
          } else {
            selectInput(paste0(colname, "_filter"), 
                        label = paste("Select", colname), 
                        choices = c("All", unique(na.omit(activities_data[[colname]]))),  # Add "All" as an option
                        selected = "All")  # Default is "All"
          }
        }
      })
      
      # Return the UI elements for dynamic filtering
      return(do.call(tagList, dropdown_ui))
    } else if (active_tab == "Daily Summary") {
      summary_data <- final_results()
      
      dropdown_ui <- lapply(columns_for_summary, function(colname) {
        if (colname %in% names(summary_data)) {
          if (is.numeric(summary_data[[colname]])) {
            # Check if the column is "Activities" and use dynamic min/max
            if (colname == "Activities") {
              range_values <- c(min(summary_data[[colname]], na.rm = TRUE), 
                                max(summary_data[[colname]], na.rm = TRUE))
            } else {
              # Otherwise, use the predefined custom range
              range_values <- custom_ranges[[colname]]
            }
            
            sliderInput(paste0(colname, "_filter"), 
                        label = paste("Select", colname), 
                        min = range_values[1],
                        max = range_values[2],
                        value = c(range_values[1], range_values[2]),
                        step = 1)
          } else {
            selectInput(paste0(colname, "_filter"), 
                        label = paste("Select", colname), 
                        choices = c("All", unique(na.omit(summary_data[[colname]]))),  # Add "All" as an option
                        selected = "All")  # Default is "All"
          }
        }
      })
      
      return(do.call(tagList, dropdown_ui))
    }
  })
  
  
  
  #Make sure the data shown is restricted to filters
  filtered_activities <- reactive({
    activities_data <- activities()
    # Convert input to Date explicitly
    date_range_start <- as.Date(input$date_range_raw[1], format = "%Y-%m-%d")
    date_range_end <- as.Date(input$date_range_raw[2], format = "%Y-%m-%d")
    
    activities_data <- filter(activities_data, Day >= date_range_start, Day <= date_range_end) 
    # Apply filters dynamically based on user input
    for (colname in names(activities_data)) {
      filter_col <- paste0(colname, "_filter")
      
      if (!is.null(input[[filter_col]]) && length(input[[filter_col]]) > 0) {
        if ("All" %in% input[[filter_col]]) {
          # Skip filtering for this column if "All" is selected
          next
        }
        
        if (is.numeric(activities_data[[colname]])) {
          # For numeric columns, filter by a range
          activities_data <- activities_data %>%
            filter(activities_data[[colname]] >= min(input[[filter_col]]) &
                     activities_data[[colname]] <= max(input[[filter_col]]))
        } else {
          # For categorical columns, filter by selected values
          activities_data <- activities_data %>%
            filter(activities_data[[colname]] %in% input[[filter_col]])
        }
      }
    }
    
    activities_data  # Return the filtered data (or unfiltered if no filter is applied)
  })
  

  # Render filtered activities table
  output$activities_table <- renderDT({
    filtered_activities()  # Use the filtered data
  })

  filtered_summary_table <- reactive({
    summary_data <- final_results()  # Assuming final_results() gives the summary data
    
    # Convert date range inputs to Date format
    date_range_start <- as.Date(input$date_range_raw[1], format = "%Y-%m-%d")
    date_range_end <- as.Date(input$date_range_raw[2], format = "%Y-%m-%d")
    
    # Apply the date filter but keep rows with missing 'Day' values (NA)
    summary_data <- summary_data %>%
      filter((is.na(Day) | (Day >= date_range_start & Day <= date_range_end))) 
    
    # Apply filters dynamically based on user input
    for (colname in names(summary_data)) {
      filter_col <- paste0(colname, "_filter")
      
      if (!is.null(input[[filter_col]]) && length(input[[filter_col]]) > 0) {
        if ("All" %in% input[[filter_col]]) {
          next  # Skip filtering if "All" is selected
        }
        
        # Handle numeric columns
        if (is.numeric(summary_data[[colname]])) {
          summary_data <- summary_data %>%
            filter((is.na(summary_data[[colname]]) | 
                      (summary_data[[colname]] >= min(input[[filter_col]]) &
                         summary_data[[colname]] <= max(input[[filter_col]]))))
        } else {
          # Handle categorical columns
          summary_data <- summary_data %>%
            filter((is.na(summary_data[[colname]]) | 
                      summary_data[[colname]] %in% input[[filter_col]]))
        }
      }
    }
    
    # Return the filtered data (with NA values retained)
    summary_data
  })
  
  
  # Render filtered summary table
  output$summary_table <- renderDT({
    filtered_summary_table()  # Use the filtered data
  })

    # Dynamically render the checkboxGroupInput based on the active tab
  output$variables_ui_2 <- renderUI({
      # Get the active tab
      active_tab <- input$summary_tabset
      # Define the available variables for each tab

      if (active_tab == "Tracking Mood") {
      checkboxGroupInput("variables", "Select Variables to Plot:",
                         choices = c("Mood" = "Mood", 
                                     "Anxiety" = "Anxiety", 
                                     "Sleep" = "Sleep",
                                     "Health" = "Health"),
                         selected = c("Mood", "Sleep"))
    } else if (active_tab == "Tracking Activities") {
      checkboxGroupInput("variables", "Select Variables to Plot:",
                         choices = c("Activities" = "Activities", 
                                     "Base Difficulty" = "Base_difficulty", 
                                     "Weighted Difficulty" = "Weighted_difficulty",
                                     "Exercise (in minutes)" = "Exercise_weighted"),
                         selected = c("Base_difficulty", "Weighted_difficulty"))
    }
  })
  
  output$summary_plot_mood <- renderPlot({
    data <- final_results()
    
    # Filter the data based on the selected date range
    data_filtered <- data %>%
      filter(Day >= input$date_range[1], Day <= input$date_range[2]) %>%
      select(Day, Mood, Anxiety, Sleep, Health)
    
    # Pivot data to long format for ggplot
    data_long <- data_filtered %>%
      pivot_longer(cols = all_of(input$variables), names_to = "Variable", values_to = "Value")
    
    # Create the plot with multiple variables
    ggplot(data_long, aes(x = Day, y = Value, color = Variable)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(title = "Tracked Variables Over Time", x = "Date", y = "Value") +
      theme_minimal() +
      scale_color_brewer(palette = "Set2") +
      scale_y_continuous()
  })
  
  output$summary_plot_activities <- renderPlot({
    data <- final_results()
    
    # Filter the data based on the selected date range
    data_filtered <- data %>%
      filter(Day >= input$date_range[1], Day <= input$date_range[2])
    
    # Dynamically select columns based on user-selected variables
    selected_columns <- c("Day", input$variables)
    
    # Ensure that the selected variables are present in the dataset
    data_filtered <- data_filtered %>%
      select(all_of(selected_columns))
    
    # Pivot data to long format for ggplot
    data_long <- data_filtered %>%
      pivot_longer(cols = -Day, names_to = "Variable", values_to = "Value")
    
    # Create the plot with multiple variables
    ggplot(data_long, aes(x = Day, y = Value, color = Variable)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(title = "Tracked Variables Over Time", x = "Date", y = "Value") +
      theme_minimal() +
      scale_color_brewer(palette = "Set2") +
      scale_y_continuous()
  })
  
  # Download handler for downloading all files as a ZIP
  output$download_data <- downloadHandler(
    filename = function() {
      paste("all_data_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      temp_dir <- tempdir()  # Create a temporary directory
      
      # Create .xlsx files for each data frame
      write_xlsx(activities(), file.path(temp_dir, "activities_file.xlsx"))
      write_xlsx(moods(), file.path(temp_dir, "mood_file.xlsx"))
      write_xlsx(per_day(), file.path(temp_dir, "Day_Analytics.xlsx"))
      
      # Zip the files into a single file
      zip(file, c(
        file.path(temp_dir, "activities_file.xlsx"),
        file.path(temp_dir, "mood_file.xlsx"),
        file.path(temp_dir, "Day_Analytics.xlsx")
      ))
    }
  )

}
# Run the app
shinyApp(ui = ui, server = server)
