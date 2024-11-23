library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)
library(writexl)
library(bslib)

# Helper functions (Assume add_activity and add_mood are defined in Functions.R)
source("Functions.R")

# Define UI
ui <- navbarPage(
  title = "Integrated Activity Tracker",
  theme = bs_theme(version = 5, bootswatch = "flatly"),  # Optional theme

  # Page 1: Add Mood or Activity
  tabPanel("Add Data",
           sidebarLayout(
             mainPanel(
               fluidRow(
                 # Left Column: Add Activity
                 column(6,
                        h4("Add Activity"),
                        dateInput("date", "Date:", value = Sys.Date()),
                        textInput("topic", "Topic:"),
                        textInput("activity", "Activity:"),
                        sliderInput("difficulty", "Difficulty:", min = 1, max = 5, value = 3),
                        textInput("sub_cat1", "Sub-Category 1:"),
                        textInput("sub_cat2", "Sub-Category 2:"),
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
               h4("Add your activity or daily evaluation data using the forms.")
             )
           )
  ),
  # Page 2: View Raw Data
  tabPanel("Raw Data",
           sidebarLayout(
             sidebarPanel(
               h4("View Recorded Data"),
               dateRangeInput("date_range_raw", "Filter by Date Range:",
                              start = Sys.Date() - 30, end = Sys.Date()),
               # Dynamically rendered dropdown inputs based on selected tab
               uiOutput("variables_ui_1"),
               actionButton("refresh", "Refresh Summary")
             ),
             mainPanel(
               tabsetPanel(
                 id = "raw_data_tabs",
                 tabPanel("Activities", tableOutput("activities_table")),
                 tabPanel("Daily Summary", DTOutput("summary_table"))
               ),
               # Debug outputs
               verbatimTextOutput("debug_summary_data"),  # To show data structure or sample
               verbatimTextOutput("debug_structure")      # To show structure of the dataset
             )
           )
  ),
  # Page 3: Summary and Graphs
  tabPanel("Summary",
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
  )
)


# Define Server
server <- function(input, output, session) {
  
  # Define file paths
  activities_file <- "Activities.xlsx"
  mood_file <- "Mood.xlsx"
  Day_Analytics <- "Day_Analytics.xlsx"
  
  # Function to create a file if it doesn't exist
  create_empty_file <- function(file_path, df) {
    if (!file.exists(file_path)) {
      write_xlsx(df, file_path)
    }
  }
  
  # Create the files if they don't exist
  create_empty_file(activities_file, data.frame(
    Day = as.Date(character()),
    Topic = character(),
    Activity = character(),
    Difficulty = numeric(),
    Sub_Category_1 = character(),
    Sub_Category_2 = character(),
    Comment = character(),
    stringsAsFactors = FALSE
  ))
  
  create_empty_file(mood_file, data.frame(
    Day = as.Date(character()),
    Sleep = numeric(),
    Anxiety = numeric(),
    Mood = numeric(),
    Health = numeric(),
    Exercise = numeric(),
    Exercise_Intensity = character(),
    Comment = character(),
    stringsAsFactors = FALSE
  ))
  
  create_empty_file(Day_Analytics, data.frame(
    Day = as.Date(character()),
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
  ))
  
  # Load the data after checking and creating files
  activities <- reactive({
    read_excel(activities_file)
  })
  
  moods <- reactive({
    read_excel(mood_file)
  })
  
  per_day <- reactive({
    read_excel(Day_Analytics)
  })
  
  # Add activity
  observeEvent(input$add_activity, {
    add_activity(
      data = activities_file,
      day = input$date,
      topic = input$topic,
      activity = input$activity,
      difficulty = input$difficulty,
      Sub_category_1 = input$sub_cat1,
      Sub_category_2 = input$sub_cat2,
      comment = input$comment
    )
    activities(read_excel(activities_file))
    showNotification("Activity added successfully!", type = "message")
  })
  
  # Add mood
  observeEvent(input$add_mood, {
    add_mood(
      data = mood_file,
      day = input$mood_date,
      sleep = input$sleep,
      anxiety = input$anxiety,
      mood = input$mood,
      health = input$health,
      exercise = input$exercise,
      exercise_intensity = input$exercise_intensity,
      comment = input$mood_comment
    )
    moods(read_excel(mood_file))
    showNotification("Daily evaluation added successfully!", type = "message")
  })
  
  # Final results processing for summary
  # Reactive processing logic for final results
  final_results <- reactive({
    Activities <- activities()
    Mood <- moods()
    Per_day <- per_day()
    
    # Ensure date coherence
    Activities$Day <- as.Date(Activities$Day, format = "%d/%m/%Y")
    Mood$Day <- as.Date(Mood$Day, format = "%d/%m/%Y")
    Per_day$Day <- as.Date(Per_day$Day, format = "%d/%m/%Y")
    
    # Avoid duplicates
    Activities <- distinct(Activities)
    Mood <- distinct(Mood)
    Per_day <- distinct(Per_day)
    
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
      Day = as.Date(character()),  # Explicitly set as Date
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
      
      # Extract and calculate metrics
      Difficulties <- activity_day$Difficulty
      numActivities <- length(activity_day$Activity[!is.na(activity_day$Difficulty)])
      
      Anxiety <- ifelse(nrow(mood_day) > 0, mood_day$Anxiety, NA)
      Mood <- ifelse(nrow(mood_day) > 0, mood_day$Mood, NA)
      Health <- ifelse(nrow(mood_day) > 0, mood_day$Health, NA)
      Sleep <- ifelse(nrow(mood_day) > 0, mood_day$Sleep, NA)
      Comment <- ifelse(nrow(mood_day) > 0, mood_day$Comment, NA)
      
      # Calculate weighted difficulty (assume weighted_sum is defined elsewhere)
      Difficulty_calcs <- weighted_sum(Difficulties, Sleep, Mood, Health)
      
      Base_difficulty <- Difficulty_calcs[1]
      Factor_difficulty <- Difficulty_calcs[2]
      Weighted_difficulty <- Difficulty_calcs[3]
      
      Exercise <- ifelse(!is.na(mood_day$Exercise), ifelse(mood_day$Exercise_intensity == "high", mood_day$Exercise*2, mood_day$Exercise), NA)
      
      # Identify the main topic
      topic_summary <- activity_day %>%
        group_by(Topic) %>%
        summarise(
          Topic_count = n(),
          Total_difficulty = sum(Difficulty, na.rm = TRUE)
        ) %>%
        arrange(desc(Total_difficulty))
      
      Main_topic <- topic_summary$Topic[1]
      Aggregated_difficulty <- topic_summary$Total_difficulty[1]
      Main_topic_Activities <- topic_summary$Topic_count[1]
      
      # Append the results
      row <- data.frame(
        Day = day,
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
    
    # Combine with existing Per_day data
    combined_results <- bind_rows(Per_day, results)
    
    # Remove duplicates and retain the last entry for each day
    final_results <- combined_results %>%
      group_by(Day) %>%
      slice_max(order_by = row_number(), n = 1) %>%
      ungroup()
    
    final_results
  })
  # Inspect the data returned by final_results()
  output$debug_summary_data <- renderPrint({
    head(final_results())  # Print the first few rows of the data
  })
  output$debug_structure <- renderPrint({
    str(final_results())  # Shows structure of the dataset
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
  
  output$activities_table <- renderTable({
    filtered_activities()  # Ensure the filtered data is passed
  })
  

  filtered_summary_table <- reactive({
    summary_data <- final_results()  # Assuming final_results() gives the summary data
    
    # Apply filters dynamically based on user input
    for (colname in names(summary_data)) {
      filter_col <- paste0(colname, "_filter")
      if (!is.null(input[[filter_col]]) && length(input[[filter_col]]) > 0) {
        if ("All" %in% input[[filter_col]]) {
          # Skip filtering for this column if "All" is selected
          next
        }
        
        # Apply filter for the current column if it exists in input
        if (is.numeric(summary_data[[colname]])) {
          # For numeric columns, filter by a range
          summary_data <- summary_data %>%
            filter(summary_data[[colname]] >= min(input[[filter_col]]) &
                     summary_data[[colname]] <= max(input[[filter_col]]))
        } else {
          # For categorical columns, filter by selected values
          summary_data <- summary_data %>%
            filter(summary_data[[colname]] %in% input[[filter_col]])
        }
      }
    }
    
    # Return filtered data
    summary_data
  })
  
  # Render filtered summary table
  output$summary_table <- renderDT({
    filtered_summary_table()  # Use the filtered data
  })
  
  
  # Render filtered summary table
  output$summary_table <- renderDT({
    filtered_summary_table()  # Pass the filtered data for rendering
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

}
# Run the app
shinyApp(ui = ui, server = server)
