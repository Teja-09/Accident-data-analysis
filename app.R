library(shiny)
library(RMySQL)
library(DT)
library(googleVis)
library(dplyr)
library(ggplot2)
library(googleway)

api_key <- "API_KEY"

# Define the database connection parameters
db_host <- "localhost" 
db_user <- "root" 
db_password <- "1234" 
db_name <- "cats"  

# Establish a connection to the MySQL database
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = db_name, 
                 host = db_host, 
                 user = db_user, 
                 password = db_password)

# Query to fetch the incident_data
data <- dbGetQuery(con, "SELECT * FROM IncidentReports")  
dbDisconnect(con)

data$Weather_Timestamp <- as.POSIXct(data$Weather_Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
unique_visibility <- sort(unique(data$Visibility_mi))

ui <- page_sidebar(
  
  # App title ----
  title ="Accident Analysis Dashboard",
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    dateRangeInput("dateRange", "Select Date Range:",
                   start = min(data$Start_Time),
                   end = max(data$Start_Time)),
    checkboxGroupInput("severityFilter", "Select Severity Levels:",
                       choices = sort(unique(data$Severity)),
                       selected = sort(unique(data$Severity)),
                       inline = TRUE),
    sliderInput("visibility_range",
                "Select minimum Visibility (mi):",
                min = min(unique_visibility, na.rm = TRUE),
                max = max(unique_visibility, na.rm = TRUE), 
                value = c(min(unique_visibility, na.rm = TRUE), 
                          max(unique_visibility, na.rm = TRUE)),
                step = 1),
    selectInput("stateFilter", "Select State:",
                choices = c("All", sort(unique(data$State))),
                selected = "All"),  
    checkboxGroupInput("timezoneFilter", "Select Timezones:",
                       choices = sort(unique(data$Timezone)),
                       selected = sort(unique(data$Timezone)),
                       inline = TRUE)
  ),
  
  # Main panel for displaying outputs ----
  navset_card_underline(
    title = "Visualizations",
    nav_panel("Severity Analysis", 
              h4("Severity Analysis"),
              htmlOutput("severityPlot")),
    nav_panel("Accidents Over Time", 
              h4("Accidents Over Time Analysis"),
              htmlOutput("accidentsOverTimePlot")),
    nav_panel("Visibility Effects", 
              h4("Visibility Effects on Accidents"),
              htmlOutput("visibilityScatterPlot")),
    nav_panel("Severity vs Visibility", 
              h4("Severity vs Visibility"),
              plotOutput("severityVisibilityPlot")),
    nav_panel("Weather Effects", 
              h4("Accidents by Weather Conditions "),
              htmlOutput("weatherConditionPie")),
    nav_panel("Temperature Humidity Relation", 
              h4("Temperature Vs Humidity Heatmap"),
              plotOutput("tempHumidityHeatmap")),
    nav_panel("Incident Map",
              h4("Incident Location Map"),
              google_mapOutput(outputId ="googleMap22", height = "600px")),
    nav_panel("Data Table",
              h4("Data Table"),
              DTOutput("dataTable")),
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data %>%
      filter(
        Start_Time >= input$dateRange[1],
        Start_Time <= input$dateRange[2],
        Severity %in% input$severityFilter,
        (input$stateFilter == "All" | State == input$stateFilter),
        (Timezone %in% input$timezoneFilter),
        Visibility_mi >= input$visibility_range[1],
        Visibility_mi <= input$visibility_range[2]
      ) %>%
      slice_head(n = 20000)  
  })
  
  # Severity Analysis Plot using Google Charts
  output$severityPlot <- renderGvis({
    severity_count <- filtered_data() %>%
      group_by(Severity) %>%
      summarise(Accident_Count = n(), .groups = 'drop') %>%
    mutate(Severity = as.factor(Severity))
    
    # Create a bar chart with Severity on x-axis and Accident_Count on y-axis
    gvisColumnChart(severity_count,
                 xvar = "Severity", 
                 yvar = "Accident_Count", 
                 options = list(
                                hAxis = "{title: 'Severity Level'}",  
                                vAxis = "{title: 'Count of Accidents'}",  
                                width = '100%', height = '100%',
                                legend = "none",
                                chartArea = "{left: 80, top: 50, width: '75%', height: '70%'}"))  
  })
  

  
  # Accidents Over Time Plot
  output$accidentsOverTimePlot <- renderGvis({
    time_data <- filtered_data() %>%
      group_by(Date = as.Date(Start_Time)) %>%
      summarise(Accident_Count = n(), .groups = 'drop')
    
    gvisLineChart(time_data,
                  xvar = "Date", 
                  yvar = "Accident_Count", 
                  options = list(
                    hAxis = "{title: 'Date', format: 'MMM dd, yyyy', gridlines: {count: 15}}", 
                    vAxis = "{title: 'Count of Accidents'}",
                    width = '100%', 
                    height = '100%',
                    legend = "none"  
                  ))
  })
  
  
  # Scatter Plot for Accidents vs Visibility using Google Charts with Legend
  output$visibilityScatterPlot <- renderGvis({
    visibility_count <- filtered_data() %>%
      group_by(Visibility_mi) %>%
      summarise(Accident_Count = n(), .groups = 'drop') %>%
      mutate(Visibility_mi = round(Visibility_mi, 1))  
    
    # Create scatter plot
    gvisScatterChart(visibility_count,
                     options = list(
                                    hAxis = "{title:'Visibility (miles)'}",
                                    vAxis = "{title:'Accident Count'}",
                                    pointSize = 5,
                                    width = '100%', height = '100%',
                                    legend = "none"))
  })
  
  
  output$severityVisibilityPlot <- renderPlot({
    # Group data by Visibility and Severity and calculate accident counts
    severity_visibility <- filtered_data() %>%
      group_by(Visibility_mi = round(Visibility_mi, 1), Severity) %>%
      summarise(Accident_Count = n(), .groups = 'drop')
    
    # Create the bar plot with ggplot
    ggplot(severity_visibility, aes(x = Visibility_mi, y = Accident_Count, fill = as.factor(Severity))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_brewer(palette = "Set1") + 
      scale_x_continuous(breaks = seq(0, max(severity_visibility$Visibility_mi, na.rm = TRUE), by = 5)) + 
      labs(title = "Accidents by Severity and Visibility", 
           x = "Visibility (miles)", 
           y = "Accident Count",
           fill = "Severity") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),  
        axis.text.x = element_text(angle = 45, hjust = 1)  
      )
  })
  
  

  # Weather Condition Pie Chart using Google Charts
  output$weatherConditionPie <- renderGvis({
    weather_count <- filtered_data() %>%
      group_by(Weather_Condition) %>%
      summarise(Accident_Count = n(), .groups = 'drop') %>%
      mutate(Percentage = round((Accident_Count / sum(Accident_Count)) * 100, 1)) 
    
    # Create a pie chart with Weather_Condition and Percentage
    gvisPieChart(weather_count,
                 labelvar = "Weather_Condition", 
                 numvar = "Percentage",
                 options = list(
                   width = '100%', height = '100%',
                   pieHole = 0.4, 
                   legend = "{position: 'right'}",
                   slices = "{offset: 0.1}"  
                 ))
  })
  
  
  # Heatmap for temperature and humidity
  output$tempHumidityHeatmap <- renderPlot({
    binned_data <- filtered_data() %>%
      mutate(Temperature_F = cut(Temperature_F, breaks = seq(min(Temperature_F), max(Temperature_F), by = 5), include.lowest = TRUE),
             Humidity = cut(Humidity, breaks = seq(0, 100, by = 5), include.lowest = TRUE)) %>%
      group_by(Temperature_F, Humidity) %>%
      summarise(Count = n(), .groups = 'drop')
    
    ggplot(binned_data, aes(x = Temperature_F, y = Humidity)) +
      geom_tile(aes(fill = Count), color = "white") +
      scale_fill_gradient(low = "white", high = "blue") +
      theme_minimal() +
      labs(title = "Heatmap of Temperature vs Humidity",
           x = "Temperature (°F)",
           y = "Humidity (%)",
           fill = "Count")
  })
  
  output$dataTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10), rownames = FALSE)
  })
  

  # Google maps for visualizing incidents
  output$googleMap22 <- renderGoogle_map({
    data <- filtered_data()
    
    data$Start_Lat <- as.numeric(data$Start_Lat)
    data$Start_Long <- as.numeric(data$Start_Lng)
    data$hover_text <- paste(
      "Severity:", data$Severity,
      "<br>Visibility:", data$Visibility_mi, "mi",
      "<br>Street:", data$Street,
      "<br>City:", data$City,
      "<br>State:", data$State
    )
    # Create the map
    google_map(key = api_key) %>%
      add_markers(data = data, lat = "Start_Lat", lon = "Start_Lng", mouse_over = "hover_text",)
    
    })
  
}

shinyApp(ui = ui, server = server)
