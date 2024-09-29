# app.R

library(shiny)
library(ggplot2)
library(DT)  
library(leaflet)
library(DBI)
library(RMySQL) 
library(dplyr)



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

# Query to fetch the data
data <- dbGetQuery(con, "SELECT * FROM IncidentReports")  
# Close the connection
dbDisconnect(con)


# Load or simulate the dataset
# data <- read.csv("C:/Teja/CATS/data.csv")  # Replace with actual path or data
data$Weather_Timestamp <- as.POSIXct(data$Weather_Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# UI
ui <- fluidPage(
  titlePanel("Accident Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filters"),
      dateRangeInput("dateRange", "Select Date Range:",
                     start = min(data$Start_Time),
                     end = max(data$Start_Time)),
      
      # Severity filter (multi-select dropdown)
      checkboxGroupInput("severityFilter", "Select Severity Levels:",
                         choices = sort(unique(data$Severity)),
                         selected = sort(unique(data$Severity)),
                         inline = TRUE),
      
      # Temperature filter (slider input)
      sliderInput("tempRange", "Select Temperature Range (°F):",
                  min = min(data$Temperature_F, na.rm = TRUE),  # Minimum temperature
                  max = max(data$Temperature_F, na.rm = TRUE),  # Maximum temperature
                  value = c(min(data$Temperature_F, na.rm = TRUE), 
                            max(data$Temperature_F, na.rm = TRUE)),  # Default values
                  step = 1),  # Step size for slider
      
      # State filter
      selectInput("stateFilter", "Select State:",
                  choices = c("All", sort(unique(data$State))),
                  selected = "All"),  
      
      # Timezone filter
      checkboxGroupInput("timezoneFilter", "Select Timezones:",
                         choices = sort(unique(data$Timezone)),
                         selected = sort(unique(data$Timezone)),
                         inline = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Analysis",
                 h3("Overview"),
                 fluidRow(
                   column(12,
                          plotOutput("severityPlot", width = "100%", height = "400px"),
                          plotOutput("temperaturePlot", width = "100%", height = "400px"),
                          plotOutput("tempHumidityHeatmap", width = "100%", height = "400px"),
                          plotOutput("weatherConditionPlot", width = "100%", height = "400px")
                   )
                 )
        ),
        tabPanel("Incident Map",
                 h3("Incident Location Map"),
                 leafletOutput("incidentMap", height = "600px")
        ),
        tabPanel("Filtered Data Summary",
                 h3("Combined Summary Table"),
                 DTOutput("dataTable")  
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Filter data based on date range and selected severity levels
  filtered_data <- reactive({
    data %>%
      filter(Start_Time >= input$dateRange[1],
             Start_Time <= input$dateRange[2],
             Severity %in% input$severityFilter,
             Temperature_F >= input$tempRange[1],  # Filter by min temperature
             Temperature_F <= input$tempRange[2],  # Filter by max temperature
             (input$stateFilter == "All" | State == input$stateFilter),
             (Timezone %in% input$timezoneFilter))
  })
  
  # Output the plot for severity analysis
  output$severityPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = factor(Severity))) +
      geom_bar(fill = "skyblue") +
      theme_minimal() +
      labs(title = "Accident Severity Distribution", 
           x = "Severity", 
           y = "Count of Accidents") +
      scale_x_discrete(drop = FALSE) +  # Keeps all levels on the x-axis
      geom_text(stat='count', aes(label=..count..), 
                position=position_stack(vjust=0.5), size=4)  # Add data labels
  })
  
  
  output$temperaturePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Weather_Timestamp, y = Temperature_F)) +
      geom_line(color = "orange") +
      geom_point(color = "blue") +
      theme_minimal() +
      labs(title = "Temperature Over Time", 
           x = "Time", 
           y = "Temperature (°F)") +
      scale_x_datetime(date_labels = "%Y-%m-%d %H:%M") +  # Format x-axis labels
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility
  })
  
  # Output for the top 5 weather condition analysis
  output$weatherConditionPlot <- renderPlot({
    top_weather_conditions <- filtered_data() %>%
      group_by(Weather_Condition) %>%
      summarise(Accident_Count = n()) %>%
      top_n(5, Accident_Count) %>%
      arrange(desc(Accident_Count))  # Get top 5 weather conditions
    
    ggplot(top_weather_conditions, aes(y = reorder(Weather_Condition, Accident_Count), x = Accident_Count)) + 
      geom_bar(stat = "identity", fill = "burlywood2") +
      geom_text(aes(label = Accident_Count), vjust = -0.5, color = "black") +  # Add labels above bars
      theme_minimal() +
      labs(title = "Top 5 Weather Conditions by Number of Accidents", 
           y = "Weather Condition",  
           x = "Count of Accidents") +  
      theme(axis.text.y = element_text(angle = 0, hjust = 1))  # Rotate y-axis labels for better visibility
  })
  
  
  # Output for the temperature vs humidity heatmap
  output$tempHumidityHeatmap <- renderPlot({
    # Create a binned data frame for counts
    binned_data <- filtered_data() %>%
      mutate(Temperature_F = cut(Temperature_F, breaks = seq(min(Temperature_F), max(Temperature_F), by = 5), include.lowest = TRUE),
             Humidity = cut(Humidity, breaks = seq(0, 100, by = 5), include.lowest = TRUE)) %>%
      group_by(Temperature_F, Humidity) %>%
      summarise(Count = n(), .groups = 'drop')  # Count occurrences
    
    ggplot(binned_data, aes(x = Temperature_F, y = Humidity)) +
      geom_tile(aes(fill = Count), color = "white") +  # Use Count for fill
      scale_fill_gradient(low = "white", high = "blue") +  # Color gradient
      theme_minimal() +
      labs(title = "Heatmap of Temperature vs Humidity",
           x = "Temperature (°F)",
           y = "Humidity (%)",
           fill = "Count")  # Legend title
  })
  
  
  # -------------------------------------------------------------
  # Output the summary table of filtered data
  output$dataTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Output the interactive map
  output$incidentMap <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Start_Lng,  
        lat = ~Start_Lat,  
        radius = 3,  # Size of the markers
        color = "blue",  # Color of the markers
        stroke = FALSE,
        fillOpacity = 0.5,
        popup = ~paste("Severity:", Severity, "<br>",
                       "Temperature:", Temperature_F, "<br>",
                       "Date:", Start_Time)  # Customize popup information
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
