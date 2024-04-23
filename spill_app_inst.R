#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# This is the app that includes the instantaneous discharge values. 
# Run the application by clicking 'Run App' on the top right of the screen :)
# Link below is a map of the gages used in the app
# https://www.google.com/maps/d/u/0/edit?mid=17w7T3YnJ21vTfO9yEXtrZ1seXRgL-Y4&ll=38.92658056010304%2C-78.20588499999998&z=8
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# install.packages(c("shiny", "shinydashboard", "dplyr", "DT", "dataRetrieval", "lubridate", "timechange"))

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(dataRetrieval)
library(lubridate)
library(timechange)
setwd("c:/workingdir/trib_velocities") # change this for your computer

# set the classes of all columns in the data frame
gage_list <- data.table::fread("gage_list.csv",
                               header = TRUE,
                               colClasses = c('gage_number' = 'character',
                                              'site_name' = 'character',
                                              'latitude' = 'character',
                                              'longitude' = 'character',
                                              'url' = 'character',
                                              'drain_area' = 'numeric', 
                                              'mean_flow' = 'numeric', 
                                              'a_intercept' = 'numeric', 
                                              'b_slope' = 'numeric',
                                              'coordinates' = 'character',
                                              'site_comma' = 'character'),
                               data.table = FALSE)

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "ICPRB Spill Tool",
                                    tags$li(class = "dropdown",
                                            textOutput("date_time_text"),
                                            class = "navbar-text",
                                            style = "color: white;",)),
                    dashboardSidebar(width = 250,
                                     # creates a link to the gage map at the top of the sidebar
                                     a(
                                       href = "https://www.google.com/maps/d/viewer?mid=17w7T3YnJ21vTfO9yEXtrZ1seXRgL-Y4&ll=39.19301986737269%2C-77.25883880838477&z=8",
                                       class = "btn btn-block btn-primary",
                                       style = "color: white;",
                                       target = "_blank",  # opens the link in a new tab
                                       icon("map-location-dot")," Go to Gage Map"
                                     ),
                                     
                                     br(),
                                     selectInput("station", 
                                                 "USGS Gage Station Number", 
                                                 c(
                                                   "01595000", "01595300", "01595500", "01595800",
                                                   "01596500", "01597500", "01598500", "01599000",
                                                   "01601000", "01601500", "01603000", "01604500",
                                                   "01605500", "01606000", "01606500", "01607500",
                                                   "01608000", "01608070", "01608500", "01609000",
                                                   "01610000", "01610155", "01610400", "01611500",
                                                   "01613000", "01613030", "01613095", "01613525",
                                                   "01613900", "01614000", "01614500", "01615000",
                                                   "01616100", "01616400", "01616425", "01616500",
                                                   "01617000", "01617800", "01618000", "01618100",
                                                   "01619000", "01619500", "01620500", "01621050",
                                                   "01622000", "01625000", "01626000", "01626850",
                                                   "01627500", "01628500", "01629500", "01631000",
                                                   "01632000", "01632082", "01632900", "01633000",
                                                   "01634000", "01634500", "01635500", "01636316",
                                                   "01636464", "01636500", "01636690", "01637500",
                                                   "01638350", "01638420", "01638480", "01638500",
                                                   "01639000", "01639500", "01642190", "01642198",
                                                   "01642199", "01643000", "01643395", "01643500",
                                                   "01643590", "01643700", "01643805", "01643880",
                                                   "01644000", "01644280", "01644371", "01644372",
                                                   "01644375", "01644380", "01644388", "01644390",
                                                   "01645000", "01645704", "01645762", "01646000",
                                                   "01646305", "01646500"),
                                                 selected = NULL,
                                                 multiple = FALSE, # only one gage at a time
                                                 selectize = TRUE, 
                                                 width = "217px"),
                                     br(),
                                     # selects the distance the spill might travel
                                     numericInput("distance",
                                                  "Estimated Travel Distance in Miles",
                                                  value = NULL,
                                                  min = 0,
                                                  max = 40,
                                                  width = "215px"),
                                     
                                     br(),
                                     # runs the app
                                     actionButton("run_something",
                                                  "Get Information",
                                                  icon("droplet"),
                                                  width = "140px")
                    ),
                    dashboardBody(
                      fluidRow(
                        column(width = 12,
                               box(
                                 title = "Stream Information",
                                 width = 12,
                                 dataTableOutput("result_table")
                               )
                        )
                      )
                    ) 
)

server <- function(input, output) {
  
  data <- reactiveValues(table = NULL)
  
  # gives real time data for the date/time at the top right of the app, also used for instant discharge value
  current_datetime <- reactive({ now() })
  formatted_datetime <- reactive({ format(current_datetime(), format = "%B %e, %Y %H:%M") })
  output$date_time_text <- renderText({ paste(formatted_datetime(),"EST") })
  
  #gives instant discharge value
  getDataFromDataFrame <- function(selected_station) {
    siteNumber <- selected_station
    parameterCd <- "00060"
    startDate <- as.Date(current_datetime()) - 1  # day before the current day
    endDate <- ""  # leaving this empty gives most recent value
    discharge <- readNWISuv(siteNumber, parameterCd, startDate, endDate, tz = "America/New_York")
    discharge <- discharge[order(discharge$dateTime, decreasing = TRUE), ] # orders the values backwards to get most recent
    num_intervals <- 1
    discharge <- discharge[1:num_intervals, ] # only picks one value
    
    gage_values <- gage_list %>%
      dplyr::filter(gage_number == selected_station)
    
    result_data <- data.frame(
      "USGS Gage Station Number" = gage_values$gage_number[1],
      "Site Name" = gage_values$site_name[1],
      "Coordinates" = gage_values$coordinates[1],
      "Current Stream Discharge in cfs" = discharge$X_00060_00000, # instant discharge, column-specific
      "Long-term Mean Discharge in cfs" = gage_values$mean_flow[1], # 10-year average
      "Stream Velocity in mph" = gage_values$a_intercept[1] * discharge$X_00060_00000^gage_values$b_slope[1],
      "a (intercept)" = gage_values$a_intercept[1],
      "b (slope)" = gage_values$b_slope[1],
      "Contaminant Travel Time in Hours" = input$distance / (gage_values$a_intercept[1] * discharge$X_00060_00000^gage_values$b_slope[1]),
      "Site URL" = gage_values$url[1]
    )
    #gets rid of "." between words in table
    colnames(result_data) <- gsub("\\.", " ", colnames(result_data))
    return(result_data)
  }
  
  # gives us the table if all data is selected
  observeEvent(input$run_something, {
    if (is.null(input$station) || is.null(input$distance)) {
      data$table <- data.frame()
    } else {
      selected_station <- input$station
      data$table <- getDataFromDataFrame(selected_station)
    }
  })
  
  # transposes table, not related to data within it
  output$result_table <- renderDataTable({
    if (is.null(data$table) || nrow(data$table) == 0) {
      return(NULL)
    } else {
      transposed_data <- t(as.matrix(data$table))
      transposed_data_df <- as.data.frame(transposed_data, stringsAsFactors = FALSE)
      datatable(transposed_data_df, 
                rownames = TRUE, 
                colnames = "", 
                options = list(searching = FALSE, 
                               lengthChange = FALSE, 
                               info = FALSE,
                               paging = FALSE))
    }
  })
}

# run app
shinyApp(ui, server)
