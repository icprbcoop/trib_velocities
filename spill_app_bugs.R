#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# This is the spill prediction tool that combines instantaneous and daily stream flow values. There are bugs that stop it from running.
# Suspected bugs: lines 145 & 151 (discharge <- discharge$_____________)
        # In the other inst and daily apps, the $______ specification must be made within the table itself, NOT before, or else there is an error. 
        # This app needs those values to be defined before the table, so this causes an issue - but only when inputDate is left blank (when inst. data is desired)
        # Overall, daily data works in this app, instantaneous does not.

# Run the application by clicking 'Run App' above :)
# Map of gages for measuring distance: 
# https://www.google.com/maps/d/edit?mid=19tJ1arCrPtuK-UJCjR3Gc-r_2qaqOB0&ll=39.469599599034964%2C-78.096646126974&z=10
# Map can also be opened through the app
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

#  install.packages(c("shiny", "shinydashboard", "dplyr", "DT", "dataRetrieval", "lubridate", "timechange"))

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(dataRetrieval)
library(lubridate)
library(timechange)

setwd("C:/workingdir/trib_velocities") #change this for your computer

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
                                     #creates a link to the gage map at the top of the sidebar
                                     a(
                                       href = "https://www.google.com/maps/d/edit?mid=19tJ1arCrPtuK-UJCjR3Gc-r_2qaqOB0&ll=39.469599599034964%2C-78.096646126974&z=10",
                                       class = "btn btn-block btn-primary",
                                       style = "color: white;",
                                       target = "_blank",  # This opens the link in a new tab
                                       icon("map-location-dot")," Go to Gage Map"),
                                    
                                     br(),
                                     #allows the user to select the date of the spill for daily discharge
                                     dateInput("selected_date", 
                                               "Date of Spill (Clear for Instantaneous Value)"),
                                     
                                     br(),
                                     #list of all gages
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
                                     #selects the distance the spill might travel
                                     numericInput("distance",
                                                  "Estimated Travel Distance in Miles",
                                                  value = NULL,
                                                  min = 0,
                                                  max = 40,
                                                  width = "215px"),
                                     
                                     br(),
                                     #runs the app
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
  
  #gives us the real time data for the date/time at the top right of the app, also used for instant discharge value
  current_datetime <- reactive({ now() })
  formatted_datetime <- reactive({ format(current_datetime(), format = "%B %e, %Y %H:%M") })
  output$date_time_text <- renderText({ paste(formatted_datetime(),"EST") })

  getDataFromDataFrame <- function(selected_station, selected_date = NULL) {
    
    siteNumber <- selected_station
    gage_values <- gage_list %>% dplyr::filter(gage_number == selected_station)
    parameterCd <- "00060" #discharge
    
    #chooses instantaneous (if) or daily (then) discharge value depending on if dateInput was filled out in UI
    if (is.null(selected_date)) {
      startDate <- as.Date(current_datetime()) - 1  # Day before the current day
      endDate <- ""  # Leaving this blank gives the most recent value
      discharge <- readNWISuv(siteNumber, parameterCd, startDate, endDate, tz = "America/New_York") #function for instant data
      #column_name <- "Current Stream Discharge in cfs" # changes column name in table, changes depending on dateInput
      discharge <- discharge$X_00060_00000 #picks only the flow value from the readNWISuv table
    } else {
      startDate <- selected_date # date selected in UI
      endDate <- selected_date # same date selected in UI
      discharge <- readNWISdv(siteNumber, parameterCd, startDate, endDate) #function for daily data
      #column_name <- "Daily Discharge in cfs" # changes column name in table, changes depending on dateInput
      discharge <- discharge$X_00060_00003 #picks only the flow value from the readNWISdv table
    }
    
    result_data <- data.frame(
      "USGS Gage Station Number" = gage_values$gage_number[1],
      "Site Name" = gage_values$site_name[1],
      "Coordinates" = gage_values$coordinates[1],
      "Stream Discharge" = discharge, # bug
      "Long-term Mean Discharge in cfs" = gage_values$mean_flow[1], # 10-year average
      "Stream Velocity in mph" = gage_values$a_intercept[1] * discharge^gage_values$b_slope[1], # bug
      "a (intercept)" = gage_values$a_intercept[1],
      "b (slope)" = gage_values$b_slope[1],
      "Contaminant Travel Time in Hours" = input$distance / (gage_values$a_intercept[1] * 
                                                               discharge^gage_values$b_slope[1]), # bug
      "Site URL" = gage_values$url[1]
    )
    
    #gets rid of "." between words in table
    colnames(result_data) <- gsub("\\.", " ", colnames(result_data))
    return(result_data)
  }
  
  # gives us the table if all data is selected, maybe an issue here?
  observeEvent(input$run_something, {
    if (is.null(input$station)) {
      data$table <- data.frame()
    } else {
      selected_station <- input$station
      selected_date <- input$selected_date
      if (is.null(selected_date)) {
        data$table <- getDataFromDataFrame(selected_station)
      } else {
        data$table <- getDataFromDataFrame(selected_station, selected_date)
      }
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

# Run that app! 
shinyApp(ui, server)

