# Title: Sample Shiny Dashboard
# Author: Hassan Saad 
# Date created: 5th June, 2020
# Date modified: 8th June, 2020


###############################
# 0. Set up Environment #######
###############################
#This command clears the environment
rm(list = ls())

# Load in needed packages

# For shiny
library(shinydashboard)

#For the bar charts
library(forcats)
library(ggplot2)
library(tidyverse)
library(DT)

#For the maps
library(rgdal)
library(broom)
library(RColorBrewer)
library(raster)
library(sf)

# Create a path to git folder - This step is not necessary for the shiny app and should only be used if configuring local changes
if (Sys.info()["sysname"] == "Windows"){
  git_path <- paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/r-shiny-sample/code")
} else {
  git_path <- paste0("/Users/", Sys.info()[7], "/Documents/GitHub/r-shiny-sample/code")
}

print(git_path)
# setwd(git_path) - This should only be done when using the app locally on your computer


###############################
# 1. Read in data  ############
###############################

# Data to graph as bar chart
data_to_graph <- read.csv("data/stacked_bar_fakedata.csv")

# Data to graph as map
data_to_map <- read.csv("data/standard_bar_fakedata.csv")

# India shapefile for geographical coordinates
shp <- read_sf("data/IndiaShapeFiles/STATE_11.shp")

# Create dataset with X and Y coordinates for each State to add as labels on GGPLOT2 map 
coords <- shp %>%
  st_centroid() %>%
  st_coordinates() %>%
  cbind(shp) %>%
  rename(lon = X, lat = Y)

# Final dataset with X,Y coords and state labels
data_map_unique <- left_join(data_to_map, dplyr::select(coords, NAME, lon, lat), by = c("State" = "NAME")) %>%
  filter(State != "Telangana")

# Final dataset with geographical information added
data_to_map <- left_join(data_to_map, dplyr::select(shp, NAME, geometry), by = c("State" = "NAME")) %>%
  filter(State != "Telangana") %>%
  st_as_sf()

###############################
# 2. Shiny App  ###############
###############################
## app.R ##

# "ui" sets the user interface of the Shiny App
ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"), # Set dashboard header
  
  dashboardSidebar( # Create a sidebar where user can provide inputs
    # Allow the user to select between viewing data for "Programs", "No Programs", or "Both"
    menuItem("Choose program status", 
             tabName = "programs",
             icon = icon("hospital"), # Add icon for visual styling
             selectInput(
               inputId = "program_input", # This Id links the input to the outputs
               label = "",
               choices = c("Program", "No Program", "Both"),
               selected = "Program" # Default to Program
             )
  ),
  br(),
  br(),
  # Allow the user to select a range of estimates they want to view
  menuItem("Choose proportion range",
           tabName = "indicator",
           icon = icon("child"),
           sliderInput(
             inputId = "indicator_input", # This Id links the input to the outputs
             label = "",
             value = c(0,100),
             min = 0, 
             max = 100,
             step = 1,
             sep = ""
           )
  )
  ),
  dashboardBody(
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 16px; }")) # change the font size to 16
    ),
    # Boxes need to be put in a row (or column)
    fluidRow(
      tabBox(
        id = "tab", # This id will link the table to the tab the user is in
        width = 12,
        tabPanel("Chart", plotOutput("plot1", height = "350px")), # Chart output for bar graph
        tabPanel("Map", plotOutput('map', height = "350px")) # Graph output for map
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Table",
        # Download button
        DTOutput("table"), # Table to show the user the info they are seeing on the graph
        downloadButton("download_data", "Download data table") # Allow the user to download the information from the table
      )
    )
      )
    )

# "server" contains all the data manipulation and processing needed to produce the final output 
server <- function(input, output) {

# First step is to alter all the datasets based on the inputs the user provides
# This is done via "reactive" dataframes. This ensures that the output will change whenever the user changes inputs
  
  # Create reactive dataframe for the graph based on the inputs the user provides
  data_graph <- reactive ({
    
    if (input$program_input == "Both") {
      
      data_to_graph <- data_to_graph %>%
      filter(Estimate >= input$indicator_input[1] & Estimate <= input$indicator_input[2]) %>% # Keep only those rows that fit the estimate range the user provides
      group_by(IndicatorName) %>% # If either Program or No Program don't fall within range, drop both from dataframe
        filter(n()>1) %>%
        ungroup()
      
    } else {
      
    data_to_graph <- data_to_graph %>%
      filter(program_status == input$program_input) %>% # Keep only those rows that fit the program input the user provides
      filter(Estimate >= input$indicator_input[1] & Estimate <= input$indicator_input[2]) # Keep only those rows that fit the estimate range the user provides
    }
    
    data_to_graph
    
  })
  
  # Create reactive dataframe for the map based on the inputs that the user provides
  data_map <- reactive ({
    
    if (input$program_input == "Both") {
      data_to_map <- data_to_map %>%
        filter(Estimate >= input$indicator_input[1] & Estimate <= input$indicator_input[2])
    } else {
      data_to_map <- data_to_map %>%
        filter(program_status == input$program_input) %>%
        filter(Estimate >= input$indicator_input[1] & Estimate <= input$indicator_input[2])
    }
    
    data_to_map
    
  })
  
  # Create a reactive dataframe for the map labels based on the inputs that the user provides
  data_map_labels <- reactive({
    if (input$program_input == "Both") {
      data_map_unique <- data_map_unique %>%
        filter(Estimate >= input$indicator_input[1] & Estimate <= input$indicator_input[2]) 
    } else {
      data_map_unique <- data_map_unique %>%
        filter(program_status == input$program_input) %>%
        filter(Estimate >= input$indicator_input[1] & Estimate <= input$indicator_input[2])
    }
    
    data_map_unique
  })

  # Once all dataframes have been updated, we then create all the outputs specified in the UI

  # Output table
  output$table <- renderDT ({ # Create a reactive Data Table
    
    if (input$tab == "Chart") { # The table will change depending on whether the user is in the "Chart" or "Map" tab as the underlying datasets are slightly different
    data_table <- data_graph() %>%
      dplyr::select(IndicatorName, Estimate, LowerCI, UpperCI, program_status) %>%
      datatable(colnames = c('Reason' = 'IndicatorName',
                             'Proportion' = 'Estimate',
                             'Lower Bound' = 'LowerCI',
                             'Upper Bound' = 'UpperCI',
                             'Program Status' = 'program_status'))
    
    data_table
    } else {
      data_table <- data_map_labels() %>%
        dplyr::select(State, Estimate, LowerCI, UpperCI, program_status) %>%
        datatable(colnames = c('State' = 'State',
                               'Proportion' = 'Estimate',
                               'Lower Bound' = 'LowerCI',
                               'Upper Bound' = 'UpperCI',
                               'Program Status' = 'program_status'))
      data_table
    }
  })
  
  # Downloadable csv of data table
  output$download_data <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      if (input$tab == "Chart") { # The downloadable data will be different based on whether the user is in the "Chart" or "Map tab. 
        write.csv(data_graph(), file, row.names = FALSE)
      } else {
        write.csv(data_map_labels(), file, row.names = FALSE)
      }
}
  )
  
  
  # Bar graph output
  output$plot1 <- renderPlot ({
    
    data <- data_graph() # All reactive output are stored as functions and hence must be called upon by using ()
    
    proportion <- data$Estimate
    reason <-  data$IndicatorName
    upperCI <-  data$UpperCI
    lowerCI <-  data$LowerCI
    program_status <-  unique(data$program_status)
    
    #Create chart title and axis labels
    title <- "Proportion of children by program status"
    proportionlabel <- "Proportion of children reporting this reason"
    reasonlabel <- ""
    
    #Labels  
    data$labels <- as.character(round(proportion,1))
    
    # Graph command
    ggplot(data, aes(x=forcats::fct_rev(reorder(reason, reason)), y=proportion, linetype= NULL, group=program_status)) +   
    
    labs(y = proportionlabel,
         x = reasonlabel) +
    ggtitle(title) +
    
    geom_bar(stat = "identity", aes(fill = program_status), position = position_dodge(), width = 0.8) +
    coord_flip() + 
    geom_errorbar(aes(ymin = lowerCI, ymax =  upperCI), color = "#3f444c", width = 0.3,  position = position_dodge(width = .8)) +
    
    geom_text(aes(label = paste0(labels,"%"), y = upperCI + 2), 
              size = 2.8, hjust = 0, position = position_dodge(width = .8), color = "#3f444c") +   
    
    theme_classic() +
    theme(legend.position = c(0.8, 0.2)) + 
    theme(plot.title = element_text(size = "10", face = "bold")) +
    scale_y_continuous(limits = c(0,100)) +
    theme(plot.title = element_text(size = "11", face = "bold", hjust = 0.5)) +
    theme(axis.title.x = element_text(size = 11)) +
    
    scale_fill_manual(values =  c("#588da8", "#b2ebf2"), name = "Program status", labels = program_status)  
    
  })
  
  output$map <- renderPlot({
    
    data <- data_map() %>%
      st_as_sf()
  
    data_map_labels <- data_map_labels() %>%
      st_as_sf()
    
    ggplot() +
      geom_sf(data = data,aes(fill=Estimate, linetype = program_status)) +
      
      geom_sf_text(data = data_map_labels, aes(lon,lat, label = State)) +
      
      scale_fill_gradient(low = "cadetblue1", high = "deepskyblue4", na.value = "grey85") +
      
      scale_linetype_discrete(name="Program status", 
                              breaks=c("Program", "No Program"), 
                              #values=c('solid', 'blank'),
                              labels = c("Program", "No Program")) +
      
      coord_sf() +
      
      theme_void() +
      
      #Remove the legend showing which linetype signifies program vs non program states
      guides(linetype = FALSE) +
      
      theme(legend.justification=c(0,0), legend.position=c(.9,.8)) +
      
      ggtitle("Proportion of children by state")


  })
}

shinyApp(ui, server)