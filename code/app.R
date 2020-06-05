# Sample Shiny Dashboard


###############################
# 0. Set up Environment #######
###############################

# Purpose: Read in master field maps dataset and check for valid polygons for treatment, control and decoy farmers

# Load in needed packages

# For shiny
library(shinydashboard)

#For the bar charts an scatterplots
library("forcats")
library("ggplot2")
library("tidyverse")
library(dplyr)
library(DT)

#For the maps
library(leaflet)
library(rgdal)
library(broom)
library(tidyverse)
library(RColorBrewer)
library(raster)
library(sf)

#This command clears the environment
rm(list = ls())

# Create a path to DB folder
if (Sys.info()["sysname"] == "Windows"){
  git_path <- paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/r-shiny-sample/code")
} else {
  git_path <- paste0("/Users/", Sys.info()[7], "/Documents/GitHub/r-shiny-sample/code")
}

print(git_path)

###############################
# 1. Read in data  ############
###############################
data_to_graph <- read.csv("data/stacked_bar_fakedata.csv")

# Create fake inputs
# input <- list()
# input$program_input = c("Program")


###############################
# 2. Shiny App  ###############
###############################
## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  
  dashboardSidebar(
    menuItem("Choose program status",
             tabName = "programs",
             icon = icon("hospital"),
             selectInput(
               inputId = "program_input",
               label = "",
               choices = c("Program", "No Program", "Both"),
               selected = "Program"
             )
  ),
  br(),
  br(),
  menuItem("Choose proportion range",
           tabName = "indicator",
           icon = icon("child"),
           sliderInput(
             inputId = "indicator_input",
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
      tags$style(HTML(".main-sidebar { font-size: 16px; }")) #change the font size to 20
    ),
    # Boxes need to be put in a row (or column)
    fluidRow(
      tabBox(
        width = 12,
        tabPanel("Chart", plotOutput("plot1", height = "350px")),
        tabPanel("Map", leafletOutput('map', height = "350px"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Table",
        # Download button
        DTOutput("table"),
        downloadButton("download_data", "Download data table")
      )
    )
      )
    )


server <- function(input, output) {


  data <- reactive ({
    
    if (input$program_input == "Both") {
      data_to_graph <- data_to_graph %>%
      filter(Estimate >= input$indicator_input[1] & Estimate <= input$indicator_input[2])
    } else {
    data_to_graph <- data_to_graph %>%
      filter(program_status == input$program_input) %>%
      filter(Estimate >= input$indicator_input[1] & Estimate <= input$indicator_input[2])
    }
    
    data_to_graph
    
  })

  
  ######################################################################################################
  #THESE ARE FORMATTING PIECES THAT SHOULD NOT REQUIRE YOU TO MAKE CHANGES
  ######################################################################################################
  
  #Table output
  output$table <- renderDT ({
    
    data_table <- data() %>%
      dplyr::select(IndicatorName, Estimate, LowerCI, UpperCI, program_status) %>%
      datatable(colnames = c('Reason' = 'IndicatorName',
                             'Proportion' = 'Estimate',
                             'Lower Bound' = 'LowerCI',
                             'Upper Bound' = 'UpperCI',
                             'Program Status' = 'program_status'))
    
    data_table
  })
  
  # Downloadable csv of data table
  output$download_data <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  
  #Graph output
  output$plot1 <- renderPlot ({
    
    data <- data()
    
    proportion <- data$Estimate
    reason <-  data$IndicatorName
    upperCI <-  data$UpperCI
    lowerCI <-  data$LowerCI
    #ssize <-  data_to_graph$SampleSize ----- this IS not be needed for this graph type because sample size is in legend
    program_status <-  unique(data$program_status)
    
    
    #Create chart title and axis labels
    #Change the terms inside the "" here to change them in the chart
    title <- "Proportion of children by program status"
    proportionlabel <- "Proportion of children reporting this reason"
    reasonlabel <- ""
    
    #Labels  
    data$labels <- as.character(round(proportion,1))
    
    ggplot(data, aes(x=forcats::fct_rev(reorder(reason, reason)), y=proportion, linetype= NULL, group=program_status)) +   
    
    #These are formatting pieces that should be the same for every bar chart, or require at most minimal change 
    labs(y = proportionlabel,
         x = reasonlabel) +
    ggtitle(title) +
    
    geom_bar(stat = "identity", aes(fill = program_status), position = position_dodge(), width = 0.8) +
    coord_flip() + 
    geom_errorbar(aes(ymin = lowerCI, ymax =  upperCI), color = "#3f444c", width = 0.3,  position = position_dodge(width = .8)) +
    
    #This code labels the bars. Can use this version if the % label is not overlapping with the error bar.
    #geom_text(aes(label = paste0(labels,"%"), y = 1), 
    #          size = 2.8, hjust = 0, position = position_dodge(width = .9), color = "#3f444c") +
    
    geom_text(aes(label = paste0(labels,"%"), y = upperCI + 2), 
              size = 2.8, hjust = 0, position = position_dodge(width = .8), color = "#3f444c") +   
    
    theme_classic() +
    #Another theme option- but classic seems best for this
    #theme(panel.background = element_rect(fill = "white", color = "grey")) +
    theme(legend.position = c(0.8, 0.2)) + 
    theme(plot.title = element_text(size = "10", face = "bold")) +
    scale_y_continuous(limits = c(0,100)) +
    theme(plot.title = element_text(size = "11", face = "bold", hjust = 0.5)) +
    theme(axis.title.x = element_text(size = 11)) +
    
    #scale_fill_manual(values =  colours_bar, name = "TA status", labels = c(NoTA_label, TA_label))
    scale_fill_manual(values =  c("#588da8", "#b2ebf2"), name = "Program status", labels = program_status)  
    
  })
  
  
  

}

shinyApp(ui, server)
