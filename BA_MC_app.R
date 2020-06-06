library(shiny)
library(tidyverse)
library(rhandsontable)

BA_MC_data =
  read_csv("./data/Relapse Data 1.csv",
           col_types = cols()) %>%
  clean_names()

col_descript = c("Condition",
                 "Session Number",
                 "Responses",
                 "Subject Number",
                 "Experimental Group")

ui = fluidPage(titlePanel("Monte Carlo for SCED"),
               
               navlistPanel(
                 #Header
                 "Data",
                 
                 tabPanel(
                   "Import",
                   
                   #Data table
                   rHandsontableOutput("hot_curr_data"),
                   #Spacer
                   h2(""),
                   
                   fluidRow(
                     column(
                       6,
                       #Row for basic buttons,
                       h2("Data controls"),
                       actionButton("update", "Update", width = "100%", style = "margin-bottom: 1em;"),
                       actionButton(
                         "rename_col",
                         "Column Rename",
                         width = "100%",
                         style = "margin-bottom: 1em;"
                       ),
                       actionButton("reset", "Example Data", width = "100%", style = "margin-bottom: 1em;")
                     ),
                     column(
                       6,
                       h2("Rename Columns"),
                       selectInput(
                         "col_rename_input",
                         "Column to Rename",
                         choices = col_descript,
                         width = "100%"
                       ),
                       textInput("new_col_name",
                                 "New Column Name", width = "100%"),
                       div(actionButton("rename_button",
                                        "Rename"), style = "float: right;")
                     )
                   ),
                   #End of rename row
                 ),
                 #End of panel
                 
                 
                 
                 tabPanel("Selection for Monte Carlo")
               )#Navlist Panel)
)
               
               
               
               server = function(input, output, session) {
                 curr_data = reactiveValues(data = BA_MC_data)
                 
                 output$hot_curr_data = renderRHandsontable({
                   rhandsontable(curr_data$data,
                                 colHeaders = col_descript,
                                 height = 400) %>%
                     hot_cols(manualColumnResize = TRUE)
                 })
                 
                 observeEvent(input$update, {
                   curr_data$data = hot_to_r(input$hot_curr_data)
                 })
                 
                 observeEvent(input$reset, {
                   curr_data$data = BA_MC_data
                 })
                 
               }
               
               runApp(list(ui = ui, server = server))