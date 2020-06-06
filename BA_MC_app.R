library(shiny)
library(tidyverse)
library(janitor)
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

colnames(BA_MC_data) = col_descript

BA_MC_data = BA_MC_data %>% clean_names

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
                 
                 #Create ractive data for handsontable
                 curr_data = reactiveValues(data = BA_MC_data)

                 #Initial render of table
                 output$hot_curr_data = renderRHandsontable({
                   rhandsontable(curr_data$data,
                                 colHeaders = col_descript,
                                 height = 400) %>%
                     hot_cols(manualColumnResize = TRUE)
                 })
                 
                 #Update data in table
                 observeEvent(input$update, {
                   curr_data$data = hot_to_r(input$hot_curr_data)
                 })
                 
                 #Reload MC data
                 observeEvent(input$reset, {
                   
                   #Reset data
                   curr_data$data = BA_MC_data
                   
                   #Relabel columns
                   col_descript = c("Condition",
                                    "Session Number",
                                    "Responses",
                                    "Subject Number",
                                    "Experimental Group")
                   #Rename columns
                   colnames(BA_MC_data) = col_descript
                   
                   #Render table
                   output$hot_curr_data = renderRHandsontable({
                     rhandsontable(curr_data$data,
                                   colHeaders = col_descript,
                                   height = 400) %>%
                       hot_cols(manualColumnResize = TRUE)
                     
                   }) #Render table
                   
                   #Update lists
                   updateSelectInput(session,
                                     "col_rename_input",
                                     choices = col_descript)
                   
                 })
                 
                 #Rename columns
                 observeEvent(input$rename_button, {
                   
                   old_column = input$col_rename_input
                   new_name = input$new_col_name
                   
                   #Check if the name exists
                   if(sum(col_descript==new_name) >0){
                     
                     new_name = paste0(new_name,2)
                     
                   }
                   
                   col_descript[col_descript==old_column] <<- new_name
                   # curr_cols$data = col_descript
                   
                   #clean new name
                   new_name = make_clean_names(new_name)
                   
                   #Change names in tibble
                   curr_data$data = curr_data$data %>%
                     rename(!!as.symbol(new_name) := !!as.symbol(make_clean_names(old_column)))
                   
                   #Rerender table with new names
                   output$hot_curr_data = renderRHandsontable({
                     rhandsontable(curr_data$data,
                                   colHeaders = col_descript,
                                   height = 400) %>%
                       hot_cols(manualColumnResize = TRUE)
                   })#Table render
                   
                   #Update lists
                   updateSelectInput(session,
                                    "col_rename_input",
                                    choices = col_descript)
                   
                   #Clear name
                   updateTextInput(session,
                                   "new_col_name",
                                   value = "")

                   })#Rename button
                   
                 
               }#Server function
               
               runApp(list(ui = ui, server = server))