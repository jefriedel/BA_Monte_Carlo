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

ui =
  fluidPage(titlePanel("Monte Carlo for SCED"),
            
            navlistPanel(
              #Header
              "Data",
              
              #Panel for import
              {
                tabPanel(
                  "Import",
                  
                  #Data table
                  rHandsontableOutput("hot_curr_data"),
                  #Spacer
                  h2(""),
                  
                  #Row for inputs
                  
                  fluidRow(#Right column
                    column(
                      6,
                      
                      #Row for specifying X, Y, subject
                      {fluidRow(
                        h2("Specify Columns"),
                        
                        #Input for selecting responses
                        selectInput(
                          "behv_select",
                          "Column with responding",
                          choices = col_descript,
                          selected = "Responses",
                          width = "100%"
                        ),
                        
                        #Input for selecting sessions
                        selectInput(
                          "sess_select",
                          "Column with sessions",
                          choices = col_descript,
                          selected = "Session Number",
                          width = "100%"
                        ),
                        
                        #Input for subject
                        selectInput(
                          "sub_select",
                          "Column with participant/subject",
                          choices = col_descript,
                          selected = "Subject Number",
                          width = "100%"
                        )
                        
                      )}#Row for specifying X, Y, subject),
                      
                    ), #Lefthand column
                      
                  {column(
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
                  )}#End of rename column
                  ),# End of input row

                    #Row for data controls
                    {fluidRow(
                      h2("Data controls"),
                      
                      #Column for import buttons
                      column(4,
                        actionButton("update", "Update", width = "100%", style = "margin-bottom: 1em;")),
                      column(4,
                      actionButton(
                        "rename_col",
                        "Column Rename",
                        width = "100%",
                        style = "margin-bottom: 1em;"
                      )),
                      column(4,
                      actionButton("reset", "Example Data", width = "100%", style = "margin-bottom: 1em;")
                    )
                      
                    )} #Row for data controls
                    
                  
                )#End of import panel
                
              },

              
              #Panel for plot and data filtering
              {tabPanel(
                "Selection for Monte Carlo",
                plotOutput("data_input_plot",
                           width = "100%"),
                fluidRow(uiOutput(outputId = "var_filters"))
              )}#End of panel for plot
              
              
            )#Navlist Panel
            
  ) #End of fluidpage

            
            
            server = function(input, output, session) {
              #Create ractive data for handsontable
              curr_data = reactiveValues(data = BA_MC_data)
              
              output$data_input_plot = renderPlot(expr = {
                data_selection_plotter(curr_data$data,
                                       make_clean_names(input$behv_select),
                                       make_clean_names(input$sess_select),
                                       make_clean_names(input$sub_select))
              })
              
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
                col_descript = c(
                  "Condition",
                  "Session Number",
                  "Responses",
                  "Subject Number",
                  "Experimental Group"
                )
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
                if (sum(col_descript == new_name) > 0) {
                  new_name = paste0(new_name, 2)
                  
                }
                
                col_descript[col_descript == old_column] <<-
                  new_name
                # curr_cols$data = col_descript
                
                #clean new name
                new_name = make_clean_names(new_name)
                
                #Change names in tibble
                curr_data$data = curr_data$data %>%
                  rename(!!as.symbol(new_name) := !!as.symbol(make_clean_names(old_column)))
                
                #Re-render table with new names
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
              
              
              #Creates a variable number of filters based on what datat is included
              output$var_filters =
                renderUI(#Loop through data columns
                  lapply(1:length(col_descript),
                         function(i) {
                           
                           if(!(col_descript[i] %in% c(input$behv_select))){
                           
                           #Get list of items in the current filter
                           curr_opts = BA_MC_data %>%
                             pull(!!as.symbol(make_clean_names(col_descript[i]))) %>%
                             unique()
                           
                           #create individual inputs
                           checkboxGroupInput("input_i",
                                              col_descript[i],
                                              choices =  curr_opts,
                                              selected = curr_opts[1:length(curr_opts)])
                           }#If..then to skip over response rate
                         }))#Variable inputs for filtering data
              
            }#Server function
            
            runApp(list(ui = ui, server = server))