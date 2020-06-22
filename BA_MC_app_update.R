library(shiny)
library(tidyverse)
library(janitor)
library(rhandsontable)

options(shiny.reactlog = TRUE)

source("./scripts/load_files.R")

{ #Bracket for UI
ui = 

    
  fluidPage(
    
    tags$head(
    tags$style(HTML("
      h2 {
        line-height: 1.1;
        margin-top: 0px;
      }"))), #CSS 
    
    titlePanel(h1("Monte Carlo for SCED")),
            
            navlistPanel(
              "Data",
              
              #Row for data controls
              {tabPanel("Import",
                column(12, #Column for spacing
                  fluidRow(
                    h2("Import Data"),
                    
                    p("The app requires that the data be in a \"Long Format.\" This means that
                      the app expects only one measure of responding per row. Other factors, such
                      as subject number or condition, should be repeated for each row of data.
                      Load the example data to see the expected format."),
                    p("The app also expects a file that includes columns for subject/participant, session 
                      numbers, and respoding. If you do not need a subject/participant identifier, just
                      add a dummy code to your data file. Session number is necessary to plot your data, if 
                      your data uses something other than sessions (i.e., date), you must recode 
                      that into session numbers."),
                    p("Finally, the data must be in a comma-seperated file format (.csv)"),
                    
                    fileInput("new_file",
                              label = "Select .csv File to Upload",
                              accept = c(".csv"),
                              multiple = FALSE),
                      
                    
                    
                    #actionButton("load_new","Load New Data",
                    #             icon = icon("file-upload", lib = "font-awesome")),
                    
                    
                h2("Example Data"),
                
                p("Click the \"Example Button\" below to reset to table back to the included
                      example data."),
                
                actionButton("reset", "Example Data",
                             icon = icon("cloud-download-alt", lib = "font-awesome"))
                )#Row
                
                ) #COlumn for spacing
              )}, #Import panel
              
              #View and update panel
              { 
                tabPanel(
                  "View & Modify",
                  h2("Data Table"),
                  
                  #Data table
                  div(rHandsontableOutput("data_display",
                                      height = 300),
                      style = "margin-bottom: 10px;"),
                  p("If you manually change values on the data table, click 
                      to update the data to ensure it loads properly into the app."),
                  
                  div(actionButton("update",
                                   "Update"),
                      style = "padding-bottom: 10px;"),
                  
                  #Column for data controls
                  
                  #Row for inputs
                  
                  fluidRow(
                    column(12,
                    column(#Right column
                      6,
                      
                      #Row for specifying X, Y, subject
                      {fluidRow(
                        h2("Specify Columns"),
                        
                        #Input for selecting responses
                        selectInput(
                          "behv_select",
                          "Column with responding",
                          choices = NULL,
                          width = "100%"
                        ),
                        
                        #Input for selecting sessions
                        selectInput(
                          "sess_select",
                          "Column with sessions",
                          choices = NULL,
                          width = "100%"
                        ),
                        
                        #Input for subject
                        selectInput(
                          "sub_select",
                          "Column with participant/subject",
                          choices = NULL,
                          width = "100%"
                        ),
                        
                      )}#Row for specifying X, Y, subject),
                      
                    ), #Lefthand column
                    
                    {column(
                      6,
                      h2("Rename Columns"),
                      selectInput(
                        "col_rename_input",
                        "Column to Rename",
                        choices = NULL,
                        width = "100%"
                      ),
                      textInput("new_col_name",
                                "New Column Name", width = "100%"),
                      div(actionButton("rename_button",
                                       "Rename"), style = "float: right;")
                    )}#End of rename column
                    
                    )#Cxtra column for spacing
                  )# End of input row
                )} #End of modify
              
            )#Nav

            
  ) #End of fluidpage

}#UI            

server = function(input, output, session) {
  
  curr_data = reactiveValues()
  
  #File upload
  observe({
    
    req(input$new_file)
    
    temp_upload = read_csv(input$new_file$datapath, col_types = cols())
    
    mc_data$upload$col_names = colnames(temp_upload)
    
    mc_data$upload$data = temp_upload %>% clean_names()
    
    curr_data$data = mc_data$upload$data
    curr_data$col_descript = mc_data$upload$col_names
    
      
  })
  
  #Create reactive rhandsontable, special table if no data loaded
  observe({
    
    output$data_display = renderRHandsontable({
    
      if(is.null(curr_data$col_descript)){
        rhandsontable(
          tibble("Data Loaded" = "None")
        )
      }else{
      
      rhandsontable(curr_data$data,
                    colHeaders = curr_data$col_descript) %>% 
        hot_cols(manualColumnResize = TRUE,
                 colWidths = 100)
      }
      
    })
    
  })
  
  observe({
    
    input_list = c(
      "behv_select",
      "sess_select",
      "sub_select",
      "col_rename_input"
    )
    
    for(input in input_list){
      
      updateSelectInput(session,
                        input,
                        choices = c("",curr_data$col_descript),
                        select = "")
      
    }# Loop for inputs
    
  }) #observe for reactive select lists
  
  #Example data button
  observeEvent(input$reset,{
    
    curr_data$data = mc_data$example$data
    curr_data$col_descript = mc_data$example$col_names
    
  })
  
  #Rename column
  observeEvent(input$rename_button,{
    
    #Require inputs
    req(input$new_col_name,input$col_rename_input)
    
    #Create new variable, for ease
    new_name = input$new_col_name
    
    #Get the col descript column to edit
    curr_column = curr_data$col_descript==input$col_rename_input
    
    while_letter = 1
    while(any(colnames(curr_data$data)==make_clean_names(new_name))){
      
      #Rename if 
      new_name = paste0(new_name,LETTERS[while_letter])
      
    }
    
    rm(while_letter)
    
    curr_data$col_descript[curr_column] = new_name
    
    curr_data$data = curr_data$data %>%
      rename(!!as.symbol(make_clean_names(new_name)) := !!as.symbol(make_clean_names(input$col_rename_input)))
    
    # #REmove this line, just for testing
    curr_data$data
    
  })#Rename button

  
}#Server function

runApp(list(ui = ui, server = server))