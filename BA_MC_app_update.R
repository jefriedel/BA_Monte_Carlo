library(shiny)
library(shinyjs)
library(tidyverse)
library(janitor)
library(rhandsontable)

options(shiny.reactlog = TRUE)

source("./scripts/load_files.R")
source("./scripts/helper_functions.R")

{ #Bracket for UI
ui = 
    
  fluidPage(
    
    useShinyjs(),
    
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
                                       "Rename"), style = "float: right;"),
                      helpText("If you manually rename columns, the app will 
                               reset any other column selections you made.")
                    )}#End of rename column
                    
                    )#Cxtra column for spacing
                  )# End of input row
                )}, #End of modify panel
              
              #Log proportion
              {tabPanel(
                p(tags$i("log")," Prop. Responding"),
                
                column(12,
                h2(tags$i("log"), "Proportion Responding"),
                
                #Text should change based on article content
                p("If you are planning on using a Monte Carlo simulation to examine either
                  transient changes in behavior (sugh as a probe desgin) or whether there 
                  is a trend in the data, then we recommend you look at ",
                  tags$i("log"), "proportion responding."),
                
                p(tags$i("log"), "proportion responding as a measure is great for detecting changes
                  in behavior. It provides you with a measure of behavior on session B as a proportion 
                  of session A. By looking at the logarithm of the proportion, increases in behavior 
                  have the same impact on our measure as decreases in behavior. For example, the
                  standard celeration chart uses logrithms on the y-axis.")
                
                ),#Top with descriptions
                
                column(4,
                       
                       numericInput("log_base",
                                    "Base for the logarithm",
                                    value = 2,
                                    min = 2,
                                    step = 1),
                       p("Click the following button to calculate the natural logarithm (base ", 
                         tags$i("e).")),
                       
                       fluidRow(div(p(actionButton("base_e",
                                    label= paste("Press for base e"))),
                                    style = "float: right;")),
                       
                       div(("The \"Calculate\" button below will add the log proportion responding.
                       A few rows of the result will be displayed to the right for inspection. The full
                       set of values can be seen in the data table on the \"View & Modify\" panel.")),
                       
                       div(actionButton("log_prop_calc",
                                        label = " Calculate",
                                        icon = icon(name = "calculator", lib = "font-awesome")),
                           style = "float: right;"
                           )
                       ),
                column(8,
                       tableOutput("log_prop_display"))
                
                
              )},#Log proportion calculator
              
              #Sample Selection
              {tabPanel(
                "Sample Selection",
                h2("Sample Selection"),
                p("The app can not inherently distinguish between the \"real\" sample
                and the rest of the data. On this panel, you must use the inputs to filter
                the data so that only \"real\" sample is highlighted. If you have selected the
                column indicators on the \"View & Modify\" panel then a simple figure will
                  be displayed below. The data you have selected for including in 
                  the \"real\" sample will be highlighted as you update the filters."),
                
                column(12,
                       plotOutput("filter_plot",
                                  width = "100%",
                                  height = "400px")),
                fluidRow(
                  
                  #Filter setup
                  column(6,
                         
                         selectizeInput("test_selectize",
                                        label = "Testing Filters",
                                        choices = NULL,
                                        multiple = TRUE
                                        )
                         
                         )#Setup column
                )#Filters row
                

                
              )#Sample selection panel
                
              }#Sample selection
              
              
            )#Nav

            
  ) #End of fluidpage

}#UI            

server = function(input, output, session) {
  
  curr_data = reactiveValues()
  
  old_sess = ""
  old_behv = ""
  old_sub = ""
  
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
  
  #Hide buttons       
  observe({
    
    curr_data$sess = input$sess_select
    curr_data$sub = input$sub_select
    curr_data$behv = input$behv_select
    
    if (!(curr_data$behv=="") &
        !(curr_data$sub=="") &
        !(curr_data$sess=="")) {
      enable("log_prop_calc")
    } else{
      disable("log_prop_calc")
    }
    
  })
  
  
  observe({
    
    input_list = c(
      "behv_select",
      "sess_select",
      "sub_select",
      "col_rename_input"
    )
    
    for(input in input_list){
      
      temp = str_split(input,"_")
      
      if (temp[[1]][2] == "select") {
        
        updateSelectInput(session,
                          input,
                          choices = c("", curr_data$col_descript),
                          selected = eval(parse(text = paste0("curr_data$",
                                                       temp[[1]][1])))
                          )
      }else{
        
        updateSelectInput(session,
                          input,
                          choices = c("", curr_data$col_descript),
                          selected = "")
        
      }
      
    }# Loop for inputs
    
    rm(temp)
    
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

  #Log base e
  observeEvent(input$base_e, {
    
    updateNumericInput(session = session,
                       inputId = "log_base",
                       value = round(exp(1),3))
    
  })
  
  #Calculate log prop.
  observeEvent(input$log_prop_calc,{
  
    
    curr_data = log_prop_calc(full_data = curr_data,
                  responding = curr_data$behv,
                  sessions = curr_data$sess,
                  log_base = input$log_base,
                  grouping = curr_data$sub)
    
    subs = curr_data$data %>% 
      pull(!!as.symbol(make_clean_names(curr_data$sub))) %>%
      unique() 
    
    if(NROW(subs)>3){
      subs = subs[1:3]
    }
    
    subs = 
      curr_data$data %>% 
      filter(!!as.symbol(make_clean_names(curr_data$sub)) %in% subs) %>%
      group_by(!!as.symbol(make_clean_names(curr_data$sub))) %>%
      slice(1:4) %>% 
      select(!!as.symbol(make_clean_names(curr_data$sub)),
             !!as.symbol(make_clean_names(curr_data$sess)),
             !!as.symbol(make_clean_names(input$behv_select)),
             log_prop_resp)
        
    colnames(subs) = c(curr_data$sub,
                       curr_data$sess,
                       input$behv_select,
                       "log Prob. Resp.")
    
    output$log_prop_display = renderTable(

      subs    
      
    )

  })
  
  output$filter_plot = renderPlot({
    
    ggplot(curr_data$data,
           aes(x = !!as.symbol(make_clean_names(curr_data$sess)),
               y = !!as.symbol(make_clean_names(curr_data$behv)))) +
      facet_wrap(vars(!!as.symbol(make_clean_names(curr_data$sub)))) + 
      geom_point() + 
      geom_line()
    
  })
  
  observe({
    
    updateSelectizeInput(session = session,
                         inputId = "test_selectize",
                         choices = mc_data$example$data %>%
                           pull(subject)%>%
                           unique(),
                         server = TRUE)
    
  })
  
  
}#Server function

runApp(list(ui = ui, server = server))