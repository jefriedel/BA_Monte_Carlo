library(shiny)
library(shinyjs)
library(tidyverse)
library(janitor)
library(rhandsontable)
library(shinycssloaders)

options(shiny.reactlog = TRUE)

source("./scripts/load_files.R")

source("./scripts/helper_functions.R")

{ #Bracket for UI
ui = 
    
  fluidPage(
    
    useShinyjs(),
    
    # # tags$head(pp
    # tags$style(HTML("
    #   h2 {
    #     line-height: 1.1;
    #     margin-top: 0px;
    #   }"))), #CSS 
    
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
                  p(tags$em("It is possible to modify the data displayed above,
                            but it is not recommended."),
                    "If you modify  values on the data table, click the \"Update\"
                    button to make sure the new data is loaded into to the app."),
                  
                  div(actionButton("update",
                                   "Update"),
                      style = "padding-bottom: 10px;"),
                  
                  #Column for data controls
                  
                  #Row for inputs
                  
                  fluidRow(
                    column(12,
                    column(#Right column
                      12,# Bumped up from size six after hiding rename columns
                      
                      #Row for specifying X, Y, subject
                      {fluidRow(
                        h2("Select Columns"),
                        
                        p("In the drop menus below, you must select the columns that
                          are for \"responding\"\\behavioral data, the session
                          indicator, and subject\\participant identifier. The menus 
                          will only function once data has been loaded into the app."),
                        
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
                    
                    # {column(
                    #   6,
                    #   h2("Rename Columns"),
                    #   selectInput(
                    #     "col_rename_input",
                    #     "Column to Rename",
                    #     choices = NULL,
                    #     width = "100%"
                    #   ),
                    #   textInput("new_col_name",
                    #             "New Column Name", width = "100%"),
                    #   div(actionButton("rename_button",
                    #                    "Rename"), style = "float: right;"),
                    #   helpText("If you manually rename columns, the app will 
                    #            reset any other column selections you made.")
                    # )}#End of rename column
                    
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
                  standard celeration chart uses logrithms on the y-axis. See ",
                  tags$b("INSERT CITATION HERE"), " for a full description of ",
                  tags$i("log"), "proportion responding.")
                
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
                         
                         p("Select a grouping factor if you want different Monte Carlo
                            analyses based on that grouping. If no grouping factor is 
                            selected, then there will be only one sample. 
                            For example, with the Example Data you may wish to have 
                            a different Monte Carlo Analysis for each subject. Thus, by 
                           selecting \"Subject\" as a grouping factor you will get a
                           seperate analysis for each subject and the results will be
                           independent of one another."),
                         
                         selectInput(inputId = "group_select",
                                     label = "Select grouping factor",
                                     choices = NULL,
                                     width = "100%")
                         
                         ),#Setup column
                  
                  column(6,
                         
                         p("Use the boxes below to select filters for the data you want included in your
                                  \"real\" sample. When completed, click he \"Update Filter\" button below,
                                  which will highlight the selected data in the plot."),
                         
                         uiOutput(outputId = "var_filters"),
                         
                         fluidRow(div(actionButton(inputId = "filter_update",
                                                   label = "Update Filter",
                                                   icon = icon(name = "filter", lib = "font-awesome")),
                                      style = "float: right; margin-right: 10px;
                                               margin-bottom: 10px;"))) #Var list column
                  
                )#Filters row
                
              )#Sample selection panel
                
              },#Sample selection
              
              "Monte Carlo",
              
              #Start MC
              {tabPanel(
                "Run Monte Carlo",
                h2("Run Monte Carlo"),
                fluidRow(column(12,
                  p("To run the Monte Carlo simulation, you must:"),
                  tags$ul(tags$li("Upload data on the \"Import\" tab."),
                          tags$li("Indicate the columns that hold behavior, sessions, and
                                  participant/subject numbers on the \"View & Modify\" tab."),
                          tags$li("Select a filter on the \"Sample Selection\" tab.")),
                  
                  p("After you run a simulation, a plot will be displayed below with different panels
                  for each group (if there were any groups). The red line on the panel displays the
                  mean(s) for the data the \"real\" data that was selected based on your filter. The gray 
                    bars are a histogram of the samples of data that were simulated by the Monte Carlo."),
                  actionButton(inputId = "run_MC",
                               "Run Monte Carlo Simulation")
                )#Top column
                ),#Fluid row
                
                div(tableOutput("MC_main") %>% 
                      withSpinner(color="#001344",
                                  color.background = "#FFFFFF",
                                  size = 3,
                                  type = 2),
                    style = "margin-top: 8px;"),
                
                fluidRow(
                  div((column(12,
                              p("A basic summary table will be displayed to show that the script is complete. On the
                    \"MC results\" tab, you will find a figure and method to export the data.")
                  )),
                  style = "margin-top: 8px;")) #Summary explanation
                
              
              )},#Start MC panel
              
              {tabPanel(
                "MC Results",
                
                h2("Monte Carlo Results"),
                
                h3("Figure"),
                
                column(12,plotOutput("MC_results",
                                     width = "100%",
                                     height = "400px") %>% 
                         withSpinner(color="#001344",
                                     color.background = "#FFFFFF",
                                     size = 3,
                                     type = 2)
                       ,
                       p("When you have completed the Monte Carlo analysis a figure will be displayed(by groups, if 
                       groups were specified). The figure(s) will show a histogram of the means for each sample that
                       was simulated by the Monte Carlo Analysis. For example, if you specified 500 simulations,
                       then the histogram will include 500 simulated means. The color for each bar 
                         are only included so that you can easily distinguish adjacent bars on the
                         histogram because the figure may appear small."),
                       p("The dashed black line shows you the mean of your \"real\" data. If the dashed line is
                       in the center of the distribution, then that indicates that the Monte Carlo analysis could easily
                       simulate data that looks similar to your \"real\" data. In typical statistical langauge, your
                       \"real\" data are not statistically significant."),
                       p("If the dashed line is at the extreme end of the distributions, then that indicates that
                       the Monte Carlo analysis was not able to reliably simulate data that looks similar to your \"real\" data. 
                       In typical statistical langauge, your \"real\" data are statistically significant.")
                )

                        
                
              )}#MC Results
              
            )#Nav

  ) #End of fluidpage

}#UI            

server = function(input, output, session) {
  
  curr_data = reactiveValues()
  
  curr_data$filter = NA
  curr_data$MC_out = NA
  
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
      "col_rename_input",
      "group_select"
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

  #Update data column
  observeEvent(input$update, {
    
    curr_data$data = hot_to_r(input$data_display)
    
  })
  
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
    
    validate(need(
      (!is.null(curr_data$data) &
         curr_data$behv !="" &
         curr_data$sess != "" &
         curr_data$sub != ""),
      "Please select data to display plot."
    ))
    
    data_selection_plotter(curr_data$data,
                           curr_data$behv,
                           curr_data$sess,
                           curr_data$sub,
                           curr_data$filter)
    
  })
  
  #Section for filter lists
  {
    #Combines:
    #https://stackoverflow.com/questions/45040598/issues-accessing-inputs-from-renderui-in-r-shiny
    #https://stackoverflow.com/questions/51700437/dynamic-number-of-selectinput
    
    output$var_filters = 
      
      renderUI({#Loop through data columns
        
        validate(need(
          !is.null(curr_data$data),
          "Please select data to display filters"
        ))
        
        lapply(1:length(curr_data$col_descript),
               function(i) {
                 if (!(curr_data$col_descript[i] %in% c(input$behv_select))) {
                   #Get list of items in the current filter
                   curr_opts = curr_data$data %>%
                     arrange(!!as.symbol(make_clean_names(curr_data$col_descript[i]))) %>%
                     pull(!!as.symbol(make_clean_names(curr_data$col_descript[i]))) %>%
                     unique()
                   
                   #create individual inputs
                   selectizeInput(
                     inputId = paste0("filter_", make_clean_names(curr_data$col_descript[i])),
                     curr_data$col_descript[i],
                     choices =  curr_opts,
                     selected = NULL,
                     multiple = TRUE
                   )
                 }#If..then to skip over response rate
               })
        }) #render UI
  } # Full brace
  
  output$MC_main = renderTable({
    
    validate(need(
      !is.na(curr_data$MC_out),
      "Run the Monte Carlo to display a summary of results."
    ))
    

    curr_data$MC_out$sim_analysis
    
    
    
  })  
  
  output$MC_results = renderPlot({
    
    validate(need(
      !is.na(curr_data$MC_out),
      "Run the Monte Carlo to display results."
    ))
    
    MC_out_plotter(MC_data = curr_data$MC_out,
                    MC_grouping = input$group_select)
    
  })
  
observeEvent(input$filter_update,{
    
    if(!is.null(curr_data$data)){
      
      input_list = c()
      output_list = list()
      
      for(j in 1:length(curr_data$col_descript)){
        
        #Get current name and add to list of names
        curr_input = make_clean_names(curr_data$col_descript[j])
        input_list = c(input_list,curr_input)
        
        #If nothing is selected in the filter, exclude from list
        if(length(input[[paste0("filter_", curr_input)]]) != 0) {
          output_list[[curr_input]] =
            input[[paste0("filter_", curr_input)]]
        }
        

        
        }#Internal loop function
      
      #Check if the output list contains anything
      if(length(output_list)!=0){
        #Create included variable grid, add include
        curr_data$filter = cross_df(output_list)%>%
          mutate(data_color = "Include")
      }else{
        #If the list is empty, return NA
        curr_data$filter = NA
      }
      
    }#check if data exists
    
  })#Full observe

observeEvent(input$run_MC,{
  
  #Erase data for updating
  curr_data$MC_out = NULL
  
  showNotification(input$group_select)
  if(!is.na(curr_data$filter)){
    
    removeNotification(id = "no_filter_msg")
  #Function output works, but nothing is output to the app currently
  curr_data$MC_out = MC_func(MC_data = curr_data$data,
          MC_responses = curr_data$behv,
          MC_filter = curr_data$filter,
          MC_grouping = input$group_select,
          MC_simulations = 500,
          MC_seed = 1)
  }else{
    showNotification("You must identify the comparison data
                     on the \"sample selection\" tab.",
                     type = "error",
                     id = "no_filter_msg",
                     duration = NULL)
  }
})
  
}#Server function

shinyApp(ui, server)
