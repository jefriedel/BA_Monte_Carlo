#Monte Carlo Shiny App for behavior analytic data
#Code written by Jonathan E. Friedel
#https://orcid.org/0000-0002-1516-330X
#email: jfriedel@georgiasouthern.edu

library(shiny)
library(shinyjs)
#library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(glue)
library(janitor)
library(rhandsontable)
library(shinycssloaders)

options(shiny.reactlog = FALSE)

source("./scripts/prod_load_files.R")

source("./scripts/prod_helper_functions.R")


{ #Bracket for UI
ui = 
    
  fluidPage(#theme = "GS_bootstrap.css",
    
    useShinyjs(),
    
    # # tags$head(pp
    # tags$style(HTML("
    #   h2 {
    #     line-height: 1.1;
    #     margin-top: 0px;
    #   }"))), #CSS 
    
    titlePanel("Monte Carlo Analysis for Single-Subject Experimental Designs"),
            
            navlistPanel(
              "Data",
              widths = c(3,9),
              
              #Row for data controls
              {tabPanel("Import",
                column(12, #Column for spacing
                  fluidRow(
                    h2("Import Data"),
                    
                    p("The app requires that the data be in a \"Long Format.\" This means that
                      the app expects only one measure of responding per row. Other factors, such
                      as subject number or condition, should be repeated for each row of data.",
                      tags$em("Load the example data to see the expected format.")),
                    p("The app also expects a file that includes columns for subject/participant, session 
                      numbers, and respoding. If you do not need a subject/participant identifier, just
                      add a dummy code to your data file. Session number is necessary to plot your data, if 
                      your data uses something other than sessions (i.e., date), you must recode 
                      that into session numbers."),
                    p("Finally, the data must be in a comma-seperated file format (.csv)."),
                    
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
                             icon = icon("cloud-download-alt", lib = "font-awesome")),
                tags$br(), tags$br(),
                
                p(tags$b("Note: "),"After you load your data or use the example data
                         set, the data will appear on the next tab (View & Modify)"),
                
                tags$br(),tags$br(),tags$br(),tags$br()
                
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
                      
                      ,
                      
                      tags$br(),tags$br(),tags$br(),tags$br()
                      
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
                  is a trend in the data, then we recommend you consider using ",
                  tags$i("log"), "proportion responding."),
                
                p(tags$i("log"), "Proportion responding as a measure is great for detecting changes
                  in behavior. It provides you with a measure of behavior on session B as a proportion 
                  of session A. By looking at the logarithm of the proportion, increases in behavior 
                  have the same impact on our measure as decreases in behavior. For example, the
                  standard celeration chart uses logrithms on the y-axis. See  
                  Friedel et al. (2019) for a full description of ",
                  tags$i("log"), " proportion responding."),
                
                p("The app separates the calcualtion of",tags$i("log"), " proportion responding
                from the Monte Carlo simulation. You can use the app to calculate", 
                tags$i("log"), " proportion responding, conduct a Monte Carlo analysis, or do both.
                For that reason, if you want to calculate",tags$i("log"), " proportion responding
                and use it in the Monte Carlo analysis then you must go back to the \"View & Modify
                \" tab and change the \"Responding column\" to the newly calculated \"log prop.
                resp.\" column."),
                
                
                p(tags$b("Warning: "),"If you have more than one measure of behavior per
                  session (such as Behavior A and Behavior B for each session) then
                  the app will not calculate ",tags$i("log"), " proportion responding
                  correctly. We recommend you calcluate the measure outside of the app
                  and then upload the data.")
                
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
                           ),
                       tags$br(),tags$br(),tags$br(),tags$br(),
                       ),
                column(8,
                       tableOutput("log_prop_display")),
              
                
                tags$br(),tags$br(),tags$br(),tags$br()
                
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
                            a different Monte Carlo Analysis for each \"Group\". Thus, by 
                           selecting \"Group\" as a grouping factor you will get a
                           seperate Monte Carlo analysis for each unique group in the data set 
                           and the results of each analysis will be independent of one another."),
                         
                         selectInput(inputId = "group_select",
                                     label = "Select grouping factor",
                                     choices = NULL,
                                     width = "100%"),
                         p(tags$b("Note: "),"The sample selection (to the right) can be difficult to use. If
                           you are struggling to highlight the proper sample using this tool, then
                           consider including an extra column in your data that indicates what
                           data should be in the sample of interest. If you upload the updated data 
                           file then you can just use the new column to select/filter your data.")
                         
                         ),#Setup column
                  
                  column(6,
                         
                         p("Use the boxes below to select filters for the data you want included in your
                                  \"real\" sample. When completed, click he \"Update Filter\" button below,
                                  which will highlight the selected data in the plot. "),
                         
                         uiOutput(outputId = "var_filters"),
                         
                         fluidRow(div(actionButton(inputId = "filter_update",
                                                   label = "Update Filter",
                                                   icon = icon(name = "filter", lib = "font-awesome")),
                                      style = "float: right; margin-right: 10px;
                                               margin-bottom: 10px;")),
                         
                         tags$br(),tags$br(),tags$br(),tags$br()
                         
                         ) #Var list column
                  
                  
                  
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
                  )),#Top column/row
                  
                  fluidRow(
                    column(12,
                           p(""),
                    p("The \"Value for randomization\" box below displays a value that is used to produce
                    the random samples for the Monte Carlo Simulation. You can use the value that was supplied when
                    the app was loaded. If you use the same value, the Monte Carlo will give you the same result.
                    Therefore, if you select your own value or re-use a previous value
                    you can replicate your Monte Carlo outcome."),
                  
                  numericInput(inputId = "seed_val",
                               label = "Value for randomization",
                               min = 1,
                               max = .Machine$integer.max,
                               value = sample.int(.Machine$integer.max,
                                                  1))
                  
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
                    \"MC results\" tab, you will find a figure and on the
                                \"Downloads\" tab a method to export the data.")
                  )),
                  style = "margin-top: 8px;"), #Summary explanation
                
                tags$br(),tags$br(),tags$br(),tags$br())
              
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
                       p("When you have completed the Monte Carlo analysis a figure will be displayed (by groups, if 
                       groups were specified). The figure(s) will show a histogram of the means for each of the 1,000 samples that
                       was simulated by the Monte Carlo Analysis.  The color for each bar 
                         are only included so that you can easily distinguish adjacent bars on the
                         histogram because the figure may appear small."),
                       p("The dashed black line shows you the mean of your \"real\" data. If the dashed line is
                       in the center of the distribution, then that indicates that the Monte Carlo analysis could easily
                       simulate data that looks similar to your \"real\" data. In typical statistical langauge, your
                       \"real\" data are not statistically significant."),
                       p("If the dashed line is at the extreme end of the distributions, then that indicates that
                       the Monte Carlo analysis was not able to reliably simulate data that looks similar to your \"real\" data. 
                       In typical statistical langauge, your \"real\" data are statistically significant."),
                       tags$br(),tags$br(),tags$br(),tags$br()
                       
                )

                        
                
              )},#MC Results
              
              {tabPanel("Downloads",
                h2("Download MC Files"),
                column(12,
                       p(tags$b("Note:")," the default name of the download files will contain
                         the value used for randomization (labeled as RV in file name) so
                         that you can replicate your results."),
                       p("Click the \"Download Figure\" button if you would like
                       a copy of the histogram on the \"MC Results\" panel."),
                       
                       downloadButton("dwn_fig",
                                       label = "Download Figure"),
                       p(""),
                       
                       p("Click the \"Download Data\" figure if you would like to
                       download a full copy of the means of each simulated sample. Due
                       to limitations of space on the server you can only download the
                       means of the samples. The specific values that were randomly
                       selected by the Monte Carlo for each sample are not saved."),
                       
                       downloadButton("dwn_dat",
                                      label = "Download Monte Carlo data"),
                       
                       tags$br(),tags$br(),tags$br(),tags$br()
                       ) #Column
                )
                }, #MC Download
              
              "Notes",
              
              {tabPanel("Program Information",
                h2("Program Information"),
                column(12,
                       fluidRow(
                         h3("Monte Carlo Analysis for Single-Subject
                            Experimental Designs"),
                         p("Jonathan E. Friedel, Ph.D."),
                         tags$a(p(img(src = "ORCIDiD_icon24x24.png"),
                           "https://orcid.org/0000-0002-1516-330X"),
                           href = "https://orcid.org/0000-0002-1516-330X"),
                         tags$a("jfriedel@georgiasouthern.edu",
                                href = "mailto:jfriedel@georgiasouthern.edu?subject=Monte Carlo App"),
                         h2("Links"),
                         tags$a("Files archived at the time of manuscript publication",
                                href = "https://osf.io/gqtxz/"),
                         p(""),
                         tags$a(p("GitHub repository"),
                                href = "https://github.com/jefriedel/BA_Monte_Carlo"),
                         
                         h2("References"),
                         div(p("Friedel, J.E., Galizio, A., Berry, M.S., Sweeney, M.M.,
                               & Odum, A.L. (2019). An alternative approach to relapse
                               analysis: Using Monte Carlo methods and proportional
                               rates of response.", tags$i("Journal of the Experimental
                               Analysis of Behavior,"),tags$i("111"),"(2),
                               289-308. ",tags$a("https://doi.org/10.1002/jeab.489",
                                                 href = "https://doi.org/10.1002/jeab.489")),
                             style = "text-indent: -36px; padding-left: 36px;"),
                         h2("Usage Requirements"),
                         h3("Disclaimer"),
                         p("This Monte Carlo simulation is intended
                           for informational purposes only.  The use of 
                           confidential information is strictly prohibited when 
                           running simulations. As a user, you are responsible 
                           for differentiating confidential information from 
                           non-confidential information."),
                         h3("Confidential includes but is not limited to:"),
                         p(tags$ul(
                          tags$li("Name"),
                          tags$li("Date of Birth"),
                          tags$li("Social Security Number"),
                          tags$li("Phone Number"),
                          tags$li("Any other personally identifying information")
                         ))
                         
                           
                         
                       )#rOW
                       ,tags$br(),tags$br(),tags$br(),tags$br()
                       )#Column
                )} #Author information
              
            )#Nav

  ) #End of fluidpage

}#UI            

server = function(input, output, session) {
  
  curr_data = reactiveValues()
  
  sim_count = 10000
  
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
    
    curr_data$filter = NA
    curr_data$MC_out = NA
    
      
  })
  
  #Create reactive rhandsontable, special "empty" table if no data loaded
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
      if(!any(is.na(curr_data$filter))){
      enable("run_MC")
      }

    }else{
      disable("log_prop_calc")
      disable("run_MC")
      disable("dwn_fig")
    }
    
    
    if(!any(is.na(curr_data$MC_out))){
      enable("dwn_fig")
      enable("dwn_dat")
    }else{
      disable("dwn_fig")
      disable("dwn_dat")
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
    
    curr_data$filter = NA
    curr_data$MC_out = NA
    
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
    
    curr_data$filter = NA
    curr_data$MC_out = NA
    
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
                       "log Prop. Resp.")
    
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
      !any(is.na(curr_data$MC_out)),
      "Select a filter and run the Monte Carlo to display a summary of results."
    ))
    

    curr_data$MC_out$sim_analysis
    
    
    
  })  
  
  output$MC_results = renderPlot({
    
    validate(need(
      !any(is.na(curr_data$MC_out)),
      "Select a filter and run the Monte Carlo to display results."
    ))
    
    curr_data$MC_fig = MC_out_plotter(MC_data = curr_data$MC_out,
                                      MC_grouping = input$group_select)
    
    curr_data$MC_fig
    
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
        
        if(make_clean_names(input$sess_select) %in% 
           colnames(curr_data$filter)){
          
          curr_data$filter = curr_data$filter %>%
            mutate(!!make_clean_names(input$sess_select):=
                     as.integer(!!as.symbol(make_clean_names(input$sess_select))))
        }
        
      }else{
        #If the list is empty, return NA
        curr_data$filter = NA
      }
      
    }#check if data exists
    
  })#Full observe

observeEvent(input$run_MC,{
  
  #Erase data for updating
  curr_data$MC_out = NULL
  
  #showNotification(input$group_select)
  if(!any(is.na(curr_data$filter))){
    
    removeNotification(id = "no_filter_msg")
  #Function output works, but nothing is output to the app currently
  curr_data$MC_out = MC_func(MC_data = curr_data$data,
          MC_responses = curr_data$behv,
          MC_filter = curr_data$filter,
          MC_grouping = input$group_select,
          MC_simulations = sim_count,
          MC_seed = as.integer(input$seed_val))
  }else{
    showNotification("You must identify the comparison data
                     on the \"sample selection\" tab.",
                     type = "error",
                     id = "no_filter_msg",
                     duration = NULL)
  }
})

#Download buttons
{
output$dwn_fig = downloadHandler(
  
  filename = function() {
    glue("MC figure RV_{input$seed_val}.png")
  },
  
  
  content = function(file) {
    ggsave(file, 
           plot = curr_data$MC_fig,
           device = "png",
           width = 7,
           height = 4,
           units = "in",
           dpi = 200)
  }
  
)
  
  output$dwn_dat = downloadHandler(
    
    filename = function() {
      glue("MC output data RV_{input$seed_val}.csv")
    },
    
    
    content = function(file) {
      write_csv(curr_data$MC_out$sim_data,
                file = file)
    }
    
  )
  
}
}#Server function

shinyApp(ui, server)
