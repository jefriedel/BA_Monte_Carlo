library(shiny)
library(dplyr)
library(purrr)

filterex <- function(data = NULL) {
  i1 <-  data %>%
    summarise_all(is.factor) %>%
    unlist()
  dvars <- i1 %>%
    names(.)[.]
  rvars <- i1 %>%
    `!` %>%
    names(.)[.]
  
  filters <-dvars %>% 
    map(~list(inputId = ., 
              label = ., 
              choices = levels(data[[.]]), 
              selected = levels(data[[.]])))
  
  
  
  
  ui = fluidPage(
    titlePanel("Dynamic filtering example"),
    sidebarPanel(
      checkboxGroupInput(inputId = "design",
                         label = "Design Variables",
                         choices = dvars,
                         selected = dvars),
      map(filters, ~do.call(what = checkboxGroupInput, .))),
    mainPanel(dataTableOutput("data"))
  )
  
  
  
  server = function(input, output, session) {
    
    dat_subset <- reactive({
      df <-  data %>%
        select(input$design, rvars) 
      dvars %>% 
        map2_df(list(df), ~.y  %>%
                  filter_at(.x, all_vars(. %in% input[[.x]])))
      
      
      
    })  
    output$data <- renderDataTable({
      dat_subset()
    })
    
  }
  
  runApp(list(ui = ui, server = server))
}

filterex(iris)
