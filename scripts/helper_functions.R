#Data for testing plotting function
figure_data = BA_MC_data
resp_col = "responses"
sessions_col = "session_number"
subject_col = "subject_number"

color_criteria =
tibble(condition = c("Baseline","Extinction"),
       experimental_group = c("Sal_Sal","Amp_Sal")) %>%
  expand(condition, experimental_group) %>%
  mutate(data_color = "Include")

#Colors for plotting
inc_colors = c("red","black")
names(inc_colors) = c("Include","Exclude")

data_selection_plotter(figure_data = figure_data,
                       resp_col = resp_col,
                       sessions_col = sessions_col,
                       subject_col = subject_col)

#Function to plot data for seleecting filter
data_selection_plotter = function(figure_data,
                                  resp_col,
                                  sessions_col,
                                  subject_col,
                                  color_criteria = NA){
  
  #Join inclusion figure to data
  if(is.na(color_criteria)) {
    
    #If there is no filter, exclude everything
    figure_data = figure_data %>%
      mutate(data_color = "Exclude")
  } else{
    
    #Add include to what is included in the filter
    figure_data = left_join(figure_data,
              color_criteria) %>%
      
      #Exclude everything not on include
      replace_na(list(data_color = "Exclude"))
    
  }
  
  #Plot
  ggplot(figure_data,aes(x = !!as.symbol(sessions_col),
                       y = !!as.symbol(resp_col))) +
  theme_classic() +
  geom_line() + 
    geom_point(aes(color = data_color))+
    scale_color_manual(values = inc_colors) +
  facet_wrap(vars(!!as.symbol(subject_col))) +
  xlab("Session Number") + 
  ylab("Responding")
  
}

# #Variables for testing
# MC_data2 = BA_MC_data %>% 
#   group_by(subject_number) %>%
#   mutate(log_responses = log2((responses+1)/(lag(responses)+1))) %>%
#   ungroup()
# 
# MC_filter2 = tibble(condition = c("Reinstatement"),
#                    experimental_group = c("Sal_Sal","Amp_Sal")) %>%
#   expand(condition, experimental_group) %>%
#   mutate(MC_include = "Include")
# 
# MC_grouping2 = "experimental_group"
# MC_responses2 = "log_responses"
# 
# MC_simulations2 = 1000
# 
# MC_seed2 = 1

MC_func = function(MC_data,
                   MC_responses,
                   MC_filter,
                   MC_grouping,
                   MC_simulations,
                   MC_seed) {

#Turn in symbol so don't have to do it repeatedly
MC_responses = as.symbol(MC_responses)
MC_grouping = as.symbol(MC_grouping)

if(is.na(MC_filter)) {
  #If there is no MC_filter, don't run
  return()
  
} else{
  
  #Add include to what is included in the filter
  MC_data = left_join(MC_data,
                      MC_filter) %>%
    
    #Exclude everything not on include
    replace_na(list(MC_include = "Exclude"))
  
  #Means, SDs, counts for each group
  exp_data = MC_data %>% 
    filter(MC_include=="Include") %>%
    group_by(!!MC_grouping) %>%
    summarize(mean = mean(!!MC_responses,
                          na.rm = TRUE),
              sd = sd(!!MC_responses,
                      na.rm=TRUE),
              sample_size = n())
  
  #Create sim_data for looping
  sim_data = tibble()
  
  
  curr_group = (exp_data %>% pull(!!MC_grouping))
  curr_group = curr_group[1]

  sim = 1
    
  for(curr_group in (exp_data %>% pull(!!MC_grouping))) {
    #Filter the data once, to avoid repetitive filtering
    filtered_MC =
      MC_data %>%
      filter(!!MC_grouping == curr_group)
    
    curr_size = exp_data %>%
      filter(!!MC_grouping == curr_group) %>%
      pull(sample_size)
    
    for (sim in 1:MC_simulations) {
      
      
      sim_data = bind_rows(
        sim_data,
        filtered_MC %>%
          sample_n(size = curr_size,
                   replace = TRUE) %>%
          group_by(!!MC_grouping) %>%
          summarize(
            mean = mean(!!MC_responses,
                        na.rm = TRUE),
            sd = sd(!!MC_responses,
                    na.rm = TRUE)
          ) %>%
          mutate(run = sim)
      )
      
      
    } # Loop for current simulation
    
    #Remove to clear out memory
    rm(filtered_MC)
    
  } #Loop for current group
  
  return(sim_data)
  
}#If then to ensure that there is filtered data


#Tictoc on 1000 is ~2 seconds, 10,000 is ~20 seconds (as predicted)
#Tictoc was only run in local. To save server time. Limit app to 1000.

 } #Brace for end of MC function
# #Function test
# temp = MC_func(MC_data2,
#         MC_responses2,
#         MC_filter2,
#         MC_grouping2,
#         MC_simulations2,
#         MC_seed2)


