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

#Data for testing
MC_data = BA_MC_data %>% 
  group_by(subject_number) %>%
  mutate(log_responses = log2((responses+1)/(lag(responses)+1))) %>%
  ungroup()

MC_filter = tibble(condition = c("Reinstatement"),
                   experimental_group = c("Sal_Sal","Amp_Sal")) %>%
  expand(condition, experimental_group) %>%
  mutate(MC_include = "Include")

MC_grouping = "experimental_group"
MC_responses = "log_responses"

MC_simulations = 1000

MC_seed = 1

# MC_func = function(MC_data,
#                    MC_responses,
#                    MC_filter,
#                    MC_grouping,
#                    MC_simulations,
#                    MC_seed) {

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
  
  
  #Create tibble to get long form sample sizes
  samples_per_group = tibble()
  
      for (curr_group in (exp_data %>% pull(!!MC_grouping))) {
      
      #For each group
      samples_per_group =
        bind_rows(samples_per_group,
                  
                  #Expand current group against sample size
                  expand_grid(
                    !!MC_grouping := curr_group,
                    MC_samp  = 1:(exp_data %>% 
                                    filter(!!MC_grouping == curr_group) %>% 
                                    pull(sample_size))
                    )
                  )
    }
  
  rm(curr_group)
  
  #Create runs for each group
  MC_run_data = expand_grid(!!MC_grouping := exp_data %>% pull(!!MC_grouping),
              run = 1:MC_simulations)
  
  #Join 
  MC_run_data = left_join(samples_per_group,
            MC_run_data) %>%
    arrange(!!MC_grouping,run)
  
  set.seed(MC_seed)
  
  # curr_group = (exp_data %>% pull(!!MC_grouping))
  # curr_group = curr_group[1]
  
  MC_temp = tibble()
  
  #Loop to pull samples per group
  for (curr_group in (exp_data %>% pull(!!MC_grouping))) {
    
    #Decided to create one sample list per group, probably faster than filtering across rows  
    
    #Take MC data
    temp = MC_data %>% 
      
      #Filter for current group
      filter(!!MC_grouping == curr_group) %>%
      
      #Sample that data
      sample_n(
        
        #The sample size is the length of the output frame
        size =     MC_run_data %>%
          filter(!!MC_grouping == curr_group) %>%
          nrow(),
        
        #Selection with replacement
        replace = TRUE
      )
    
    MC_temp = bind_rows(MC_temp,
                        temp)
    
    rm(temp)
    
  }
  
  #Rename simulated grouping column
  MC_temp = MC_temp %>% 
    rename("sim_group" = !!MC_grouping)

  
  #Bind run list with sample data
  MC_run_data = bind_cols(MC_run_data,MC_temp)
  
  rm(MC_temp)
  
  #Get summary data from sims
  sim_data = MC_run_data %>% 
    group_by(!!MC_grouping,run) %>%
    summarize(mean = mean(!!MC_responses,
                          na.rm = TRUE),
              sd = sd(!!MC_responses,
                      na.rm=TRUE),
              sample_size = n())
  
}#If then to ensure that there is filtered data

# } #Brace for end of MC function

