# #Function to calc log prop----
# # 
# full_data = list()
# full_data$data = mc_data$example$data
# full_data$col_descript = mc_data$example$col_names
# log_base = 2
# grouping = "Subject"
# responding = "Responses"
# sessions = "Session"
# 
# full_data2 = full_data
# log_base2 = 2
# grouping2 = "Subject"
# responding2 = "Responses"
# sessions2 = sessions
# 
# log_prop_calc(full_data2,
#               responding2,
#               log_base2,
#               grouping2,
#               sessions2)
#

log_prop_calc = function(full_data,
                         responding,
                         sessions,
                         log_base = 2,
                         grouping = NA) {
  
  responding = as.symbol(make_clean_names(responding))
  sessions = as.symbol(make_clean_names(sessions))

  if (!is.na(grouping)) {
    grouping = as.symbol(make_clean_names(grouping))

    full_data$data = full_data$data %>%
      group_by(!!grouping)
  }

  col_name = "log Prop. Resp."

  full_data$data = full_data$data %>%
    arrange(!!sessions) %>%
    mutate(!!as.symbol(make_clean_names(col_name)) :=
             logb((!!responding + 1) / (lag(!!responding)+1),
                  base = log_base)) %>%
    ungroup() %>%
    arrange(!!grouping)
  
  
  #full_data$col_names = c(full_data$col_names, col_name)

  #Error creeped in from old version. Kept because it somehow updates name
  #on added column
  full_data$col_descript = c(full_data$col_descript, col_name)
  
  full_data$behv = col_name

  return(full_data)
}
# #Function for plotting input/filtering----
# 
# #Data for testing plotting function
# figure_data = mc_data$example$data
# resp_col = "Responses"
# sessions_col = "Session"
# subject_col = "Subject"
# 
# # Original
# # filter_criteria =
# #   tibble(condition = c("Baseline","Extinction"),
# #          experimental_group = c("Sal_Sal","Amp_Sal")) %>%
# #   expand(condition, experimental_group) %>%
# #   mutate(data_color = "Include")
# 
# #Testing filter error
# filter_criteria =
#   tibble(session = 1:5) %>%
#   mutate(data_color = "Include")
# 
# color_criteria = NA
# 
# color_criteria = filter_criteria
# 
# #Colors for plotting
# inc_colors = c("red","black")
# names(inc_colors) = c("Include","Exclude")

#
# data_selection_plotter(figure_data = figure_data,
#                        resp_col = resp_col,
#                        sessions_col = sessions_col,
#                        subject_col = subject_col)

# #Function to plot data for seleecting filter
data_selection_plotter = function(figure_data,
                                  resp_col,
                                  sessions_col,
                                  subject_col,
                                  color_criteria = NA,
                                  inc_colors = NULL){

  #Colors for plotting
  if (is.null(inc_colors)) {
    inc_colors = c("red", "black")
    names(inc_colors) = c("Include", "Exclude")
  }
  
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
  ggplot(figure_data,aes(x = !!as.symbol(make_clean_names(sessions_col)),
                       y = !!as.symbol(make_clean_names(resp_col)))) +
  theme_classic() +
  geom_line() +
    geom_point(aes(color = data_color))+
    scale_color_manual(values = inc_colors) +
  facet_wrap(vars(!!as.symbol(make_clean_names((subject_col))))) +
  xlab(sessions_col) +
  ylab(resp_col) + 
    theme(legend.position = "none")
  

}


#Function for MC simulations----
# #Variables for testing build
# MC_data = mc_data$example$data
# 
# MC_filter = tibble(condition = c("Reinstatement"),
#                    group = c("Sal_Sal","Amp_Sal")) %>%
#   expand(condition, group) %>%
#   mutate(MC_include = "Include")
# 
# MC_grouping = "Group"
# 
# MC_responses = "responses"
# 
# 
# MC_simulations = 500
# MC_seed = 1
# 
# # other_filter = tibble(med_change = TRUE, MC_include = "Include")
# # 
# # MC_out = MC_func(
# #   MC_data = other_data,
# #   MC_responses = "log_behv",
# #   MC_filter = other_filter,
# #   MC_grouping = NA,
# #   MC_simulations = 1000,
# #   MC_seed = 1
# # # )
# # 
# 
# MC_data2 = MC_data
# MC_responses2 = MC_responses
# MC_filter2 = MC_filter
# MC_grouping2 = MC_grouping
# MC_simulations2 = 1000
# MC_seed2 = 1
# 
# MC_func(MC_data = MC_data2,
#         MC_responses = MC_responses2,
#         MC_filter = MC_filter2,
#         MC_grouping = MC_grouping2,
#         MC_simulations = MC_simulations2,
#         MC_seed = MC_seed2)


# #Live testing
# MC_grouping = ""
# MC_responses = "Responses"
# MC_sessions = "Session"
# MC_subjects = "Subject"
# 
# MC_data = mc_data$example
# 
# MC_data = log_prop_calc(MC_data,
#               responding = MC_responses,
#               sessions = MC_sessions,
#               grouping = MC_subjects)
# 
# MC_filter =
#   tibble(condition = c("Reinstatement")) %>%
#   expand(condition) %>%
#   mutate(data_color = "Include")
# 
# MC_grouping = "Group"
# 
# #For reststing within code
# MC_responses = MC_data$behv
# MC_data = MC_data$data
# 
# MC_simulations = 500
# MC_seed = 1
# 
# temp = MC_func(MC_data = MC_data$data,
#                    MC_responses = MC_responses,
#                    MC_filter = MC_filter,
#                    MC_grouping = MC_grouping,
#                    MC_simulations = 500,
#                    MC_seed = 1)


MC_func = function(MC_data,
                   MC_responses,
                   MC_filter = NA,
                   MC_grouping = NA,
                   MC_simulations = 500,
                   MC_seed = 1) {

#Turn in symbol so don't have to do it repeatedly
MC_responses = as.symbol(make_clean_names(MC_responses))

MC_data = MC_data %>% ungroup()

if(is.na(MC_grouping) | MC_grouping == ""){

  #If there is no grouping factor, create one with no groups
  MC_data = MC_data %>% mutate(grouping = "None")
  MC_grouping = "grouping"

}

MC_grouping = as.symbol(make_clean_names(MC_grouping))


if(is.na(MC_filter)) {
  #If there is no MC_filter, include everything

  MC_data = MC_data %>%
    mutate(data_color = "Include")

} else{

  #Add include to what is included in the filter
  MC_data = left_join(MC_data,
                      MC_filter) %>%

    #Exclude everything not on include
    replace_na(list(data_color = "Exclude"))
}

options(dplyr.summarise.inform = FALSE)
  # #Means, SDs, counts for each group
  exp_data = MC_data %>%
    filter(data_color=="Include") %>%
    group_by(!!MC_grouping) %>%
    summarize(mean = mean(!!MC_responses,
                          na.rm = TRUE),
              sd = sd(!!MC_responses,
                      na.rm=TRUE),
              sample_size = n(),
              .groups ="drop")
  

  #Create sim_data for looping
  sim_data = tibble()

  set.seed(MC_seed)

  curr_group = (exp_data %>% pull(!!MC_grouping))
  curr_group = curr_group[1]

  sim = 1

  for(curr_group in (exp_data %>% pull(!!MC_grouping))) {
    #Filter the data once, to avoid repetitive filtering
    filtered_MC =
      MC_data %>%
      filter(!!MC_grouping == curr_group) %>%
      drop_na(!!MC_responses)

    curr_size = exp_data %>%
      filter(!!MC_grouping == curr_group) %>%
      pull(sample_size)

    for (sim in 1:MC_simulations) {
      
      
      #For testing to get full samples
      # sim_data = bind_rows(
      #   sim_data,
      #   filtered_MC %>%
      #     sample_n(size = curr_size,
      #              replace = TRUE) %>%
      #     mutate(run = sim)
      # )
      
      
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
                    na.rm = TRUE),
            MC_samp = (n())
          ) %>%
          mutate(run = sim)
      )

    } # Loop for current simulation

    #Remove to clear out memory
    rm(filtered_MC)

  } #Loop for current group

  options(dplyr.summarise.inform = TRUE)
  #Join experimental mean to sim mean
  sim_data = left_join(sim_data,
                       exp_data %>% 
                         select(!!MC_grouping, mean) %>% 
                         rename("exp_mean" = mean))
  
  sim_labs = c("Sim less than real",
    "Sim equal to real" ,
    "Sim greater than real")
  
  #Chose if then categorization rather than cut because == is easiest for matching the mean
  sim_out = sim_data %>%
    mutate(comp_exp_mean = if_else(round(mean,digits = 4)==round(exp_mean,digits =4),
                                   2,
                                   if_else(mean<exp_mean,1,3))) %>%
    select(-exp_mean) %>%
    mutate(comp_exp_mean = factor(comp_exp_mean,
                                   levels = c(1,2,3),
                                   labels = sim_labs))  %>%
    group_by(!!MC_grouping,comp_exp_mean) %>%
    summarize(frequency = n(),
              .groups = "drop")
  
  sim_out = 
    right_join(sim_out,
             expand_grid(group = sim_data %>% pull(!!MC_grouping) %>%unique() %>% as.character(),
                           comp_exp_mean = sim_labs)) %>%
    replace_na(list(frequency = 0)) %>%
    group_by(!!MC_grouping) %>%
    mutate(perc = frequency/sum(frequency)) %>%
      ungroup() %>%
      mutate(!!MC_grouping := as_factor(!!MC_grouping),
             comp_exp_mean = parse_factor(comp_exp_mean,
                                       levels = c("Sim less than real",
                                                  "Sim equal to real" ,
                                                  "Sim greater than real"),
                                       ordered = TRUE)) %>%
    arrange(!!MC_grouping,comp_exp_mean)

  colnames(sim_out) = c(
    "Group",
    "Real mean compared to simulated means",
    "Relative frequency",
    "Relative percentage"
  )

  mc_output = list()
  
  mc_output$sim_data = sim_data
  mc_output$exp_data = exp_data
  mc_output$sim_analysis = sim_out
  
  return(mc_output)

#If then to ensure that there is filtered data


#Tictoc on 1000 is ~2 seconds, 10,000 is ~20 seconds (as predicted)
#Tictoc was only run in local. To save server time. Limit app to 1000.

 } #Brace for end of MC function



#Plotting output from MC simulations----

# #Live testing
# MC_grouping = "Group"
# MC_responses = "Responses"
# MC_sessions = "Session"
# MC_subjects = "Subject"
# 
# MC_data = mc_data$example
# 
# MC_data = log_prop_calc(MC_data,
#               responding = MC_responses,
#               sessions = MC_sessions,
#               grouping = MC_subjects)
# 
# MC_filter =
#   tibble(condition = c("Reinstatement")) %>%
#   expand(condition) %>%
#   mutate(data_color = "Include")
# 
# # #For reststing within code
# # MC_responses = MC_data$behv
# # MC_data = MC_data$data
# 
# MC_simulations = 500
# MC_seed = 1
# 
# MC_data$MC_out = MC_func(MC_data = MC_data$data,
#                    MC_responses = MC_data$behv,
#                    MC_filter = MC_filter,
#                    MC_grouping = MC_grouping,
#                    MC_simulations = 500,
#                    MC_seed = 1)
# 
# MC_data2 = MC_data
# MC_grouping2 = MC_grouping
# 
# MC_out_plotter(MC_data = MC_data2,
#                MC_grouping = MC_grouping2)
#
# #Live testing
# MC_data3 = isolate(curr_data$MC_out)
# MC_grouping3 = input$group_select
# 
# MC_data = MC_data3
# MC_grouping = MC_grouping3
# 
# MC_out_plotter(MC_data = MC_data3,
#          MC_grouping = MC_grouping3)

# #Live testing
# MC_grouping = ""
# MC_responses = "Responses"
# MC_sessions = "Session"
# MC_subjects = "Subject"
# 
# MC_data = mc_data$example
# 
# MC_data = log_prop_calc(MC_data,
#               responding = MC_responses,
#               sessions = MC_sessions,
#               grouping = MC_subjects)
# 
# MC_filter =
#   tibble(condition = c("Reinstatement")) %>%
#   expand(condition) %>%
#   mutate(data_color = "Include")
# 
# #For reststing within code
# MC_responses = MC_data$behv
# MC_data = MC_data$data
# 
# MC_simulations = 500
# MC_seed = 1
# 
# temp = MC_func(MC_data = MC_data,
#                    MC_responses = MC_responses,
#                    MC_filter = MC_filter,
#                    MC_grouping = MC_grouping,
#                    MC_simulations = 500,
#                    MC_seed = 1)
# 
# MC_out_plotter(temp, MC_grouping = MC_grouping)
# MC_data = temp

MC_out_plotter = function(MC_data,
                          MC_grouping = ""){

  if (MC_grouping=="") {
    MC_grouping = "grouping"
    
  } else {
    
    MC_grouping = as.symbol(make_clean_names(MC_grouping))
    
  }
  
  min_mean = MC_data$sim_data %>% 
    pull(mean) %>% min()
  
  max_mean =MC_data$sim_data %>% 
    pull(mean) %>% max()
  
  bin_width = (max_mean - min_mean) / 50
  
  plot_data = MC_data$sim_data %>%
    mutate(bin = cut_interval(mean,
                              n = 50,
                              ordered_result = TRUE))
  
  plot_data = right_join(
    plot_data %>%
      group_by(!!MC_grouping, bin) %>%
      summarize(freq = n()),
    expand_grid(bin = plot_data %>%
                  pull(bin) %>% levels(),
                !!MC_grouping:= MC_data$sim_data %>%
                  pull(!!MC_grouping) %>%
                  unique())
  ) %>%
    replace_na(list(freq = 0)) %>%
    arrange(!!MC_grouping) %>%
    separate(bin,
             into = c("min_bound","max_bound"),
             sep = ",",
             remove = FALSE) %>%
    mutate(min_bound = as.numeric(str_sub(min_bound,2)),
           max_bound = as.numeric(str_sub(max_bound, end = -2)),
           bin_cen = (min_bound + max_bound) / 2) %>%
    ungroup()
  
  plot_data = plot_data %>%
    mutate(bin_color = rep(c("#e69f00","#009ee9"),
                           NROW(plot_data)/2))
  
  fig = ggplot(plot_data) + 
    geom_col(aes(x = bin_cen,
                 y=freq,
                 fill = I(bin_color))) + 
    theme_classic() +
    ylab("Frequency") +
    xlab(paste("Mean",MC_data$behv,"from Simulated Samples")) +
    geom_vline(data = MC_data$exp_data,
               aes(xintercept = mean),
               color = "black",
               linetype = "dashed",
               size = 1)
  
  if(deparse((MC_grouping)) != "grouping"){
    fig = fig +
      facet_wrap(vars(!!MC_grouping))
  }
  
  return(fig)

}

# # MC_data2 = MC_out
# # 
# # MC_filter2 = tibble(condition = c("Reinstatement"),
# #                    experimental_group = c("Sal_Sal","Amp_Sal")) %>%
# #   expand(condition, experimental_group) %>%
# #   mutate(MC_include = "Include")
# # 
# # MC_grouping2 = "experimental_group"
# # 
# # MC_responses2 = "log_responses"
# # 
# # exp_out = exp_out_func(MC_data,
# #                      MC_grouping = "experimental_group",
# #                      MC_filter2,
# #                      MC_responses2)
# # 
# # MC_out_plotter(MC_data2,
# #                exp_out,
# #                MC_grouping2)
# 
# #For new data
# 
# other_filter = tibble(med_change = TRUE, MC_include = "Include")
# 
# MC_data = other_data
# 
# MC_out_plotter(MC_data = MC_out,
#                           exp_data = exp_data,
#                           MC_grouping = NA,
#                           MC_responses = "log_behavior")
# 
# 
# MC_out_plotter(MC_data = sim_data,
#                exp_data = exp_data,
#                MC_grouping = NA,
#                MC_responses = "log_behavior")
# 
# 
