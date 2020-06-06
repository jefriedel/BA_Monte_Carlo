

figure_data = BA_MC_data
resp_col = "responses"
sessions_col = "session_number"
subject_col = "subject_number"

color_criteria =
tibble(condition = c("Baseline","Extinction"),
       experimental_group = c("Sal_Sal","Amp_Sal")) %>%
  expand(condition, experimental_group) %>%
  mutate(data_color = "Include")

inc_colors = c("red","black")

names(inc_colors) = c("Include","Exclude")

data_selection_plotter = function(figure_data,
                                  resp_col,
                                  sessions_col,
                                  subject_col,
                                  color_criteria = NA){
  
  if(is.na(color_criteria)) {
    figure_data = figure_data %>%
      mutate(color_data = FALSE)
  } else{
    
    figure_data = left_join(figure_data,
              color_criteria) %>%
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
