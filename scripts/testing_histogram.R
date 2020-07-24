if (is.na(MC_grouping)) {
  #If there is no grouping factor, create one with no groups
  MC_data$sim_data = MC_data$sim_data %>% mutate(grouping = "None")
  MC_grouping = "grouping"
  
} else {
  
  MC_grouping = as.symbol(make_clean_names(MC_grouping))
  
}

min_mean = MC_data$MC_out$sim_data %>% 
  pull(mean) %>% min()

max_mean = MC_data$MC_out$sim_data %>% 
  pull(mean) %>% max()

bin_width = (max_mean - min_mean) / 50

MC_data$MC_out$sim_data = MC_data$MC_out$sim_data %>%
  mutate(bin = cut_interval(mean,
                            n = 50,
                            ordered_result = TRUE))

MC_data$MC_out$sim_data = right_join(
  MC_data$MC_out$sim_data %>%
    group_by(!!MC_grouping, bin) %>%
    summarize(freq = n()),
  expand_grid(bin = MC_data$MC_out$sim_data %>%
    pull(bin) %>% levels(),
    !!MC_grouping:= MC_data$MC_out$sim_data %>%
      pull(!!MC_grouping) %>%
      unique())
) %>%
  replace_na(list(freq = 0)) %>%
  arrange(group) %>%
  separate(bin,
           into = c("min_bound","max_bound"),
           sep = ",",
           remove = FALSE) %>%
  mutate(min_bound = as.numeric(str_sub(min_bound,2)),
         max_bound = as.numeric(str_sub(max_bound, end = -2)),
         bin_cen = (min_bound + max_bound) / 2) %>%
  ungroup()

MC_data$MC_out$sim_data = MC_data$MC_out$sim_data %>%
  mutate(bin_color = rep(c("#e69f00","#009ee9"),
                         NROW(MC_data$MC_out$sim_data)/2))

fig = ggplot(MC_data$MC_out$sim_data) + 
  geom_col(aes(x = bin_cen,
               y=freq,
               fill = I(bin_color))) + 
  theme_classic() +
  ylab("Frequency") +
  xlab(paste("Mean",MC_data$behv,"from Simulated Samples")) +
  geom_vline(data = MC_data$MC_out$exp_data,
             aes(xintercept = mean),
             color = "red")

if(deparse((MC_grouping)) != "grouping"){
  fig = fig +
    facet_wrap(vars(!!MC_grouping))
}
  