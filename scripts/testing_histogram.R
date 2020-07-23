min_mean = MC_data$MC_out$sim_data %>% 
  pull(mean) %>% min()

max_mean = MC_data$MC_out$sim_data %>% 
  pull(mean) %>% max()

bin_width = (max_mean - min_mean) / 50

MC_data$MC_out$sim_data %>%
  mutate(bin = cut_interval(mean,
                          n = 50,
                         ordered_result = TRUE)) %>%
  group_by(bin) %>%
  summarize(count = n())

findInterval(MC_data$MC_out$sim_data %>% pull(mean),
  seq(from = min_mean,
    to = max_mean,
    length.out = 50)
)

MC_data$MC_out$sim_data %>%
  mutate(bin = cut(x = mean,
                   breaks = 50)) %>%
pull(bin) %>% levels()

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
               fill = I(bin_color)))

fig +
  facet_wrap(vars(!!MC_grouping))

  