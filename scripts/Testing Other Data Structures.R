library(tidyverse)
library(janitor)
library(glue)



raw_data = 
  read_csv("E:/Google Drive/.Professional/Research/Projects/Active/ABAParCor/aba_parcor_calc/Data/ClaJ.csv")


raw_data = raw_data %>% 
  clean_names() %>%
  mutate(week_ending_sun = parse_date(week_ending_sun,
                                      format = "%d %b %y")) %>%
  
  #Change med cond into numerics
  separate(med_cond,
           into = c(NA,"med_cond")) %>%
  mutate(med_cond = parse_integer(med_cond)) %>%
  
  #Change behavior treatment into numeric
  mutate(programme_changes =
           parse_integer(str_sub(
             programme_changes,
             start = 2,
             end = str_length(programme_changes)
           )))

other_data = raw_data %>% select(week, med_cond,programme_changes,total_prob_bx_types)

other_data = other_data %>% 
  mutate(log_behv = log2(total_prob_bx_types / lag(total_prob_bx_types))) %>%
  group_by(med_cond) %>%
  mutate(med_change = row_number()==1)

other_data %>% pull(med_cond) %>% max()
