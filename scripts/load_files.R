library(tidyverse)
library(janitor)

mc_data = list()

relapse1  = read_csv("./data/Examp_data.csv",
                     col_types = cols())

mc_data$example$col_names = colnames(relapse1)

relapse1 = relapse1  %>%
  clean_names()

relapse1 = relapse1 %>%
  mutate(subject = parse_factor(subject),
         group = parse_factor(group))

# relapse3 = read_csv("./data/Relapse Data 3.csv") %>%
#   clean_names()

# relapse3 = relapse3 %>%
#   mutate(group = parse_factor(group),
#          reinf = parse_factor(reinf),
#          condition = parse_factor(condition,
#                                   ordered = TRUE,
#                                   levels = c("Baseline","Extinction","Renewal")))



mc_data$example$data = relapse1

rm(relapse1)
