library(tidyverse)
library(janitor)

relapse1  = read_csv("./data/Relapse Data 1.csv") %>%
  clean_names()

relapse1 = relapse1 %>%
  mutate(subject = parse_factor(subject),
         group = parse_factor(group))

relapse3 = read_csv("./data/Relapse Data 3.csv") %>%
  clean_names()

relapse = relapse %>%
  mutate(group = parse_factor(group),
         reinf = parse_factor(reinf),
         condition = parse_factor(condition,
                                  ordered = TRUE,
                                  levels = c("Baseline","Extinction","Renewal")))


mc_data = relapse1

rm(relapse1,relapse3)
