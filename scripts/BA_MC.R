library(tidyverse)
library(glue)

source("./scripts/load_files.R")

#Set correction to avoid divide by zero and log(0)
prop_cor = 1

#Set base for log
log_base = 2

#Number of simulations
runs = 1000

#Set seed for replicability
set.seed(1)

#Get column names to create variable list
var_list = colnames(mc_data)

#Create complete lists for resetting) and filter lists
for(curr_var in var_list){

  #Original list
  assign(glue("{curr_var}_full"),
       mc_data %>%
         select(curr_var) %>%
         pull() %>%
         unique())
  
  #List for filtering
  assign(glue("{curr_var}_filter"),
         eval(as.name(glue("{curr_var}_full"))))
  
}

rm(curr_var)


#calc log proportion responding
mc_data = mc_data %>%
  group_by(subject) %>%
  mutate(log_prop = logb((responses + prop_cor) / (lag(responses) + prop_cor),
                         base = log_base)) %>%
  ungroup()

#Select subset of data for current simulation
curr_data = mc_data %>%
  filter(group=="Sal_Sal")

curr_data_summary = curr_data %>%
  group_by(condition) %>%
  summarize(freq = n(),
            mean = mean(responses),
            sd = sd(responses))

conds = unique(mc_data$condition)

mc_data %>%
  filter(condition %in% conds)

conds = setdiff(conds,c("Baseline"))

mc_data %>%
  filter(condition %in% conds)
