#Code for testing MC function on new data----

relapse3  = read_csv("./data/Relapse Data 3 Rich.csv",
                     col_types = cols())

MC_data = list()

MC_data$col_names = colnames(relapse3)

relapse3 = relapse3  %>%
  clean_names()

relapse3 = relapse3 %>%
  mutate(subject = as_factor(subject),
         group = parse_factor(group))

MC_data$data = relapse3

MC_grouping = "Group"
MC_responses = "Rate"
MC_sessions = "Session"
MC_subjects = "Subject"

source("./scripts/helper_functions.R")

MC_data = log_prop_calc(MC_data,
              responding = MC_responses,
              sessions = MC_sessions,
              grouping = MC_subjects)

MC_responses = "log Prop. Resp."

MC_filter =
  tibble(condition = c("Renewal")) %>%
  expand(condition) %>%
  mutate(data_color = "Include")

MC_grouping = "Group"

# #For reststing within code
# MC_responses = MC_data$behv
# MC_data = MC_data$data

MC_simulations = 500
MC_seed = 1

MC_data = MC_data$data

#Code for testing grouping error----

#Live testing

source("./scripts/load_files.R")
source("./scripts/helper_functions.R")

MC_grouping = NA
MC_responses = "Responses"
MC_sessions = "Session"
MC_subjects = "Subject"

MC_data = mc_data$example

# 
# MC_data = read_csv("./data/Relapse test data 2.csv")

MC_data = log_prop_calc(MC_data,
              responding = MC_responses,
              sessions = MC_sessions,
              grouping = MC_subjects)

MC_filter =
  tibble(condition = c("Reinstatement")) %>%
  expand(condition) %>%
  mutate(data_color = "Include")


# #For reststing within code
# MC_responses = MC_data$behv
# MC_data = MC_data$data

MC_simulations = 500
MC_seed = 1

temp = MC_func(MC_data = MC_data$data,
                   MC_responses = MC_responses,
                   MC_filter = MC_filter,
                   MC_grouping = MC_grouping,
                   MC_simulations = 500,
                   MC_seed = 1)
#Fixing error with selecting session leading to crash

#Testing filter error
filter_criteria =
  tibble(session = 1:5) %>%
  mutate(data_color = "Include")

"session" %in% colnames(filter_criteria)
