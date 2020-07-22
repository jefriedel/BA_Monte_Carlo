library(tidyverse)

data = tibble(
  data = rep(1:4,3),
  groups = c(rep("A",4),rep("B",4),rep("C",4))
)

p = ggplot(data) + 
  geom_histogram(aes(x = data),
                 bins = 4,
                 color = "black",
                 fill = rep(c("#e69f00","#009ee9"),2))

#Histogram of everything
p

p =  p + facet_wrap(vars(groups))

#Faceted histogram
p
