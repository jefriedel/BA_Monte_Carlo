library(tidyverse)

data = tibble(
  data = rep(1:6,2),
  groups = c(rep("A",4),rep("B",4),rep("C",4)),
  myfill = rep(c("#e69f00","#009ee9"),6)
)

p = ggplot(data) + 
  geom_histogram(aes(x = data,
                     fill = I(myfill)),
                 bins = 4,
                 color = "black"
  )

#Histogram of everything
p

p =  p + facet_wrap(vars(groups))

#Faceted histogram
p
