# EES Field Course - Group 1 - Freshwater Invertebrates and Water Quality
# Sophie Rose, s1758915@sms.ed.ac.uk
# Last edited: 14/9/2020



#loading ggplot
library(ggplot2)

#reading new data set for creating graph
data1 <- read.csv("Data/graph_data.csv")

#graph waterq/distance
ggplot(data1, aes(x = Dist_source, y = ASPT)) +
  theme_light() +
  labs(
       x = "Direct Distance from Source (m)",
       y = "ASPT Water Quality",
       title = "Water Quality with Increasing Distance from Source of Braid Burn",
       subtitle = "Water quality according to ASPT index."
       ) +
  geom_point(aes(shape = factor(Land_use)))

data2 <-                



