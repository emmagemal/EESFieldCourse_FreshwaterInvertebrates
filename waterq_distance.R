#loading ggplot
library(ggplot2)

#reading new data set for creating graph
data1 <- read.csv("Data/graph_data.csv")

ggplot(data1, aes(x = Dist_source, y = ASPT)) +
  theme_light() +
  labs(
       x = "Direct Distance from Source (m)",
       y = "ASPT Water Quality",
       title = "Water Quality with Increasing Distance from Source of Braid Burn",
       subtitle = "Water quality is according to ASPT index."
       ) +
  geom_point(aes(shape = factor(Land_use)) +
               geom_label(aes(data1$Site_no)))
               



