# EES Field Course - Group 1 - Freshwater Invertebrates and Water Quality
# Sophie Rose, s1739832@sms.ed.ac.uk; Emma Gemal, s1758915@sms.ed.ac.uk
# Last edited: 14/9/2020



## Library ----
library(ggplot2)


# loading the data
wq_data <- read.csv("Data/waterqualityvalues_alldata.csv")
wq_data <- wq_data %>% 
              slice(-c(17:27))


## Plotting water quality with distance ----
(wq_plot <- ggplot(wq_data, aes(x = distance_source_km, y = WQ)) +
                theme_light() +
                labs(
                      x = "Distance from Source (km)",
                      y = "Relative Water Quality"
                    ) +
                geom_point() +
                geom_text(aes(label = site_order, hjust=1.3, vjust=1)) +
                stat_smooth(method = "lm"))


# to do: ggscatter, add = reg.line

o# barchart of water quality by land use
hist <- ggplot(wq_data, aes(x = landuse_type, y = WQ)) +
            geom_col()
hist


ggplot(wq_data, aes(x = distance_source_km, y = ASPT)) +
  theme_light() +
  labs(
    x = "Distance from Source (km)",
    y = "ASPT Water Quality"
  ) +
  geom_point() +
  geom_text(aes(label = site_order, hjust=1.3, vjust=1))




#--old code for graphs--

#reading new data set for creating graph
data1 <- read.csv("Data/graph_data.csv")

#graph waterq/distance and land use
ggplot(data1, aes(x = Dist_source, y = ASPT)) +
  theme_light() +
  labs(
       x = "Direct Distance from Source (m)",
       y = "ASPT Water Quality",
       title = "Water Quality with Increasing Distance from Source of Braid Burn",
       subtitle = "Water quality according to ASPT index."
       ) +
  geom_point(aes(shape = factor(Land_use)))

#same graph but no land use
ggplot(data1, aes(x = Dist_source, y = ASPT)) +
  theme_light() +
  labs(
    x = "Direct Distance from Source (m)",
    y = "ASPT Water Quality",
    title = "Water Quality with Increasing Distance from Source of Braid Burn",
    subtitle = "Water quality according to ASPT index."
  ) +
  geom_point() +
  geom_text(aes(label = data1$Site_no, hjust=1.3, vjust=1))


#reading data for spearmans
data2 <- read.csv("Data/alldata.csv")               

#spearman water q/distance
cor.test(data2$distance_source_km, data2$ASPT)


