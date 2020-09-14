# EES Field Course - Group 1 - Freshwater Invertebrates and Water Quality
# Sophie Rose, s1739832@sms.ed.ac.uk; Emma Gemal, s1758915@sms.ed.ac.uk
# Last edited: 14/9/2020



## Library ----
library(ggplot2)
library(ggpubr)
library(ggsci)


# loading the data
wq_data <- read.csv("Data/waterqualityvalues_alldata.csv")
wq_data <- wq_data %>% 
              slice(-c(17:27))


## Plotting water quality with distance ----
# creating the initial plot 
(wq_scatter <- ggscatter(wq_data, x = "distance_source_km", y = "WQ",
                         size = 7,
                         xlab = "Distance from Source (km)",
                         ylab = "Water Quality Index",
                         add = c("reg.line"), 
                         conf.int = TRUE,
                         cor.method = "pearson"))

# creating the final plot 
(wq_plot <- ggplot(wq_data, aes(x = distance_source_km, y = WQ)) +
                labs(
                      x = "Distance from Source (km)",
                      y = "Water Quality Index"
                    ) +
                stat_smooth(method = "lm", color = "black", alpha = 0.7) +
                geom_point(size = 7.5) +
                geom_text(x = 5.6, y = 7, label = "R = -0.43, p = 0.092", 
                          fontface = "italic", size = 14) +
                theme_pubr() +
                theme(axis.text = element_text(size = 36),
                      axis.title = element_text(size = 40),
                      axis.title.x = element_text(margin = margin(t = 20)),
                      axis.title.y = element_text(margin = margin(r = 20))))

ggsave(file = "final_plot.png", width = 14, height = 14, units = c("in"), path = "Figures")


