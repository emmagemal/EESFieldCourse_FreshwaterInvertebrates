# EES Field Course - Group 1 - Freshwater Invertebrates and Water Quality
# Sophie Rose, s1739832@sms.ed.ac.uk; Emma Gemal, s1758915@sms.ed.ac.uk
# Last edited: 14/9/2020



## Library ----
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggsci)


# loading the data
wq_data <- read.csv("Data/waterqualityvalues_alldata.csv")
wq_data <- wq_data %>% 
              slice(-c(17:27))


## Plotting water quality with distance ----
# determining the statistics to be displayed on the ASPT vs distance plot 
ASPT_stats <- lm(ASPT ~ distance_source_km,
                data = wq_data)
summary(ASPT_stats)
# p-value = 0.03859
# R-squared = 0.2712

# creating the plot of ASPT vs distance 
(wq_plot <- ggplot(wq_data, aes(x = distance_source_km, y = ASPT)) +
              labs(
                x = "Distance from Source (km)",
                y = "Average Score Per Taxon"
              ) +
              stat_smooth(method = "lm", color = "black", alpha = 0.7) +
              geom_point(size = 7.5) +
              geom_text(x = 5.6, y = 7.7, label = expression(paste("R"^2, " = 0.2712; p = 0.039")), 
                        parse = TRUE, fontface = "italic", size = 14) +
              theme_pubr() +
              theme(axis.text = element_text(size = 36),
                    axis.title = element_text(size = 40),
                    axis.title.x = element_text(margin = margin(t = 20)),
                    axis.title.y = element_text(margin = margin(r = 20))))

ggsave(file = "ASPTdist_plot_r2.png", width = 14, height = 14, units = c("in"), path = "Figures")


# determining the statistics to be displayed on the BBWI vs distance plot 
BBWI_stats <- lm(WQ ~ distance_source_km,
                data = wq_data)
summary(BBWI_stats)
# p-value = 0.09241
# R-squared = 0.189

# creating the plot of our water quality index (BBWI) vs. distance from source
(wq_plot2 <- ggplot(wq_data, aes(x = distance_source_km, y = WQ)) +
                labs(
                      x = "Distance from Source (km)",
                      y = "Water Quality Index"
                    ) +
                stat_smooth(method = "lm", color = "black", alpha = 0.7) +
                geom_point(size = 7.5) +
                geom_text(x = 5.6, y = 7, label = expression(paste("R"^2, " = 0.189; p = 0.092")),
                          parse = TRUE, fontface = "italic", size = 14) +
                theme_pubr() +
                theme(axis.text = element_text(size = 36),
                      axis.title = element_text(size = 40),
                      axis.title.x = element_text(margin = margin(t = 20)),
                      axis.title.y = element_text(margin = margin(r = 20))))

ggsave(file = "BBWIdist_plot.png", width = 14, height = 14, units = c("in"), path = "Figures")





## Plotting relationship between lm vs. ASPT ----
# comparison of our water quality index (BBWI) and ASPT
index_stats <- lm(WQ ~ ASPT,
                  data = wq_data)
summary(index_stats)
# p-value = 0.00035
# R-squared = 0.6106

# plotting the index comparison
(wq_aspt_plot <- ggplot(wq_data, aes(x = WQ, y = ASPT)) +
                   labs(
                     x = "BBWI",
                     y = "ASPT"
                   ) +
                   stat_smooth(method = "lm", color = "black", alpha = 0.7) +
                   geom_point(size = 7.5) +
                   geom_text(x = 5.6, y = 7.7, label = expression(paste("R"^2, " = 0.6106; p = 0.0004")),
                             parse = TRUE, fontface = "italic", size = 14) +
                   theme_pubr() +
                   theme(axis.text = element_text(size = 36),
                         axis.title = element_text(size = 40),
                         axis.title.x = element_text(margin = margin(t = 20)),
                         axis.title.y = element_text(margin = margin(r = 20))))

ggsave(file = "ASPT_BBWI_plot_r2.png", width = 14, height = 14, units = c("in"), path = "Figures")
