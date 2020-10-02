# EES Field Course - Group 1 - Freshwater Invertebrates and Water Quality
# Emma Gemal, s1758915@sms.ed.ac.uk; Sophie Rose, s1739832@sms.ed.ac.uk
# University of Edinburgh
# Last edited: 28/9/2020



## Library ----
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggsci)


# loading the data
wq_data <- read.csv("Data/alldata.csv")
wq_data <- wq_data %>% 
              slice(-c(17:27))

## Data exploration ----
(hist <- ggplot(wq_data, aes(x = ASPT))  +
            geom_histogram())

# checking for normality
(normcheck <- ggqqplot(wq_data$ASPT))   # looks fairly fine
ggsave(normcheck, file = "QQplot_normality.png", width = 5, height = 5, units = c("in"), path = "Figures")

shapiro.test(wq_data$ASPT)  # is not statistically significant (can assume normality of ASPT)

# assessing normality of residuals
resids <- resid(lm(ASPT ~ distance_source_km, data = wq_data))  
shapiro.test(resids)   # not statistically significant (can assume normality)

resids_bbwi <- resid(lm(WQ ~ distance_source_km, data = wq_data))
shapiro.test(resids_bbwi)  # also not statistically significant (can assume normality)

resids_ind <- resid(lm(ASPT ~ WQ, data = wq_data))
shapiro.test(resids_ind)   # also not statistically significant (can assume normality)



## Plotting ASPT water quality with distance ----
# linear regression for ASPT vs distance from source 
ASPT_stats <- lm(ASPT ~ distance_source_km,
                data = wq_data)
summary(ASPT_stats)
# p-value = 0.03859
# F-value = 5.211 (1 and 14 degrees of freedom) 
# R-squared = 0.2712

# creating the plot of ASPT vs distance (no land use)
(wq_plot <- ggplot(wq_data, aes(x = distance_source_km, y = ASPT)) +
              labs(
                x = "Distance from Source (km)",
                y = "Average Score Per Taxon"
              ) +
              stat_smooth(method = "lm", color = "black", alpha = 0.7) +
              geom_point(size = 7.5) +
              theme_pubr() +
              theme(axis.text = element_text(size = 36),
                    axis.title = element_text(size = 40),
                    axis.title.x = element_text(margin = margin(t = 20)),
                    axis.title.y = element_text(margin = margin(r = 20)),
                    plot.margin=unit(c(1,1,1,1),"cm")))

ggsave(wq_plot, file = "ASPTdist_plot.png", width = 14, height = 14, units = c("in"), path = "Figures")


# creating plot of ASPT vs distance, with land use as a variable 
(landuse_plot <- ggplot(wq_data, aes(x = distance_source_km, y = ASPT,
                                     color = landuse_type)) +
                    labs(
                      x = "Distance from Source (km)",
                      y = "Average Score Per Taxon",
                      color = "Land Use Type",
                      shape = "Land Use Type") +
                    stat_smooth(method = "lm", color = "black", alpha = 0.7) +
                    geom_point(size = 3.5, aes(shape = landuse_type, stroke = 1.2)) +
                    theme_pubr() +
                    theme(axis.text = element_text(size = 13),
                          axis.text.x = element_text(margin = margin(t = 0)),
                          axis.text.y = element_text(margin = margin(r = 0)),
                          axis.title = element_text(size = 16),
                          axis.title.x = element_text(margin = margin(t = 10)),
                          axis.title.y = element_text(margin = margin(r = 10)),
                          axis.ticks = element_blank(),
                          legend.position = "right",
                          plot.margin=unit(c(1,1,1,1),"cm")) +
                    scale_color_jama(labels = c("Natural Woodland",
                                                "Managed Woodland",
                                                "Parkland",
                                                "Urban",
                                                "Grassland")) +
                    scale_shape_manual(values = c(18, 15, 16, 17, 8),
                                       labels = c("Natural Woodland",
                                                  "Managed Woodland",
                                                  "Parkland",
                                                  "Urban",
                                                  "Grassland")))

ggsave(landuse_plot, file = "ASPTdist_landuse.png", width = 8, height = 5.5, units = c("in"), path = "Figures")

# moving legend to the bottom (if better for report format)
(keybot_aspt <- ggplot(wq_data, aes(x = distance_source_km, y = ASPT,
                                     color = landuse_type)) +
                    labs(
                      x = "Distance from Source (km)",
                      y = "ASPT",
                      color = "Land Use Type",
                      shape = "Land Use Type") +
                    stat_smooth(method = "lm", color = "black", alpha = 0.7) +
                    geom_point(size = 3.5, aes(shape = landuse_type, stroke = 1.2)) +
                    theme_pubr() +
                    theme(axis.text = element_text(size = 13),
                          axis.text.x = element_text(margin = margin(t = 2)),
                          axis.text.y = element_text(margin = margin(r = 2)),
                          axis.title = element_text(size = 16),
                          axis.title.x = element_text(margin = margin(t = 10)),
                          axis.title.y = element_text(margin = margin(r = 10)),
                          legend.position = "bottom",
                          plot.margin=unit(c(1,1,1,1),"cm")) +
                    scale_color_jama(labels = c("Natural Woodland",
                                                "Managed Woodland",
                                                "Parkland",
                                                "Urban",
                                                "Grassland")) +
                    scale_shape_manual(values = c(18, 15, 16, 17, 8),
                                       labels = c("Natural Woodland",
                                                  "Managed Woodland",
                                                  "Parkland",
                                                  "Urban",
                                                  "Grassland")))
keybot_aspt <- keybot_aspt + guides(color = guide_legend(nrow = 2)) 

ggsave(keybot_aspt, file = "ASPTdist_landuse_bottom.png", width = 6, height = 5.5, units = c("in"), path = "Figures")



## Plotting BBWI water quality with distance ----
# linear regression for BBWI vs distance from source  
BBWI_stats <- lm(WQ ~ distance_source_km,
                data = wq_data)
summary(BBWI_stats)
# p-value = 0.09241
# R-squared = 0.189
# F-value = 3.262, 1 and 14 degrees of freedom

# creating the plot of our water quality index (BBWI) vs. distance from source (no land use)
(bbwi_plot <- ggplot(wq_data, aes(x = distance_source_km, y = WQ)) +
                  labs(
                    x = "Distance from Source (km)",
                    y = "Braid Burn Water Quality Index"
                  ) +
                  stat_smooth(method = "lm", color = "black", alpha = 0.7) +
                  geom_point(size = 7.5) +
                  theme_pubr() +
                  theme(axis.text = element_text(size = 36),
                        axis.title = element_text(size = 40),
                        axis.title.x = element_text(margin = margin(t = 20)),
                        axis.title.y = element_text(margin = margin(r = 20)),
                        plot.margin=unit(c(1,1,1,1),"cm")))

ggsave(file = "BBWIdist_plot.png", width = 14, height = 14, units = c("in"), path = "Figures")

# creating plot of BBWI vs distance, with land use as a variable 
(landuse_plot2 <- ggplot(wq_data, aes(x = distance_source_km, y = WQ,
                                      color = landuse_type)) +
                    labs(
                      x = "Distance from Source (km)",
                      y = "Braid Burn Water Quality Index",
                      color = "Land Use Type",
                      shape = "Land Use Type") +
                    stat_smooth(method = "lm", color = "black", alpha = 0.7) +
                    geom_point(size = 3.5, aes(shape = landuse_type, stroke = 1.2)) +
                    theme_pubr() +
                    theme(axis.text = element_text(size = 13),
                          axis.text.x = element_text(margin = margin(t = 0)),
                          axis.text.y = element_text(margin = margin(r = 0)),
                          axis.title = element_text(size = 16),
                          axis.title.x = element_text(margin = margin(t = 10)),
                          axis.title.y = element_text(margin = margin(r = 10)),
                          axis.ticks = element_blank(),
                          legend.position = "right",
                          plot.margin=unit(c(1,1,1,1),"cm")) +
                    scale_color_jama(labels = c("Natural Woodland",
                                                "Managed Woodland",
                                                "Parkland",
                                                "Urban",
                                                "Grassland")) +
                    scale_shape_manual(values = c(18, 15, 16, 17, 8),
                                       labels = c("Natural Woodland",
                                                  "Managed Woodland",
                                                  "Parkland",
                                                  "Urban",
                                                  "Grassland")))

ggsave(landuse_plot2, file = "BBWIdist_landuse.png", width = 8, height = 5.5, units = c("in"), path = "Figures")

# moving legend to the bottom (if better for report format)
(keybot_bbwi <- ggplot(wq_data, aes(x = distance_source_km, y = WQ,
                                      color = landuse_type)) +
                  labs(
                    x = "Distance from Source (km)",
                    y = "BBWI",
                    color = "Land Use Type",
                    shape = "Land Use Type") +
                  stat_smooth(method = "lm", color = "black", alpha = 0.7) +
                  geom_point(size = 3.5, aes(shape = landuse_type, stroke = 1.2)) +
                  theme_pubr() +
                  theme(axis.text = element_text(size = 13),
                        axis.text.x = element_text(margin = margin(t = 2)),
                        axis.text.y = element_text(margin = margin(r = 2)),
                        axis.title = element_text(size = 16),
                        axis.title.x = element_text(margin = margin(t = 10)),
                        axis.title.y = element_text(margin = margin(r = 10)),
                        legend.position = "bottom",
                        plot.margin=unit(c(1,1,1,1),"cm")) +
                  scale_color_jama(labels = c("Natural Woodland",
                                              "Managed Woodland",
                                              "Parkland",
                                              "Urban",
                                              "Grassland")) +
                  scale_shape_manual(values = c(18, 15, 16, 17, 8),
                                     labels = c("Natural Woodland",
                                                "Managed Woodland",
                                                "Parkland",
                                                "Urban",
                                                "Grassland")))
keybot_bbwi <- keybot_bbwi + guides(color = guide_legend(nrow = 2)) 

ggsave(keybot_bbwi, file = "BBWIdist_landuse_bottom.png", width = 6, height = 5.5, units = c("in"), path = "Figures")


## Plotting relationship between BBWI vs. ASPT ----
# comparison of our water quality index (BBWI) and ASPT
index_stats <- lm(WQ ~ ASPT,
                  data = wq_data)
summary(index_stats)
# p-value = 0.000351
# F-value = 21.95 (1 and 14 degrees of freedom)
# R-squared = 0.6106

# plotting the index comparison
(wq_aspt_plot <- ggplot(wq_data, aes(x = ASPT, y = WQ)) +
                    labs(
                      x = "BBWI",
                      y = "ASPT"
                    ) +
                    stat_smooth(method = "lm", color = "black", alpha = 0.7) +
                    geom_point(size = 7.5) +
                    theme_pubr() +
                    theme(axis.text = element_text(size = 36),
                          axis.text.x = element_text(margin = margin(t = 0)),
                          axis.text.y = element_text(margin = margin(r = 0)),
                          axis.title = element_text(size = 40),
                          axis.title.x = element_text(margin = margin(t = 20)),
                          axis.title.y = element_text(margin = margin(r = 20)),
                          axis.ticks = element_blank(),
                          plot.margin=unit(c(1,1,1,1),"cm")))

ggsave(file = "ASPT_BBWI_plot.png", width = 14, height = 14, units = c("in"), path = "Figures")

# making a panel of BBWI vs distance and ASPT vs BBWI 
otherfigs <- ggarrange(bbwi_plot, wq_aspt_plot, nrow = 1) 
ggsave(otherfigs, file = "bbwi_aspt_distancepanel.png", width = 24, height = 11, units = c("in"), path = "Figures")



## Investigation of impacts of outlier ----
wq_less <- wq_data[-c(8), ]   # removing a potential outlier to see effects 

less_stats <- lm(ASPT ~ distance_source_km,
                 data = wq_less)
summary(less_stats)
# p-value = 0.04944 (still significant, but barely!!)
# F-value = 4.694 (1 and 13 DF)
# R-squared = 0.2653

(plot_less <- ggplot(wq_less, aes(x = distance_source_km, y = ASPT,
                                  color = landuse_type)) +
                labs(
                  x = "Distance from Source (km)",
                  y = "Average Score Per Taxon",
                  color = "Land Use Type",
                  shape = "Land Use Type") +
                stat_smooth(method = "lm", color = "black", alpha = 0.7) +
                geom_point(size = 3.5, aes(shape = landuse_type, stroke = 1.2)) +
                theme_pubr() +
                theme(axis.text = element_text(size = 10),
                      axis.text.x = element_text(margin = margin(t = 0)),
                      axis.text.y = element_text(margin = margin(r = 0)),
                      axis.title = element_text(size = 15),
                      axis.title.x = element_text(margin = margin(t = 10)),
                      axis.title.y = element_text(margin = margin(r = 10)),
                      axis.ticks = element_blank(),
                      legend.position = "right",
                      plot.margin=unit(c(1,1,1,1),"cm")) +
                scale_color_jama(labels = c("Natural Woodland",
                                            "Managed Woodland",
                                            "Parkland",
                                            "Urban",
                                            "Grassland")) +
                scale_shape_manual(values = c(18, 15, 16, 17, 8),
                                   labels = c("Natural Woodland",
                                              "Managed Woodland",
                                              "Parkland",
                                              "Urban",
                                              "Grassland")) +
                expand_limits(y = c(4, 8.1)))   # to show direct comparison with all data            

ggsave(plot_less, file = "ASPTdist_nooutlier.png", width = 7, height = 5.5, units = c("in"), path = "Figures")

# plotting both land use plots (ASPT & no outlier) on one panel
landuse_grid <- ggarrange(landuse_plot, plot_less, nrow = 1, common.legend = TRUE, legend = "top") 
ggsave(landuse_grid, file = "landuseplots.png", width = 12, height = 5.5, units = c("in"), path = "Figures")
