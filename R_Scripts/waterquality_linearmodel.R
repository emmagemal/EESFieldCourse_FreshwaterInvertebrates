# EES Field Course - Group 1 - Freshwater Invertebrates and Water Quality
# Linear model of water quality 
# Emma Gemal, s1758915@sms.ed.ac.uk
# Last edited: 14/9/2020

## Library ----
library(tidyverse)
library(GGally)
library(ggpubr)  # idk if I need this 


## Data manipulation ----
# load data
all_data <- read.csv("Data/alldata.csv")

str(all_data)
all_data <- all_data %>% 
                slice(-c(17:27))


imp_data <- all_data %>% 
                subset(select = c(site_order,
                                  landuse_type,
                                  avg.width_cm,
                                  avg.depth_cm,
                                  avg.velocity_ms,
                                  avg.sediment_cm,
                                  avg.turbidity_FTU,
                                  avg.colour_mgLPt,
                                  avg.conductivity_uScm,
                                  avg.pH,
                                  avg.nitrate,
                                  avg.phosphate,
                                  BOD_DOperc,
                                  avg.absorbance,
                                  sum_PS,
                                  ASPT))
str(imp_data)

cm_data <- imp_data %>% 
                  subset(select = -c(site_order,
                                     landuse_type,
                                     sum_PS))
str(model_data)


## Relationship exploration ----
ggcorr(cm_data, size = 3, hjust = 0.9, label = TRUE, layout.exp = 2)
ggsave(file = "correlation_matrix.png", width = 5, height = 5)

# nitrate is the most correlated parameter with ASPT (R = -0.6)
# color has no effect on ASPT (R = 0)
# absorbance, turbidity, sediment size, and width have basically no effect on ASPT (R = ±0.1)


## Making a linear model for WQ ----
m1 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm
         + avg.pH
         + avg.velocity_ms
         + avg.depth_cm
         + avg.colour_mgLPt
         + avg.turbidity_FTU
         + avg.sediment_cm
         + BOD_DOperc
         + avg.width_cm
         + avg.absorbance, data = imp_data)  

m2 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm
         + avg.pH
         + avg.velocity_ms
         + avg.depth_cm
         + avg.colour_mgLPt
         + avg.turbidity_FTU
         + avg.sediment_cm
         + BOD_DOperc
         + avg.width_cm, data = imp_data) 

m3 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm
         + avg.pH
         + avg.velocity_ms
         + avg.depth_cm
         + avg.colour_mgLPt
         + avg.turbidity_FTU
         + avg.sediment_cm
         + BOD_DOperc, data = imp_data) 

m4 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm
         + avg.pH
         + avg.velocity_ms
         + avg.depth_cm
         + avg.colour_mgLPt
         + avg.turbidity_FTU
         + avg.sediment_cm, data = imp_data) 

m5 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm
         + avg.pH
         + avg.velocity_ms
         + avg.depth_cm
         + avg.colour_mgLPt
         + avg.turbidity_FTU, data = imp_data) 

m6 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm
         + avg.pH
         + avg.velocity_ms
         + avg.depth_cm
         + avg.colour_mgLPt, data = imp_data) 

m7 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm
         + avg.pH
         + avg.velocity_ms
         + avg.depth_cm, data = imp_data) 

m8 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm
         + avg.pH
         + avg.velocity_ms, data = imp_data) 

m9 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm
         + avg.pH, data = imp_data) 

m10 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm, data = imp_data) 

m11 <- lm(ASPT ~ avg.nitrate 
          + avg.phosphate, data = imp_data) 

m12 <- lm(ASPT ~ avg.nitrate, data = imp_data) 

# comparing the F-values of the models
AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)

# since conductivity and NO3- are somewhat correlated (R=0.6), 
# testing the best model (lowest F value using AIC) without conductivity
m8x <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.pH
         + avg.velocity_ms, data = imp_data) 

# testing to see if nitrate should be removed instead of conductivity 
m8y <- lm(ASPT ~ avg.phosphate
         + avg.conductivity_uScm
         + avg.pH
         + avg.velocity_ms, data = imp_data) 

AIC(m8, m8x, m8y)   # m8x is the best (only <1.0 difference, but it's simpler)

summary(m8x)
# intercept = 12.7565
# nitrate slope = -3.1772 
# phosphate slope = -5.3183
# pH slope = -0.4949
# velocity slope = -2.1734



