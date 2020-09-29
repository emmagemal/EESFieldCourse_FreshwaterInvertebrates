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
                                  distance_source_km,
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
                                  BODmgL,
                                  avg.absorbance,
                                  sum_PS,
                                  ASPT))
str(imp_data)

cm_data <- imp_data %>% 
                  subset(select = -c(site_order,
                                     landuse_type,
                                     sum_PS))
str(cm_data)


## Relationship exploration ----
ggcorr(cm_data, size = 3, hjust = 0.9, label = TRUE, layout.exp = 2)
ggsave(file = "correlation_matrix.png", width = 5, height = 5)

# nitrate is the most correlated parameter with ASPT (R = -0.6)
# other correlated parameters (>|0.3|) = conductivity, depth, phosphate, velocity, BOD
# color has no effect on ASPT (R = 0)
# absorbance, turbidity, sediment size, and width have basically no effect on ASPT (R = ±0.1)

# conductivity very correlated with nitrate (0.6), will probably only use 1 of the 2
# depth and pH fairly correlated (0.5), width and phosphate fairly correlated (0.5)


## Making a linear model for WQ ----
# comparison of main correlated parameters (nitrate, phosphate, conductivity, depth)
main1 <- lm(ASPT ~ avg.nitrate, data = imp_data)   

main2 <- lm(ASPT ~ avg.phosphate, data = imp_data) 

main3 <- lm(ASPT ~ avg.conductivity_uScm, data = imp_data) 

main4 <- lm(ASPT ~ avg.depth_cm, data = imp_data) 

AIC(main1, main2, main3, main4)
# nitrate is the best, do not remove this from the model 

# not including parameters with ≤0.2 correlation with ASPT
# elimination of bottom parameter each time
m1 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm
         + avg.pH
         + avg.velocity_ms
         + avg.depth_cm
         + BODmgL, data = imp_data)  

m2 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm
         + avg.pH
         + avg.velocity_ms
         + avg.depth_cm, data = imp_data)  

m3 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm
         + avg.pH
         + avg.velocity_ms, data = imp_data)  

m4 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm
         + avg.pH, data = imp_data)  

m5 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate
         + avg.conductivity_uScm, data = imp_data)  

m6 <- lm(ASPT ~ avg.nitrate 
         + avg.phosphate, data = imp_data)  


# comparing the F-values of the models
AIC(m1, m2, m3, m4, m5, m6) 
# best is m3

# removal of conductivity from best model thus far
m3_cond <- lm(ASPT ~ avg.nitrate 
           + avg.phosphate
           + avg.pH
           + avg.velocity_ms, data = imp_data) 

AIC(m3, m3_cond)
# removing conductivity made it better 

# elimination of top parameter each time (always keeping nitrate), based on m3_cond
m7 <- lm(ASPT ~ avg.nitrate 
          + avg.pH
          + avg.velocity_ms, data = imp_data)  

m8 <- lm(ASPT ~ avg.nitrate 
          + avg.velocity_ms, data = imp_data)  

# other possible logical combinations
m9 <- lm(ASPT ~ avg.nitrate 
          + avg.pH, data = imp_data)  

m10 <- lm(ASPT ~ avg.nitrate 
         + avg.depth_cm   # since it's quite correlated to pH
         + avg.width_cm   # since it's quite correlated to phosphate
         + avg.velocity_ms, data = imp_data) 

m11 <- lm(ASPT ~ avg.nitrate 
          + avg.pH  
          + avg.width_cm   # since it's quite correlated to phosphate
          + avg.velocity_ms, data = imp_data) 

m12 <- lm(ASPT ~ avg.nitrate 
          + avg.depth_cm   # since it's quite correlated to pH
          + avg.phosphate   
          + avg.velocity_ms, data = imp_data) 

# comparing the F-values of the models
AIC(m3, m3_cond, m7, m8, m9, m10, m11, m12)
# depth and width did not improve the model, made it worse, so sticking to pH and phosphate
# m3_cond still the best by almost a value of 2


summary(m3_cond)
# intercept = 12.7565
# nitrate slope = -3.1772 
# phosphate slope = -5.3183
# pH slope = -0.4949
# velocity slope = -2.1734




# testing another model 
wq_model <- lm(ASPT ~ 1 
               + avg.nitrate 
               + avg.phosphate
               + avg.pH
               + avg.velocity_ms, data = imp_data) 
summary(wq_model)

nulltest <- lm(ASPT ~
               + avg.nitrate 
               + avg.phosphate
               + avg.pH
               + avg.velocity_ms, data = imp_data) 
summary(nulltest)

                      


