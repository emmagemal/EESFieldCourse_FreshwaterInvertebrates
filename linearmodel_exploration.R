# EES Field Course - Group 1 - Freshwater Invertebrates and Water Quality
# Emma Gemal, s1758915@sms.ed.ac.uk
# Last edited: 13/9/2020

## Library ----
library(tidyverse)

# load data
all_data <- read.csv("alldata_missingvalues.csv")

## Data manipulation ----
str(all_data)
all_data <- all_data %>% 
  slice(-c(17:27))

imp_data <- all_data %>% 
  subset(select = -c(date, 
                     time_sampled, 
                     site_description, 
                     weather_conditions)) %>% 
  subset(select = -c(96:124))

str(imp_data)

## Relationship exploration ----
# nitrate
no_aspt <- ggplot(imp_data, aes(x = ASPT, y = avg.nitrate)) +
                  geom_point(size = 2)
no_aspt   # lower NO3- in higher water qualities 

# phosphate
po_aspt <- ggplot(imp_data, aes(x = ASPT, y = avg.phosphate)) +
  geom_point(size = 2)
po_aspt   # high PO43- at low water qualities, spread at higher water qualities 

# pH
pH_aspt <- ggplot(imp_data, aes(x = ASPT, y = avg.pH)) +
  geom_point(size = 2)
pH_aspt   # more alkaline at lower water qualities, but not very correlated at all 

# velocity
vel_aspt <- ggplot(imp_data, aes(x = ASPT, y = avg.velocity_.m.s.)) +
                   geom_point(size = 2)
vel_aspt   # spread of velocities at lower water qualities, a bit of a decline in velocity with quality

# width
width_aspt <- ggplot(imp_data, aes(x = ASPT, y = avg.width_.cm.)) +
                     geom_point(size = 2)
width_aspt   # a spread 

# depth
depth_aspt <- ggplot(imp_data, aes(x = ASPT, y = avg.depth_.cm.)) +
                     geom_point(size = 2)
depth_aspt   # a spread across low water qualities, lower depth at higher water qualities 

# turbidity 
turb_aspt <- ggplot(imp_data, aes(x = ASPT, y = avg.turbidity_.FTU.)) +
                     geom_point(size = 2)
turb_aspt   # a spread, no real relationship 

# color
colour_aspt <- ggplot(imp_data, aes(x = ASPT, y = avg.colour_.mg.LPt.)) +
                     geom_point(size = 2)
colour_aspt   # a spread, no real relationship 

# conductivity 
cond_aspt <- ggplot(imp_data, aes(x = ASPT, y = avg.conductivity_.uS.cm.)) +
                    geom_point(size = 2)
cond_aspt   # a cluster across water qualities 


## Model exploration ----
wq_model1 <- lm(ASPT ~ avg.nitrate, data = imp_data)  
summary(wq_model1)
    # intercept = 7.2712
    # slope = -3.4430
    # p-value = 0.02085

wq_model2 <- lm(ASPT ~ avg.phosphate, data = imp_data)  
summary(wq_model2)
    # p-value = 0.251 (don't use this as the main parameter)

wq_model3 <- lm(ASPT ~ avg.pH, data = imp_data)  
summary(wq_model3)
    # p-value = 0.2564 (don't use this)

wq_model4 <- lm(ASPT ~ avg.velocity_.m.s., data = imp_data)  
summary(wq_model4)
    # p-values = 0.255 (don't use this)

wq_model5 <- lm(ASPT ~ avg.conductivity_.uS.cm., data = imp_data)  
summary(wq_model5)
    # p-value = 0.04749 (definitely use with nitrate)



wq_model3 <- lm(ASPT ~ avg.nitrate + avg.phosphate, data = imp_data)  
summary(wq_model3)
    # p-value = 0.1305, makes the first model worse 

wq_model4 <- lm(ASPT ~ avg.nitrate + avg.pH, data = imp_data)  
summary(wq_model4)
    # p-value = 0.02155

