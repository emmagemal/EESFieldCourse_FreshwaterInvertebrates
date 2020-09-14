# EES Field Course - Group 1 - Freshwater Invertebrates and Water Quality
# Emma Gemal, s1758915@sms.ed.ac.uk
# Last edited: 13/9/2020

## Library ----
library(tidyverse)


# load data
all_data <- read.csv("Data/alldata_missingvalues.csv")

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
    # p-value = 0.251 (don't use phosphate as the main parameter)

wq_model3 <- lm(ASPT ~ avg.pH, data = imp_data)  
summary(wq_model3)
    # p-value = 0.2564 (don't use pH as the main parameter)

wq_model4 <- lm(ASPT ~ avg.velocity_.m.s., data = imp_data)  
summary(wq_model4)
    # p-values = 0.255 (don't use velocity as the main parameter)

wq_model5 <- lm(ASPT ~ avg.conductivity_.uS.cm., data = imp_data)  
summary(wq_model5)
    # p-value = 0.04749 

wq_model6 <- lm(ASPT ~ avg.depth_.cm., data = imp_data)  
summary(wq_model6)
    # p-value = 0.1688 (don't use depth as the main parameter)


# deduction model (start with everything)
wq_dm1 <- lm(ASPT ~ avg.nitrate 
                + avg.phosphate
                + avg.conductivity_.uS.cm.
                + avg.pH
                + avg.velocity_.m.s.
                + avg.depth_.cm.
                + avg.colour_.mg.LPt.
                + avg.turbidity_.FTU.
                + avg.sediment_.cm.
                + BOD_DO., data = imp_data)  
summary(wq_model7)
    # p-value = 0.2086

wq_dm2 <- lm(ASPT ~ avg.conductivity_.uS.cm. 
                + avg.phosphate
                + avg.nitrate
                + avg.pH
                + avg.velocity_.m.s.
                + avg.depth_.cm.
                + avg.colour_.mg.LPt.
                + avg.turbidity_.FTU.
                + avg.sediment_.cm.
                + BOD_DO., data = imp_data)  
summary(wq_dm2)
    # p-value = 0.507 (don't use conductivity as the main parameter)

wq_dm3 <- lm(ASPT ~ avg.nitrate 
             + avg.phosphate
             + avg.conductivity_.uS.cm.
             + avg.pH
             + avg.velocity_.m.s.
             + avg.depth_.cm.
             + avg.colour_.mg.LPt.
             + avg.turbidity_.FTU.
             + BOD_DO., data = imp_data)  
summary(wq_dm3)
    # p-value = 0.303 (removing sediment size made it worse)

wq_dm4 <- lm(ASPT ~ avg.nitrate 
             + avg.phosphate
             + avg.conductivity_.uS.cm.
             + avg.pH
             + avg.velocity_.m.s.
             + avg.depth_.cm.
             + avg.colour_.mg.LPt.
             + avg.sediment_.cm.
             + BOD_DO., data = imp_data)  
summary(wq_dm4)
    # p-value = 0.5105 (removing turbidity made it way worse, so it's needed)

wq_dm5 <- lm(ASPT ~ avg.nitrate 
             + avg.phosphate
             + avg.conductivity_.uS.cm.
             + avg.pH
             + avg.velocity_.m.s.
             + avg.depth_.cm.
             + avg.turbidity_.FTU.
             + avg.sediment_.cm.
             + BOD_DO., data = imp_data)  
summary(wq_dm5)
    # p-value = 0.2932 (removing color doesn't have a massive impact)

wq_dm6 <- lm(ASPT ~ avg.nitrate 
             + avg.phosphate
             + avg.conductivity_.uS.cm.
             + avg.pH
             + avg.velocity_.m.s.
             + avg.depth_.cm.
             + avg.colour_.mg.LPt.
             + avg.turbidity_.FTU.
             + avg.sediment_.cm., data = imp_data)  
summary(wq_dm6)
    # p-value = 0.2943 (removing BOD doesn't have a massive impact)

wq_dm7 <- lm(ASPT ~ avg.nitrate 
             + avg.phosphate
             + avg.conductivity_.uS.cm.
             + avg.pH
             + avg.velocity_.m.s.
             + avg.colour_.mg.LPt.
             + avg.turbidity_.FTU.
             + avg.sediment_.cm.
             + BOD_DO., data = imp_data)  
summary(wq_dm7)
    # p-value = 0.2893 (removing depth doesn't have a massive impact)

wq_dm8 <- lm(ASPT ~ avg.nitrate 
             + avg.phosphate
             + avg.conductivity_.uS.cm.
             + avg.pH
             + avg.depth_.cm.
             + avg.colour_.mg.LPt.
             + avg.turbidity_.FTU.
             + avg.sediment_.cm.
             + BOD_DO., data = imp_data)  
summary(wq_dm8)
    # p-value = 0.2833 (removing velocity has the least effect so far)

wq_dm9 <- lm(ASPT ~ avg.nitrate 
             + avg.phosphate
             + avg.conductivity_.uS.cm.
             + avg.velocity_.m.s.
             + avg.depth_.cm.
             + avg.colour_.mg.LPt.
             + avg.turbidity_.FTU.
             + avg.sediment_.cm.
             + BOD_DO., data = imp_data) 
summary(wq_dm9)
    # p-value = 0.6141 (removing pH has a large effect, needs to be kept)

wq_dm10 <- lm(ASPT ~ avg.nitrate 
             + avg.conductivity_.uS.cm.
             + avg.pH
             + avg.velocity_.m.s.
             + avg.depth_.cm.
             + avg.colour_.mg.LPt.
             + avg.turbidity_.FTU.
             + avg.sediment_.cm.
             + BOD_DO., data = imp_data) 
summary(wq_dm10)
    # p-value = 0.4015 (removing phosphate has a large effect, should be kept)

wq_elim_m1 <- lm(ASPT ~ avg.nitrate 
             + avg.phosphate
             + avg.conductivity_.uS.cm.
             + avg.pH
             + avg.colour_.mg.LPt.
             + avg.turbidity_.FTU.
             + avg.sediment_.cm.
             + BOD_DO., data = imp_data) 
summary(wq_elim_m1)
    # p-value = 0.1392 (velocity and depth removed, improved from original)

wq_elim_m2 <- lm(ASPT ~ avg.nitrate 
                 + avg.phosphate
                 + avg.conductivity_.uS.cm.
                 + avg.pH
                 + avg.colour_.mg.LPt.
                 + avg.turbidity_.FTU.
                 + avg.sediment_.cm., data = imp_data) 
summary(wq_elim_m2)
    # p-value = 0.06042 (removed BOD, as well as velocity and depth, improved even more)

wq_elim_m3 <- lm(ASPT ~ avg.nitrate 
                 + avg.phosphate
                 + avg.conductivity_.uS.cm.
                 + avg.pH
                 + avg.turbidity_.FTU.
                 + avg.sediment_.cm., data = imp_data) 
summary(wq_elim_m3)
    # p-value = 0.04626 (removed color, plus BOD, velocity and depth, improved even more)

wq_elim_m4 <- lm(ASPT ~ avg.nitrate 
                 + avg.phosphate
                 + avg.conductivity_.uS.cm.
                 + avg.pH
                 + avg.turbidity_.FTU., data = imp_data) 
summary(wq_elim_m4)
    # p-value = 0.02674 (removed sediment size, plus color, BOD, velocity, depth, it's even better)

