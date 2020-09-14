# EES Field Course - Group 1 - Freshwater Invertebrates and Water Quality
# Linear model of water quality 
# Emma Gemal, s1758915@sms.ed.ac.uk
# Last edited: 14/9/2020

## Library ----
library(tidyverse)
library(ggpubr)


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
