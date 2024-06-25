#### Process ISIMIP shocks data across sectors
#### Coding: Aurore A. Maureaud, June 2024
#### For more information about methods testing, see shock_detection.Rmd file

# load libraries
library(here)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# load functions
source("functions/get_probas.R")


################################################################################
#### 1. GLOBAL CS CHANGE
################################################################################
# load time-series data
# avg = average (end of century); g = global; r = region; cs = cross-sector
pct_diff_ts_g_cs <- read_csv("data/long-term_change/global_time_series_2010-2019_2090-2099_revised.csv")

yrs <- c(2069,2079,2089)
change_proba_g_cs <- data.frame()

for(y in 1:length(yrs)){
  # average of % change across the last 10 years of the time-series
  yy <- pct_diff_ts_g_cs %>% 
    filter(years>yrs[y],
           years<2100,
           !output_variable %in% c("ptotww","tcb","tws")) %>% 
    group_by(sector, spatial_scale, eco_model, climate_model, experiment_climate, output_variable) %>% 
    summarize(percent_diff = mean(percent_diff, na.rm=T)) %>% 
    mutate(change_5 = ifelse(percent_diff>5 | percent_diff<(-5), 1, 0),
           change_10 = ifelse(percent_diff>10 | percent_diff<(-10), 1, 0),
           change_25 = ifelse(percent_diff>25 | percent_diff<(-25), 1, 0)) %>% 
    group_by(sector, spatial_scale, climate_model, experiment_climate, output_variable) %>% 
    summarize(change_5 = round(mean(change_5, na.rm=T),4),
              change_10 = round(mean(change_10, na.rm=T),4),
              change_25 = round(mean(change_25, na.rm=T),4)) %>% 
    mutate(climates = paste(climate_model, experiment_climate))%>% 
    group_by(spatial_scale, climates) %>% 
    summarize(at_least_one_change_5 = get_at_least_one(6, change_5),
              at_least_two_change_5 = get_at_least_two(6, change_5),
              at_least_three_change_5 = get_at_least_three(6, change_5),
              at_least_one_change_10 = get_at_least_one(6, change_10),
              at_least_two_change_10 = get_at_least_two(6, change_10),
              at_least_three_change_10 = get_at_least_three(6, change_10),
              at_least_one_change_25 = get_at_least_one(6, change_25),
              at_least_two_change_25 = get_at_least_two(6, change_25),
              at_least_three_change_25 = get_at_least_three(6, change_25)) %>% 
    pivot_longer(3:11, names_to = "type", values_to = "probas") %>% 
    mutate(time_window = 2099-yrs[y])
  
  if(y==1){change_proba_g_cs <- yy
  } else {change_proba_g_cs <- rbind(change_proba_g_cs,yy)}
  rm(yy)
  
}

# save outputs
write.csv(change_proba_g_cs, file = "data/long-term_change/global_long-term_change_2010-2019_probas.csv",
          row.names = F)


################################################################################
#### 2. COUNTRY CS CHANGE
################################################################################
# load time-series data
pct_diff_ts_r_cs <- read_csv("data/long-term_change/countries_time_series_2010-2019_2090-2099_revised.csv")

yrs <- c(2069,2079,2089)
change_proba_r_cs <- data.frame()

for(y in 1:length(yrs)){
  # average of % change across the last 10 years of the time-series
  yy <- pct_diff_ts_r_cs %>% 
    filter(years>yrs[y],
           years<2100,
           !output_variable %in% c("ptotww","tcb","tws")) %>% 
    data.frame() %>% 
    group_by(sector, spatial_scale, regions, eco_model, climate_model, experiment_climate, output_variable) %>% 
    summarize(percent_diff = mean(percent_diff, na.rm=T)) %>% 
    mutate(change_5 = ifelse(percent_diff>5 | percent_diff<(-5), 1, 0),
           change_10 = ifelse(percent_diff>10 | percent_diff<(-10), 1, 0),
           change_25 = ifelse(percent_diff>25 | percent_diff<(-25), 1, 0)) %>% 
    group_by(sector, spatial_scale, regions, climate_model, experiment_climate, output_variable) %>% 
    summarize(change_5 = round(mean(change_5, na.rm=T),4),
              change_10 = round(mean(change_10, na.rm=T),4),
              change_25 = round(mean(change_25, na.rm=T),4)) %>% 
    mutate(climates = paste(climate_model, experiment_climate),
           change_5 = ifelse(is.na(change_5), 0, change_5),
           change_10 = ifelse(is.na(change_10), 0, change_10),
           change_25 = ifelse(is.na(change_25), 0, change_25)) %>% 
    group_by(spatial_scale, regions, climates) %>% 
    summarize(at_least_one_change_5 = get_at_least_one(6, change_5),
              at_least_two_change_5 = get_at_least_two(6, change_5),
              at_least_three_change_5 = get_at_least_three(6, change_5),
              at_least_one_change_10 = get_at_least_one(6, change_10),
              at_least_two_change_10 = get_at_least_two(6, change_10),
              at_least_three_change_10 = get_at_least_three(6, change_10),
              at_least_one_change_25 = get_at_least_one(6, change_25),
              at_least_two_change_25 = get_at_least_two(6, change_25),
              at_least_three_change_25 = get_at_least_three(6, change_25)) %>% 
    pivot_longer(4:12, names_to = "type", values_to = "probas") %>% 
    mutate(time_window = 2099-yrs[y])
  
  if(y==1){change_proba_r_cs <- yy
  } else {change_proba_r_cs <- rbind(change_proba_r_cs,yy)}
  rm(yy)
  
}

# save outputs
write.csv(change_proba_r_cs, file = "data/long-term_change/countries_long-term_change_2010-2019_probas.csv",
          row.names = F)
