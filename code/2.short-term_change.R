#### Process ISIMIP shocks data across sectors
#### Coding: Aurore A. Maureaud, June 2024
#### For more information about methods testing, see shock_detection.Rmd file

# load libraries
library(here)
library(tidyverse)
library(ggplot2)
library(gridExtra)


################################################################################
#### 1. GLOBAL SHOCKS
################################################################################
# load time-series data
# avg = average (end of century); g = global; r = region; cs = cross-sector
pct_diff_ts_g_cs <- read_csv("data/long-term_change/global_time_series_2010-2019_2090-2099_revised.csv") %>% 
  mutate(id = paste(sector, eco_model, climate_model, experiment_climate, output_variable, sep = "/"))
ids <- sort(unique(pct_diff_ts_g_cs$id))
shock_ts_g_cs <- data.frame()

for(i in 1:length(ids)){
  
  # subset data
  xx <- pct_diff_ts_g_cs %>% 
    filter(id==ids[i])
  
  if(length(unique(xx$percent_diff))==1){
    xx <- xx %>% 
      mutate(res = NA,
             shock = NA,
             shock_up = NA,
             shock_down = NA)
  } else {
    # loess with span = 1
    model_smooth <- loess(xx$percent_diff ~ xx$years, span = 0.75)
    xx$res <- (model_smooth$residuals - mean(model_smooth$residuals))/sd(model_smooth$residuals)
    
    # count shocks
    xx <- xx %>% 
      mutate(shock = ifelse(res>2, 1, NA),
             shock = ifelse(res<(-2), 1, shock),
             shock_up = ifelse(res>2, 1, NA),
             shock_down = ifelse(res<(-2), 1, NA))
  }
  
  if(i==1){shock_ts_g_cs <- xx}
  if(i>1){shock_ts_g_cs <- rbind(shock_ts_g_cs, xx)}
  
  rm(xx, model_smooth)
}

write.csv(shock_ts_g_cs, file = "data/short-term_change/global_shocks_time_series_2010-2019.csv", row.names = F)


################################################################################
#### 2. COUNTRY SHOCKS
################################################################################

# load time-series data
# avg = average (end of century); g = global; r = region; cs = cross-sector
pct_diff_ts_r_cs <- read_csv("data/long-term_change/countries_time_series_2010-2019_2090-2099_revised.csv") %>% 
  mutate(id = paste(sector, regions, eco_model, climate_model, experiment_climate, output_variable, sep = "/"))
ids <- sort(unique(pct_diff_ts_r_cs$id))
shock_ts_r_cs <- data.frame()

for(i in 1:length(ids)){
  
  # subset data
  xx <- pct_diff_ts_r_cs %>% 
    filter(id==ids[i])
  
  if(length(unique(xx$percent_diff))==1){
    xx <- xx %>% 
      mutate(res = NA,
             shock = NA,
             shock_up = NA,
             shock_down = NA)
  } else {
    # loess with span = 1
    model_smooth <- loess(xx$percent_diff ~ xx$years, span = 0.75)
    xx$res <- (model_smooth$residuals - mean(model_smooth$residuals))/sd(model_smooth$residuals)
    
    # count shocks
    xx <- xx %>% 
      mutate(shock = ifelse(res>2, 1, NA),
             shock = ifelse(res<(-2), 1, shock),
             shock_up = ifelse(res>2, 1, NA),
             shock_down = ifelse(res<(-2), 1, NA))
  }
  
  if(i==1){shock_ts_r_cs <- xx}
  if(i>1){shock_ts_r_cs <- rbind(shock_ts_r_cs, xx)}
  
  rm(xx, model_smooth)
}

write.csv(shock_ts_r_cs, file = "data/short-term_change/countries_shocks_time_series_2010-2019.csv", row.names = F)

