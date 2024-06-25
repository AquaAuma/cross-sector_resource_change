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

# get shocks metrics across models, sectors, and time
xx <- read_csv("data/short-term_change/global_shocks_time_series_2010-2019.csv")%>% 
  filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
  mutate(shock = ifelse(is.na(shock),0,shock),
         shock_up = ifelse(is.na(shock_up),0,shock_up),
         shock_down = ifelse(is.na(shock_down),0,shock_down)) %>%
  mutate(climates = paste(climate_model, experiment_climate)) %>%
  group_by(climates, years, output_variable) %>% 
  summarize(shock = round(mean(shock),4),
            shock_down = round(mean(shock_down),4),
            shock_up = round(mean(shock_up),4)) %>% 
  dplyr::select(climates, years, output_variable, shock) %>% 
  pivot_wider(names_from = output_variable, values_from = shock) %>% 
  data.frame() 
  
yrs <- c(2069,2079,2089)
shock_proba_g_cs <- data.frame()
for(y in 1:length(yrs)){
  yy <- xx %>% 
    filter(years>yrs[y],
           years<2100)
  
  yy$at_least_one_shock <- yy$at_least_two_shocks <- yy$at_least_three_shocks <- NA
  yy$at_least_one_shock <- apply(yy[,3:8], 1, function(x) get_at_least_one(6,x))
  yy$at_least_two_shocks <- apply(yy[,3:8], 1, function(x) get_at_least_two(6,x))
  yy$at_least_three_shocks <- apply(yy[,3:8], 1, function(x) get_at_least_three(6,x))
  
  yy <- yy %>% 
    dplyr::select(climates, years, at_least_one_shock, at_least_two_shocks, at_least_three_shocks) %>% 
    group_by(climates) %>% 
    summarize(at_least_one_shock = get_at_least_one(10,at_least_one_shock),
              at_least_two_shocks = get_at_least_two(10, at_least_two_shocks),
              at_least_three_shocks = get_at_least_three(10, at_least_three_shocks),
              time_window = 2099-yrs[y])
  
  if(y==1){shock_proba_g_cs <- yy
  } else {shock_proba_g_cs <- rbind(shock_proba_g_cs,yy)}
  rm(yy)
}

write.csv(shock_proba_g_cs, file = "data/short-term_change/global_shocks_2010-2019_probas.csv", 
          row.names = F)


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
  
  # print id
  print(ids[i])
  
  # subset data
  xx <- pct_diff_ts_r_cs %>% 
    filter(id==ids[i])
  
  if(length(unique(xx$percent_diff))==1 | length(unique(xx[!is.na(xx$percent_diff),]$percent_diff))==1){
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
    rm(model_smooth)
  }
  
  if(i==1){shock_ts_r_cs <- xx}
  if(i>1){shock_ts_r_cs <- rbind(shock_ts_r_cs, xx)}
  
  rm(xx)
}

write.csv(shock_ts_r_cs, file = "data/short-term_change/countries_shocks_time_series_2010-2019.csv", row.names = F)

# get shocks metrics across models, sectors, and time
xx <- read_csv("data/short-term_change/countries_shocks_time_series_2010-2019.csv")%>% 
  filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
  mutate(shock = ifelse(is.na(shock),0,shock),
         shock_up = ifelse(is.na(shock_up),0,shock_up),
         shock_down = ifelse(is.na(shock_down),0,shock_down)) %>%
  mutate(climates = paste(climate_model, experiment_climate)) %>%
  group_by(regions, climates, years, output_variable) %>% 
  summarize(shock = round(mean(shock),4),
            shock_down = round(mean(shock_down),4),
            shock_up = round(mean(shock_up),4)) %>% 
  dplyr::select(regions, climates, years, output_variable, shock) %>% 
  pivot_wider(names_from = output_variable, values_from = shock) %>% 
  data.frame() 

yrs <- c(2069,2079,2089)
shock_proba_r_cs <- data.frame()
for(y in 1:length(yrs)){
  yy <- xx %>% 
    filter(years>yrs[y],
           years<2100)
  
  yy$at_least_one_shock <- yy$at_least_two_shocks <- yy$at_least_three_shocks <- NA
  yy$at_least_one_shock <- apply(yy[,4:9], 1, function(x) get_at_least_one(6,x))
  yy$at_least_two_shocks <- apply(yy[,4:9], 1, function(x) get_at_least_two(6,x))
  yy$at_least_three_shocks <- apply(yy[,4:9], 1, function(x) get_at_least_three(6,x))
  
  yy <- yy %>% 
    dplyr::select(regions, climates, years, at_least_one_shock, at_least_two_shocks, at_least_three_shocks) %>% 
    group_by(regions, climates) %>% 
    summarize(at_least_one_shock = get_at_least_one(10,at_least_one_shock),
              at_least_two_shocks = get_at_least_two(10, at_least_two_shocks),
              at_least_three_shocks = get_at_least_three(10, at_least_three_shocks),
              time_window = 2099-yrs[y])
  
  if(y==1){shock_proba_r_cs <- yy
  } else {shock_proba_r_cs <- rbind(shock_proba_r_cs,yy)}
  rm(yy)
}

write.csv(shock_proba_r_cs, file = "data/short-term_change/countries_shocks_2010-2019_probas.csv", 
          row.names = F)

