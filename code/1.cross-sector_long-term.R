#### Process ISIMIP shocks data across sectors
#### Coding: Aurore A. Maureaud, July 2024
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
pct_diff_ts_g_cs_ag <- read_csv("data/data_processing/global_time_series_1985-2015_2070-2099_agriculture.csv")
pct_diff_ts_g_cs_fi <- read_csv("data/data_processing/global_time-series_1985-2015_2070-2099_marine-fishery_global.csv")
pct_diff_ts_g_cs_wa <- read_csv("data/data_processing/global_time_series_1985-2015_2070-2099_water_global.csv")
pct_diff_ts_g_cs <- rbind(pct_diff_ts_g_cs_ag, pct_diff_ts_g_cs_fi, pct_diff_ts_g_cs_wa)

#### A. get the long-term change probabilities ----
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
write.csv(change_proba_g_cs, file = "data/long-term_change/global_long-term_change_1985-2015_probas.csv",
          row.names = F)

#### B. get long-term change probability synchrony and compensation ----
yrs <- c(2069,2079,2089)
change_proba_g_cs_mec <- data.frame()

for(y in 1:length(yrs)){
  # average of % change across the last 10 years of the time-series
  yy <- pct_diff_ts_g_cs %>% 
    filter(years>yrs[y],
           years<2100,
           !output_variable %in% c("ptotww","tcb","tws")) %>% 
    group_by(sector, spatial_scale, eco_model, climate_model, experiment_climate, output_variable) %>% 
    summarize(percent_diff = mean(percent_diff, na.rm=T)) %>% 
    mutate(change_5_up = ifelse(percent_diff>5 & output_variable!="qtot", 1, 0),
           change_10_up = ifelse(percent_diff>10 & output_variable!="qtot", 1, 0),
           change_25_up = ifelse(percent_diff>25 & output_variable!="qtot", 1, 0),
           change_5_down = ifelse(percent_diff<(-5), 1, 0),
           change_10_down = ifelse(percent_diff<(-10), 1, 0),
           change_25_down = ifelse(percent_diff<(-25), 1, 0),
           # transform increase in water into negative change
           change_5_down = ifelse(percent_diff>5 & output_variable=="qtot", 1, change_5_down),
           change_10_down = ifelse(percent_diff>10 & output_variable=="qtot", 1, change_10_down),
           change_25_down = ifelse(percent_diff>25 & output_variable=="qtot", 1, change_25_down)) %>% 
    group_by(sector, spatial_scale, climate_model, experiment_climate, output_variable) %>% 
    summarize(change_5_up = round(mean(change_5_up, na.rm=T),4),
              change_10_up = round(mean(change_10_up, na.rm=T),4),
              change_25_up = round(mean(change_25_up, na.rm=T),4),
              change_5_down = round(mean(change_5_down, na.rm=T),4),
              change_10_down = round(mean(change_10_down, na.rm=T),4),
              change_25_down = round(mean(change_25_down, na.rm=T),4)) %>% 
    mutate(climates = paste(climate_model, experiment_climate))%>% 
    group_by(spatial_scale, climates) %>% 
    summarize(# get at least one res. up proba
              at_least_one_5_up = get_at_least_one(6, change_5_up),
              at_least_one_10_up = get_at_least_one(6, change_10_up),
              at_least_one_25_up = get_at_least_one(6, change_25_up),
              # get at least one res. down proba
              at_least_one_5_down = get_at_least_one(6, change_5_down),
              at_least_one_10_down = get_at_least_one(6, change_10_down),
              at_least_one_25_down = get_at_least_one(6, change_25_down),
              # get at least one res. up and one res. down proba
              at_least_one_5_up_down = at_least_one_5_up*at_least_one_5_down,
              at_least_one_10_up_down = at_least_one_10_up*at_least_one_10_down,
              at_least_one_25_up_down = at_least_one_25_up*at_least_one_25_down,
              # get at least two res. down proba
              at_least_two_5_down = get_at_least_two(6, change_5_down),
              at_least_two_10_down = get_at_least_two(6, change_10_down),
              at_least_two_25_down = get_at_least_two(6, change_25_down),
              # get at least three res. down proba
              at_least_three_5_down = get_at_least_three(6, change_5_down),
              at_least_three_10_down = get_at_least_three(6, change_10_down),
              at_least_three_25_down = get_at_least_three(6, change_25_down)) %>% 
    # dplyr::select(spatial_scale, climates, at_least_one_5_up_down, at_least_one_10_up_down,
    #               at_least_one_25_up_down, at_least_two_5_down, at_least_two_10_down,
    #               at_least_two_25_down, at_least_three_5_down, at_least_three_10_down,
    #               at_least_three_25_down) %>% 
    pivot_longer(3:17, names_to = "type", values_to = "probas") %>% 
    mutate(time_window = 2099-yrs[y])
  
  if(y==1){change_proba_g_cs_mec <- yy
  } else {change_proba_g_cs_mec <- rbind(change_proba_g_cs_mec,yy)}
  rm(yy)
  
}

# save outputs
write.csv(change_proba_g_cs_mec, file = "data/long-term_change/global_long-term_change_1985-2015_probas_mechanisms.csv",
          row.names = F)


################################################################################
#### 2. COUNTRY CS CHANGE
################################################################################
# load time-series data
pct_diff_ts_r_cs_ag <- read_csv("data/data_processing/countries_time_series_1985-2015_2070-2099_agriculture.csv")
pct_diff_ts_r_cs_fi <- read_csv("data/data_processing/countries_time-series_1985-2015_2070-2099_marine-fishery_global.csv")
pct_diff_ts_r_cs_wa <- read_csv("data/data_processing/countries_time_series_1985-2015_2070-2099_water_global.csv")
pct_diff_ts_r_cs <- rbind(pct_diff_ts_r_cs_ag, pct_diff_ts_r_cs_fi, pct_diff_ts_r_cs_wa)

#### A. get the long-term change probabilities ----
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
write.csv(change_proba_r_cs, file = "data/long-term_change/countries_long-term_change_1985-2015_probas.csv",
          row.names = F)

#### B. get long-term change probability synchrony and compensation ----
yrs <- c(2069,2079,2089)
change_proba_r_cs_mec <- data.frame()

for(y in 1:length(yrs)){
  # average of % change across the last 10 years of the time-series
  yy <- pct_diff_ts_r_cs %>% 
    filter(years>yrs[y],
           years<2100,
           !output_variable %in% c("ptotww","tcb","tws")) %>% 
    group_by(sector, spatial_scale, regions, eco_model, climate_model, experiment_climate, output_variable) %>% 
    summarize(percent_diff = mean(percent_diff, na.rm=T)) %>% 
    mutate(change_5_up = ifelse(percent_diff>5 & output_variable!="qtot", 1, 0),
           change_10_up = ifelse(percent_diff>10 & output_variable!="qtot", 1, 0),
           change_25_up = ifelse(percent_diff>25 & output_variable!="qtot", 1, 0),
           change_5_up = ifelse(is.na(percent_diff) & output_variable=="qtot", NA, change_5_up),
           change_10_up = ifelse(is.na(percent_diff) & output_variable=="qtot", NA, change_10_up),
           change_25_up = ifelse(is.na(percent_diff) & output_variable=="qtot", NA, change_25_up),
           change_5_down = ifelse(percent_diff<(-5), 1, 0),
           change_10_down = ifelse(percent_diff<(-10), 1, 0),
           change_25_down = ifelse(percent_diff<(-25), 1, 0),
           # transform increase in water into negative change
           change_5_down = ifelse(percent_diff>5 & output_variable=="qtot", 1, change_5_down),
           change_10_down = ifelse(percent_diff>10 & output_variable=="qtot", 1, change_10_down),
           change_25_down = ifelse(percent_diff>25 & output_variable=="qtot", 1, change_25_down)) %>% 
    group_by(sector, spatial_scale, regions, climate_model, experiment_climate, output_variable) %>% 
    summarize(change_5_up = round(mean(change_5_up, na.rm=T),4),
              change_10_up = round(mean(change_10_up, na.rm=T),4),
              change_25_up = round(mean(change_25_up, na.rm=T),4),
              change_5_down = round(mean(change_5_down, na.rm=T),4),
              change_10_down = round(mean(change_10_down, na.rm=T),4),
              change_25_down = round(mean(change_25_down, na.rm=T),4)) %>% 
    mutate(climates = paste(climate_model, experiment_climate)) %>% 
    group_by(spatial_scale, regions, climates) %>% 
    summarize(# get at least one res. up proba
      at_least_one_5_up = get_at_least_one(6, change_5_up),
      at_least_one_10_up = get_at_least_one(6, change_10_up),
      at_least_one_25_up = get_at_least_one(6, change_25_up),
      # get at least one res. down proba
      at_least_one_5_down = get_at_least_one(6, change_5_down),
      at_least_one_10_down = get_at_least_one(6, change_10_down),
      at_least_one_25_down = get_at_least_one(6, change_25_down),
      # get at least one res. up and one res. down proba
      at_least_one_5_up_down = at_least_one_5_up*at_least_one_5_down,
      at_least_one_10_up_down = at_least_one_10_up*at_least_one_10_down,
      at_least_one_25_up_down = at_least_one_25_up*at_least_one_25_down,
      # get at least two res. down proba
      at_least_two_5_down = get_at_least_two(6, change_5_down),
      at_least_two_10_down = get_at_least_two(6, change_10_down),
      at_least_two_25_down = get_at_least_two(6, change_25_down),
      # get at least three res. down proba
      at_least_three_5_down = get_at_least_three(6, change_5_down),
      at_least_three_10_down = get_at_least_three(6, change_10_down),
      at_least_three_25_down = get_at_least_three(6, change_25_down),
      # get at least two res. up and three res. up probas
      at_least_two_5_up = get_at_least_two(6, change_5_up),
      at_least_two_10_up = get_at_least_two(6, change_10_up),
      at_least_two_25_up = get_at_least_two(6, change_25_up),
      at_least_three_5_up = get_at_least_three(6, change_5_up),
      at_least_three_10_up = get_at_least_three(6, change_10_up),
      at_least_three_25_up = get_at_least_three(6, change_25_up)) %>% 
    # dplyr::select(spatial_scale, climates, at_least_one_5_up_down, at_least_one_10_up_down,
    #               at_least_one_25_up_down, at_least_two_5_down, at_least_two_10_down,
    #               at_least_two_25_down, at_least_three_5_down, at_least_three_10_down,
    #               at_least_three_25_down) %>% 
    pivot_longer(4:24, names_to = "type", values_to = "probas") %>% 
    mutate(time_window = 2099-yrs[y])
  
  if(y==1){change_proba_r_cs_mec <- yy
  } else {change_proba_r_cs_mec <- rbind(change_proba_r_cs_mec,yy)}
  rm(yy)
  
}

# save outputs
write.csv(change_proba_r_cs_mec, file = "data/long-term_change/countries_long-term_change_1985-2015_probas_mechanisms.csv",
          row.names = F)


################################################################################
#### 3. Regional Trade Agreements CS CHANGE
################################################################################

# load time-series data
pct_diff_ts_rta_cs_ag <- read_csv("data/data_processing/rta_time-series_1985-2015_2070-2099_agriculture.csv") %>% 
  dplyr::rename(regions = rtas)
pct_diff_ts_rta_cs_fi <- read_csv("data/data_processing/rta_time-series_1985-2015_2070-2099_marine-fishery_global.csv")
pct_diff_ts_rta_cs_wa <- read_csv("data/data_processing/rta_time-series_1985-2015_2070-2099_water_global.csv")
pct_diff_ts_rta_cs <- rbind(pct_diff_ts_rta_cs_ag, pct_diff_ts_rta_cs_fi, pct_diff_ts_rta_cs_wa)

#### A. get the long-term change probabilities ----
yrs <- c(2069,2079,2089)
change_proba_rta_cs <- data.frame()

for(y in 1:length(yrs)){
  # average of % change across the last 10 years of the time-series
  yy <- pct_diff_ts_rta_cs %>% 
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
  
  if(y==1){change_proba_rta_cs <- yy
  } else {change_proba_rta_cs <- rbind(change_proba_rta_cs,yy)}
  rm(yy)
  
}

# save outputs
write.csv(change_proba_rta_cs, file = "data/long-term_change/rta_long-term_change_1985-2015_probas.csv",
          row.names = F)

#### B. get long-term change probability synchrony and compensation ----
yrs <- c(2069,2079,2089)
change_proba_rta_cs_mec <- data.frame()

for(y in 1:length(yrs)){
  # average of % change across the last 10 years of the time-series
  yy <- pct_diff_ts_rta_cs %>% 
    filter(years>yrs[y],
           years<2100,
           !output_variable %in% c("ptotww","tcb","tws")) %>% 
    group_by(sector, spatial_scale, regions, eco_model, climate_model, experiment_climate, output_variable) %>% 
    summarize(percent_diff = mean(percent_diff, na.rm=T)) %>% 
    mutate(change_5_up = ifelse(percent_diff>5 & output_variable!="qtot", 1, 0),
           change_10_up = ifelse(percent_diff>10 & output_variable!="qtot", 1, 0),
           change_25_up = ifelse(percent_diff>25 & output_variable!="qtot", 1, 0),
           change_5_up = ifelse(is.na(percent_diff) & output_variable=="qtot", NA, change_5_up),
           change_10_up = ifelse(is.na(percent_diff) & output_variable=="qtot", NA, change_10_up),
           change_25_up = ifelse(is.na(percent_diff) & output_variable=="qtot", NA, change_25_up),
           change_5_down = ifelse(percent_diff<(-5), 1, 0),
           change_10_down = ifelse(percent_diff<(-10), 1, 0),
           change_25_down = ifelse(percent_diff<(-25), 1, 0),
           # transform increase in water into negative change
           change_5_down = ifelse(percent_diff>5 & output_variable=="qtot", 1, change_5_down),
           change_10_down = ifelse(percent_diff>10 & output_variable=="qtot", 1, change_10_down),
           change_25_down = ifelse(percent_diff>25 & output_variable=="qtot", 1, change_25_down)) %>% 
    group_by(sector, spatial_scale, regions, climate_model, experiment_climate, output_variable) %>% 
    summarize(change_5_up = round(mean(change_5_up, na.rm=T),4),
              change_10_up = round(mean(change_10_up, na.rm=T),4),
              change_25_up = round(mean(change_25_up, na.rm=T),4),
              change_5_down = round(mean(change_5_down, na.rm=T),4),
              change_10_down = round(mean(change_10_down, na.rm=T),4),
              change_25_down = round(mean(change_25_down, na.rm=T),4)) %>% 
    mutate(climates = paste(climate_model, experiment_climate)) %>% 
    group_by(spatial_scale, regions, climates) %>% 
    summarize(# get at least one res. up proba
      at_least_one_5_up = get_at_least_one(6, change_5_up),
      at_least_one_10_up = get_at_least_one(6, change_10_up),
      at_least_one_25_up = get_at_least_one(6, change_25_up),
      # get at least one res. down proba
      at_least_one_5_down = get_at_least_one(6, change_5_down),
      at_least_one_10_down = get_at_least_one(6, change_10_down),
      at_least_one_25_down = get_at_least_one(6, change_25_down),
      # get at least one res. up and one res. down proba
      at_least_one_5_up_down = at_least_one_5_up*at_least_one_5_down,
      at_least_one_10_up_down = at_least_one_10_up*at_least_one_10_down,
      at_least_one_25_up_down = at_least_one_25_up*at_least_one_25_down,
      # get at least two res. down proba
      at_least_two_5_down = get_at_least_two(6, change_5_down),
      at_least_two_10_down = get_at_least_two(6, change_10_down),
      at_least_two_25_down = get_at_least_two(6, change_25_down),
      # get at least three res. down proba
      at_least_three_5_down = get_at_least_three(6, change_5_down),
      at_least_three_10_down = get_at_least_three(6, change_10_down),
      at_least_three_25_down = get_at_least_three(6, change_25_down)) %>% 
    # dplyr::select(spatial_scale, climates, at_least_one_5_up_down, at_least_one_10_up_down,
    #               at_least_one_25_up_down, at_least_two_5_down, at_least_two_10_down,
    #               at_least_two_25_down, at_least_three_5_down, at_least_three_10_down,
    #               at_least_three_25_down) %>% 
    pivot_longer(4:18, names_to = "type", values_to = "probas") %>% 
    mutate(time_window = 2099-yrs[y])
  
  if(y==1){change_proba_rta_cs_mec <- yy
  } else {change_proba_rta_cs_mec <- rbind(change_proba_rta_cs_mec,yy)}
  rm(yy)
  
}

# save outputs
write.csv(change_proba_rta_cs_mec, file = "data/long-term_change/rta_long-term_change_1985-2015_probas_mechanisms.csv",
          row.names = F)

