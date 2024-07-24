#### Process ISIMIP shocks data across sectors
#### Coding: Aurore A. Maureaud, July 2024
#### For more information about methods testing, see shock_detection.Rmd file

rm(list=ls())

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
pct_diff_ts_g_cs_ag <- read_csv("data/data_processing/global_time_series_1985-2015_2070-2099_agriculture.csv")
pct_diff_ts_g_cs_fi <- read_csv("data/data_processing/global_time-series_1985-2015_2070-2099_marine-fishery_global.csv")
pct_diff_ts_g_cs_wa <- read_csv("data/data_processing/global_time_series_1985-2015_2070-2099_water_global.csv")
pct_diff_ts_g_cs <- rbind(pct_diff_ts_g_cs_ag, pct_diff_ts_g_cs_fi, pct_diff_ts_g_cs_wa) %>% 
  mutate(id = paste(sector, eco_model, climate_model, experiment_climate, output_variable, sep = "/"))
ids <- sort(unique(pct_diff_ts_g_cs$id))
shock_ts_g_cs <- data.frame()

# A. count shocks ----
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
      mutate(shock = ifelse(res>2, 1, 0),
             shock = ifelse(res<(-2), 1, shock),
             shock_up = ifelse(res>2, 1, 0),
             shock_down = ifelse(res<(-2), 1, 0))
    
    rm(model_smooth)
  }
  
  if(i==1){shock_ts_g_cs <- xx}
  if(i>1){shock_ts_g_cs <- rbind(shock_ts_g_cs, xx)}
  
  rm(xx)
}

write.csv(shock_ts_g_cs, file = "data/short-term_change/global_shocks_time_series_1985-2015.csv", row.names = F)

# B. get probabilities of shock occurrence across models, sectors and time ----
xx <- read_csv("data/short-term_change/global_shocks_time_series_1985-2015.csv")%>% 
  filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
  # mutate(shock = ifelse(is.na(shock),0,shock),
  #        shock_up = ifelse(is.na(shock_up),0,shock_up),
  #        shock_down = ifelse(is.na(shock_down),0,shock_down)) %>%
  mutate(climates = paste(climate_model, experiment_climate)) %>%
  group_by(climates, years, output_variable) %>% 
  summarize(shock = round(mean(shock, na.rm=T),4),
            shock_down = round(mean(shock_down, na.rm=T),4),
            shock_up = round(mean(shock_up, na.rm=T),4)) %>% 
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
    summarize(at_least_one_shock = get_at_least_one(2099-yrs[y],at_least_one_shock),
              at_least_two_shocks = get_at_least_one(2099-yrs[y], at_least_two_shocks),
              at_least_three_shocks = get_at_least_one(2099-yrs[y], at_least_three_shocks),
              time_window = 2099-yrs[y])
  
  if(y==1){shock_proba_g_cs <- yy
  } else {shock_proba_g_cs <- rbind(shock_proba_g_cs,yy)}
  rm(yy)
}

write.csv(shock_proba_g_cs, file = "data/short-term_change/global_shocks_1985-2015_probas.csv", 
          row.names = F)

# C. get probabilities of synchrony across models, sectors and time ----
xx <- read_csv("data/short-term_change/global_shocks_time_series_1985-2015.csv")%>% 
  filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
  mutate(
         # shock = ifelse(is.na(shock),0,shock),
         # shock_up = ifelse(is.na(shock_up),0,shock_up),
         # shock_down = ifelse(is.na(shock_down),0,shock_down),
         # transform water shocks into negative shocks
         # crop and fish shocks remain positive if increasing and negative is decreasing
         shock_positive = ifelse(output_variable=="qtot", 0, shock_up),
         shock_negative = ifelse(output_variable=="qtot" & shock_up==1, 1, shock_down),
         shock_sign = ifelse(shock_up>0, shock, -shock_down)) %>%
  mutate(climates = paste(climate_model, experiment_climate)) %>%
  group_by(climates, years, output_variable) %>% 
  summarize(shock_negative = round(mean(shock_negative),4),
            shock_positive = round(mean(shock_positive),4),
            shock_sign = round(mean(shock_sign),4))

yrs <- c(2069,2079,2089)
shock_proba_g_cs_mec <- data.frame()
for(y in 1:length(yrs)){
  
  # negative synchrony
  ss <- xx %>% 
    filter(years>yrs[y],
           years<2100) %>% 
    dplyr::select(climates, years, output_variable, shock_negative) %>% 
    pivot_wider(names_from = output_variable, values_from = shock_negative) %>% 
    data.frame()
  
  ss$at_least_two_shocks_down <- ss$at_least_three_shocks_down <- NA
  ss$at_least_two_shocks_down <- apply(ss[,3:8], 1, function(x) get_at_least_two(6,x))
  ss$at_least_three_shocks_down <- apply(ss[,3:8], 1, function(x) get_at_least_three(6,x))
  
  ss <- ss %>% 
    dplyr::select(climates, years, at_least_two_shocks_down, at_least_three_shocks_down) %>% 
    group_by(climates) %>% 
    summarize(at_least_two_shocks_down = get_at_least_one(2099-yrs[y], at_least_two_shocks_down),
              at_least_three_shocks_down = get_at_least_one(2099-yrs[y], at_least_three_shocks_down),
              time_window = 2099-yrs[y])
  
  # compensation
  # P(at least one shock up)
  cc_up <- xx %>% 
    filter(years>yrs[y],
           years<2100) %>% 
    dplyr::select(climates, years, output_variable, shock_positive) %>% 
    pivot_wider(names_from = output_variable, values_from = shock_positive) %>% 
    data.frame()
  
  cc_up$at_least_one_shock_up <- NA
  cc_up$at_least_one_shock_up <- apply(cc_up[,3:8], 1, function(x) get_at_least_one(6,x))
  cc_up$maize <- cc_up$qtot <- cc_up$rice <- cc_up$soybean <- cc_up$tcblog10 <- cc_up$wheat <- NULL
  
  # P(at least one shock down)
  cc_down <- xx %>% 
    filter(years>yrs[y],
           years<2100) %>% 
    dplyr::select(climates, years, output_variable, shock_negative) %>% 
    pivot_wider(names_from = output_variable, values_from = shock_negative) %>% 
    data.frame()
  
  cc_down$at_least_one_shock_down <- NA
  cc_down$at_least_one_shock_down <- apply(cc_down[,3:8], 1, function(x) get_at_least_one(6,x))
  cc_down$maize <- cc_down$qtot <- cc_down$rice <- cc_down$soybean <- cc_down$tcblog10 <- cc_down$wheat <- NULL
  
  # P(at least one shock up and one shock down)
  cc <- left_join(cc_up, cc_down) %>% 
    mutate(at_least_one_shock_up_down = at_least_one_shock_up*at_least_one_shock_down) %>% 
    group_by(climates) %>% 
    summarize(at_least_one_shock_up_down = get_at_least_one(2099-yrs[y], at_least_one_shock_up_down),
              at_least_one_shock_up = get_at_least_one(2099-yrs[y], at_least_one_shock_up),
              at_least_one_shock_down = get_at_least_one(2099-yrs[y], at_least_one_shock_down))
  
  yy <- left_join(ss, cc)
  
  if(y==1){shock_proba_g_cs_mec <- yy
  } else {shock_proba_g_cs_mec <- rbind(shock_proba_g_cs_mec,yy)}
  rm(yy)
}

write.csv(shock_proba_g_cs_mec, file = "data/short-term_change/global_shocks_1985-2015_probas_mechanisms.csv", 
          row.names = F)


################################################################################
#### 2. COUNTRY SHOCKS
################################################################################

# load time-series data
# avg = average (end of century); g = global; r = region; cs = cross-sector
pct_diff_ts_r_cs_ag <- read_csv("data/data_processing/countries_time_series_1985-2015_2070-2099_agriculture.csv")
pct_diff_ts_r_cs_fi <- read_csv("data/data_processing/countries_time-series_1985-2015_2070-2099_marine-fishery_global.csv")
pct_diff_ts_r_cs_wa <- read_csv("data/data_processing/countries_time_series_1985-2015_2070-2099_water_global.csv")
pct_diff_ts_r_cs <- rbind(pct_diff_ts_r_cs_ag, pct_diff_ts_r_cs_fi, pct_diff_ts_r_cs_wa) %>% 
  mutate(id = paste(sector, regions, eco_model, climate_model, experiment_climate, output_variable, sep = "/"))
ids <- sort(unique(pct_diff_ts_r_cs$id))
shock_ts_r_cs <- data.frame()

# A. count shocks ----
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
      mutate(shock = ifelse(res>2, 1, 0),
             shock = ifelse(res<(-2), 1, shock),
             shock_up = ifelse(res>2, 1, 0),
             shock_down = ifelse(res<(-2), 1, 0))
    rm(model_smooth)
  }
  
  if(i==1){shock_ts_r_cs <- xx}
  if(i>1){shock_ts_r_cs <- rbind(shock_ts_r_cs, xx)}
  
  rm(xx)
}

write.csv(shock_ts_r_cs, file = "data/short-term_change/countries_shocks_time_series_1985-2015.csv", row.names = F)

# B. get probabilities of shock occurrence across models, sectors and time ----
xx <- read_csv("data/short-term_change/countries_shocks_time_series_1985-2015.csv")%>% 
  filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
  # mutate(shock = ifelse(is.na(shock),0,shock),
  #        shock_up = ifelse(is.na(shock_up),0,shock_up),
  #        shock_down = ifelse(is.na(shock_down),0,shock_down)) %>%
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
    summarize(at_least_one_shock = get_at_least_one(2099-yrs[y],at_least_one_shock),
              at_least_two_shocks = get_at_least_one(2099-yrs[y], at_least_two_shocks),
              at_least_three_shocks = get_at_least_one(2099-yrs[y], at_least_three_shocks),
              time_window = 2099-yrs[y])
  
  if(y==1){shock_proba_r_cs <- yy
  } else {shock_proba_r_cs <- rbind(shock_proba_r_cs,yy)}
  rm(yy)
}

write.csv(shock_proba_r_cs, file = "data/short-term_change/countries_shocks_1985-2015_probas.csv", 
          row.names = F)

# C. get probabilities of synchrony across models, sectors and time ----
xx <- read_csv("data/short-term_change/countries_shocks_time_series_1985-2015.csv")%>% 
  filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
  mutate(
         shock = ifelse(is.na(shock),0,shock),
         shock_up = ifelse(is.na(shock_up),0,shock_up),
         shock_down = ifelse(is.na(shock_down),0,shock_down),
         # transform water shocks into negative shocks
         # crop and fish shocks remain positive if increasing and negative is decreasing
         shock_positive = ifelse(output_variable=="qtot", 0, shock_up),
         shock_negative = ifelse(output_variable=="qtot" & shock_up==1, 1, shock_down),
         shock_sign = ifelse(shock_up>0, shock, -shock_down)) %>%
  mutate(climates = paste(climate_model, experiment_climate)) %>%
  group_by(regions, climates, years, output_variable) %>% 
  summarize(shock_negative = round(mean(shock_negative),4),
            shock_positive = round(mean(shock_positive),4),
            shock_sign = round(mean(shock_sign),4))

yrs <- c(2069,2079,2089)
shock_proba_r_cs_mec <- data.frame()
for(y in 1:length(yrs)){
  
  # negative synchrony
  ss <- xx %>% 
    filter(years>yrs[y],
           years<2100) %>% 
    dplyr::select(regions, climates, years, output_variable, shock_negative) %>% 
    pivot_wider(names_from = output_variable, values_from = shock_negative) %>% 
    data.frame()
  
  ss$at_least_two_shocks_down <- ss$at_least_three_shocks_down <- NA
  ss$at_least_two_shocks_down <- apply(ss[,4:9], 1, function(x) get_at_least_two(6,x))
  ss$at_least_three_shocks_down <- apply(ss[,4:9], 1, function(x) get_at_least_three(6,x))
  
  ss <- ss %>% 
    dplyr::select(regions, climates, years, at_least_two_shocks_down, at_least_three_shocks_down) %>% 
    group_by(regions, climates) %>% 
    summarize(at_least_two_shocks_down = get_at_least_one(2099-yrs[y], at_least_two_shocks_down),
              at_least_three_shocks_down = get_at_least_one(2099-yrs[y], at_least_three_shocks_down),
              time_window = 2099-yrs[y])
  
  # compensation
  # P(at least one shock up)
  cc_up <- xx %>% 
    filter(years>yrs[y],
           years<2100) %>% 
    dplyr::select(regions, climates, years, output_variable, shock_positive) %>% 
    pivot_wider(names_from = output_variable, values_from = shock_positive) %>% 
    data.frame()
  
  cc_up$at_least_one_shock_up <- NA
  cc_up$at_least_one_shock_up <- apply(cc_up[,4:9], 1, function(x) get_at_least_one(6,x))
  cc_up$maize <- cc_up$qtot <- cc_up$rice <- cc_up$soybean <- cc_up$tcblog10 <- cc_up$wheat <- NULL
  
  # P(at least one shock down)
  cc_down <- xx %>% 
    filter(years>yrs[y],
           years<2100) %>% 
    dplyr::select(regions, climates, years, output_variable, shock_negative) %>% 
    pivot_wider(names_from = output_variable, values_from = shock_negative) %>% 
    data.frame()
  
  cc_down$at_least_one_shock_down <- NA
  cc_down$at_least_one_shock_down <- apply(cc_down[,4:9], 1, function(x) get_at_least_one(6,x))
  cc_down$maize <- cc_down$qtot <- cc_down$rice <- cc_down$soybean <- cc_down$tcblog10 <- cc_down$wheat <- NULL
  
  # P(at least one shock up and one shock down)
  cc <- left_join(cc_up, cc_down) %>% 
    mutate(at_least_one_shock_up_down = at_least_one_shock_up*at_least_one_shock_down) %>% 
    group_by(regions, climates) %>% 
    summarize(at_least_one_shock_up_down = get_at_least_one(2099-yrs[y], at_least_one_shock_up_down),
              at_least_one_shock_up = get_at_least_one(2099-yrs[y], at_least_one_shock_up),
              at_least_one_shock_down = get_at_least_one(2099-yrs[y], at_least_one_shock_down))
  
  yy <- left_join(ss, cc)
  
  if(y==1){shock_proba_r_cs_mec <- yy
  } else {shock_proba_r_cs_mec <- rbind(shock_proba_r_cs_mec,yy)}
  rm(yy)
}

write.csv(shock_proba_r_cs_mec, file = "data/short-term_change/countries_shocks_1985-2015_probas_mechanisms.csv", 
          row.names = F)


################################################################################
#### 3. RTA SHOCKS
################################################################################

# load time-series data
pct_diff_ts_rta_cs_ag <- read_csv("data/data_processing/rta_time-series_1985-2015_2070-2099_agriculture.csv") %>% 
  dplyr::rename(regions = rtas)
pct_diff_ts_rta_cs_fi <- read_csv("data/data_processing/rta_time-series_1985-2015_2070-2099_marine-fishery_global.csv")
pct_diff_ts_rta_cs_wa <- read_csv("data/data_processing/rta_time-series_1985-2015_2070-2099_water_global.csv")
pct_diff_ts_rta_cs <- rbind(pct_diff_ts_rta_cs_ag, pct_diff_ts_rta_cs_fi, pct_diff_ts_rta_cs_wa) %>% 
  mutate(id = paste(sector, regions, eco_model, climate_model, experiment_climate, output_variable, sep = "/"))
ids <- sort(unique(pct_diff_ts_rta_cs$id))
shock_ts_rta_cs <- data.frame()

#### A. count rta shocks ---- 
for(i in 1:length(ids)){
  
  # print id
  print(ids[i])
  
  # subset data
  xx <- pct_diff_ts_rta_cs %>% 
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
      mutate(shock = ifelse(res>2, 1, 0),
             shock = ifelse(res<(-2), 1, shock),
             shock_up = ifelse(res>2, 1, 0),
             shock_down = ifelse(res<(-2), 1, 0))
    rm(model_smooth)
  }
  
  if(i==1){shock_ts_rta_cs <- xx}
  if(i>1){shock_ts_rta_cs <- rbind(shock_ts_rta_cs, xx)}
  
  rm(xx)
}

write.csv(shock_ts_rta_cs, file = "data/short-term_change/rta_shocks_time_series_1985-2015.csv", row.names = F)

#### B. get probabilities of shock occurrence across models, sectors and time ----
xx <- read_csv("data/short-term_change/rta_shocks_time_series_1985-2015.csv")%>% 
  filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
  # mutate(shock = ifelse(is.na(shock),0,shock),
  #        shock_up = ifelse(is.na(shock_up),0,shock_up),
  #        shock_down = ifelse(is.na(shock_down),0,shock_down)) %>%
  mutate(climates = paste(climate_model, experiment_climate)) %>%
  group_by(regions, climates, years, output_variable) %>% 
  summarize(shock = round(mean(shock),4),
            shock_down = round(mean(shock_down),4),
            shock_up = round(mean(shock_up),4)) %>% 
  dplyr::select(regions, climates, years, output_variable, shock) %>% 
  pivot_wider(names_from = output_variable, values_from = shock) %>% 
  data.frame() 

yrs <- c(2069,2079,2089)
shock_proba_rta_cs <- data.frame()
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
    summarize(at_least_one_shock = get_at_least_one(2099-yrs[y],at_least_one_shock),
              at_least_two_shocks = get_at_least_one(2099-yrs[y], at_least_two_shocks),
              at_least_three_shocks = get_at_least_one(2099-yrs[y], at_least_three_shocks),
              time_window = 2099-yrs[y])
  
  if(y==1){shock_proba_rta_cs <- yy
  } else {shock_proba_rta_cs <- rbind(shock_proba_rta_cs,yy)}
  rm(yy)
}

write.csv(shock_proba_rta_cs, file = "data/short-term_change/rta_shocks_1985-2015_probas.csv", 
          row.names = F)

#### C. get probabilities of synchrony across models, sectors and time ----
xx <- read_csv("data/short-term_change/rta_shocks_time_series_1985-2015.csv")%>% 
  filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
  mutate(
    shock = ifelse(is.na(shock),0,shock),
    shock_up = ifelse(is.na(shock_up),0,shock_up),
    shock_down = ifelse(is.na(shock_down),0,shock_down),
    # transform water shocks into negative shocks
    # crop and fish shocks remain positive if increasing and negative is decreasing
    shock_positive = ifelse(output_variable=="qtot", 0, shock_up),
    shock_negative = ifelse(output_variable=="qtot" & shock_up==1, 1, shock_down),
    shock_sign = ifelse(shock_up>0, shock, -shock_down)) %>%
  mutate(climates = paste(climate_model, experiment_climate)) %>%
  group_by(regions, climates, years, output_variable) %>% 
  summarize(shock_negative = round(mean(shock_negative),4),
            shock_positive = round(mean(shock_positive),4),
            shock_sign = round(mean(shock_sign),4))

yrs <- c(2069,2079,2089)
shock_proba_rta_cs_mec <- data.frame()
for(y in 1:length(yrs)){
  
  # negative synchrony
  ss <- xx %>% 
    filter(years>yrs[y],
           years<2100) %>% 
    dplyr::select(regions, climates, years, output_variable, shock_negative) %>% 
    pivot_wider(names_from = output_variable, values_from = shock_negative) %>% 
    data.frame()
  
  ss$at_least_two_shocks_down <- ss$at_least_three_shocks_down <- NA
  ss$at_least_two_shocks_down <- apply(ss[,4:9], 1, function(x) get_at_least_two(6,x))
  ss$at_least_three_shocks_down <- apply(ss[,4:9], 1, function(x) get_at_least_three(6,x))
  
  ss <- ss %>% 
    dplyr::select(regions, climates, years, at_least_two_shocks_down, at_least_three_shocks_down) %>% 
    group_by(regions, climates) %>% 
    summarize(at_least_two_shocks_down = get_at_least_one(2099-yrs[y], at_least_two_shocks_down),
              at_least_three_shocks_down = get_at_least_one(2099-yrs[y], at_least_three_shocks_down),
              time_window = 2099-yrs[y])
  
  # compensation
  # P(at least one shock up)
  cc_up <- xx %>% 
    filter(years>yrs[y],
           years<2100) %>% 
    dplyr::select(regions, climates, years, output_variable, shock_positive) %>% 
    pivot_wider(names_from = output_variable, values_from = shock_positive) %>% 
    data.frame()
  
  cc_up$at_least_one_shock_up <- NA
  cc_up$at_least_one_shock_up <- apply(cc_up[,4:9], 1, function(x) get_at_least_one(6,x))
  cc_up$maize <- cc_up$qtot <- cc_up$rice <- cc_up$soybean <- cc_up$tcblog10 <- cc_up$wheat <- NULL
  
  # P(at least one shock down)
  cc_down <- xx %>% 
    filter(years>yrs[y],
           years<2100) %>% 
    dplyr::select(regions, climates, years, output_variable, shock_negative) %>% 
    pivot_wider(names_from = output_variable, values_from = shock_negative) %>% 
    data.frame()
  
  cc_down$at_least_one_shock_down <- NA
  cc_down$at_least_one_shock_down <- apply(cc_down[,4:9], 1, function(x) get_at_least_one(6,x))
  cc_down$maize <- cc_down$qtot <- cc_down$rice <- cc_down$soybean <- cc_down$tcblog10 <- cc_down$wheat <- NULL
  
  # P(at least one shock up and one shock down)
  cc <- left_join(cc_up, cc_down) %>% 
    mutate(at_least_one_shock_up_down = at_least_one_shock_up*at_least_one_shock_down) %>% 
    group_by(regions, climates) %>% 
    summarize(at_least_one_shock_up_down = get_at_least_one(2099-yrs[y], at_least_one_shock_up_down),
              at_least_one_shock_up = get_at_least_one(2099-yrs[y], at_least_one_shock_up),
              at_least_one_shock_down = get_at_least_one(2099-yrs[y], at_least_one_shock_down))
  
  yy <- left_join(ss, cc)
  
  if(y==1){shock_proba_rta_cs_mec <- yy
  } else {shock_proba_rta_cs_mec <- rbind(shock_proba_rta_cs_mec,yy)}
  rm(yy)
}

write.csv(shock_proba_rta_cs_mec, file = "data/short-term_change/rta_shocks_1985-2015_probas_mechanisms.csv", 
          row.names = F)
