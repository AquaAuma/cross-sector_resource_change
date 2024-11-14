#### Process ISIMIP shocks data across sectors
#### Coding: Aurore A. Maureaud, November 2024
#### For more information about methods testing, see shock_detection.Rmd file
#### Compared to 1.cross-sector_shocks.R, this code calculates 

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
# no need to re-run the code from 2.cross-sector_shocks.R because identical

# B. get probabilities of shock occurrence across models, sectors and time ----
xx <- read_csv("data/short-term_change/global_shocks_time_series_1985-2015_spans.csv")%>% 
  filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
  mutate(climates = paste(climate_model, experiment_climate)) %>%
  group_by(climates, years, output_variable) %>% 
  summarize(shock_0.75 = round(mean(shock_0.75, na.rm=T),4),
            shock_0.75_down = round(mean(shock_0.75_down, na.rm=T),4),
            shock_0.75_up = round(mean(shock_0.75_up, na.rm=T),4),
            shock_0.5 = round(mean(shock_0.5, na.rm=T),4),
            shock_0.5_down = round(mean(shock_0.5_down, na.rm=T),4),
            shock_0.5_up = round(mean(shock_0.5_up, na.rm=T),4),
            shock_0.25 = round(mean(shock_0.25, na.rm=T),4),
            shock_0.25_down = round(mean(shock_0.25_down, na.rm=T),4),
            shock_0.25_up = round(mean(shock_0.25_up, na.rm=T),4)) %>% 
  dplyr::select(climates, years, output_variable, shock_0.75, shock_0.5, shock_0.25) %>% 
  pivot_longer(4:6, names_to = "spans", values_to = "shock") %>% 
  pivot_wider(names_from = output_variable, values_from = "shock") %>% 
  data.frame() 

spans <- c("0.25","0.5","0.75")
shock_proba_g_cs <- data.frame()
for (s in 1:length(spans)){
  
  span <- spans[s]
  yy <- xx %>% 
    filter(years>2069,
           years<2100,
           str_detect(spans, span)==TRUE)
  
  yy$at_least_half_shocks <- yy$at_least_half_bis_shocks <- NA
  yy$at_least_half_shocks <- apply(yy[,4:9], 1, function(x) get_at_least_half(6,x))
  yy$at_least_half_bis_shocks <- apply(yy[,4:9], 1, function(x) get_at_least_half_bis(6,x))
  
  yy <- yy %>% 
    dplyr::select(climates, years, spans, at_least_half_shocks, at_least_half_bis_shocks) %>% 
    group_by(climates, spans) %>% 
    summarize(at_least_half_shocks = get_at_least_one(2099-2069,at_least_half_shocks),
              at_least_half_bis_shocks = get_at_least_one(2099-2069, at_least_half_bis_shocks),
              time_window = 2099-2069)
  
  if(s==1){shock_proba_g_cs <- yy
  } else {shock_proba_g_cs <- rbind(shock_proba_g_cs,yy)}
  rm(yy)
  
}

write.csv(shock_proba_g_cs, file = "data/short-term_change/global_shocks_1985-2015_probas_spans_half.csv", 
          row.names = F)

# C. get probabilities of synchrony across models, sectors and time ----
# xx <- read_csv("data/short-term_change/global_shocks_time_series_1985-2015_spans.csv")%>% 
#   filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
#   mutate(
#     # transform water shocks into negative shocks
#     # crop and fish shocks remain positive if increasing and negative is decreasing
#     shock_0.75_positive = ifelse(output_variable=="qtot", 0, shock_0.75_up),
#     shock_0.75_negative = ifelse(output_variable=="qtot" & shock_0.75_up==1, 1, shock_0.75_down),
#     shock_0.75_sign = ifelse(shock_0.75_up>0, shock_0.75, -shock_0.75_down),
#     shock_0.5_positive = ifelse(output_variable=="qtot", 0, shock_0.5_up),
#     shock_0.5_negative = ifelse(output_variable=="qtot" & shock_0.5_up==1, 1, shock_0.5_down),
#     shock_0.5_sign = ifelse(shock_0.5_up>0, shock_0.5, -shock_0.5_down),
#     shock_0.25_positive = ifelse(output_variable=="qtot", 0, shock_0.25_up),
#     shock_0.25_negative = ifelse(output_variable=="qtot" & shock_0.25_up==1, 1, shock_0.25_down),
#     shock_0.25_sign = ifelse(shock_0.25_up>0, shock_0.25, -shock_0.25_down)) %>%
#   mutate(climates = paste(climate_model, experiment_climate)) %>%
#   group_by(climates, years, output_variable) %>% 
#   summarize(shock_0.75_negative = round(mean(shock_0.75_negative, na.rm=T),4),
#             shock_0.75_positive = round(mean(shock_0.75_positive, na.rm=T),4),
#             shock_0.75_sign = round(mean(shock_0.75_sign, na.rm=T),4),
#             shock_0.5_negative = round(mean(shock_0.5_negative, na.rm=T),4),
#             shock_0.5_positive = round(mean(shock_0.5_positive, na.rm=T),4),
#             shock_0.5_sign = round(mean(shock_0.5_sign, na.rm=T),4),
#             shock_0.25_negative = round(mean(shock_0.25_negative, na.rm=T),4),
#             shock_0.25_positive = round(mean(shock_0.25_positive, na.rm=T),4),
#             shock_0.25_sign = round(mean(shock_0.25_sign, na.rm=T),4)) %>% 
#   pivot_longer(4:12, names_to = "spans", values_to = "shock")
# 
# spans <- c("0.25","0.5","0.75")
# shock_proba_g_cs_mec <- data.frame()
# for(s in 1:length(spans)){
#   
#   # negative synchrony
#   span <- spans[s]
#   ss <- xx %>% 
#     filter(years>2069,
#            years<2100,
#            str_detect(spans, span)==TRUE,
#            str_detect(spans, "negative")==TRUE) %>% 
#     dplyr::select(climates, years, output_variable, spans, shock) %>% 
#     pivot_wider(names_from = output_variable, values_from = shock) %>% 
#     data.frame()
#   
#   ss$at_least_two_shocks_down <- ss$at_least_three_shocks_down <- NA
#   ss$at_least_two_shocks_down <- apply(ss[,4:9], 1, function(x) get_at_least_two(6,x))
#   ss$at_least_three_shocks_down <- apply(ss[,4:9], 1, function(x) get_at_least_three(6,x))
#   
#   ss <- ss %>% 
#     dplyr::select(climates, years, spans, at_least_two_shocks_down, at_least_three_shocks_down) %>% 
#     group_by(climates, spans) %>% 
#     summarize(at_least_two_shocks_down = get_at_least_one(2099-2069, at_least_two_shocks_down),
#               at_least_three_shocks_down = get_at_least_one(2099-2069, at_least_three_shocks_down),
#               time_window = 2099-2069)
#   
#   # compensation
#   # P(at least one shock up)
#   cc_up <- xx %>% 
#     filter(years>2069,
#            years<2100,
#            str_detect(spans, span)==TRUE,
#            str_detect(spans, "positive")==TRUE) %>% 
#     dplyr::select(climates, years, output_variable, spans, shock) %>% 
#     pivot_wider(names_from = output_variable, values_from = shock) %>% 
#     data.frame()
#   
#   cc_up$at_least_one_shock_up <- NA
#   cc_up$at_least_one_shock_up <- apply(cc_up[,4:9], 1, function(x) get_at_least_one(6,x))
#   cc_up$maize <- cc_up$qtot <- cc_up$rice <- cc_up$soybean <- cc_up$tcblog10 <- cc_up$wheat <- NULL
#   cc_up$spans <- NULL
#   
#   # P(at least one shock down)
#   cc_down <- xx %>% 
#     filter(years>2069,
#            years<2100,
#            str_detect(spans, span)==TRUE,
#            str_detect(spans, "negative")==TRUE) %>% 
#     dplyr::select(climates, years, output_variable, spans, shock) %>% 
#     pivot_wider(names_from = output_variable, values_from = shock) %>% 
#     data.frame()
#   
#   cc_down$at_least_one_shock_down <- NA
#   cc_down$at_least_one_shock_down <- apply(cc_down[,4:9], 1, function(x) get_at_least_one(6,x))
#   cc_down$maize <- cc_down$qtot <- cc_down$rice <- cc_down$soybean <- cc_down$tcblog10 <- cc_down$wheat <- NULL
#   cc_down$spans <- NULL
#   
#   # P(at least one shock up and one shock down)
#   cc <- full_join(cc_up, cc_down) %>% 
#     mutate(at_least_one_shock_up_down = at_least_one_shock_up*at_least_one_shock_down) %>% 
#     group_by(climates) %>% 
#     summarize(at_least_one_shock_up_down = get_at_least_one(30, at_least_one_shock_up_down),
#               at_least_one_shock_up = get_at_least_one(30, at_least_one_shock_up),
#               at_least_one_shock_down = get_at_least_one(30, at_least_one_shock_down))
#   
#   yy <- left_join(ss, cc) %>% 
#     mutate(spans = span)
#   
#   if(s==1){shock_proba_g_cs_mec <- yy
#   } else {shock_proba_g_cs_mec <- rbind(shock_proba_g_cs_mec,yy)}
#   rm(yy)
# }
# 
# write.csv(shock_proba_g_cs_mec, file = "data/short-term_change/global_shocks_1985-2015_probas_spans_mechanisms.csv", 
#           row.names = F)


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
# no need to re-run the code from 2.cross-sector_shocks.R because identical

# B. get probabilities of shock occurrence across models, sectors and time ----
xx <- read_csv("data/short-term_change/countries_shocks_time_series_1985-2015_spans.csv")%>% 
  filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
  mutate(climates = paste(climate_model, experiment_climate)) %>%
  group_by(regions, climates, years, output_variable) %>% 
  summarize(shock_0.75 = round(mean(shock_0.75, na.rm=T),4),
            shock_0.75_down = round(mean(shock_0.75_down, na.rm=T),4),
            shock_0.75_up = round(mean(shock_0.75_up, na.rm=T),4),
            shock_0.5 = round(mean(shock_0.5, na.rm=T),4),
            shock_0.5_down = round(mean(shock_0.5_down, na.rm=T),4),
            shock_0.5_up = round(mean(shock_0.5_up, na.rm=T),4),
            shock_0.25 = round(mean(shock_0.25, na.rm=T),4),
            shock_0.25_down = round(mean(shock_0.25_down, na.rm=T),4),
            shock_0.25_up = round(mean(shock_0.25_up, na.rm=T),4)) %>% 
  dplyr::select(regions, climates, years, output_variable, shock_0.75, shock_0.5, shock_0.25) %>% 
  pivot_longer(5:7, names_to = "spans", values_to = "shock") %>% 
  pivot_wider(names_from = output_variable, values_from = "shock") %>% 
  data.frame() 

spans <- c("0.25","0.5","0.75")
shock_proba_r_cs <- data.frame()
for(s in 1:length(spans)){
  
  span <- spans[s]
  yy <- xx %>% 
    filter(years>2069,
           years<2100,
           str_detect(spans, span)==TRUE)
  
  yy$at_least_half_shocks <- yy$at_least_half_bis_shocks <- NA
  yy$at_least_half_shocks <- apply(yy[,5:10], 1, function(x) get_at_least_half(6,x))
  yy$at_least_half_bis_shocks <- apply(yy[,5:10], 1, function(x) get_at_least_half_bis(6,x))
  
  yy <- yy %>% 
    dplyr::select(regions, climates, years, spans, at_least_half_shocks, at_least_half_bis_shocks) %>% 
    group_by(regions, climates, spans) %>% 
    summarize(at_least_half_shocks = get_at_least_one(30, at_least_half_shocks),
              at_least_half_bis_shocks = get_at_least_one(30, at_least_half_bis_shocks),
              time_window = 30)
  
  if(s==1){shock_proba_r_cs <- yy
  } else {shock_proba_r_cs <- rbind(shock_proba_r_cs,yy)}
  rm(yy)
}

write.csv(shock_proba_r_cs, file = "data/short-term_change/countries_shocks_1985-2015_probas_spans_half.csv", 
          row.names = F)


# C. get probabilities of synchrony across models, sectors and time ----
# xx <- read_csv("data/short-term_change/countries_shocks_time_series_1985-2015_spans.csv")%>% 
#   filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
#   mutate(
#     # transform water shocks into negative shocks
#     # crop and fish shocks remain positive if increasing and negative is decreasing
#     shock_0.75_positive = ifelse(output_variable=="qtot", 0, shock_0.75_up),
#     shock_0.75_negative = ifelse(output_variable=="qtot" & shock_0.75_up==1, 1, shock_0.75_down),
#     shock_0.75_sign = ifelse(shock_0.75_up>0, shock_0.75, -shock_0.75_down),
#     shock_0.5_positive = ifelse(output_variable=="qtot", 0, shock_0.5_up),
#     shock_0.5_negative = ifelse(output_variable=="qtot" & shock_0.5_up==1, 1, shock_0.5_down),
#     shock_0.5_sign = ifelse(shock_0.5_up>0, shock_0.5, -shock_0.5_down),
#     shock_0.25_positive = ifelse(output_variable=="qtot", 0, shock_0.25_up),
#     shock_0.25_negative = ifelse(output_variable=="qtot" & shock_0.25_up==1, 1, shock_0.25_down),
#     shock_0.25_sign = ifelse(shock_0.25_up>0, shock_0.25, -shock_0.25_down)) %>%
#   mutate(climates = paste(climate_model, experiment_climate)) %>%
#   group_by(regions, climates, years, output_variable) %>% 
#   summarize(shock_0.75_negative = round(mean(shock_0.75_negative, na.rm=T),4),
#             shock_0.75_positive = round(mean(shock_0.75_positive, na.rm=T),4),
#             shock_0.75_sign = round(mean(shock_0.75_sign, na.rm=T),4),
#             shock_0.5_negative = round(mean(shock_0.5_negative, na.rm=T),4),
#             shock_0.5_positive = round(mean(shock_0.5_positive, na.rm=T),4),
#             shock_0.5_sign = round(mean(shock_0.5_sign, na.rm=T),4),
#             shock_0.25_negative = round(mean(shock_0.25_negative, na.rm=T),4),
#             shock_0.25_positive = round(mean(shock_0.25_positive, na.rm=T),4),
#             shock_0.25_sign = round(mean(shock_0.25_sign, na.rm=T),4)) %>% 
#   pivot_longer(5:13, names_to = "spans", values_to = "shock")
# 
# spans <- c("0.25","0.5","0.75")
# shock_proba_r_cs_mec <- data.frame()
# for(s in 1:length(spans)){
#   
#   # negative synchrony
#   span <- spans[s]
#   ss <- xx %>% 
#     filter(years>2069,
#            years<2100,
#            str_detect(spans, span)==TRUE,
#            str_detect(spans, "negative")==TRUE) %>% 
#     dplyr::select(regions, climates, years, output_variable, spans, shock) %>% 
#     pivot_wider(names_from = output_variable, values_from = shock) %>% 
#     data.frame()
#   
#   ss$at_least_two_shocks_down <- ss$at_least_three_shocks_down <- NA
#   ss$at_least_two_shocks_down <- apply(ss[,5:10], 1, function(x) get_at_least_two(6,x))
#   ss$at_least_three_shocks_down <- apply(ss[,5:10], 1, function(x) get_at_least_three(6,x))
#   
#   ss <- ss %>% 
#     dplyr::select(regions, climates, years, spans, at_least_two_shocks_down, at_least_three_shocks_down) %>% 
#     group_by(regions, climates, spans) %>% 
#     summarize(at_least_two_shocks_down = get_at_least_one(30, at_least_two_shocks_down),
#               at_least_three_shocks_down = get_at_least_one(30, at_least_three_shocks_down),
#               time_window = 30)
#   
#   # compensation
#   # P(at least one shock up)
#   cc_up <- xx %>% 
#     filter(years>2069,
#            years<2100,
#            str_detect(spans, "positive")==TRUE,
#            str_detect(spans, span)==TRUE) %>% 
#     dplyr::select(regions, climates, years, output_variable, spans, shock) %>% 
#     pivot_wider(names_from = output_variable, values_from = shock) %>% 
#     data.frame()
#   
#   cc_up$at_least_one_shock_up <- cc_up$at_least_two_shocks_up <- cc_up$at_least_three_shocks_up <- NA
#   cc_up$at_least_one_shock_up <- apply(cc_up[,5:10], 1, function(x) get_at_least_one(6,x))
#   cc_up$at_least_two_shocks_up <- apply(cc_up[,5:10], 1, function(x) get_at_least_two(6,x))
#   cc_up$at_least_three_shocks_up <- apply(cc_up[,5:10], 1, function(x) get_at_least_three(6,x))
#   cc_up$maize <- cc_up$qtot <- cc_up$rice <- cc_up$soybean <- cc_up$tcblog10 <- cc_up$wheat <- NULL
#   cc_up$spans <- NULL
#   
#   # P(at least one shock down)
#   cc_down <- xx %>% 
#     filter(years>2069,
#            years<2100,
#            str_detect(spans, "negative")==TRUE,
#            str_detect(spans, span)==TRUE) %>% 
#     dplyr::select(regions, climates, years, output_variable, spans, shock) %>% 
#     pivot_wider(names_from = output_variable, values_from = shock) %>% 
#     data.frame()
#   
#   cc_down$at_least_one_shock_down <- NA
#   cc_down$at_least_one_shock_down <- apply(cc_down[,5:10], 1, function(x) get_at_least_one(6,x))
#   cc_down$maize <- cc_down$qtot <- cc_down$rice <- cc_down$soybean <- cc_down$tcblog10 <- cc_down$wheat <- NULL
#   cc_down$spans <- NULL
#   
#   # P(at least one shock up and one shock down)
#   cc <- left_join(cc_up, cc_down) %>% 
#     mutate(at_least_one_shock_up_down = at_least_one_shock_up*at_least_one_shock_down) %>% 
#     group_by(regions, climates) %>% 
#     summarize(at_least_one_shock_up_down = get_at_least_one(30, at_least_one_shock_up_down),
#               at_least_one_shock_up = get_at_least_one(30, at_least_one_shock_up),
#               at_least_one_shock_down = get_at_least_one(30, at_least_one_shock_down),
#               at_least_two_shocks_up = get_at_least_one(30, at_least_two_shocks_up),
#               at_least_three_shocks_up = get_at_least_one(30, at_least_three_shocks_up))
#   
#   yy <- left_join(ss, cc) %>% 
#     mutate(spans = span)
#   
#   if(s==1){shock_proba_r_cs_mec <- yy
#   } else {shock_proba_r_cs_mec <- rbind(shock_proba_r_cs_mec,yy)}
#   rm(yy)
# }
# 
# write.csv(shock_proba_r_cs_mec, file = "data/short-term_change/countries_shocks_1985-2015_probas_spans_mechanisms.csv", 
#           row.names = F)


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
# no need to re-run the code from 2.cross-sector_shocks.R because identical

#### B. get probabilities of shock occurrence across models, sectors and time ----
xx <- read_csv("data/short-term_change/rta_shocks_time_series_1985-2015_spans.csv")%>% 
  filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
  mutate(climates = paste(climate_model, experiment_climate)) %>%
  group_by(regions, climates, years, output_variable) %>% 
  summarize(shock_0.75 = round(mean(shock_0.75, na.rm=T),4),
            shock_0.75_down = round(mean(shock_0.75_down, na.rm=T),4),
            shock_0.75_up = round(mean(shock_0.75_up, na.rm=T),4),
            shock_0.5 = round(mean(shock_0.5, na.rm=T),4),
            shock_0.5_down = round(mean(shock_0.5_down, na.rm=T),4),
            shock_0.5_up = round(mean(shock_0.5_up, na.rm=T),4),
            shock_0.25 = round(mean(shock_0.25, na.rm=T),4),
            shock_0.25_down = round(mean(shock_0.25_down, na.rm=T),4),
            shock_0.25_up = round(mean(shock_0.25_up, na.rm=T),4)) %>% 
  dplyr::select(regions, climates, years, output_variable, shock_0.75, shock_0.5, shock_0.25) %>% 
  pivot_longer(5:7, names_to = "spans", values_to = "shock") %>% 
  pivot_wider(names_from = output_variable, values_from = "shock") %>% 
  data.frame() 

spans <- c("0.25","0.5","0.75")
shock_proba_rta_cs <- data.frame()
for(s in 1:length(spans)){
  span <- spans[s]
  yy <- xx %>% 
    filter(years>2069,
           years<2100,
           str_detect(spans, span)==TRUE)
  
  yy$at_least_half_shocks <- yy$at_least_half_bis_shocks <- NA
  yy$at_least_half_shocks <- apply(yy[,5:10], 1, function(x) get_at_least_half(6,x))
  yy$at_least_half_bis_shocks <- apply(yy[,5:10], 1, function(x) get_at_least_half_bis(6,x))
  
  yy <- yy %>% 
    dplyr::select(regions, climates, years, spans, at_least_half_shocks, at_least_half_bis_shocks) %>% 
    group_by(regions, climates, spans) %>% 
    summarize(at_least_half_shocks = get_at_least_one(30, at_least_half_shocks),
              at_least_half_bis_shocks = get_at_least_one(30, at_least_half_bis_shocks),
              time_window = 30)
  
  if(s==1){shock_proba_rta_cs <- yy
  } else {shock_proba_rta_cs <- rbind(shock_proba_rta_cs,yy)}
  rm(yy)
}

write.csv(shock_proba_rta_cs, file = "data/short-term_change/rta_shocks_1985-2015_probas_spans_half.csv", 
          row.names = F)

#### C. get probabilities of synchrony across models, sectors and time ----
# xx <- read_csv("data/short-term_change/rta_shocks_time_series_1985-2015_spans.csv")%>% 
#   filter(!output_variable %in% c("tws","tcb","ptotww")) %>%
#   mutate(
#     shock_0.75_positive = ifelse(output_variable=="qtot", 0, shock_0.75_up),
#     shock_0.75_negative = ifelse(output_variable=="qtot" & shock_0.75_up==1, 1, shock_0.75_down),
#     shock_0.75_sign = ifelse(shock_0.75_up>0, shock_0.75, -shock_0.75_down),
#     shock_0.5_positive = ifelse(output_variable=="qtot", 0, shock_0.5_up),
#     shock_0.5_negative = ifelse(output_variable=="qtot" & shock_0.5_up==1, 1, shock_0.5_down),
#     shock_0.5_sign = ifelse(shock_0.5_up>0, shock_0.5, -shock_0.5_down),
#     shock_0.25_positive = ifelse(output_variable=="qtot", 0, shock_0.25_up),
#     shock_0.25_negative = ifelse(output_variable=="qtot" & shock_0.25_up==1, 1, shock_0.25_down),
#     shock_0.25_sign = ifelse(shock_0.25_up>0, shock_0.25, -shock_0.25_down)) %>%
#   mutate(climates = paste(climate_model, experiment_climate)) %>%
#   group_by(regions, climates, years, output_variable) %>% 
#   summarize(shock_0.75_negative = round(mean(shock_0.75_negative, na.rm=T),4),
#             shock_0.75_positive = round(mean(shock_0.75_positive, na.rm=T),4),
#             shock_0.75_sign = round(mean(shock_0.75_sign, na.rm=T),4),
#             shock_0.5_negative = round(mean(shock_0.5_negative, na.rm=T),4),
#             shock_0.5_positive = round(mean(shock_0.5_positive, na.rm=T),4),
#             shock_0.5_sign = round(mean(shock_0.5_sign, na.rm=T),4),
#             shock_0.25_negative = round(mean(shock_0.25_negative, na.rm=T),4),
#             shock_0.25_positive = round(mean(shock_0.25_positive, na.rm=T),4),
#             shock_0.25_sign = round(mean(shock_0.25_sign, na.rm=T),4)) %>% 
#   pivot_longer(5:13, names_to = "spans", values_to = "shock")
# 
# spans <- c("0.25","0.5","0.75")
# shock_proba_rta_cs_mec <- data.frame()
# for(s in 1:length(spans)){
#   
#   # negative synchrony
#   span <- spans[s]
#   ss <- xx %>% 
#     filter(years>2069,
#            years<2100,
#            str_detect(spans, span)==TRUE,
#            str_detect(spans, "negative")==TRUE) %>% 
#     dplyr::select(regions, climates, years, output_variable, spans, shock) %>% 
#     pivot_wider(names_from = output_variable, values_from = shock) %>% 
#     data.frame()
#   
#   ss$at_least_two_shocks_down <- ss$at_least_three_shocks_down <- NA
#   ss$at_least_two_shocks_down <- apply(ss[,5:10], 1, function(x) get_at_least_two(6,x))
#   ss$at_least_three_shocks_down <- apply(ss[,5:10], 1, function(x) get_at_least_three(6,x))
#   
#   ss <- ss %>% 
#     dplyr::select(regions, climates, years, spans, at_least_two_shocks_down, at_least_three_shocks_down) %>% 
#     group_by(regions, climates, spans) %>% 
#     summarize(at_least_two_shocks_down = get_at_least_one(30, at_least_two_shocks_down),
#               at_least_three_shocks_down = get_at_least_one(30, at_least_three_shocks_down),
#               time_window = 30)
#   
#   # compensation
#   # P(at least one shock up)
#   cc_up <- xx %>% 
#     filter(years>2069,
#            years<2100,
#            str_detect(spans, span)==TRUE,
#            str_detect(spans, "positive")==TRUE) %>% 
#     dplyr::select(regions, climates, years, output_variable, spans, shock) %>% 
#     pivot_wider(names_from = output_variable, values_from = shock) %>% 
#     data.frame()
#   
#   cc_up$at_least_one_shock_up <- NA
#   cc_up$at_least_one_shock_up <- apply(cc_up[,5:10], 1, function(x) get_at_least_one(6,x))
#   cc_up$maize <- cc_up$qtot <- cc_up$rice <- cc_up$soybean <- cc_up$tcblog10 <- cc_up$wheat <- NULL
#   cc_up$spans <- NULL
#   
#   # P(at least one shock down)
#   cc_down <- xx %>% 
#     filter(years>2069,
#            years<2100,
#            str_detect(spans, span)==TRUE,
#            str_detect(spans, "negative")==TRUE) %>% 
#     dplyr::select(regions, climates, years, output_variable, spans, shock) %>% 
#     pivot_wider(names_from = output_variable, values_from = shock) %>% 
#     data.frame()
#   
#   cc_down$at_least_one_shock_down <- NA
#   cc_down$at_least_one_shock_down <- apply(cc_down[,5:10], 1, function(x) get_at_least_one(6,x))
#   cc_down$maize <- cc_down$qtot <- cc_down$rice <- cc_down$soybean <- cc_down$tcblog10 <- cc_down$wheat <- NULL
#   cc_down$spans <- NULL
#   
#   # P(at least one shock up and one shock down)
#   cc <- left_join(cc_up, cc_down) %>% 
#     mutate(at_least_one_shock_up_down = at_least_one_shock_up*at_least_one_shock_down) %>% 
#     group_by(regions, climates) %>% 
#     summarize(at_least_one_shock_up_down = get_at_least_one(30, at_least_one_shock_up_down),
#               at_least_one_shock_up = get_at_least_one(30, at_least_one_shock_up),
#               at_least_one_shock_down = get_at_least_one(30, at_least_one_shock_down))
#   
#   yy <- left_join(ss, cc) %>% 
#     mutate(spans = span)
#   
#   if(s==1){shock_proba_rta_cs_mec <- yy
#   } else {shock_proba_rta_cs_mec <- rbind(shock_proba_rta_cs_mec,yy)}
#   rm(yy)
# }
# 
# write.csv(shock_proba_rta_cs_mec, file = "data/short-term_change/rta_shocks_1985-2015_probas_spans_mechanisms.csv", 
#           row.names = F)
