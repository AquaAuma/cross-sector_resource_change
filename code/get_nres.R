#### Extract number of resources per country
#### Coding: Aurore A. Maureaud, December 2024

rm(list = ls())

# load libraries
library(tidyverse)

pct_diff_ts_r_cs_ag <- read_csv("data/data_processing/countries_time_series_1985-2015_2070-2099_agriculture.csv")
pct_diff_ts_r_cs_fi <- read_csv("data/data_processing/countries_time-series_1985-2015_2070-2099_marine-fishery_global.csv")
pct_diff_ts_r_cs_wa <- read_csv("data/data_processing/countries_time_series_1985-2015_2070-2099_water_global.csv")
pct_diff_ts_r_cs <- rbind(pct_diff_ts_r_cs_ag, pct_diff_ts_r_cs_fi, pct_diff_ts_r_cs_wa)

nres_r <- data.frame(pct_diff_ts_r_cs) %>% 
  filter(!is.na(percent_diff),
         output_variable %in% c("rice", "wheat", "soybean", "maize", "tcblog10", "qtot")) %>% 
  group_by(regions) %>% 
  summarize(n_res = length(unique(output_variable)))

write_csv(nres_r, file = "data/data_processing/nres.csv")
