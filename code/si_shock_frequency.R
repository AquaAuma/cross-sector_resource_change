#### Comparing shock frequency over time with Cottrell et al., 2019
#### Coding: Aurore A. Maureaud, August 2024

rm(list = ls())

# load libraries
library(here)
library(tidyverse)
library(ggplot2)
library(gridExtra)


#### Country shock frequency across time ----
# to compare our global shock frequency and Figure 1 from Cottrell et al., 2019 panels e and c
# computing shock frequencies = proportion of countries experiencing a shock in the specific sector per year
annual_freq <- read_csv("data/short-term_change/countries_shocks_time_series_1985-2015.csv") %>% 
  filter(!output_variable %in% c("tws","tcb","ptotww"),
         experiment_climate == "ssp585") %>% 
  dplyr::select(-id, -percent_diff, -experiment_climate, -shock_up,
                -shock_down, -res, -spatial_scale, -sector) %>%
  group_by(regions, climate_model, years, output_variable) %>% 
  summarize(shock = round(mean(shock, na.rm=T),2)) %>% 
  ungroup() %>% 
  mutate(shock = ifelse(shock>0.5 & !is.na(shock), 1, shock),
         shock = ifelse(shock<0.5 & !is.na(shock), 0, shock),
         shock = ifelse(shock == 0.5 & !is.na(shock), 1, shock)) %>%
  group_by(climate_model, years, output_variable) %>% 
  summarize(shock = mean(shock, na.rm=T))


ggplot(annual_freq, aes(x = years, y = shock, color = climate_model)) +
  geom_line() +
  facet_wrap(~ output_variable, scales = "free") +
  theme_bw() +
  ylab("Shock frequency") + xlab("Year") +
  geom_smooth(method=lm) +
  scale_color_manual(values = c("darksalmon", "firebrick2"))

annual_freq %>% 
  filter(years<2014) %>% 
  ggplot(aes(x = years, y = shock, color = climate_model)) +
  geom_line() +
  facet_wrap(~ output_variable, scales = "free") +
  theme_bw() +
  ylab("Shock frequency") + xlab("Year") +
  geom_smooth(method=lm) +
  scale_color_manual(values = c("darksalmon", "firebrick2"))

# annual_freq %>% 
#   filter(years<2014) %>% 
#   ggplot(aes(x = years, y = shock, color = climate_model)) +
#   geom_line() +
#   facet_wrap(~ output_variable, scales = "free") +
#   theme_bw() +
#   geom_smooth() +
#   ylab("Shock frequency") + xlab("Year") +
#   scale_color_manual(values = c("darksalmon", "firebrick2"))


#### Normalized production for Kuwait, Afghanistan, Dominica, and Ecuador
pct_diff_ts_r_cs_ag <- read_csv("data/data_processing/countries_time_series_1985-2015_2070-2099_agriculture.csv")
pct_diff_ts_r_cs_fi <- read_csv("data/data_processing/countries_time-series_1985-2015_2070-2099_marine-fishery_global.csv")
pct_diff_ts_r_cs_wa <- read_csv("data/data_processing/countries_time_series_1985-2015_2070-2099_water_global.csv")
pct_diff_ts_r_cs <- rbind(pct_diff_ts_r_cs_ag, pct_diff_ts_r_cs_fi, pct_diff_ts_r_cs_wa)
rm(pct_diff_ts_r_cs_ag, pct_diff_ts_r_cs_fi, pct_diff_ts_r_cs_wa)

# reproduce figure 4 from Cottrell et al., 2019 as much as possible
pct_diff_ts_r_cs %>% 
  filter(!output_variable %in% c("tws","tcb","ptotww","qtot"),
         regions %in% c("Kuwait","Afghanistan","Dominica","Ecuador"),
         experiment_climate == "ssp585",
         years < 2014) %>% 
  group_by(years, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T)) %>% 
  ggplot(aes(x = years, y = percent_diff)) + 
  geom_line(aes(color = output_variable)) +
  facet_wrap(~regions, scale = "free") +
  theme_bw() +
  ylab("% Difference") + xlab("Year") +
  geom_vline(data = pct_diff_ts_r_cs %>% filter(regions == "Afghanistan"),aes(xintercept = 2000)) +
  geom_vline(data = pct_diff_ts_r_cs %>% filter(regions == "Kuwait"),aes(xintercept = 1990)) +
  geom_vline(data = pct_diff_ts_r_cs %>% filter(regions == "Dominica"),aes(xintercept = 1983)) +
  geom_vline(data = pct_diff_ts_r_cs %>% filter(regions == "Ecuador"),aes(xintercept = 1998)) +
  geom_vline(data = pct_diff_ts_r_cs %>% filter(regions == "Ecuador"),aes(xintercept = 2000))

# with different line types for climate models
pct_diff_ts_r_cs %>% 
  filter(!output_variable %in% c("tws","tcb","ptotww","qtot"),
         regions %in% c("Kuwait","Afghanistan","Dominica","Ecuador"),
         experiment_climate == "ssp585",
         years < 2014) %>% 
  group_by(years, climate_model, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T)) %>% 
  ggplot(aes(x = years, y = percent_diff)) + 
  geom_line(aes(color = output_variable, linetype = climate_model)) +
  facet_wrap(~regions, scale = "free") +
  theme_bw() +
  ylab("% Difference") + xlab("Year") +
  geom_vline(data = pct_diff_ts_r_cs %>% filter(regions == "Afghanistan"),aes(xintercept = 2000)) +
  geom_vline(data = pct_diff_ts_r_cs %>% filter(regions == "Kuwait"),aes(xintercept = 1990)) +
  geom_vline(data = pct_diff_ts_r_cs %>% filter(regions == "Dominica"),aes(xintercept = 1983)) +
  geom_vline(data = pct_diff_ts_r_cs %>% filter(regions == "Ecuador"),aes(xintercept = 1998)) +
  geom_vline(data = pct_diff_ts_r_cs %>% filter(regions == "Ecuador"),aes(xintercept = 2000))


#### Cross-sector heat map ----
annual_freq <- read_csv("data/short-term_change/countries_shocks_time_series_1985-2015.csv") %>% 
  filter(!output_variable %in% c("tws","tcb","ptotww"),
         regions %in% c("Philippines","North Korea","Pakistan","Maldives","Afghanistan",
                        "Kuwait","Iraq","Somalia","Madagascar","Burundi","Democratic Republic of the Congo",
                        "Nigeria","Mali","Liberia","Hungary","Albania","Austria","Venezuela","Ecuador",
                        "Saint Vincent and the Grenadines","Grenada","Barbados"),
         experiment_climate == "ssp585",
         years<2014) %>% 
  dplyr::select(-id, -percent_diff, -experiment_climate, -shock_up,
                -shock_down, -res, -spatial_scale, -sector) %>%
  group_by(regions, climate_model, years, output_variable) %>% 
  summarize(shock = round(mean(shock, na.rm=T),2)) %>% 
  ungroup() %>% 
  mutate(shock = ifelse(shock>0.5 & !is.na(shock), 1, shock),
         shock = ifelse(shock<0.5 & !is.na(shock), 0, shock),
         shock = ifelse(shock == 0.5 & !is.na(shock), 1, shock),
         group_years = case_when(years<1988 ~ "1983-87",
                                 years>1987 & years<1993 ~ "1988-92",
                                 years>1992 & years<1998 ~ "1993-97",
                                 years>1997 & years<2003 ~ "1998-2002",
                                 years>2002 & years<2008 ~ "2003-2007",
                                 years>2007 & years<2014 ~ "2008-2013")) %>%
  group_by(regions, climate_model, group_years) %>% 
  summarize(cs_shock = sum(shock, na.rm=T)) 
  
# reproduce some version of figure 3b

annual_freq %>% 
  filter(cs_shock>0) %>% 
  ggplot(aes(x = group_years, y = regions, fill = cs_shock)) +
  geom_tile() +
  theme_bw() +
  scale_fill_gradient2(low = "lightgoldenrodyellow", high = "red") +
  facet_wrap(~ climate_model) +
  scale_y_discrete(limits = c("Barbados","Grenada","Saint Vincent and the Grenadines",
                              "Ecuador","Venezuela","Austria","Albania","Hungary","Liberia",
                              "Mali","Nigeria","Democratic Republic of the Congo",
                              "Burundi","Madagascar","Somalia","Iraq","Kuwait",
                              "Afghanistan","Maldives","Pakistan","North Korea","Philippines"))+
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

  