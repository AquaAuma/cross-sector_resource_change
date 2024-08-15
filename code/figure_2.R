#### Figure 2 and alternative versions
#### Coding: Aurore A. Maureaud, August 2024

rm(list = ls())

# load libraries
library(here)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggExtra)
library(ggrepel)
library(GGally)
library(readxl)
library(sf)
sf_use_s2(FALSE)
library(colorspace)


################################################################################
#### 2. ARE COUNTRIES EXPERIENCING SYNCHRONY AND/OR COMPENSATION?
################################################################################
#### A. load data ----
# countries
shock_probas_r_cs_mec <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas_mechanisms.csv")
change_probas_r_cs_mec <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas_mechanisms.csv") %>% 
  dplyr::select(-spatial_scale)

# global
shock_probas_g_cs_mec <- read_csv("data/short-term_change/global_shocks_1985-2015_probas_mechanisms.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(shock_probas_r_cs_mec))
change_probas_g_cs_mec <- read_csv("data/long-term_change/global_long-term_change_1985-2015_probas_mechanisms.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(change_probas_r_cs_mec))

# rbind
shock_probas <- rbind(shock_probas_g_cs_mec, shock_probas_r_cs_mec)
long_term <- rbind(change_probas_g_cs_mec, change_probas_r_cs_mec) %>% 
  pivot_wider(names_from = "type", values_from = "probas")
dat_mec <- left_join(long_term, shock_probas, by = c("climates", "regions", "time_window")) %>% 
  filter(regions != "Antarctica")

# countries
shock_probas_r_cs <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas.csv")
change_probas_r_cs <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas.csv") %>% 
  dplyr::select(-spatial_scale)

# global
shock_probas_g_cs <- read_csv("data/short-term_change/global_shocks_1985-2015_probas.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(shock_probas_r_cs))
change_probas_g_cs <- read_csv("data/long-term_change/global_long-term_change_1985-2015_probas.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(change_probas_r_cs))

# rbind
shock_probas <- rbind(shock_probas_g_cs, shock_probas_r_cs)
long_term <- rbind(change_probas_g_cs, change_probas_r_cs) %>% 
  pivot_wider(names_from = "type", values_from = "probas")
dat <- left_join(long_term, shock_probas, by = c("climates", "regions", "time_window")) %>% 
  filter(regions != "Antarctica")

dat_mec <- left_join(dat_mec, dat, by = c("regions","climates","time_window"))

# eez-land merge shapefile
regions <- st_read("data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp") %>% 
  filter(POL_TYPE != "Joint regime (EEZ)",
         is.na(SOVEREIGN2),
         SOVEREIGN1 != "Republic of Mauritius",
         UNION != "Antarctica") %>% 
  dplyr::select(UNION, SOVEREIGN1)

#### B. figure 2 maps ssp5.85----
dat_mec_ssp585 <- dat_mec %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr-ssp585"),
         time_window==30,
         regions != "global") %>% 
  group_by(regions) %>% 
  summarize(at_least_one_25_up_down = mean(at_least_one_25_up_down, na.rm=T),
            at_least_two_25_down = mean(at_least_two_25_down, na.rm=T),
            at_least_one_shock_up_down = mean(at_least_one_shock_up_down, na.rm=T),
            at_least_two_shocks_down = mean(at_least_two_shocks_down, na.rm=T),
            at_least_two_change_25 = mean(at_least_two_change_25, na.rm = T),
            at_least_two_shocks = mean(at_least_two_shocks, na.rm = T))

regions_dat <- left_join(regions, dat_mec_ssp585, by = c("SOVEREIGN1" = "regions")) 

# average climate models for ssp585 with the 25% threshold
png(paste0("figures/manuscript_figures/figure_2.png"),
    width = 6*200, height = 7*200, res = 200)
regions_dat %>% 
  rename(`Gradual compensation` = at_least_one_25_up_down,
         `Gradual synchrony` = at_least_two_25_down,
         `Abrupt compensation` = at_least_one_shock_up_down,
         `Abrupt synchrony` = at_least_two_shocks_down,
         `Gradual change` = at_least_two_change_25,
         `Abrupt change` = at_least_two_shocks) %>% 
  pivot_longer(3:8, names_to = "mechanism", values_to = "probability") %>% 
  ggplot() + geom_sf(aes(fill = probability)) +
  facet_wrap(~ factor(mechanism, 
                      levels = c("Gradual change","Abrupt change",
                                 "Gradual compensation","Abrupt compensation",
                                 "Gradual synchrony","Abrupt synchrony"))
             , ncol = 2) + 
  theme_bw() +
  scale_fill_continuous_sequential(palette = "PuBuGn", expand = c(0,0)) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.25,'lines'))
dev.off()


#### C. figure 2 maps for ssp1.26 ----
dat_mec_ssp126 <- dat_mec %>% 
  filter(climates %in% c("gfdl-esm4 ssp126","ipsl-cm6a-lr-ssp126"),
         time_window==30,
         regions != "global") %>% 
  group_by(regions) %>% 
  summarize(at_least_one_25_up_down = mean(at_least_one_25_up_down, na.rm=T),
            at_least_two_25_down = mean(at_least_two_25_down, na.rm=T),
            at_least_one_shock_up_down = mean(at_least_one_shock_up_down, na.rm=T),
            at_least_two_shocks_down = mean(at_least_two_shocks_down, na.rm=T),
            at_least_two_change_25 = mean(at_least_two_change_25, na.rm = T),
            at_least_two_shocks = mean(at_least_two_shocks, na.rm = T))

regions_dat <- left_join(regions, dat_mec_ssp126, by = c("SOVEREIGN1" = "regions")) 

# average climate models for ssp1.26 for SI
png(paste0("figures/manuscript_figures/figure_2_ssp126_si.png"),
    width = 6*200, height = 7*200, res = 200)
regions_dat %>% 
  rename(`Gradual compensation` = at_least_one_25_up_down,
         `Gradual synchrony` = at_least_two_25_down,
         `Abrupt compensation` = at_least_one_shock_up_down,
         `Abrupt synchrony` = at_least_two_shocks_down,
         `Gradual change` = at_least_two_change_25,
         `Abrupt change` = at_least_two_shocks) %>% 
  pivot_longer(3:8, names_to = "mechanism", values_to = "probability") %>% 
  ggplot() + geom_sf(aes(fill = probability)) +
  facet_wrap(~ factor(mechanism, 
                      levels = c("Gradual change","Abrupt change",
                                 "Gradual compensation","Abrupt compensation",
                                 "Gradual synchrony","Abrupt synchrony"))
             , ncol = 2) + 
  theme_bw() +
  scale_fill_continuous_sequential(palette = "PuBuGn", expand = c(0,0)) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.25,'lines'))
dev.off()


#### D. Summary statistics for the results description ----
# 195 countries
dat_mec_ssp585 %>% 
  pivot_longer(2:7, names_to = "type", values_to = "probas") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "shock", "gradual"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation", "change"),
         mechanism = ifelse(str_detect(type, "two_shocks_down")==TRUE,"synchrony",mechanism),
         mechanism = ifelse(str_detect(type, "two_change_25")==TRUE, "synchrony",mechanism)) %>% 
  group_by(type, mechanism, scale) %>% 
  summarize(proba_higher_0.5 = length(regions[probas>0.5])/195*100,
            proba_higher_0.75 = length(regions[probas>0.75])/195*100)

dat_mec_ssp585 %>% 
  pivot_longer(2:7, names_to = "type", values_to = "probas") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "shock", "gradual"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation", "change"),
         mechanism = ifelse(str_detect(type, "two_shocks_down")==TRUE,"synchrony",mechanism),
         mechanism = ifelse(str_detect(type, "two_change_25")==TRUE, "synchrony",mechanism)) %>% 
  dplyr::select(-type) %>% 
  pivot_wider(names_from = scale, values_from = probas) %>% 
  group_by(mechanism) %>% 
  summarize(proba_higher_0.5 = length(regions[gradual>0.5 & shock>0.5])/195*100,
            proba_higher_0.75 = length(regions[gradual>0.75 & shock>0.75])/195*100)

dat_mec_ssp585 %>% 
  pivot_longer(2:7, names_to = "type", values_to = "probas") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "shock", "gradual"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation", "change"),
         mechanism = ifelse(str_detect(type, "two_shocks_down")==TRUE,"synchrony",mechanism),
         mechanism = ifelse(str_detect(type, "two_change_25")==TRUE, "synchrony",mechanism)) %>% 
  dplyr::select(-type) %>% 
  filter(mechanism!="change") %>% 
  pivot_wider(names_from = mechanism, values_from = probas) %>% 
  group_by(scale) %>% 
  summarize(proba_higher_0.5 = length(regions[synchrony>0.5 & compensation>0.5])/195*100,
            proba_higher_0.75 = length(regions[synchrony>0.75 & compensation>0.75])/195*100)

  


