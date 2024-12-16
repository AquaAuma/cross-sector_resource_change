#### Figure 1 and alternative versions
#### Coding: Aurore A. Maureaud, December 2024

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
library(ggpubr)
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")


################################################################################
#### 1. Exposure of countries to cross-sector exposure types
################################################################################

#### A. load data of cross-sector change ----
# countries
shock_probas_r_cs <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas_spans.csv") %>% 
  filter(spans == "shock_0.75") %>% 
  dplyr::select(-spans)
change_probas_r_cs <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas.csv") %>% 
  dplyr::select(-spatial_scale)

# global
shock_probas_g_cs <- read_csv("data/short-term_change/global_shocks_1985-2015_probas_spans.csv") %>% 
  mutate(regions = "global") %>% 
  filter(spans == "shock_0.75") %>% 
  dplyr::select(-spans) %>% 
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

# eez-land merge shapefile
regions <- st_read("data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp") %>% 
  filter(POL_TYPE != "Joint regime (EEZ)",
         is.na(SOVEREIGN2),
         SOVEREIGN1 != "Republic of Mauritius",
         UNION != "Antarctica") %>% 
  dplyr::select(UNION, SOVEREIGN1)

#### B. figure 1 maps ssp5.85 ----
dat_ssp585 <- dat %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585"),
         time_window==30,
         regions != "global") %>% 
  group_by(regions) %>% 
  summarize(at_least_two_change_25 = mean(at_least_two_change_25, na.rm = T),
            at_least_two_shocks = mean(at_least_two_shocks, na.rm = T))

regions_dat <- left_join(regions, dat_ssp585, by = c("SOVEREIGN1" = "regions")) 

# SSP 585 shocks
png(paste0("figures/figure_1_a.png"),
    width = 8*200, height = 4*200, res = 200)
regions_dat %>%
  ggplot() +   
  geom_sf(data = world, fill = "white") + 
  geom_sf(aes(fill = at_least_two_shocks), alpha = 0.8) +
  theme_bw() +
  scale_fill_continuous_sequential(palette = "PuBuGn",
                                   begin = 0, end = 1,
                                   limits=c(0,1),
                                   breaks= c(0,0.5,1))+
  coord_sf(ylim = c(-80, 90), xlim = c(-180, 180), expand = FALSE) +
  theme(legend.position = c(.15, .15),
        legend.direction="horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.5,'lines'),
        legend.key.size = unit(0.75,'cm'),
        legend.background=element_blank(),
        text = element_text(size = 22))
dev.off()

# SSP 585 gradual changes
png(paste0("figures/figure_1_b.png"),
    width = 8*200, height = 4*200, res = 200)
regions_dat %>%
  mutate(at_least_two_change_25 = ifelse(at_least_two_change_25<0, 0, at_least_two_change_25)) %>% 
  ggplot() +
  geom_sf(data = world, fill = "white") + 
  geom_sf(aes(fill = at_least_two_change_25), alpha = 0.8) +
  theme_bw() +
  scale_fill_continuous_sequential(palette = "PuBuGn",
                                   begin = 0, end = 1,
                                   limits=c(0,1),
                                   breaks = c(0,0.5,1)) +
  coord_sf(ylim = c(-80, 90), xlim = c(-180, 180), expand = FALSE) +
  theme(legend.position = c(.15, .15),
        legend.direction="horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.5,'lines'),
        legend.key.size = unit(0.75, 'cm'),
        legend.background=element_blank(),
        text = element_text(size = 22))
dev.off()

# number of resources
nres_r <- read_csv("data/data_processing/nres.csv")
nres_r_map <- left_join(regions, nres_r, by = c("SOVEREIGN1" = "regions")) 

png(paste0("figures/figure_1_c.png"),
    width = 8*200, height = 4*200, res = 200)
nres_r_map %>% 
  ggplot() + 
  geom_sf(data = world, fill = "white") + 
  geom_sf(aes(fill = as.factor(n_res)), alpha = 0.8) +
  theme_bw() +
  scale_fill_brewer(palette = "Greys") +
  coord_sf(ylim = c(-80, 90), xlim = c(-180, 180), expand = FALSE) +
  theme(legend.position = c(.15, .1),
        legend.direction="horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.5,'lines'),
        legend.key.size = unit(0.4, 'cm'),
        legend.background=element_blank(),
        text = element_text(size = 22))
dev.off()


#### C. Supplemental figure 1 ----

# latitudinal distributions
coords <- regions_dat %>%
  st_centroid() %>%
  st_coordinates()

densities_dat <- cbind(regions_dat, coords) %>%
  st_drop_geometry() %>%
  pivot_longer(c(3:4), values_to = "proba", names_to = "type") %>%
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "shock", "gradual"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation","change"),
         mechanism = ifelse(str_detect(type, "two_shocks_down")==TRUE,"synchrony",mechanism),
         mechanism = ifelse(str_detect(type, "two_25_down")==TRUE,"synchrony",mechanism))

# plot latitudinal gradients
gradients <- ggplot(densities_dat, aes(y = proba, x = Y, linetype = scale)) +
  geom_smooth(span = 0.35, se = FALSE, col = "black") +
  theme_bw() +
  facet_wrap(~mechanism, scales = "free", nrow = 1) + coord_flip() +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.75,'lines'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14)) +
  ylab("Probability") + xlab("Latitude") +
  scale_linetype_discrete(name="")

png(paste0("figures/supplementary_figure_1.png"),
    width = 7*150, height = 7*200, res = 200)
gradients
dev.off()


#### D. Supplemental figure 2 ----
# figure 1 for ssp 1.26

dat_ssp126 <- dat %>% 
  filter(climates %in% c("gfdl-esm4 ssp126","ipsl-cm6a-lr ssp126"),
         time_window==30,
         regions != "global") %>% 
  group_by(regions) %>% 
  summarize(at_least_two_change_25 = mean(at_least_two_change_25, na.rm = T),
            at_least_two_shocks = mean(at_least_two_shocks, na.rm = T))

regions_dat <- left_join(regions, dat_ssp126, by = c("SOVEREIGN1" = "regions")) 

# SSP 126 shocks
png(paste0("figures/supplementary_figure_2_a.png"),
    width = 8*200, height = 4*200, res = 200)
regions_dat %>%
  ggplot() +   
  geom_sf(data = world, fill = "white") + 
  geom_sf(aes(fill = at_least_two_shocks), alpha = 0.8) +
  theme_bw() +
  scale_fill_continuous_sequential(palette = "PuBuGn",
                                   begin = 0, end = 1,
                                   limits=c(0,1),
                                   breaks= c(0,0.5,1))+
  coord_sf(ylim = c(-80, 90), xlim = c(-180, 180), expand = FALSE) +
  theme(legend.position = c(.15, .15),
        legend.direction="horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.5,'lines'),
        legend.key.size = unit(0.75,'cm'),
        legend.background=element_blank(),
        text = element_text(size = 22))
dev.off()

# SSP 126 gradual changes
png(paste0("figures/supplementary_figure_2_b.png"),
    width = 8*200, height = 4*200, res = 200)
regions_dat %>%
  mutate(at_least_two_change_25 = ifelse(at_least_two_change_25<0, 0, at_least_two_change_25)) %>% 
  ggplot() +
  geom_sf(data = world, fill = "white") + 
  geom_sf(aes(fill = at_least_two_change_25), alpha = 0.8) +
  theme_bw() +
  scale_fill_continuous_sequential(palette = "PuBuGn",
                                   begin = 0, end = 1,
                                   limits=c(0,1),
                                   breaks = c(0,0.5,1)) +
  coord_sf(ylim = c(-80, 90), xlim = c(-180, 180), expand = FALSE) +
  theme(legend.position = c(.15, .15),
        legend.direction="horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.5,'lines'),
        legend.key.size = unit(0.75, 'cm'),
        legend.background=element_blank(),
        text = element_text(size = 22))
dev.off()


#### E. supplemental figure 3 ----
shock_probas_r_cs_mec <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas_spans_mechanisms.csv") %>% 
  filter(spans == "0.75") %>% 
  dplyr::select(-spans, -at_least_three_shocks_up, -at_least_two_shocks_up)
change_probas_r_cs_mec <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas_mechanisms.csv") %>% 
  dplyr::select(-spatial_scale)

# global
shock_probas_g_cs_mec <- read_csv("data/short-term_change/global_shocks_1985-2015_probas_spans_mechanisms.csv") %>% 
  mutate(regions = "global") %>% 
  filter(spans == "0.75") %>% 
  dplyr::select(-spans) %>% 
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
shock_probas_r_cs <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas_spans.csv") %>% 
  filter(spans == "shock_0.75") %>% 
  dplyr::select(-spans)
change_probas_r_cs <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas.csv") %>% 
  dplyr::select(-spatial_scale)

# global
shock_probas_g_cs <- read_csv("data/short-term_change/global_shocks_1985-2015_probas_spans.csv") %>% 
  mutate(regions = "global") %>% 
  filter(spans == "shock_0.75") %>% 
  dplyr::select(-spans) %>% 
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

# ssp5.85
dat_mec_ssp585 <- dat_mec %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585"),
         time_window==30,
         regions != "global") %>% 
  group_by(regions) %>% 
  summarize(at_least_one_25_up_down = mean(at_least_one_25_up_down, na.rm=T),
            at_least_two_25_down = mean(at_least_two_25_down, na.rm=T),
            at_least_one_shock_up_down = mean(at_least_one_shock_up_down, na.rm=T),
            at_least_two_shocks_down = mean(at_least_two_shocks_down, na.rm=T),
            at_least_two_change_25 = mean(at_least_two_change_25, na.rm = T),
            at_least_two_shocks = mean(at_least_two_shocks, na.rm = T))

nres_r_metrics <- left_join(nres_r, dat_mec_ssp585) %>% 
  rename(`Gradual compensation` = at_least_one_25_up_down,
         `Gradual synchrony` = at_least_two_25_down,
         `Abrupt compensation` = at_least_one_shock_up_down,
         `Abrupt synchrony` = at_least_two_shocks_down,
         `Gradual change` = at_least_two_change_25,
         `Abrupt change` = at_least_two_shocks) %>% 
  pivot_longer(3:8, names_to = "metric", values_to = "value") %>% 
  filter(metric %in% c("Gradual change","Abrupt change"))

png(paste0("figures/supplementary_figure_3.png"),
    width = 8*200, height = 4*400, res = 200)
ggplot(nres_r_metrics) +
  geom_boxplot(aes(x = as.factor(n_res), y = value)) +
  facet_wrap(~ factor(metric, 
                      levels = c("Abrupt change","Gradual change"),
                      labels = c("Probability of at least 1 year with at least 2 shocks",
                                 "Probability of at least 2 res. changing by 25% or more")),
             nrow = 2) +
  theme_bw() +
  ylab("Probability") +
  xlab("Number of resources") +
  theme(text = element_text(size = 22))
dev.off()


#### F. supplemental figure 4 ----
# link between p(X>=2) and p(X>=50%)

## SHOCKS
# load p(X>=2)
shocks_two <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas_spans.csv") %>% 
  filter(spans == "shock_0.75") %>% 
  dplyr::select(regions, climates, at_least_two_shocks)

# load p(X>=50%)
shocks_half <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas_spans_half.csv") %>% 
  filter(spans == "shock_0.75") %>% 
  dplyr::select(regions, climates, at_least_half_shocks, at_least_half_bis_shocks)

# join the past and future shock probabilities
shocks <- left_join(shocks_two, shocks_half, by = c("regions","climates")) %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  pivot_longer(3:5, names_to = "type", values_to = "proba") %>% 
  group_by(regions, type) %>% 
  summarize(proba = median(proba, na.rm=T)) %>% 
  pivot_wider(values_from = proba, names_from = type)

shocks_map <- left_join(regions, shocks, by = c("SOVEREIGN1" = "regions")) %>% 
  select(-at_least_half_shocks,-at_least_two_shocks) %>% 
  rename(`p(X>=50%)` = at_least_half_bis_shocks) %>% 
  pivot_longer(3, names_to = "type", values_to = "proba")

png(paste0("figures/supplementary_figure_4_a.png"),
    width = 8*200, height = 4*200, res = 200)
shocks_map %>% 
  ggplot() +
  theme_bw() +
  geom_sf(data = world, fill = "white") + 
  geom_sf(aes(fill = proba), alpha = 0.8) +
  scale_fill_continuous_sequential(palette = "PuBuGn",
                                   begin = 0, end = 1,
                                   limits=c(0,1),
                                   breaks = c(0,0.5,1)) +
  coord_sf(ylim = c(-80, 90), xlim = c(-180, 180), expand = FALSE) +
  theme(legend.position = c(.15, .15),
        legend.direction="horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.5,'lines'),
        legend.key.size = unit(0.75, 'cm'),
        legend.background=element_blank(),
        text = element_text(size = 22))
dev.off()

## GRADUAL
# load p(X>=2)
gradual_two <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas.csv") %>% 
  filter(time_window == 30,
         type == "at_least_two_change_25") %>% 
  dplyr::select(regions, climates, type, probas) %>% 
  pivot_wider(names_from = type, values_from = probas)

# load p(X>=50%)
gradual_half <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas_half.csv") %>% 
  filter(time_window == 30,
         type %in% c("at_least_half_change_25","at_least_half_bis_change_25")) %>%
  dplyr::select(regions, climates, type, probas) %>% 
  pivot_wider(names_from = type, values_from = probas)

# join the past and future shock probabilities
gradual <- left_join(gradual_two, gradual_half, by = c("regions","climates")) %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  pivot_longer(3:5, names_to = "type", values_to = "proba") %>% 
  group_by(regions, type) %>% 
  summarize(proba = median(proba, na.rm=T)) %>% 
  pivot_wider(values_from = proba, names_from = type)

# n_res
gradual <- left_join(gradual, nres_r, by = "regions")

# join the shapefile and data
gradual_map <- left_join(regions, gradual, by = c("SOVEREIGN1" = "regions")) %>% 
  rename(`p(X>=2)` = at_least_two_change_25,
         `p(X>=50%)` = at_least_half_change_25,
         `p(X>=50%)bis` = at_least_half_bis_change_25,
         `# resources` = n_res) %>% 
  pivot_longer(3:6, names_to = "type", values_to = "proba") %>% 
  filter(type == "p(X>=50%)bis")

# map
png(paste0("figures/supplementary_figure_4_b.png"),
    width = 8*200, height = 4*200, res = 200)
gradual_map %>% 
  ggplot() + 
  theme_bw() +
  geom_sf(data = world, fill = "white") + 
  geom_sf(aes(fill = proba), alpha = 0.8) +
  scale_fill_continuous_sequential(palette = "PuBuGn",
                                   begin = 0, end = 1,
                                   limits=c(0,1),
                                   breaks = c(0,0.5,1)) +
  coord_sf(ylim = c(-80, 90), xlim = c(-180, 180), expand = FALSE) +
  theme(legend.position = c(.15, .15),
        legend.direction="horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.5,'lines'),
        legend.key.size = unit(0.75, 'cm'),
        legend.background=element_blank(),
        text = element_text(size = 22))
dev.off()
