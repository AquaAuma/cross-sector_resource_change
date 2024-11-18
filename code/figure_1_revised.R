#### Figure 1 and alternative versions
#### Coding: Aurore A. Maureaud, November 2024

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
png(paste0("figures/revised_figures/figure_1_a.png"),
    width = 8*200, height = 4*200, res = 200)
regions_dat %>%
  ggplot() + geom_sf(aes(fill = at_least_two_shocks)) +
  theme_bw() +
  scale_fill_continuous_sequential(palette = "PuBuGn",
                                   begin = 0, end = 1,
                                   limits=c(0,1),
                                   breaks= c(0,0.5,1))+
  coord_sf(ylim = c(-80, 90), xlim = c(-180, 180), expand = FALSE) +
  theme(legend.position = c(.15, .1),
        legend.direction="horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.5,'lines'),
        legend.key.size = unit(0.5, 'cm'),
        legend.background=element_blank())
dev.off()

# SSP 585 gradual changes
png(paste0("figures/revised_figures/figure_1_b.png"),
    width = 8*200, height = 4*200, res = 200)
regions_dat %>%
  mutate(at_least_two_change_25 = ifelse(at_least_two_change_25<0, 0, at_least_two_change_25)) %>% 
  ggplot() + geom_sf(aes(fill = at_least_two_change_25)) +
  theme_bw() +
  scale_fill_continuous_sequential(palette = "PuBuGn",
                                   begin = 0, end = 1,
                                   limits=c(0,1),
                                   breaks = c(0,0.5,1)) +
  coord_sf(ylim = c(-80, 90), xlim = c(-180, 180), expand = FALSE) +
  theme(legend.position = c(.15, .1),
        legend.direction="horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.5,'lines'),
        legend.key.size = unit(0.5, 'cm'),
        legend.background=element_blank())
dev.off()

# number of resources
pct_diff_ts_r_cs_ag <- read_csv("data/data_processing/countries_time_series_1985-2015_2070-2099_agriculture.csv")
pct_diff_ts_r_cs_fi <- read_csv("data/data_processing/countries_time-series_1985-2015_2070-2099_marine-fishery_global.csv")
pct_diff_ts_r_cs_wa <- read_csv("data/data_processing/countries_time_series_1985-2015_2070-2099_water_global.csv")
pct_diff_ts_r_cs <- rbind(pct_diff_ts_r_cs_ag, pct_diff_ts_r_cs_fi, pct_diff_ts_r_cs_wa)

nres_r <- data.frame(pct_diff_ts_r_cs) %>% 
  filter(!is.na(percent_diff),
         output_variable %in% c("rice", "wheat", "soybean", "maize", "tcblog10", "qtot")) %>% 
  group_by(regions) %>% 
  summarize(n_res = length(unique(output_variable)))

nres_r_map <- left_join(regions, nres_r, by = c("SOVEREIGN1" = "regions")) 

png(paste0("figures/revised_figures/figure_1_c.png"),
    width = 8*200, height = 4*200, res = 200)
nres_r_map %>% 
  ggplot() + geom_sf(aes(fill = as.factor(n_res))) +
  theme_bw() +
  scale_fill_brewer(palette = "Greys") +
  coord_sf(ylim = c(-80, 90), xlim = c(-180, 180), expand = FALSE) +
  theme(legend.position = c(.2, .075),
        legend.direction="horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.5,'lines'),
        legend.key.size = unit(0.4, 'cm'),
        legend.background=element_blank())
dev.off()


# link between p(X>=2) and p(X>=50%)
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

gradual <- gradual %>% 
  rename(p_2 = at_least_two_change_25,
         p_50 = at_least_half_bis_change_25) %>% 
  pivot_longer(2:4, names_to = "type", values_to = "proba") %>% 
  filter(type != "at_least_half_change_25") %>% 
  mutate(exposure_type = "gradual")
shocks <- shocks %>% 
  rename(p_2 = at_least_two_shocks,
         p_50 = at_least_half_bis_shocks) %>% 
  pivot_longer(2:4, names_to = "type", values_to = "proba") %>% 
  filter(type != "at_least_half_shocks") %>% 
  mutate(exposure_type = "shocks") 

dat <- rbind(shocks, gradual) %>% 
  pivot_wider(names_from = type, values_from = proba) %>% 
  data.frame()
dat <- left_join(dat, nres_r, by = "regions")

png(paste0("figures/revised_figures/figure_1_d.png"),
    width = 8*200, height = 4*200, res = 200)
ggplot(dat) + geom_point(aes(x = p_2, y = p_50, shape = exposure_type, colour = as.factor(n_res)), size = 2) +
  scale_shape_manual(values=c(17,19)) +
  scale_color_brewer(palette = "Greys", guide = "none") +
  theme_bw() +
  xlab("p(X>=2)") + ylab("p(X>=50%)") +
  theme(legend.position = c(.91, .075),
        legend.direction="vertical",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.5,'lines'),
        legend.key.size = unit(0.4, 'cm'),
        legend.background=element_blank()) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")
  
dev.off()

# # latitudinal distributions
# coords <- regions_dat %>% 
#   st_centroid() %>% 
#   st_coordinates()
# 
# densities_dat <- cbind(regions_dat, coords) %>% 
#   st_drop_geometry() %>% 
#   pivot_longer(c(3:8), values_to = "proba", names_to = "type") %>% 
#   mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "shock", "gradual"),
#          mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation","change"),
#          mechanism = ifelse(str_detect(type, "two_shocks_down")==TRUE,"synchrony",mechanism),
#          mechanism = ifelse(str_detect(type, "two_25_down")==TRUE,"synchrony",mechanism))
# 
# # plot latitudinal gradients
# gradients <- ggplot(densities_dat, aes(y = proba, x = Y, color = scale)) +
#   geom_smooth(span = 0.35, se = FALSE) +
#   theme_bw() +
#   facet_wrap(~mechanism, scales = "free", nrow = 1) + coord_flip() +
#   theme(strip.background = element_rect(color = "white", fill = "white"),
#         panel.spacing = unit(0.75,'lines'),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         text = element_text(size = 14),
#         legend.position = "bottom") +
#   ylab("Probability") + xlab("Latitude") +
#   scale_linetype_discrete(name="") +
#   scale_colour_manual(values = c("grey40","grey"), name="") +
#   scale_x_continuous(breaks = c(-60,-30,0,30,60), limits = c(-75,75)) 
# 
# png(paste0("figures/manuscript_figures/figure_2_gradients.png"),
#     width = 7*200, height = 7*200, res = 200)
# gradients
# dev.off()