#### Figure 3
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
library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")


################################################################################
#### 3. ARE COUNTRIES EXPERIENCING SYNCHRONY AND/OR COMPENSATION?
################################################################################
#### A. load data ----
# countries
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

# eez-land merge shapefile
regions <- st_read("data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp") %>% 
  filter(POL_TYPE != "Joint regime (EEZ)",
         is.na(SOVEREIGN2),
         SOVEREIGN1 != "Republic of Mauritius",
         UNION != "Antarctica") %>% 
  dplyr::select(UNION, SOVEREIGN1)

#### B. figure 2 maps ssp5.85----
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

regions_dat <- left_join(regions, dat_mec_ssp585, by = c("SOVEREIGN1" = "regions")) 

# average climate models for ssp585 with the 25% threshold
png(paste0("figures/revised_figures/figure_3_a.png"),
    width = 8*200, height = 4*200, res = 200)
regions_dat %>%
  ggplot() + 
  geom_sf(data = world, fill = "white") + 
  geom_sf(aes(fill = at_least_two_shocks_down), alpha = 0.8) +
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

png(paste0("figures/revised_figures/figure_3_c.png"),
    width = 8*200, height = 4*200, res = 200)
regions_dat %>%
  mutate(at_least_two_25_down = ifelse(at_least_two_25_down<0, 0, at_least_two_25_down)) %>% 
  ggplot() + 
  geom_sf(data = world, fill = "white") + 
  geom_sf(aes(fill = at_least_two_25_down), alpha = 0.8) +
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
        legend.key.size = unit(0.75, 'cm'),
        legend.background=element_blank(),
        text = element_text(size = 22))
dev.off()

png(paste0("figures/revised_figures/figure_3_b.png"),
    width = 8*200, height = 4*200, res = 200)
regions_dat %>%
  ggplot() + 
  geom_sf(data = world, fill = "white") + 
  geom_sf(aes(fill = at_least_one_shock_up_down), alpha = 0.8) +
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
        legend.key.size = unit(0.75, 'cm'),
        legend.background=element_blank(),
        text = element_text(size = 22))
dev.off()

png(paste0("figures/revised_figures/figure_3_d.png"),
    width = 8*200, height = 4*200, res = 200)
regions_dat %>%
  mutate(at_least_one_25_up_down = ifelse(at_least_one_25_up_down<0, 0, at_least_one_25_up_down)) %>% 
  ggplot() + 
  geom_sf(data = world, fill = "white") + 
  geom_sf(aes(fill = at_least_one_25_up_down), alpha = 0.8) +
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
        legend.key.size = unit(0.75, 'cm'),
        legend.background=element_blank(),
        text = element_text(size = 22))
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
# 
