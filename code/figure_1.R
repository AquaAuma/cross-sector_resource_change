#### Figure 1 and alternative versions
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
#### 1. ARE COUNTRIES EXPERIENCING SHORT- AND LONG-TERM CROSS-SECTOR CHANGES?
################################################################################

#### A. load data ----
# countries
shock_probas_r_cs <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas_spans.csv") %>% 
  filter(spans == "shock_0.75") %>% 
  dplyr::select(-spans)
change_probas_r_cs <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas.csv") %>% 
  filter(time_window == 30) %>% 
  dplyr::select(-spatial_scale, -time_window)

# global
shock_probas_g_cs <- read_csv("data/short-term_change/global_shocks_1985-2015_probas_spans.csv") %>% 
  mutate(regions = "global") %>% 
  filter(spans == "shock_0.75") %>% 
  dplyr::select(names(shock_probas_r_cs))
change_probas_g_cs <- read_csv("data/long-term_change/global_long-term_change_1985-2015_probas.csv") %>% 
  filter(time_window == 30) %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(change_probas_r_cs))

# rbind
shock_probas <- rbind(shock_probas_g_cs, shock_probas_r_cs)
long_term <- rbind(change_probas_g_cs, change_probas_r_cs) %>% 
  pivot_wider(names_from = "type", values_from = "probas")
dat <- left_join(long_term, shock_probas, by = c("climates", "regions")) %>% 
  filter(regions != "Antarctica")


#### B. main figure ----
# example plots for cross-sector conditions

dat_points <- dat %>% 
  filter(!is.na(at_least_two_shocks),
         !is.na(at_least_two_change_10)) %>% 
  mutate(ssp = ifelse(str_detect(climates, "ssp126"), "SSP 1.26", NA_character_),
         ssp = ifelse(str_detect(climates, "ssp585"), "SSP 5.85", ssp),
         climate_model = ifelse(str_detect(climates, "gfdl"), "GFDL-ESM4", NA_character_),
         climate_model = ifelse(str_detect(climates, "ipsl"), "IPSL-CM6A-LR", climate_model)) 

map_points <- dat_points %>% 
  ggplot(aes(y = at_least_two_change_10, x = at_least_two_shocks)) +
  geom_point(alpha = 0.5) +
  facet_grid(ssp ~ climate_model) +
  geom_point(data = dat_points[dat_points$regions=="global",], 
             aes(y = at_least_two_change_10, x = at_least_two_shocks),
             col = "red") +
  theme_bw() + xlim(0,1) + ylim(0,1) +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.75,'lines'),
        strip.text = element_text(size = 14), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 14)) +
  scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0.02,0.02)) +
  ylab("Probability of at least 2 res. changing by >10%") + xlab("Probability of at least 2 shocks") +
  geom_text_repel(data = dat_points[dat_points$at_least_two_shocks>0.5 & dat_points$at_least_two_change_10>0.5,], 
                  aes(label = regions), size=3, max.overlaps=20)

dat_densities <- dat %>% 
  filter(!is.na(at_least_two_shocks),
         !is.na(at_least_two_change_10)) %>% 
  mutate(ssp = ifelse(str_detect(climates, "ssp126"), "SSP 1.26", NA_character_),
         ssp = ifelse(str_detect(climates, "ssp585"), "SSP 5.85", ssp),
         climate_model = ifelse(str_detect(climates, "gfdl"), "GFDL-ESM4", NA_character_),
         climate_model = ifelse(str_detect(climates, "ipsl"), "IPSL-CM6A-LR", climate_model)) %>% 
  select(regions, climates, ssp, climate_model, at_least_two_change_10, at_least_two_shocks) %>%
  rename(`gradual` = at_least_two_change_10, 
         `abrupt` = at_least_two_shocks) %>% 
  pivot_longer(5:6, names_to = "type", values_to = "probas") 

mean_densities <- dat_densities %>% 
  filter(regions != "global") %>% 
  group_by(ssp, climate_model, type) %>% 
  summarize(probas = mean(probas, na.rm=T))

plot_densities <- dat_densities %>% 
  ggplot(aes(x = probas, fill = type, linetype = climate_model, color = type)) + geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~ssp, scales = "free", nrow=2) +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.75,'lines'),
        strip.text = element_blank(),
        legend.position = c(0.35,0.25),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 14)) +
  scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0.02,0.02)) +
  xlab("Probability") + ylab("") +
  scale_fill_manual(values = c("grey","grey40"), name="") + 
  scale_linetype_discrete(name="") + ggtitle("Density") +
  geom_vline(data = mean_densities, aes(xintercept = probas, linetype = climate_model, color = type)) +
  scale_colour_manual(values = c("grey","grey40"), name="") 


png(paste0("figures/manuscript_figures/figure_1_version_1.png"),
    width = 12*200, height = 6*200, res = 200)
grid.arrange(map_points, plot_densities, ncol = 2, widths = c(3,1))
dev.off()


#### C. Alternative versions of Figure 1 ----
# with threshold of 25%
dat_points <- dat %>% 
  filter(!is.na(at_least_two_shocks),
         !is.na(at_least_two_change_25)) %>% 
  mutate(ssp = ifelse(str_detect(climates, "ssp126"), "SSP 1.26", NA_character_),
         ssp = ifelse(str_detect(climates, "ssp585"), "SSP 5.85", ssp),
         climate_model = ifelse(str_detect(climates, "gfdl"), "GFDL-ESM4", NA_character_),
         climate_model = ifelse(str_detect(climates, "ipsl"), "IPSL-CM6A-LR", climate_model)) 

map_points <- dat_points %>% 
  ggplot(aes(y = at_least_two_change_25, x = at_least_two_shocks)) +
  geom_point(alpha = 0.5) +
  facet_grid(ssp ~ climate_model) +
  geom_point(data = dat_points[dat_points$regions=="global",], 
             aes(y = at_least_two_change_25, x = at_least_two_shocks),
             col = "red") +
  theme_bw() + xlim(0,1) + ylim(0,1) +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.75,'lines'),
        strip.text = element_text(size = 14), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 14)) +
  scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0.02,0.02)) +
  ylab("Probability of at least 2 res. changing by >25%") + xlab("Probability of at least 2 shocks") +
  geom_text_repel(data = dat_points[dat_points$at_least_two_shocks>0.5 & dat_points$at_least_two_change_25>0.5,], 
                  aes(label = regions), size=3, max.overlaps=20)

dat_densities <- dat %>% 
  filter(!is.na(at_least_two_shocks),
         !is.na(at_least_two_change_25)) %>% 
  mutate(ssp = ifelse(str_detect(climates, "ssp126"), "SSP 1.26", NA_character_),
         ssp = ifelse(str_detect(climates, "ssp585"), "SSP 5.85", ssp),
         climate_model = ifelse(str_detect(climates, "gfdl"), "GFDL-ESM4", NA_character_),
         climate_model = ifelse(str_detect(climates, "ipsl"), "IPSL-CM6A-LR", climate_model)) %>% 
  select(regions, climates, ssp, climate_model, at_least_two_change_25, at_least_two_shocks) %>%
  rename(`gradual` = at_least_two_change_25, 
         `abrupt` = at_least_two_shocks) %>% 
  pivot_longer(5:6, names_to = "type", values_to = "probas")

mean_densities <- dat_densities %>%
  filter(regions != "global") %>% 
  group_by(ssp, climate_model, type) %>% 
  summarize(probas = mean(probas, na.rm=T))

plot_densities <- ggplot(dat_densities, aes(x = probas, fill = type, linetype = climate_model, color = type)) + geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~ssp, scales = "free", nrow=2) +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.75,'lines'),
        strip.text = element_blank(),
        legend.position = c(0.65,0.8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 14)) +
  scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0.02,0.02)) +
  xlab("Probability") + ylab("") +
  scale_fill_manual(values = c("grey","grey40"), name="") + 
  scale_linetype_discrete(name="") + ggtitle("Density") +
  geom_vline(data = mean_densities, aes(xintercept = probas, linetype = climate_model, color = type)) +
  scale_colour_manual(values = c("grey","grey40"), name="") 


png(paste0("figures/manuscript_figures/figure_1_version_2.png"),
    width = 12*200, height = 6*200, res = 200)
grid.arrange(map_points, plot_densities, ncol = 2, widths = c(3,1))
dev.off()


#### D. figure 1 with average of climate models ----
dat_points <- dat %>% 
  mutate(ssp = ifelse(str_detect(climates, "ssp126"), "SSP 1.26", NA_character_),
         ssp = ifelse(str_detect(climates, "ssp585"), "SSP 5.85", ssp),
         climate_model = ifelse(str_detect(climates, "gfdl"), "GFDL-ESM4", NA_character_),
         climate_model = ifelse(str_detect(climates, "ipsl"), "IPSL-CM6A-LR", climate_model)) %>% 
  group_by(regions, ssp) %>% 
  summarize(at_least_two_shocks = mean(at_least_two_shocks, na.rm=T),
            at_least_two_change_25 = mean(at_least_two_change_25, na.rm=T))

map_points <- dat_points %>% 
  ggplot(aes(y = at_least_two_change_25, x = at_least_two_shocks)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ssp, nrow = 2) +
  geom_point(data = dat_points[dat_points$regions=="global",], 
             aes(y = at_least_two_change_25, x = at_least_two_shocks),
             col = "red") +
  theme_bw() + xlim(0,1) + ylim(0,1) +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.75,'lines'),
        strip.text = element_text(size = 14), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 14)) +
  scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0.02,0.02)) +
  ylab("Probability of at least 2 res. changing by >25%") + xlab("Probability of at least 2 shocks") +
  geom_text_repel(data = dat_points[dat_points$at_least_two_shocks>0.75 & dat_points$at_least_two_change_25>0.75,], 
                  aes(label = regions), size=3, max.overlaps=20) +
  geom_text_repel(data = dat_points[dat_points$at_least_two_shocks<0.25 & dat_points$at_least_two_change_25>0.75,], 
                  aes(label = regions), size=3, max.overlaps=20) +
  geom_text_repel(data = dat_points[dat_points$at_least_two_shocks>0.75 & dat_points$at_least_two_change_25<0.25,], 
                  aes(label = regions), size=3, max.overlaps=20)

dat_densities <- dat %>% 
  mutate(ssp = ifelse(str_detect(climates, "ssp126"), "SSP 1.26", NA_character_),
         ssp = ifelse(str_detect(climates, "ssp585"), "SSP 5.85", ssp),
         climate_model = ifelse(str_detect(climates, "gfdl"), "GFDL-ESM4", NA_character_),
         climate_model = ifelse(str_detect(climates, "ipsl"), "IPSL-CM6A-LR", climate_model)) %>% 
  group_by(regions, ssp) %>% 
  summarize(at_least_two_shocks = mean(at_least_two_shocks, na.rm=T),
            at_least_two_change_25 = mean(at_least_two_change_25, na.rm=T)) %>% 
  rename(`gradual` = at_least_two_change_25, 
         `abrupt` = at_least_two_shocks) %>% 
  pivot_longer(3:4, names_to = "type", values_to = "probas")

mean_densities <- dat_densities %>%
  filter(regions != "global") %>% 
  group_by(ssp, type) %>% 
  summarize(probas = mean(probas, na.rm=T))

plot_densities <- ggplot(dat_densities, aes(x = probas, fill = type, color = type)) + geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~ssp, scales = "free", nrow=2) +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.75,'lines'),
        strip.text = element_blank(),
        legend.position = c(0.65,0.8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 14)) +
  scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0.02,0.02)) +
  xlab("Probability") + ylab("") +
  scale_fill_manual(values = c("grey","grey40"), name="") + 
  scale_linetype_discrete(name="") + ggtitle("Density") +
  geom_vline(data = mean_densities, aes(xintercept = probas, color = type)) +
  scale_colour_manual(values = c("grey","grey40"), name="") 


png(paste0("figures/manuscript_figures/figure_1_version_3.png"),
    width = 10*200, height = 6*200, res = 200)
grid.arrange(map_points, plot_densities, ncol = 2)
dev.off()

# list of countries in corners of the plots
list_countries_1 <- dat_points %>% 
  group_by(regions, ssp) %>% 
  filter(at_least_two_change_25>0.75 & at_least_two_shocks>0.75) %>% 
  pull(regions)

list_countries_2 <- dat_points %>% 
  group_by(regions, ssp) %>% 
  filter(at_least_two_change_25>0.75 & at_least_two_shocks<0.25) %>% 
  pull(regions)

list_countries_3 <- dat_points %>% 
  group_by(regions, ssp) %>% 
  filter(at_least_two_change_25<0.25 & at_least_two_shocks>0.75) %>% 
  pull(regions)

list_countries_4 <- dat_points %>% 
  group_by(regions, ssp) %>% 
  filter(at_least_two_change_25<0.25 & at_least_two_shocks<0.25,
         ssp == "SSP 1.26") %>% 
  pull(regions)

list_countries_5 <- dat_points %>% 
  group_by(regions, ssp) %>% 
  filter(at_least_two_change_25<0.25 & at_least_two_shocks<0.25,
         ssp == "SSP 5.85") %>% 
  pull(regions)

# correlation between metrics under different SSPs
cor(dat_points$at_least_two_shocks[dat_points$ssp=="SSP 1.26"],
    dat_points$at_least_two_change_25[dat_points$ssp=="SSP 1.26"],
    method = "pearson")
cor.test(dat_points$at_least_two_shocks[dat_points$ssp=="SSP 1.26"],
         dat_points$at_least_two_change_25[dat_points$ssp=="SSP 1.26"],
         method = "pearson")

cor(dat_points$at_least_two_shocks[dat_points$ssp=="SSP 5.85"],
    dat_points$at_least_two_change_25[dat_points$ssp=="SSP 5.85"],
    method = "pearson")
cor.test(dat_points$at_least_two_shocks[dat_points$ssp=="SSP 5.85"],
         dat_points$at_least_two_change_25[dat_points$ssp=="SSP 5.85"],
         method = "pearson")
