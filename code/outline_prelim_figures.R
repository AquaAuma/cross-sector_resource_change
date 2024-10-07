#### Long-term cross-sector resource change figures
#### Coding: Aurore A. Maureaud, June 2024

# load libraries
library(raster)
library(ncdf4)
library(here)
library(tidyverse)
library(ggplot2)
library(sf)
sf_use_s2(FALSE)
library(gridExtra)

# load shapefile
regions <- st_read("data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp") %>% 
  filter(POL_TYPE != "Joint regime (EEZ)") %>% 
  dplyr::select(UNION)

#### A. Map of % difference for 2090-2099 compared to 2010-2019 ----
dat <- read_csv("data/long-term_change/countries_average_2010-2019_2090-2099_revised.csv") %>% 
  filter(experiment_climate == "ssp585",
         output_variable != "ptotww") %>% 
  mutate(percent_diff = ifelse(!is.finite(percent_diff), NA, percent_diff)) %>% 
  group_by(sector, climate_model, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T))

dat_map <- data.frame(left_join(dat, regions, by = c("regions" = "UNION")))
dat_map <- st_as_sf(dat_map)

# map for GFDL for all sectors ----
png("figures/long-term_change/percent_difference_gfdl_esm4_ssp585_2010-2019_2090-2099_revised.png",
    width = 15*200, height = 8*200, res = 200)
dat_map %>% 
  filter(climate_model == "gfdl-esm4") %>% 
  ggplot() + geom_sf(aes(fill = percent_diff)) +
  scale_fill_distiller(palette = "Spectral", direction = 1,
                       limits = c(-100,100)) +
  theme_bw() +
  facet_wrap(~ output_variable) +
  ggtitle("GFDL-ESM4 SSP585")
dev.off()

# map for IPSL for all sectors ----
png("figures/long-term_change/percent_difference_ipsl-cm6a-lr_ssp585_2010-2019_2090-2099_revised.png",
    width = 15*200, height = 8*200, res = 200)
dat_map %>% 
  filter(climate_model == "ipsl-cm6a-lr") %>% 
  ggplot() + geom_sf(aes(fill = percent_diff)) +
  scale_fill_distiller(palette = "Spectral", direction = 1,
                       limits = c(-100,100)) +
  theme_bw() +
  facet_wrap(~ output_variable) +
  ggtitle("IPSL-CM6A-LR SSP585")
dev.off()

# map for climate models median ----
dat <- read_csv("data/long-term_change/countries_average_2010-2019_2090-2099_revised.csv") %>% 
  filter(experiment_climate == "ssp585",
         output_variable != "ptotww") %>% 
  mutate(percent_diff = ifelse(!is.finite(percent_diff), NA, percent_diff)) %>% 
  group_by(sector, climate_model, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T)) %>% 
  group_by(sector, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T))
dat_map <- data.frame(left_join(dat, regions, by = c("regions" = "UNION")))
dat_map <- st_as_sf(dat_map)

png("figures/long-term_change/percent_difference_climate_ensemble_ssp585_2010-2019_2090-2099_revised.png",
    width = 15*200, height = 8*200, res = 200)
  ggplot(dat_map) + geom_sf(aes(fill = percent_diff)) +
  scale_fill_distiller(palette = "Spectral", direction = 1,
                       limits = c(-100,100)) +
  theme_bw() +
  facet_wrap(~ output_variable) +
  ggtitle("SSP585")
dev.off()

# number of sectors/output_variable per country ----
dat <- read_csv("data/long-term_change/countries_average_2010-2019_2090-2099_revised.csv") %>% 
  filter(experiment_climate == "ssp585",
         output_variable != "ptotww") %>% 
  mutate(percent_diff = ifelse(!is.finite(percent_diff), NA, percent_diff)) %>% 
  group_by(sector, climate_model, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T)) %>% 
  group_by(sector, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T)) %>% 
  filter(!is.na(percent_diff)) %>% 
  group_by(regions) %>%
  summarize(nvar = length(output_variable)/6)
dat_map <- data.frame(left_join(dat, regions, by = c("regions" = "UNION")))
dat_map <- st_as_sf(dat_map)

png("figures/long-term_change/nvar_by_country_revised.png",
    width = 10*200, height = 5*200, res = 200)
ggplot(dat_map) + geom_sf(aes(fill = nvar)) +
  scale_fill_distiller(palette = "Greys", direction = 1, limits = c(0,1)) +
  theme_bw() +
  ggtitle("Number of output variables by country")
dev.off()

# cross-sector maps ----
dat <- read_csv("data/long-term_change/countries_average_2010-2019_2090-2099_revised.csv") %>% 
  filter(experiment_climate == "ssp585",
         output_variable != "ptotww") %>% 
  mutate(percent_diff = ifelse(!is.finite(percent_diff), NA, percent_diff)) %>% 
  group_by(sector, climate_model, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T)) %>% 
  # group_by(sector, output_variable, regions) %>% 
  # summarize(percent_diff = median(percent_diff, na.rm=T)) %>% 
  filter(!is.na(percent_diff)) %>% 
  group_by(regions, climate_model) %>%
  summarize(nvar = length(output_variable),
            nhigher25 = length(percent_diff[percent_diff>(25)]),
            nlower25 = length(percent_diff[percent_diff<(-25)]),
            npositive = length(percent_diff[percent_diff>0]),
            nnegative = length(percent_diff[percent_diff<0]))
dat_map <- data.frame(left_join(dat, regions, by = c("regions" = "UNION")))
dat_map <- st_as_sf(dat_map)

climates <- sort(unique(dat_map$climate_model))

for(c in 1:length(climates)){
  nhigher <- ggplot(dat_map[dat_map$climate_model==climates[c],]) + geom_sf(aes(fill = nhigher25)) +
    scale_fill_distiller(palette = "BuPu", direction = 1, limits = c(0,8)) +
    theme_bw() +
    ggtitle(climates[c])
  nlower <- ggplot(dat_map[dat_map$climate_model==climates[c],]) + geom_sf(aes(fill = nlower25)) +
    scale_fill_distiller(palette = "BuPu", direction = 1, limits = c(0,8)) +
    theme_bw() +
    ggtitle(climates[c])
  npositive <- ggplot(dat_map[dat_map$climate_model==climates[c],]) + geom_sf(aes(fill = npositive)) +
    scale_fill_distiller(palette = "BuPu", direction = 1, limits = c(0,8)) +
    theme_bw() +
    ggtitle(climates[c])
  nnegative <- ggplot(dat_map[dat_map$climate_model==climates[c],]) + geom_sf(aes(fill = nnegative)) +
    scale_fill_distiller(palette = "BuPu", direction = 1, limits = c(0,8)) +
    theme_bw() +
    ggtitle(climates[c])
  
  png(paste0("figures/long-term_change/cross-sector_map_",climates[c],"_ssp585_2010-2019_2090-2099_revised.png"), 
      width = 15*200, height = 8*200, res = 200)
  grid.arrange(nhigher, nlower, npositive, nnegative, ncol = 2)
  dev.off()
}


# Nigeria resource long-term change ----

dat <- read_csv("data/long-term_change/countries_average_2010-2019_2090-2099_revised.csv") %>% 
  filter(experiment_climate == "ssp585",
         output_variable != "ptotww",
         regions == "Nigeria") %>% 
  mutate(output_variable = ifelse(output_variable == "total consumer biomass","tcb",output_variable),
         percent_diff = ifelse(!is.finite(percent_diff), NA, percent_diff)) %>% 
  filter(!is.na(percent_diff))

png("figures/long-term_change/nigeria_ensembles.png",
    width = 8*200, height = 5*200, res = 200)
ggplot(dat) + 
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=25), fill = "lightgrey", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
  geom_hline(yintercept = -25, linetype = "dashed", col = "grey") +
  geom_hline(yintercept = 25, linetype = "dashed", col = "grey") +
  geom_boxplot(data = dat, aes(x = output_variable, y = percent_diff, fill = climate_model)) +
  theme_bw() +
  theme(text = element_text(size = 22)) + xlab("") + ylab("% difference") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("indianred1","indianred4"))
dev.off()


