#### Long-term cross-sector resource change figures
#### Coding: Aurore A. Maureaud, April 2024

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
dat <- read_csv("data/long-term_change/countries_average_2010-2019_2090-2099.csv") %>% 
  filter(experiment_climate == "ssp585",
         output_variable != "ptotww") %>% 
  mutate(percent_diff = ifelse(!is.finite(percent_diff), NA, percent_diff)) %>% 
  group_by(sector, climate_model, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T))

dat_map <- data.frame(left_join(dat, regions, by = c("regions" = "UNION")))
dat_map <- st_as_sf(dat_map)

# map for GFDL for all sectors ----
png("figures/long-term_change/percent_difference_gfdl_esm4_ssp585_2010-2019_2090-2099.png",
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
png("figures/long-term_change/percent_difference_ipsl-cm6a-lr_ssp585_2010-2019_2090-2099.png",
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
dat <- read_csv("data/long-term_change/countries_average_2010-2019_2090-2099.csv") %>% 
  filter(experiment_climate == "ssp585",
         output_variable != "ptotww") %>% 
  mutate(percent_diff = ifelse(!is.finite(percent_diff), NA, percent_diff)) %>% 
  group_by(sector, climate_model, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T)) %>% 
  group_by(sector, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T))
dat_map <- data.frame(left_join(dat, regions, by = c("regions" = "UNION")))
dat_map <- st_as_sf(dat_map)

png("figures/long-term_change/percent_difference_climate_ensemble_ssp585_2010-2019_2090-2099.png",
    width = 15*200, height = 8*200, res = 200)
  ggplot(dat_map) + geom_sf(aes(fill = percent_diff)) +
  scale_fill_distiller(palette = "Spectral", direction = 1,
                       limits = c(-100,100)) +
  theme_bw() +
  facet_wrap(~ output_variable) +
  ggtitle("SSP585")
dev.off()

# number of sectors/output_variable per country ----
dat <- read_csv("data/long-term_change/countries_average_2010-2019_2090-2099.csv") %>% 
  filter(experiment_climate == "ssp585",
         output_variable != "ptotww") %>% 
  mutate(percent_diff = ifelse(!is.finite(percent_diff), NA, percent_diff)) %>% 
  group_by(sector, climate_model, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T)) %>% 
  group_by(sector, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T)) %>% 
  filter(!is.na(percent_diff)) %>% 
  group_by(regions) %>%
  summarize(nvar = length(output_variable))
dat_map <- data.frame(left_join(dat, regions, by = c("regions" = "UNION")))
dat_map <- st_as_sf(dat_map)

png("figures/long-term_change/nvar_by_country.png",
    width = 10*200, height = 8*200, res = 200)
ggplot(dat_map) + geom_sf(aes(fill = nvar)) +
  scale_fill_distiller(palette = "Greys", direction = 1) +
  theme_bw() +
  ggtitle("Number of output variables by country")
dev.off()

# number of sectors/output_variable per country ----
dat <- read_csv("data/long-term_change/countries_average_2010-2019_2090-2099.csv") %>% 
  filter(experiment_climate == "ssp585",
         output_variable != "ptotww") %>% 
  mutate(percent_diff = ifelse(!is.finite(percent_diff), NA, percent_diff)) %>% 
  group_by(sector, climate_model, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T)) %>% 
  group_by(sector, output_variable, regions) %>% 
  summarize(percent_diff = median(percent_diff, na.rm=T)) %>% 
  filter(!is.na(percent_diff)) %>% 
  group_by(regions) %>%
  summarize(nvar = length(output_variable),
            nhigher25 = length(percent_diff[percent_diff>(25)]),
            nlower25 = length(percent_diff[percent_diff<(-25)]),
            npositive = length(percent_diff[percent_diff>0]),
            nnegative = length(percent_diff[percent_diff<0]))
dat_map <- data.frame(left_join(dat, regions, by = c("regions" = "UNION")))
dat_map <- st_as_sf(dat_map)

nhigher <- ggplot(dat_map) + geom_sf(aes(fill = nhigher25)) +
  scale_fill_distiller(palette = "BuPu", direction = 1, limits = c(0,6)) +
  theme_bw() +
  ggtitle("nvar >25% difference increase")
nlower <- ggplot(dat_map) + geom_sf(aes(fill = nlower25)) +
  scale_fill_distiller(palette = "BuPu", direction = 1, limits = c(0,6)) +
  theme_bw() +
  ggtitle("nvar >25% difference decrease")
npositive <- ggplot(dat_map) + geom_sf(aes(fill = npositive)) +
  scale_fill_distiller(palette = "BuPu", direction = 1, limits = c(0,6)) +
  theme_bw() +
  ggtitle("nvar increasing")
nnegative <- ggplot(dat_map) + geom_sf(aes(fill = nnegative)) +
  scale_fill_distiller(palette = "BuPu", direction = 1, limits = c(0,6)) +
  theme_bw() +
  ggtitle("nvar decreasing")

png(paste0("figures/long-term_change/cross-sector_map_ssp585_2010-2019_2090-2099.png"), 
    width = 15*200, height = 8*200, res = 200)
grid.arrange(nhigher, nlower, npositive, nnegative, 
             ncol = 2, top = "Cross-sector map SSP585")
dev.off()


#### B. Maps of % difference for 2080-2099 compared to 2000-2019 ----
