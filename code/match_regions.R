#### Match region names
#### Coding: Aurore A. Maureaud, July 2024

rm(list = ls())

# load libraries
library(raster)
library(ncdf4)
library(here)
library(tidyverse)
library(ggplot2)
library(sf)
sf_use_s2(FALSE)
library(exactextractr)
library(abind)

# regions from EEZ shapefile
regions <- st_read("data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp") %>% 
  filter(POL_TYPE != "Joint regime (EEZ)",
         is.na(SOVEREIGN2)) %>% 
  dplyr::select(UNION, SOVEREIGN1) %>% 
  st_drop_geometry()

# regions from RTA/WTO
dat_rta <- read_xls("data/RTA WHO/FTA.xls", sheet = 1) %>% 
  dplyr::select(`RTA ID`,`RTA Name`,`Status`,
                `RTA Composition`,`Current signatories`) %>% 
  rename(rta_id = `RTA ID`,
         rta_name = `RTA Name`,
         status = `Status`,
         composition = `RTA Composition`,
         signatories = `Current signatories`) %>% 
  filter(composition %in% c("Plurilateral"),
         status %in% c("In Force","In Force for at least one Party",
                       "Early announcement-Signed"))

# make list of RTA countries
rta <- data.frame()
for(i in 1:nrow(dat_rta)){
  signatories <- strsplit(dat_rta$signatories[i],"; ")[[1]]
  xx <- dat_rta[i,]
  xx[1:length(signatories),] <- dat_rta[i,]
  xx$signatories <- signatories
  
  if(i == 1){rta <- xx
  } else {rta <- rbind(rta, xx)}
  
  rm(xx, signatories)
}

rta_signatories <- rta %>% 
  dplyr::select(signatories) %>% 
  distinct()

# full join
match_regions <- full_join(regions, rta_signatories, by = c("SOVEREIGN1" = "signatories"), keep = TRUE)
write_csv(match_regions, file = "data/match_regions/match_regions_eez_rta.csv")




