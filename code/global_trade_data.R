#### Process the RTA database
#### Coding: Aurore A. Maureaud, July 2024

rm(list=ls())

# load libraries
library(here)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(readxl)

# load cross-sector climate exposure
dat_ecology <- read_csv("data/ecological_data/cross-sector_climate_exposure.csv")

# load RTA database
dat_rta <- read_xls("data/RTA WHO/FTA.xls", sheet = 1) %>% 
  dplyr::select(`RTA ID`,`RTA Name`,`Status`,`Accession?`,
                `RTA Composition`,`Current signatories`,
                `Region`) %>% 
  rename(rta_id = `RTA ID`,
         rta_name = `RTA Name`,
         status = `Status`,
         composition = `RTA Composition`,
         signatories = `Current signatories`,
         accession = `Accession?`,
         region = `Region`) %>% 
  filter(composition %in% c("Plurilateral"),
         status %in% c("In Force","In Force for at least one Party",
                       "Early announcement-Signed"),
         accession == "No")

write.csv(dat_rta, file = "data/RTA WHO/RTA_filtered.csv",
          row.names = F)


# ################################################################################
# #### 1. EXAMPLE OF THE USA
# ################################################################################
# 
# dat_rta_usa <- dat_rta %>% 
#   filter(str_detect(signatories, "United States of America")==TRUE)
# 
# # make list of RTA countries
# dat_rta_eco <- data.frame()
# for(i in 1:nrow(dat_rta_usa)){
#   signatories <- strsplit(dat_rta_usa$signatories[i],"; ")[[1]]
#   xx <- dat_rta_usa[i,]
#   xx[1:length(signatories),] <- dat_rta_usa[i,]
#   xx$signatories <- signatories
#   
#   if(i == 1){dat_rta_eco <- xx
#   } else {dat_rta_eco <- rbind(dat_rta_eco, xx)}
#   
#   rm(xx, signatories)
# }
# 
# # compare ecological exposure of RTA countries with the USA
# dat_rta_eco <- dat_rta_eco %>% 
#   mutate(signatories = ifelse(signatories == "United States of America","United States",signatories))
# dat_rta_eco <- left_join(dat_rta_eco, dat_ecology, by = c("signatories"="regions"))
# 
# # visualize USA RTA partners exposures to climate
# png("figures/combined/exposure_rta_United_States.png",
#     width = 10*200, height = 5*200, res = 200)
# dat_rta_eco %>% 
#   filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
#   ggplot(aes(y = reorder(signatories, proba, FUN = median), x = proba)) + geom_boxplot() +
#   geom_point(aes(shape = scale, color = mechanism), fill = "grey", stroke = 1) +
#   scale_shape_manual(values = c(21, 24)) +
#   scale_color_manual(values = c("darkorchid2","grey40","forestgreen")) +
#   theme_bw() + 
#   ylab("regions") + xlab("cross-sector exposure probabilities") +
#   facet_wrap(~ rta_id, scale = "free")
# dev.off()
