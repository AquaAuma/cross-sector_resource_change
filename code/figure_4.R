#### Figure 4 and alternative versions
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
#### 4. EXPOSURE TO CLIMATE CS IMPACTS for CNT versus RTAs
################################################################################

#### A. load data for short- and long-term changes ----
# countries
shock_probas_r_cs <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas_spans.csv") %>% 
  mutate(spatial_scale = "regions") %>% 
  filter(spans == "shock_0.75") %>% 
  dplyr::select(-spans)
change_probas_r_cs <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas.csv") %>% 
  filter(time_window == 30)

# global
shock_probas_g_cs <- read_csv("data/short-term_change/global_shocks_1985-2015_probas_spans.csv") %>% 
  mutate(regions = "global",
         spatial_scale = "global") %>% 
  filter(spans == "shock_0.75") %>% 
  dplyr::select(names(shock_probas_r_cs))
change_probas_g_cs <- read_csv("data/long-term_change/global_long-term_change_1985-2015_probas.csv") %>% 
  mutate(regions = "global",
         spatial_scale = "global") %>% 
  filter(time_window == 30) %>% 
  dplyr::select(names(change_probas_r_cs))

# rtas
shock_probas_rta_cs <- read_csv("data/short-term_change/rta_shocks_1985-2015_probas_spans.csv") %>% 
  mutate(spatial_scale = "rta") %>% 
  filter(spans == "shock_0.75") %>% 
  dplyr::select(names(shock_probas_r_cs))
change_probas_rta_cs <- read_csv("data/long-term_change/rta_long-term_change_1985-2015_probas.csv") %>% 
  filter(time_window == 30) %>% 
  dplyr::select(names(change_probas_r_cs))

# rbind
shock_probas <- rbind(shock_probas_g_cs, shock_probas_r_cs, shock_probas_rta_cs)
long_term <- rbind(change_probas_g_cs, change_probas_r_cs, change_probas_rta_cs) %>% 
  pivot_wider(names_from = "type", values_from = "probas")
dat <- left_join(long_term, shock_probas, by = c("climates", "regions", "spatial_scale")) %>% 
  filter(regions != "Antarctica")

#### B. load data for compensatory and synchronous mechanisms ----
# countries
shock_probas_r_cs_mec <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas_spans_mechanisms.csv") %>% 
  mutate(spatial_scale = "regions") %>% 
  filter(spans == "0.75") %>% 
  dplyr::select(-spans, -at_least_two_shocks_up, -at_least_three_shocks_up)
change_probas_r_cs_mec <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas_mechanisms.csv") %>% 
  filter(time_window == 30) %>% 
  dplyr::select(-time_window)

# global
shock_probas_g_cs_mec <- read_csv("data/short-term_change/global_shocks_1985-2015_probas_spans_mechanisms.csv") %>% 
  mutate(regions = "global",
         spatial_scale = "global") %>% 
  filter(spans == "0.75") %>% 
  dplyr::select(names(shock_probas_r_cs_mec))
change_probas_g_cs_mec <- read_csv("data/long-term_change/global_long-term_change_1985-2015_probas_mechanisms.csv") %>% 
  mutate(regions = "global",
         saptial_scale = "global") %>% 
  filter(time_window == 30) %>% 
  dplyr::select(names(change_probas_r_cs_mec))

# rta
shock_probas_rta_cs_mec <- read_csv("data/short-term_change/rta_shocks_1985-2015_probas_spans_mechanisms.csv") %>% 
  mutate(spatial_scale = "rta") %>% 
  filter(spans == "0.75") %>% 
  dplyr::select(names(shock_probas_r_cs_mec))
change_probas_rta_cs_mec <- read_csv("data/long-term_change/rta_long-term_change_1985-2015_probas_mechanisms.csv")  %>% 
  filter(time_window == 30) %>% 
  dplyr::select(names(change_probas_r_cs_mec))

# rbind
shock_probas <- rbind(shock_probas_g_cs_mec, shock_probas_r_cs_mec, shock_probas_rta_cs_mec)
long_term <- rbind(change_probas_g_cs_mec, change_probas_r_cs_mec, change_probas_rta_cs_mec) %>% 
  pivot_wider(names_from = "type", values_from = "probas")
dat_mec <- left_join(long_term, shock_probas, by = c("climates", "regions", "spatial_scale")) %>% 
  filter(regions != "Antarctica")


#### C. Data across metrics ----
# cbind both datasets
dat_ecology_rta <- left_join(dat, dat_mec, by = c("regions", "climates", "spatial_scale")) %>% 
  dplyr::select(regions, climates, spatial_scale,
                at_least_two_change_25, at_least_two_shocks,
                at_least_two_25_down, at_least_one_25_up_down,
                at_least_two_shocks_down, at_least_one_shock_up_down) %>%
  mutate(at_least_one_25_up_down = 1-at_least_one_25_up_down,
         at_least_one_shock_up_down = 1-at_least_one_shock_up_down) %>%
  pivot_longer(4:9, names_to = "type", values_to = "proba") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "abrupt", "gradual"),
         mechanism = ifelse(str_detect(type, "down")==TRUE, "synchrony", "stability"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation", mechanism))

# make matrix of country versus RTA
dat_rta <- read_csv("data/RTA WHO/RTA_filtered.csv")

dat_rta_list <- data.frame()
for(i in 1:nrow(dat_rta)){
  signatories <- strsplit(dat_rta$signatories[i],"; ")[[1]]
  xx <- dat_rta[i,]
  xx[1:length(signatories),] <- dat_rta[i,]
  xx$signatories <- signatories
  
  if(i == 1){dat_rta_list <- xx
  } else {dat_rta_list <- rbind(dat_rta_list, xx)}
  
  rm(xx, signatories)
}

match_with_rta <- read_csv("data/match_regions/match_regions_eez_rta_reconciled.csv") %>% 
  dplyr::select(SOVEREIGN1, signatories) %>% 
  filter(SOVEREIGN1 != "Republic of Mauritius") %>% 
  distinct()

dat_rta_list <- left_join(dat_rta_list, match_with_rta, by = "signatories") %>% 
  dplyr::select(rta_id, SOVEREIGN1)

dat_ecology_rta <- dat_ecology_rta %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  group_by(scale, spatial_scale, regions) %>% 
  summarize(proba = median(proba, na.rm=T)) %>% 
  filter(spatial_scale!="global")

dat_ecology_cnt <- dat_ecology_rta %>% 
  filter(spatial_scale == "regions")
dat_ecology_rta <- dat_ecology_rta %>% 
  filter(spatial_scale == "rta")

dat_matrix <- left_join(dat_rta_list, dat_ecology_cnt, by = c("SOVEREIGN1" = "regions"),
                        relationship = "many-to-many") %>% 
  mutate(rta_id = as.character(rta_id))
dat_matrix <- left_join(dat_matrix, dat_ecology_rta, by = c("rta_id" = "regions","scale")) %>% 
  mutate(proba_ratio = proba.x/proba.y)


#### D. Figure 4 ----
dat_rta$rta_id <- as.character(dat_rta$rta_id)
dat_fig4 <- dat_matrix %>% 
  left_join(dat_rta, "rta_id") %>% 
  filter(!is.na(scale)) %>% 
  mutate(region = ifelse(str_detect(region, ";")==TRUE, "Cross-regional",region),
         region = ifelse(region == "Commonwealth of Independent States (CIS), including certain associate and former member States", "Cross-regional", region)) %>% 
  mutate(proba_ratio = ifelse(proba_ratio>2, 2, proba_ratio)) %>% 
  arrange(region)

png(paste0("figures/manuscript_figures/figure_4_version_1.png"),
    width = 12*200, height = 12*200, res = 200)
ggplot(dat_fig4, aes(y = proba_ratio, x = reorder(rta_acronym, desc(rta_acronym)))) +
  geom_boxplot(linetype = 2, lwd = 0.15, fill = "grey95") +
  geom_point(alpha = 0.5, size = 2) +
  geom_point(data = dat_fig4[dat_fig4$SOVEREIGN1 %in% c("United Kingdom","United States","Russia","China","Australia","Norway","Sweden","Spain"),], 
             aes(y = proba_ratio, x = reorder(rta_acronym, desc(rta_acronym)),
                 fill = SOVEREIGN1), shape = 21, size = 2, alpha = 0.8) +
  theme_bw() +
  facet_grid(region ~ scale,scale="free_y", space = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0,'lines'),
        legend.position = "bottom",
        text = element_text(size = 18)) +
  xlab("Plurilateral RTAs") + ylab("Exposure ratio") +
  geom_hline(aes(yintercept=1), linetype = "dashed") +
  coord_flip() + ylim(0,2) +
  guides(fill=guide_legend(title="")) +
  scale_fill_manual(values = c("coral","darkred","blue","cornflowerblue","darkgoldenrod1","purple","plum","darkorange"))
dev.off()


#### E. Figure 4 ssp126 SI----
dat_ecology_rta <- left_join(dat, dat_mec, by = c("regions", "climates", "spatial_scale")) %>% 
  dplyr::select(regions, climates, spatial_scale,
                at_least_two_change_25, at_least_two_shocks,
                at_least_two_25_down, at_least_one_25_up_down,
                at_least_two_shocks_down, at_least_one_shock_up_down) %>%
  mutate(at_least_one_25_up_down = 1-at_least_one_25_up_down,
         at_least_one_shock_up_down = 1-at_least_one_shock_up_down) %>%
  pivot_longer(4:9, names_to = "type", values_to = "proba") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "abrupt", "gradual"),
         mechanism = ifelse(str_detect(type, "down")==TRUE, "synchrony", "stability"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation", mechanism))

# make matrix of country versus RTA
dat_rta <- read_csv("data/RTA WHO/RTA_filtered.csv")

dat_rta_list <- data.frame()
for(i in 1:nrow(dat_rta)){
  signatories <- strsplit(dat_rta$signatories[i],"; ")[[1]]
  xx <- dat_rta[i,]
  xx[1:length(signatories),] <- dat_rta[i,]
  xx$signatories <- signatories
  
  if(i == 1){dat_rta_list <- xx
  } else {dat_rta_list <- rbind(dat_rta_list, xx)}
  
  rm(xx, signatories)
}

match_with_rta <- read_csv("data/match_regions/match_regions_eez_rta_reconciled.csv") %>% 
  dplyr::select(SOVEREIGN1, signatories) %>% 
  filter(SOVEREIGN1 != "Republic of Mauritius") %>% 
  distinct()

dat_rta_list <- left_join(dat_rta_list, match_with_rta, by = "signatories") %>% 
  dplyr::select(rta_id, SOVEREIGN1)

dat_ecology_rta <- dat_ecology_rta %>% 
  filter(climates %in% c("gfdl-esm4 ssp126","ipsl-cm6a-lr ssp126")) %>% 
  group_by(scale, spatial_scale, regions) %>% 
  summarize(proba = median(proba, na.rm=T)) %>% 
  filter(spatial_scale!="global")

dat_ecology_cnt <- dat_ecology_rta %>% 
  filter(spatial_scale == "regions")
dat_ecology_rta <- dat_ecology_rta %>% 
  filter(spatial_scale == "rta")

dat_matrix <- left_join(dat_rta_list, dat_ecology_cnt, by = c("SOVEREIGN1" = "regions"),
                        relationship = "many-to-many") %>% 
  mutate(rta_id = as.character(rta_id))
dat_matrix <- left_join(dat_matrix, dat_ecology_rta, by = c("rta_id" = "regions","scale")) %>% 
  mutate(proba_ratio = proba.x/proba.y)

# figure
dat_rta$rta_id <- as.character(dat_rta$rta_id)
dat_fig4 <- dat_matrix %>% 
  left_join(dat_rta, "rta_id") %>% 
  filter(!is.na(scale)) %>% 
  mutate(region = ifelse(str_detect(region, ";")==TRUE, "Cross-regional",region),
         region = ifelse(region == "Commonwealth of Independent States (CIS), including certain associate and former member States", "Cross-regional", region)) %>% 
  mutate(proba_ratio = ifelse(proba_ratio>2, 2, proba_ratio)) %>% 
  arrange(region)

png(paste0("figures/manuscript_figures/figure_4_ssp126_si.png"),
    width = 12*200, height = 12*200, res = 200)
ggplot(dat_fig4, aes(y = proba_ratio, x = reorder(rta_acronym, desc(rta_acronym)))) +
  geom_boxplot(linetype = 2, lwd = 0.15, fill = "grey95") +
  geom_point(alpha = 0.5, size = 2) +
  geom_point(data = dat_fig4[dat_fig4$SOVEREIGN1 %in% c("United Kingdom","United States","Russia","China","Australia","Norway","Sweden","Spain"),], 
             aes(y = proba_ratio, x = reorder(rta_acronym, desc(rta_acronym)),
                 fill = SOVEREIGN1), shape = 21, size = 2, alpha = 0.8) +
  theme_bw() +
  facet_grid(region ~ scale,scale="free_y", space = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0,'lines'),
        legend.position = "bottom",
        text = element_text(size = 18)) +
  xlab("Plurilateral RTAs") + ylab("Exposure ratio") +
  geom_hline(aes(yintercept=1), linetype = "dashed") +
  coord_flip() + ylim(0,2) +
  guides(fill=guide_legend(title="")) +
  scale_fill_manual(values = c("coral","darkred","blue","cornflowerblue","darkgoldenrod1","purple","plum","darkorange"))
dev.off()


#### F. Figure 4 compensation SSP585 SI----
dat_ecology_rta <- left_join(dat, dat_mec, by = c("regions", "climates", "spatial_scale")) %>% 
  dplyr::select(regions, climates, spatial_scale,
                at_least_two_25_down, at_least_two_shocks_down) %>%
  pivot_longer(4:5, names_to = "type", values_to = "proba") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "abrupt", "gradual"),
         mechanism = ifelse(str_detect(type, "down")==TRUE, "synchrony", "stability"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation", mechanism))

# make matrix of country versus RTA
dat_rta <- read_csv("data/RTA WHO/RTA_filtered.csv")

dat_rta_list <- data.frame()
for(i in 1:nrow(dat_rta)){
  signatories <- strsplit(dat_rta$signatories[i],"; ")[[1]]
  xx <- dat_rta[i,]
  xx[1:length(signatories),] <- dat_rta[i,]
  xx$signatories <- signatories
  
  if(i == 1){dat_rta_list <- xx
  } else {dat_rta_list <- rbind(dat_rta_list, xx)}
  
  rm(xx, signatories)
}

match_with_rta <- read_csv("data/match_regions/match_regions_eez_rta_reconciled.csv") %>% 
  dplyr::select(SOVEREIGN1, signatories) %>% 
  filter(SOVEREIGN1 != "Republic of Mauritius") %>% 
  distinct()

dat_rta_list <- left_join(dat_rta_list, match_with_rta, by = "signatories") %>% 
  dplyr::select(rta_id, SOVEREIGN1)

dat_ecology_rta <- dat_ecology_rta %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  group_by(scale, spatial_scale, regions) %>% 
  summarize(proba = median(proba, na.rm=T)) %>% 
  filter(spatial_scale!="global")

dat_ecology_cnt <- dat_ecology_rta %>% 
  filter(spatial_scale == "regions")
dat_ecology_rta <- dat_ecology_rta %>% 
  filter(spatial_scale == "rta")

dat_matrix <- left_join(dat_rta_list, dat_ecology_cnt, by = c("SOVEREIGN1" = "regions"),
                        relationship = "many-to-many") %>% 
  mutate(rta_id = as.character(rta_id))
dat_matrix <- left_join(dat_matrix, dat_ecology_rta, by = c("rta_id" = "regions","scale")) %>% 
  mutate(proba_ratio = proba.x/proba.y)

# figure
dat_rta$rta_id <- as.character(dat_rta$rta_id)
dat_fig4 <- dat_matrix %>% 
  left_join(dat_rta, "rta_id") %>% 
  filter(!is.na(scale)) %>% 
  mutate(region = ifelse(str_detect(region, ";")==TRUE, "Cross-regional",region),
         region = ifelse(region == "Commonwealth of Independent States (CIS), including certain associate and former member States", "Cross-regional", region)) %>% 
  mutate(proba_ratio = ifelse(proba_ratio>2, 2, proba_ratio)) %>% 
  arrange(region)

png(paste0("figures/manuscript_figures/figure_4_compensation_si.png"),
    width = 12*200, height = 12*200, res = 200)
ggplot(dat_fig4, aes(y = proba_ratio, x = reorder(rta_acronym, desc(rta_acronym)))) +
  geom_boxplot(linetype = 2, lwd = 0.15, fill = "grey95") +
  geom_point(alpha = 0.5, size = 2) +
  geom_point(data = dat_fig4[dat_fig4$SOVEREIGN1 %in% c("United Kingdom","United States","Russia","China","Australia","Norway","Sweden","Spain"),], 
             aes(y = proba_ratio, x = reorder(rta_acronym, desc(rta_acronym)),
                 fill = SOVEREIGN1), shape = 21, size = 2, alpha = 0.8) +
  theme_bw() +
  facet_grid(region ~ scale, scale="free_y", space = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0,'lines'),
        legend.position = "bottom",
        text = element_text(size = 18)) +
  xlab("Plurilateral RTAs") + ylab("Exposure ratio") +
  geom_hline(aes(yintercept=1), linetype = "dashed") +
  coord_flip() + ylim(0,2) +
  guides(fill=guide_legend(title="")) +
  scale_fill_manual(values = c("coral","darkred","blue","cornflowerblue","darkgoldenrod1","purple","plum","darkorange"))
dev.off()


#### G. Maps of RTA and national exposure metrics for SSP 585 ----
dat_ecology_rta <- left_join(dat, dat_mec, by = c("regions", "climates", "spatial_scale")) %>% 
  dplyr::select(regions, climates, spatial_scale,
                at_least_two_change_25, at_least_two_shocks,
                at_least_two_25_down, at_least_one_25_up_down,
                at_least_two_shocks_down, at_least_one_shock_up_down) %>%
  mutate(at_least_one_25_up_down = 1-at_least_one_25_up_down,
         at_least_one_shock_up_down = 1-at_least_one_shock_up_down) %>%
  pivot_longer(4:9, names_to = "type", values_to = "proba") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "abrupt", "gradual"),
         mechanism = ifelse(str_detect(type, "down")==TRUE, "synchrony", "stability"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation", mechanism))

dat_rta <- read_csv("data/RTA WHO/RTA_filtered.csv")

dat_rta_list <- data.frame()
for(i in 1:nrow(dat_rta)){
  signatories <- strsplit(dat_rta$signatories[i],"; ")[[1]]
  xx <- dat_rta[i,]
  xx[1:length(signatories),] <- dat_rta[i,]
  xx$signatories <- signatories
  
  if(i == 1){dat_rta_list <- xx
  } else {dat_rta_list <- rbind(dat_rta_list, xx)}
  
  rm(xx, signatories)
}

match_with_rta <- read_csv("data/match_regions/match_regions_eez_rta_reconciled.csv") %>% 
  dplyr::select(SOVEREIGN1, signatories) %>% 
  filter(SOVEREIGN1 != "Republic of Mauritius") %>% 
  distinct()

dat_rta_list <- left_join(dat_rta_list, match_with_rta, by = "signatories") %>% 
  dplyr::select(rta_id, SOVEREIGN1)

dat_ecology_rta <- dat_ecology_rta %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  group_by(scale, spatial_scale, regions) %>% 
  summarize(proba = median(proba, na.rm=T)) %>% 
  filter(spatial_scale!="global")

dat_ecology_cnt <- dat_ecology_rta %>% 
  filter(spatial_scale == "regions")
dat_ecology_rta <- dat_ecology_rta %>% 
  filter(spatial_scale == "rta")

dat_matrix <- left_join(dat_rta_list, dat_ecology_cnt, by = c("SOVEREIGN1" = "regions"),
                        relationship = "many-to-many") %>% 
  mutate(rta_id = as.character(rta_id))
dat_matrix <- left_join(dat_matrix, dat_ecology_rta, by = c("rta_id" = "regions","scale")) %>% 
  mutate(proba_ratio = proba.x/proba.y)

dat_rta$rta_id <- as.character(dat_rta$rta_id)
dat_fig4 <- dat_matrix %>% 
  left_join(dat_rta, "rta_id") %>% 
  filter(!is.na(scale)) %>% 
  mutate(region = ifelse(str_detect(region, ";")==TRUE, "Cross-regional",region),
         region = ifelse(region == "Commonwealth of Independent States (CIS), including certain associate and former member States", "Cross-regional", region)) %>% 
  mutate(proba_ratio = ifelse(proba_ratio>2, 2, proba_ratio)) %>% 
  arrange(region) %>% 
  filter(rta_acronym %in% c("EU","AFTA","MERCOSUR","GSTP"))

regions <- st_read("data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp") %>% 
  filter(POL_TYPE != "Joint regime (EEZ)",
         is.na(SOVEREIGN2),
         SOVEREIGN1 != "Republic of Mauritius",
         UNION != "Antarctica") %>% 
  dplyr::select(UNION, SOVEREIGN1)

dat_maps <- left_join(regions, dat_fig4, by="SOVEREIGN1")

png(paste0("figures/manuscript_figures/figure_4_maps_si.png"),
    width = 7*200, height = 7*200, res = 200)
dat_maps %>% 
  st_as_sf() %>% 
  filter(!is.na(rta_acronym),
         !is.na(scale)) %>% 
  ggplot() + 
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill = `proba.y`), 
            alpha=0.5) +
  scale_colour_continuous_sequential(palette = "PuBuGn", expand = c(0,0),
                                     limits = c(0,1)) +
  geom_sf(data = regions, fill = "white", col = "grey") +
  geom_sf(aes(fill = `proba.x`)) +
  theme_bw() +
  scale_fill_continuous_sequential(palette = "PuBuGn", expand = c(0,0),
                                   limits = c(0,1)) +
  guides(fill = guide_legend(title = "Probability")) +
  theme(legend.position = "bottom") +
  facet_grid(rta_acronym ~ scale)
  
dev.off()
  

#### H. Summary statistics for results description ----

dat_fig4 %>% 
  group_by(rta_id, rta_acronym, scale) %>% 
  summarize(proba_ratio = median(proba_ratio, na.rm=T)) %>% 
  group_by(scale) %>% 
  summarize(proba_ratio = median(proba_ratio, na.rm=T))

dat_fig4 %>% 
  filter(SOVEREIGN1 == "United Kingdom") %>% 
  group_by(scale) %>% 
  summarize(proba_ratio = mean(proba_ratio, na.rm=T),
            proba.x = median(proba.x),
            proba.y = median(proba.y))

us <- dat_fig4 %>% 
  filter(SOVEREIGN1 == "United States")

russia <- dat_fig4 %>% 
  filter(SOVEREIGN1 == "Russia")

ukraine <- dat_fig4 %>% 
  filter(SOVEREIGN1 == "Ukraine")

aussie <- dat_fig4 %>% 
  filter(SOVEREIGN1 == "Australia")

eu <- dat_fig4 %>% 
  filter(rta_acronym == "EU")

median(eu$proba_ratio[eu$scale=="gradual"])
min(eu$proba_ratio[eu$scale=="gradual"])
max(eu$proba_ratio[eu$scale=="gradual"])
median(eu$proba_ratio[eu$scale=="abrupt"])
min(eu$proba_ratio[eu$scale=="abrupt"])
max(eu$proba_ratio[eu$scale=="abrupt"])

cnts <- dat_fig4 %>% 
  group_by(SOVEREIGN1, rta_acronym, scale) %>% 
  summarize(beneficial = ifelse(proba_ratio>1, "beneficial", "NA")) %>% 
  filter(beneficial=="beneficial") %>% 
  group_by(SOVEREIGN1, rta_acronym) %>% 
  summarize(beneficial = length(beneficial)) %>% 
  filter(beneficial==2)

gstp <- dat_fig4 %>%
  filter(rta_acronym == "GSTP")

median(gstp$proba.x[gstp$scale=="gradual"])
median(gstp$proba.y[gstp$scale=="gradual"])
median(gstp$proba_ratio[gstp$scale=="gradual"])
min(gstp$proba_ratio[gstp$scale=="gradual"])
max(gstp$proba_ratio[gstp$scale=="gradual"])
median(gstp$proba.x[gstp$scale=="abrupt"])
median(gstp$proba.y[gstp$scale=="abrupt"])
median(gstp$proba_ratio[gstp$scale=="abrupt"])
min(gstp$proba_ratio[gstp$scale=="abrupt"])
max(gstp$proba_ratio[gstp$scale=="abrupt"])

mercosur <- dat_fig4 %>% 
  filter(rta_acronym == "MERCOSUR",
         scale == "gradual")

sapta <- dat_fig4 %>% 
  filter(rta_acronym == "SAPTA",
         scale == "gradual")

eac <- dat_fig4 %>% 
  filter(rta_acronym == "EAC",
         scale == "abrupt")
