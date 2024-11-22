#### Figure 5
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
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")

regions <- st_read("data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp") %>% 
  filter(POL_TYPE != "Joint regime (EEZ)",
         is.na(SOVEREIGN2),
         SOVEREIGN1 != "Republic of Mauritius",
         UNION != "Antarctica") %>% 
  dplyr::select(UNION, TERRITORY1)


################################################################################
#### 5. EXPOSURE TO CLIMATE CS IMPACTS for CNT versus RTAs
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
dat_rta <- read_csv("data/RTA WTO/RTA_filtered.csv")

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


#### D. Figure 5 ----
dat_rta$rta_id <- as.character(dat_rta$rta_id)
dat_fig4 <- dat_matrix %>% 
  left_join(dat_rta, "rta_id") %>% 
  filter(!is.na(scale)) %>% 
  mutate(region = ifelse(str_detect(region, ";")==TRUE, "Cross-regional",region),
         region = ifelse(region == "Commonwealth of Independent States (CIS), including certain associate and former member States", "Cross-regional", region)) %>% 
  mutate(proba_ratio = ifelse(proba_ratio>2, 2, proba_ratio),
         rta_acronym = ifelse(rta_acronym == "USMCA/CUSMAT/T-MEC","USA/MEX/CAN",rta_acronym)) %>% 
  filter(rta_acronym %in% c("COMESA","CACM","GSTP","PAFTA","APTA","GUAM","EAEU","EU","GCC","USMCA/CUSMAT/T-MEC",
                            "SPARTECA","MERCOSUR","SAFTA"),
         scale == "abrupt") %>% 
  arrange(region)

png(paste0("figures/revised_figures/figure_5_a.png"),
    width = 6*200, height = 8*200, res = 200)
ggplot(dat_fig4, aes(y = proba_ratio, x = reorder(rta_acronym, desc(rta_acronym)))) +
  geom_boxplot(linetype = 1, lwd = 0.15, fill = "grey70") +
  geom_point(alpha = 0.5, size = 2) +
  geom_point(data = dat_fig4[dat_fig4$SOVEREIGN1 %in% c("United States","Russia","China","Guatemala","Spain","Ukraine"),], 
             aes(y = proba_ratio, x = reorder(rta_acronym, desc(rta_acronym)),
                 fill = SOVEREIGN1), shape = 21, size = 3, alpha = 0.8) +
  geom_text(data = dat_fig4[dat_fig4$SOVEREIGN1 %in% c("United States","Russia","China","Guatemala","Spain","Ukraine"),],
            aes(color = factor(SOVEREIGN1),
                label = SOVEREIGN1), size = 5, nudge_x = 0.35, nudge_y = 0.25) +
  theme_bw() +
  facet_grid(region ~ factor(scale, labels = "shocks"),
             scale="free_y", space = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0,'lines'),
        legend.position = "none",
        text = element_text(size = 18),
        panel.grid = element_blank()) +
  xlab("Plurilateral RTAs") + ylab("Exposure ratio") +
  geom_hline(aes(yintercept=1), linetype = "dashed") +
  coord_flip() + ylim(0,2) +
  guides(fill=guide_legend(title="")) +
  scale_fill_manual(values = c("orange","red","cornflowerblue","blue","darkorchid1","forestgreen")) +
  scale_color_manual(values = c("orange","red","cornflowerblue","blue","darkorchid1","forestgreen"))
dev.off()


dat_fig4b <- dat_matrix %>% 
  left_join(dat_rta, "rta_id") %>% 
  filter(!is.na(scale)) %>% 
  mutate(region = ifelse(str_detect(region, ";")==TRUE, "Cross-regional",region),
         region = ifelse(region == "Commonwealth of Independent States (CIS), including certain associate and former member States", "Cross-regional", region)) %>% 
  mutate(proba_ratio = ifelse(proba_ratio>2, 2, proba_ratio),
         rta_acronym = ifelse(rta_acronym == "USMCA/CUSMAT/T-MEC","USA/MEX/CAN",rta_acronym),
         scale = ifelse(scale == "abrupt","shocks",scale)) %>% 
  group_by(rta_acronym, scale) %>% 
  summarize(proba_ratio = median(proba_ratio, na.rm=T)) %>% 
  ungroup()

png(paste0("figures/revised_figures/figure_5_b.png"),
    width = 12*200, height = 3*200, res = 200)
ggplot(dat_fig4b, aes(x = scale, y = proba_ratio)) +
  geom_boxplot(linetype = 1, fill = "white") +
  geom_point(alpha = 0.5, size = 2) +
  geom_point(data = dat_fig4b[dat_fig4b$rta_acronym %in% c("EU","MERCOSUR"),], 
             aes(y = proba_ratio, x = scale, fill = rta_acronym), 
             shape = 23, size = 5, alpha = 0.6) +
  geom_text(data = dat_fig4b[dat_fig4b$rta_acronym %in% c("EU","MERCOSUR"),],
            aes(color = factor(rta_acronym),label = rta_acronym), 
            size = 5, nudge_x = 0.2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0,'lines'),
        legend.position = "none",
        text = element_text(size = 18),
        panel.grid = element_blank()) +
  xlab("Exposure type") + ylab("Exposure ratio") +
  geom_hline(aes(yintercept=1), linetype = "dashed") +
  coord_flip() + ylim(0,2) +
  guides(fill=guide_legend(title="")) +
  scale_fill_manual(values = c("red","cornflowerblue")) +
  scale_color_manual(values = c("red","cornflowerblue"))
dev.off()

# RTA-specific maps
dat_eu <- dat_matrix %>% 
  left_join(dat_rta, "rta_id") %>% 
  filter(!is.na(scale)) %>% 
  mutate(region = ifelse(str_detect(region, ";")==TRUE, "Cross-regional",region),
         region = ifelse(region == "Commonwealth of Independent States (CIS), including certain associate and former member States", "Cross-regional", region)) %>% 
  mutate(proba_ratio = ifelse(proba_ratio>2, 2, proba_ratio),
         rta_acronym = ifelse(rta_acronym == "USMCA/CUSMAT/T-MEC","USA/MEX/CAN",rta_acronym)) %>% 
  filter(rta_acronym %in% c("EU"),
         scale == "abrupt") %>% 
  arrange(region)
dat_eu_map <- left_join(regions, dat_eu, by=c("TERRITORY1"="SOVEREIGN1"))

png(paste0("figures/revised_figures/figure_5_c.png"),
    width = 8*200, height = 6*200, res = 200)
dat_eu_map %>% 
  st_as_sf() %>% 
  filter(!is.na(proba_ratio)) %>% 
  ggplot() + 
  geom_sf(data = world[world$admin %in% c("Netherlands","Finland","Andorra","Albania","France","Austria",
                                               "Belgium","Bulgaria","Bosnia and Herzegovina",
                                               "Belarus","Switzerland","Cyprus","Czech Republic",
                                               "Germany","Denmark","Spain","Estonia","Greece",
                                               "Croatia","Hungary","Ireland","Italy","Liechtenstein",
                                               "Lithuania","Luxembourg","Latvia","Monaco","Moldova",
                                               "Malta","Montenegro","Norway","United Kingdom","Portugal",
                                               "Poland","Romania","Slovakia","Slovenia","Sweden","Ukraine",
                                               "Vatican","Iceland","Greenland","Republic of Serbia","Macedonia","Kosovo",
                                          "Russia","Morocco","Tunisia","Algeria","Syria","Egypt","Libya","Turkey"),],
          fill = "white") +
  geom_sf(aes(fill = proba_ratio), alpha = 0.75) +
  coord_sf(xlim = c(-30,40), ylim = c(34,69)) +
  theme_bw() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "lemonchiffon", expand = c(0,0), midpoint = 1,
                       limits = c(0,2), name = "") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 22))
dev.off()


dat_mer <- dat_matrix %>% 
  left_join(dat_rta, "rta_id") %>% 
  filter(!is.na(scale)) %>% 
  mutate(region = ifelse(str_detect(region, ";")==TRUE, "Cross-regional",region),
         region = ifelse(region == "Commonwealth of Independent States (CIS), including certain associate and former member States", "Cross-regional", region)) %>% 
  mutate(proba_ratio = ifelse(proba_ratio>2, 2, proba_ratio),
         rta_acronym = ifelse(rta_acronym == "USMCA/CUSMAT/T-MEC","USA/MEX/CAN",rta_acronym)) %>% 
  filter(rta_acronym %in% c("MERCOSUR"),
         scale == "abrupt") %>% 
  arrange(region)
dat_mer_map <- left_join(regions, dat_mer, by=c("TERRITORY1"="SOVEREIGN1"))


png(paste0("figures/revised_figures/figure_5_d.png"),
    width = 8*200, height = 6*200, res = 200)
dat_mer_map %>% 
  st_as_sf() %>% 
  filter(!is.na(proba_ratio)) %>% 
  ggplot() + 
  geom_sf(data = world[world$admin %in% c("Uruguay","Paraguay","Argentina","Brazil","Bolivia","Chile","Peru",
                                          "Colombia","Venezuela","Guyana","Suriname","French Guiana","Ecuador",
                                          "Falkland Islands","France","Panama","Costa Rica","Nicaragua"),],
          fill = "white") +
  geom_sf(aes(fill = proba_ratio), alpha = 0.75) +
  coord_sf(xlim = c(-90,-15), ylim = c(-56,5)) +
  theme_bw() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "lemonchiffon", expand = c(0,0), midpoint = 1,
                       limits = c(0,2), name = "") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 22))
dev.off()
  arrange(region)