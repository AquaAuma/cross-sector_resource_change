#### Figure 3 and alternative versions
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
#### 3. SUMMARIZING ECOLOGICAL METRICS TO FIND WORSE-CASE SCENARIOS
################################################################################
#### A. load data for short- and long-term changes ----
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
  mutate(regions = "global") %>% 
  filter(time_window == 30) %>% 
  dplyr::select(names(change_probas_r_cs))

# rbind
shock_probas <- rbind(shock_probas_g_cs, shock_probas_r_cs)
long_term <- rbind(change_probas_g_cs, change_probas_r_cs) %>% 
  pivot_wider(names_from = "type", values_from = "probas")
dat <- left_join(long_term, shock_probas, by = c("climates", "regions"))

#### B. load data for compensatory and synchronous mechanisms ----
# countries
shock_probas_r_cs_mec <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas_spans_mechanisms.csv") %>% 
  filter(spans == "0.75") %>% 
  dplyr::select(-spans, -at_least_two_shocks_up, -at_least_three_shocks_up)
change_probas_r_cs_mec <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas_mechanisms.csv") %>% 
  filter(time_window == 30) %>% 
  dplyr::select(-spatial_scale, -time_window)

# global
shock_probas_g_cs_mec <- read_csv("data/short-term_change/global_shocks_1985-2015_probas_spans_mechanisms.csv") %>% 
  mutate(regions = "global") %>% 
  filter(spans == "0.75") %>% 
  dplyr::select(names(shock_probas_r_cs_mec))
change_probas_g_cs_mec <- read_csv("data/long-term_change/global_long-term_change_1985-2015_probas_mechanisms.csv") %>% 
  mutate(regions = "global") %>% 
  filter(time_window == 30) %>% 
  dplyr::select(names(change_probas_r_cs_mec))

# rbind
shock_probas <- rbind(shock_probas_g_cs_mec, shock_probas_r_cs_mec)
long_term <- rbind(change_probas_g_cs_mec, change_probas_r_cs_mec) %>% 
  pivot_wider(names_from = "type", values_from = "probas")
dat_mec <- left_join(long_term, shock_probas, by = c("climates", "regions"))

#### C. Data across metrics ----
# cbind both datasets
dat_ecology <- left_join(dat, dat_mec, by = c("regions", "climates")) %>% 
  dplyr::select(regions, climates,
                at_least_two_change_25, at_least_two_shocks,
                at_least_two_25_down, at_least_one_25_up_down,
                at_least_two_shocks_down, at_least_one_shock_up_down) %>%
  mutate(at_least_one_25_up_down = 1-at_least_one_25_up_down,
         at_least_one_shock_up_down = 1-at_least_one_shock_up_down) %>%
  pivot_longer(3:8, names_to = "type", values_to = "proba") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "abrupt", "gradual"),
         mechanism = ifelse(str_detect(type, "down")==TRUE, "synchrony", "change"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation", mechanism))


#### D. Figure 3 ----
plot_abrupt <- dat_ecology %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585"),
         regions != "Antarctica",
         regions %in% c("Afghanistan","Algeria","Angola",
                        "Argentina","Australia","Austria" ,
                        "Azerbaijan","Bahamas","Bahrain" ,"Bangladesh" ,
                        "Belarus", "Belgium","Belize","Benin" ,"Bhutan",
                        "Bolivia" ,"Botswana","Brazil", "Bulgaria",
                        "Burundi","Cambodia", "Cameroon","Canada",
                        "Central African Republic","Chad","Chile",
                        "China", "Colombia", "Costa Rica", "Croatia",
                        "Denmark", "Ecuador" , "Egypt","Ethiopia","Finland",
                        "France", "Gabon","Gambia","Germany","Greece","Guatemala" ,
                        "Guinea","Iceland","India","Indonesia","Iran",
                        "Iraq","Ireland","Israel","Italy","Ivory Coast",                        
                        "Japan","Kazakhstan", "Kenya", "Kuwait",                          
                        "Macedonia","Madagascar","Malaysia", "Mali",                            
                        "Mauritania","Mexico", "Mongolia", "Morocco","Mozambique",
                        "Namibia","Netherlands", "New Zealand","Niger","Nigeria","Norway",                            
                        "Pakistan","Panama" ,"Papua New Guinea", "Peru",                            
                        "Philippines","Poland","Portugal","Republic of the Congo","Romania",                         
                        "Russia","Rwanda","Senegal",                 
                        "South Africa", "Spain",  "Sri Lanka","Sweden" ,"Switzerland" ,                    
                        "Syria" ,"Tanzania","Thailand","Turkey", "Uganda","Ukraine",
                        "United Arab Emirates","United Kingdom","United States","Venezuela","Yemen" ),
         scale == "abrupt") %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = function(x) median(x, na.rm=TRUE)), x = proba)) + geom_boxplot() +
  geom_point(aes(color = mechanism), fill = "grey", stroke = 1, alpha = 0.6) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = c("grey40","forestgreen","deepskyblue4")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "bottom") +
  ylab("") + xlab("Cross-sector exposure probability") + ggtitle("Abrupt")

plot_gradual <- dat_ecology %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585"),
         regions != "Antarctica",
         regions %in% c("Afghanistan","Algeria","Angola",
                        "Argentina","Australia","Austria" ,
                        "Azerbaijan","Bahamas","Bahrain" ,"Bangladesh" ,
                        "Belarus", "Belgium","Belize","Benin" ,"Bhutan",
                        "Bolivia" ,"Botswana","Brazil", "Bulgaria",
                        "Burundi","Cambodia", "Cameroon","Canada",
                        "Central African Republic","Chad","Chile",
                        "China", "Colombia", "Costa Rica", "Croatia",
                        "Denmark", "Ecuador" , "Egypt","Ethiopia","Finland",
                        "France", "Gabon","Gambia","Germany","Greece","Guatemala" ,
                        "Guinea","Iceland","India","Indonesia","Iran",
                        "Iraq","Ireland","Israel","Italy","Ivory Coast",                        
                        "Japan","Kazakhstan", "Kenya", "Kuwait",                          
                        "Macedonia","Madagascar","Malaysia", "Mali",                            
                        "Mauritania","Mexico", "Mongolia", "Morocco","Mozambique",
                        "Namibia","Netherlands", "New Zealand","Niger","Nigeria","Norway",                            
                        "Pakistan","Panama" ,"Papua New Guinea", "Peru",                            
                        "Philippines","Poland","Portugal","Republic of the Congo","Romania",                         
                        "Russia","Rwanda","Senegal",                 
                        "South Africa", "Spain",  "Sri Lanka","Sweden" ,"Switzerland" ,                    
                        "Syria" ,"Tanzania","Thailand","Turkey", "Uganda","Ukraine",
                        "United Arab Emirates","United Kingdom","United States","Venezuela","Yemen" ),
         scale == "gradual") %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = median), x = proba)) + geom_boxplot() +
  geom_point(aes(color = mechanism), fill = "grey", stroke = 1, alpha = 0.6) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = c("grey40","forestgreen","deepskyblue4")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.background=element_blank()) +
  ylab("Regions") + xlab("Cross-sector exposure probability") + ggtitle("Gradual")

png(paste0("figures/manuscript_figures/figure_3.png"),
    width = 11*200, height = 10*200, res = 200)
grid.arrange(plot_gradual, plot_abrupt, nrow = 1)
dev.off()


#### E. Figure 3 for ssp 1.26 ----

plot_abrupt <- dat_ecology %>% 
  filter(climates %in% c("gfdl-esm4 ssp126","ipsl-cm6a-lr ssp126"),
         regions != "Antarctica",
         regions %in% c("Afghanistan","Algeria","Angola",
                        "Argentina","Australia","Austria" ,
                        "Azerbaijan","Bahamas","Bahrain" ,"Bangladesh" ,
                        "Belarus", "Belgium","Belize","Benin" ,"Bhutan",
                        "Bolivia" ,"Botswana","Brazil", "Bulgaria",
                        "Burundi","Cambodia", "Cameroon","Canada",
                        "Central African Republic","Chad","Chile",
                        "China", "Colombia", "Costa Rica", "Croatia",
                        "Denmark", "Ecuador" , "Egypt","Ethiopia","Finland",
                        "France", "Gabon","Gambia","Germany","Greece","Guatemala" ,
                        "Guinea","Iceland","India","Indonesia","Iran",
                        "Iraq","Ireland","Israel","Italy","Ivory Coast",                        
                        "Japan","Kazakhstan", "Kenya", "Kuwait",                          
                        "Macedonia","Madagascar","Malaysia", "Mali",                            
                        "Mauritania","Mexico", "Mongolia", "Morocco","Mozambique",
                        "Namibia","Netherlands", "New Zealand","Niger","Nigeria","Norway",                            
                        "Pakistan","Panama" ,"Papua New Guinea", "Peru",                            
                        "Philippines","Poland","Portugal","Republic of the Congo","Romania",                         
                        "Russia","Rwanda","Senegal",                 
                        "South Africa", "Spain",  "Sri Lanka","Sweden" ,"Switzerland" ,                    
                        "Syria" ,"Tanzania","Thailand","Turkey", "Uganda","Ukraine",
                        "United Arab Emirates","United Kingdom","United States","Venezuela","Yemen" ),
         scale == "abrupt") %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = function(x) median(x, na.rm=T)), x = proba)) + geom_boxplot() +
  geom_point(aes(color = mechanism), fill = "grey", stroke = 1, alpha = 0.6) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = c("grey40","forestgreen","deepskyblue4")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "bottom") +
  ylab("") + xlab("Cross-sector exposure probability") + ggtitle("Abrupt")

plot_gradual <- dat_ecology %>% 
  filter(climates %in% c("gfdl-esm4 ssp126","ipsl-cm6a-lr ssp126"),
         regions != "Antarctica",
         regions %in% c("Afghanistan","Algeria","Angola",
                        "Argentina","Australia","Austria" ,
                        "Azerbaijan","Bahamas","Bahrain" ,"Bangladesh" ,
                        "Belarus", "Belgium","Belize","Benin" ,"Bhutan",
                        "Bolivia" ,"Botswana","Brazil", "Bulgaria",
                        "Burundi","Cambodia", "Cameroon","Canada",
                        "Central African Republic","Chad","Chile",
                        "China", "Colombia", "Costa Rica", "Croatia",
                        "Denmark", "Ecuador" , "Egypt","Ethiopia","Finland",
                        "France", "Gabon","Gambia","Germany","Greece","Guatemala" ,
                        "Guinea","Iceland","India","Indonesia","Iran",
                        "Iraq","Ireland","Israel","Italy","Ivory Coast",                        
                        "Japan","Kazakhstan", "Kenya", "Kuwait",                          
                        "Macedonia","Madagascar","Malaysia", "Mali",                            
                        "Mauritania","Mexico", "Mongolia", "Morocco","Mozambique",
                        "Namibia","Netherlands", "New Zealand","Niger","Nigeria","Norway",                            
                        "Pakistan","Panama" ,"Papua New Guinea", "Peru",                            
                        "Philippines","Poland","Portugal","Republic of the Congo","Romania",                         
                        "Russia","Rwanda","Senegal",                 
                        "South Africa", "Spain",  "Sri Lanka","Sweden" ,"Switzerland" ,                    
                        "Syria" ,"Tanzania","Thailand","Turkey", "Uganda","Ukraine",
                        "United Arab Emirates","United Kingdom","United States","Venezuela","Yemen" ),
         scale == "gradual") %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = median), x = proba)) + geom_boxplot() +
  geom_point(aes(color = mechanism), fill = "grey", stroke = 1, alpha = 0.6) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = c("grey40","forestgreen","deepskyblue4")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.background=element_blank()) +
  ylab("Regions") + xlab("Cross-sector exposure probability") + ggtitle("Gradual")

png(paste0("figures/manuscript_figures/figure_3_ssp126_si.png"),
    width = 11*200, height = 10*200, res = 200)
grid.arrange(plot_gradual, plot_abrupt, nrow = 1)
dev.off()


#### F. Summary statistics for the results description ----
View(dat_ecology %>% 
  filter(!regions %in% c("Antarctica","global"),
         climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  group_by(scale, regions) %>% 
  summarize(overall = median(proba, na.rm=T)) %>% 
  pivot_wider(names_from = scale, values_from = overall))

dat_ecology %>% 
  filter(!regions %in% c("Antarctica","global"),
         climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  group_by(scale, regions) %>% 
  summarize(overall = median(proba, na.rm=T)) %>% 
  group_by(scale) %>% 
  summarize(overall = median(overall, na.rm=T))
  
dat_ecology %>% 
  filter(!regions %in% c("Antarctica","global"),
         climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  group_by(scale, regions) %>% 
  summarize(overall = median(proba, na.rm=T)) %>% 
  group_by() %>% 
  summarize(overall = median(overall, na.rm=T))

dat_ecology %>% 
  filter(!regions %in% c("Antarctica","global"),
         climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  group_by(scale, regions) %>% 
  summarize(overall = median(proba, na.rm=T)) %>% 
  group_by(scale) %>% 
  summarize(overall_0.25 = length(regions[overall>0.25])/195*100,
            overall_0.5 = length(regions[overall>0.5])/195*100,
            overall_0.75 = length(regions[overall>0.75])/195*100)

dat_ecology %>% 
  filter(!regions %in% c("Antarctica","global"),
         climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  group_by(scale, regions) %>% 
  summarize(overall = median(proba, na.rm=T)) %>% 
  pivot_wider(names_from = scale, values_from = overall) %>% 
  summarize(overall_0.25 = length(regions[abrupt>0.25 & gradual>0.25])/195*100,
            overall_0.5 = length(regions[abrupt>0.5 & gradual>0.5])/195*100,
            overall_0.75 = length(regions[abrupt>0.75 & gradual>0.75])/195*100)

# most exposed countries to both impacts
dat_ecology %>% 
  filter(!regions %in% c("Antarctica","global"),
         climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  group_by(scale, regions) %>% 
  summarize(overall = median(proba, na.rm=T)) %>% 
  pivot_wider(names_from = scale, values_from = overall) %>% 
  filter(gradual>0.75, abrupt>0.75) %>% 
  pull(regions)

# least exposed countries
dat_ecology %>% 
  filter(!regions %in% c("Antarctica","global"),
         climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  group_by(scale, regions) %>% 
  summarize(overall = median(proba, na.rm=T)) %>% 
  pivot_wider(names_from = scale, values_from = overall) %>% 
  filter(gradual<0.25, abrupt<0.25) %>% 
  pull(regions)

dat_ecology %>% 
  filter(!regions %in% c("Antarctica","global"),
         climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  group_by(scale, regions) %>% 
  summarize(overall = median(proba, na.rm=T)) %>% 
  pivot_wider(names_from = scale, values_from = overall) %>% 
  arrange(desc(abrupt)) %>% 
  tibble::rownames_to_column("ranking_abrupt") %>% 
  arrange(desc(gradual)) %>% 
  tibble::rownames_to_column("ranking_gradual") %>% 
  arrange(regions) %>% 
  filter(regions %in% c("Russia","United States","France","Spain",
                        "China","Ukraine","Madagascar","Sweden",
                        "Ecuador","Kuwait","Ethiopia","Egypt","Pakistan",
                        "Democratic Republic of the Congo","Peru"))

dat_ecology %>% 
  filter(!regions %in% c("Antarctica","global"),
         climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  group_by(scale, regions) %>% 
  summarize(overall = median(proba, na.rm=T),
            min = min(proba, na.rm=T),
            max = max(proba, na.rm=T)) %>% 
  group_by(scale) %>% 
  summarize(median = median(overall, na.rm=T),
            min = min(overall, na.rm=T),
            max = max(overall, na.rm=T))

