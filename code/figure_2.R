#### Figure 2 and alternative versions
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
library(ggpubr)


################################################################################
#### 2. ARE COUNTRIES EXPERIENCING SYNCHRONY AND/OR COMPENSATION?
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
png(paste0("figures/manuscript_figures/figure_2.png"),
    width = 6*200, height = 7*200, res = 200)
regions_dat %>% 
  rename(`Gradual compensation` = at_least_one_25_up_down,
         `Gradual synchrony` = at_least_two_25_down,
         `Abrupt compensation` = at_least_one_shock_up_down,
         `Abrupt synchrony` = at_least_two_shocks_down,
         `Gradual change` = at_least_two_change_25,
         `Abrupt change` = at_least_two_shocks) %>% 
  pivot_longer(3:8, names_to = "mechanism", values_to = "probability") %>% 
  ggplot() + geom_sf(aes(fill = probability)) +
  facet_wrap(~ factor(mechanism, 
                      levels = c("Gradual change","Abrupt change",
                                 "Gradual compensation","Abrupt compensation",
                                 "Gradual synchrony","Abrupt synchrony"))
             , ncol = 2) + 
  theme_bw() +
  scale_fill_continuous_sequential(palette = "PuBuGn", expand = c(0,0)) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.25,'lines'))
dev.off()

# latitudinal distributions
coords <- regions_dat %>% 
  st_centroid() %>% 
  st_coordinates()

densities_dat <- cbind(regions_dat, coords) %>% 
  st_drop_geometry() %>% 
  pivot_longer(c(3:8), values_to = "proba", names_to = "type") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "shock", "gradual"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation","change"),
         mechanism = ifelse(str_detect(type, "two_shocks_down")==TRUE,"synchrony",mechanism),
         mechanism = ifelse(str_detect(type, "two_25_down")==TRUE,"synchrony",mechanism))

# plot latitudinal gradients
gradients <- ggplot(densities_dat, aes(y = proba, x = Y, color = scale)) +
  geom_smooth(span = 0.35, se = FALSE) +
  theme_bw() +
  facet_wrap(~mechanism, scales = "free", nrow = 1) + coord_flip() +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.75,'lines'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 14),
        legend.position = "bottom") +
  ylab("Probability") + xlab("Latitude") +
  scale_linetype_discrete(name="") +
  scale_colour_manual(values = c("grey40","grey"), name="") +
  scale_x_continuous(breaks = c(-60,-30,0,30,60), limits = c(-75,75)) 

png(paste0("figures/manuscript_figures/figure_2_gradients.png"),
    width = 7*200, height = 7*200, res = 200)
gradients
dev.off()


#### C. figure 2 maps for ssp1.26 ----
dat_mec_ssp126 <- dat_mec %>% 
  filter(climates %in% c("gfdl-esm4 ssp126","ipsl-cm6a-lr ssp126"),
         time_window==30,
         regions != "global") %>% 
  group_by(regions) %>% 
  summarize(at_least_one_25_up_down = mean(at_least_one_25_up_down, na.rm=T),
            at_least_two_25_down = mean(at_least_two_25_down, na.rm=T),
            at_least_one_shock_up_down = mean(at_least_one_shock_up_down, na.rm=T),
            at_least_two_shocks_down = mean(at_least_two_shocks_down, na.rm=T),
            at_least_two_change_25 = mean(at_least_two_change_25, na.rm = T),
            at_least_two_shocks = mean(at_least_two_shocks, na.rm = T))

regions_dat <- left_join(regions, dat_mec_ssp126, by = c("SOVEREIGN1" = "regions")) 

# average climate models for ssp1.26 for SI
png(paste0("figures/manuscript_figures/figure_2_ssp126_si.png"),
    width = 6*200, height = 7*200, res = 200)
regions_dat %>% 
  rename(`Gradual compensation` = at_least_one_25_up_down,
         `Gradual synchrony` = at_least_two_25_down,
         `Abrupt compensation` = at_least_one_shock_up_down,
         `Abrupt synchrony` = at_least_two_shocks_down,
         `Gradual change` = at_least_two_change_25,
         `Abrupt change` = at_least_two_shocks) %>% 
  pivot_longer(3:8, names_to = "mechanism", values_to = "probability") %>% 
  ggplot() + geom_sf(aes(fill = probability)) +
  facet_wrap(~ factor(mechanism, 
                      levels = c("Gradual change","Abrupt change",
                                 "Gradual compensation","Abrupt compensation",
                                 "Gradual synchrony","Abrupt synchrony"))
             , ncol = 2) + 
  theme_bw() +
  scale_fill_continuous_sequential(palette = "PuBuGn", expand = c(0,0)) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.25,'lines'))
dev.off()


#### D. Summary statistics for the results description ----
# 195 countries
View(dat_mec_ssp585 %>% 
  pivot_longer(2:7, names_to = "type", values_to = "probas") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "shock", "gradual"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation","change"),
         mechanism = ifelse(str_detect(type, "two_shocks_down")==TRUE,"synchrony",mechanism),
         mechanism = ifelse(str_detect(type, "two_25_down")==TRUE,"synchrony",mechanism)) %>% 
  group_by(type, mechanism, scale) %>% 
  summarize(proba_higher_0.5 = length(regions[probas>0.5])/195*100,
            proba_higher_0.75 = length(regions[probas>0.75])/195*100))

dat_mec_ssp585 %>% 
  pivot_longer(2:7, names_to = "type", values_to = "probas") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "shock", "gradual"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation","change"),
         mechanism = ifelse(str_detect(type, "two_shocks_down")==TRUE,"synchrony",mechanism),
         mechanism = ifelse(str_detect(type, "two_25_down")==TRUE,"synchrony",mechanism)) %>% 
  dplyr::select(-type) %>% 
  pivot_wider(names_from = scale, values_from = probas) %>% 
  group_by(mechanism) %>% 
  summarize(proba_higher_0.5 = length(regions[gradual>0.5 & shock>0.5])/195*100,
            proba_higher_0.75 = length(regions[gradual>0.75 & shock>0.75])/195*100)

# countries with higher synchronies across scales
dat_mec_ssp585 %>% 
  pivot_longer(2:7, names_to = "type", values_to = "probas") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "shock", "gradual"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation","change"),
         mechanism = ifelse(str_detect(type, "two_shocks_down")==TRUE,"synchrony",mechanism),
         mechanism = ifelse(str_detect(type, "two_25_down")==TRUE,"synchrony",mechanism)) %>% 
  filter(mechanism == "synchrony") %>% 
  dplyr::select(-type) %>% 
  pivot_wider(names_from = scale, values_from = probas) %>% 
  filter(gradual>0.5 & shock>0.5) %>% 
  pull(regions)

dat_mec_ssp585 %>% 
  pivot_longer(2:7, names_to = "type", values_to = "probas") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "shock", "gradual"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation","change"),
         mechanism = ifelse(str_detect(type, "two_shocks_down")==TRUE,"synchrony",mechanism),
         mechanism = ifelse(str_detect(type, "two_25_down")==TRUE,"synchrony",mechanism)) %>% 
  dplyr::select(-type) %>% 
  filter(mechanism!="change") %>% 
  pivot_wider(names_from = mechanism, values_from = probas) %>% 
  group_by(scale) %>% 
  summarize(proba_higher_0.5 = length(regions[synchrony>0.5 & compensation>0.5])/195*100,
            proba_higher_0.75 = length(regions[synchrony>0.75 & compensation>0.75])/195*100)


#### E. Relationship between compensation, synchrony, and overall change ----
ggplot(dat_mec_ssp585, aes(x = at_least_two_change_25, y = at_least_two_25_down)) +
  geom_point() + theme_bw() +
  geom_abline(aes(intercept = 0, slope = 1))

ggplot(dat_mec_ssp585, aes(x = at_least_two_change_25, y = at_least_one_25_up_down)) +
  geom_point() + theme_bw() +
  geom_abline(aes(intercept = 0, slope = 1))

ggplot(dat_mec_ssp585, aes(x = at_least_two_shocks, y = at_least_two_shocks_down)) +
  geom_point() + theme_bw() +
  geom_abline(aes(intercept = 0, slope = 1))

ggplot(dat_mec_ssp585, aes(x = at_least_two_shocks, y = at_least_one_shock_up_down)) +
  geom_point() + theme_bw() +
  geom_abline(aes(intercept = 0, slope = 1))

ggplot(dat_mec_ssp126, aes(x = at_least_two_change_25, y = at_least_two_25_down)) +
  geom_point() + theme_bw() +
  geom_abline(aes(intercept = 0, slope = 1))

ggplot(dat_mec_ssp126, aes(x = at_least_two_change_25, y = at_least_one_25_up_down)) +
  geom_point() + theme_bw() +
  geom_abline(aes(intercept = 0, slope = 1))

ggplot(dat_mec_ssp126, aes(x = at_least_two_shocks, y = at_least_two_shocks_down)) +
  geom_point() + theme_bw() +
  geom_abline(aes(intercept = 0, slope = 1))

ggplot(dat_mec_ssp126, aes(x = at_least_two_shocks, y = at_least_one_shock_up_down)) +
  geom_point() + theme_bw() +
  geom_abline(aes(intercept = 0, slope = 1))


#### F. Compare values distributions ----
dat_mec_ssp585 <- dat_mec_ssp585 %>% 
  pivot_longer(c(2:7), values_to = "proba", names_to = "type") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "shock", "gradual"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation","change"),
         mechanism = ifelse(str_detect(type, "two_shocks_down")==TRUE,"synchrony",mechanism),
         mechanism = ifelse(str_detect(type, "two_25_down")==TRUE,"synchrony",mechanism))

# medians
medians <- dat_mec_ssp585 %>% 
  group_by(type, mechanism, scale) %>% 
  summarize(proba = median(proba))

# check distributions of mechanisms
ggplot(dat_mec_ssp585, aes(x = type, y = proba, fill = mechanism)) + geom_boxplot()

# plot densities
ggplot(dat_mec_ssp585, aes(x = proba, fill = mechanism, linetype = scale)) + geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~mechanism) +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.75,'lines'),
        strip.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 14)) +
  xlab("Probability") + ylab("Density") +
  scale_linetype_discrete(name="") +
  geom_vline(data = medians, aes(xintercept = proba, linetype = scale))

# check normality of data
ggqqplot(dat_mec_ssp585[dat_mec_ssp585$mechanism=="compensation" & dat_mec_ssp585$scale=="gradual",]$proba)
ggqqplot(dat_mec_ssp585[dat_mec_ssp585$mechanism=="compensation" & dat_mec_ssp585$scale=="shock",]$proba)
ggqqplot(dat_mec_ssp585[dat_mec_ssp585$mechanism=="synchrony" & dat_mec_ssp585$scale=="gradual",]$proba)
ggqqplot(dat_mec_ssp585[dat_mec_ssp585$mechanism=="synchrony" & dat_mec_ssp585$scale=="shock",]$proba)
ggqqplot(dat_mec_ssp585[dat_mec_ssp585$mechanism=="change" & dat_mec_ssp585$scale=="gradual",]$proba)
ggqqplot(dat_mec_ssp585[dat_mec_ssp585$mechanism=="change" & dat_mec_ssp585$scale=="shock",]$proba)
# there are always data points outside of the grey area

shapiro.test(dat_mec_ssp585[dat_mec_ssp585$mechanism=="compensation" & dat_mec_ssp585$scale=="gradual",]$proba)
shapiro.test(dat_mec_ssp585[dat_mec_ssp585$mechanism=="compensation" & dat_mec_ssp585$scale=="shock",]$proba)
shapiro.test(dat_mec_ssp585[dat_mec_ssp585$mechanism=="synchrony" & dat_mec_ssp585$scale=="gradual",]$proba)
shapiro.test(dat_mec_ssp585[dat_mec_ssp585$mechanism=="synchrony" & dat_mec_ssp585$scale=="shock",]$proba)
shapiro.test(dat_mec_ssp585[dat_mec_ssp585$mechanism=="synchrony" & dat_mec_ssp585$scale=="shock",]$proba)
shapiro.test(dat_mec_ssp585[dat_mec_ssp585$mechanism=="change" & dat_mec_ssp585$scale=="shock",]$proba)
# all shapiro tests have p values lower than 0.05, so the distribution is significantly different from normal distribution

# kruskal-wallis test
# non-parametric one-way anova test
# http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
# https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
dat_mec_ssp585 <- dat_mec_ssp585 %>% filter(mechanism!="change")
kruskal.test(proba ~ type, data = dat_mec_ssp585)
pairwise.wilcox.test(dat_mec_ssp585$proba, dat_mec_ssp585$type)
dat_mec_ssp585 %>% kruskal_effsize(proba ~ type)
pwc <- dat_mec_ssp585 %>% 
  dunn_test(proba ~ type, p.adjust.method = "bonferroni")
pwc2 <- dat_mec_ssp585 %>% 
  wilcox_test(proba ~ type, p.adjust.method = "bonferroni")

# report results
res.kruskal <- dat_mec_ssp585 %>% kruskal_test(proba ~ type)
pwc <- pwc %>% add_xy_position(x = "type")
ggboxplot(dat_mec_ssp585, x = "type", y = "proba") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(subtitle = get_test_label(res.kruskal, detailed = TRUE),
       caption = get_pwc_label(pwc))


