#### Figure 2 
#### Coding: Aurore A. Maureaud, December 2024

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
library(ggpattern)

regions <- st_read("data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp") %>% 
  filter(POL_TYPE != "Joint regime (EEZ)",
         is.na(SOVEREIGN2),
         SOVEREIGN1 != "Republic of Mauritius",
         UNION != "Antarctica") %>% 
  dplyr::select(SOVEREIGN1, ISO_SOV1) %>% 
  st_drop_geometry() %>% 
  distinct()


################################################################################
#### 2. SSP effect on the metrics of gradual and sudden change
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


#### B. boxplot ----
dat <- dat %>% 
  select(regions, climates, at_least_two_shocks, at_least_two_change_25) %>% 
  mutate(ssp = ifelse(str_detect(climates, "ssp126"),"1.26","5.85"),
         model = ifelse(str_detect(climates, "gfdl-esm4"),"gfdl-esm4","ipsl-cm6a-lr")) %>% 
  group_by(regions, ssp) %>% 
  summarize(shock = mean(at_least_two_shocks, na.rm=T),
            gradual = mean(at_least_two_change_25, na.rm=T)) %>% 
  pivot_longer(3:4, names_to = "type", values_to = "proba") %>% 
  data.frame()

# boxplot
png(paste0("figures/figure_2a.png"),
    width = 4*200, height = 6*200, res = 200)
ggplot(dat, aes(x = factor(type, levels = c("shock","gradual"), labels = c("shock","gradual")), y = proba, pattern = ssp)) +
  geom_boxplot_pattern(position = position_dodge(width = 0.8),
                       aes(pattern = ssp), pattern_fill = "black",
                       pattern_density = 0.025, pattern_angle = 45,
                       pattern_spacing = 0.01, width = 0.6) +
  scale_pattern_manual(values = c("none", "stripe")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = -45, vjust = -0.5),
        text = element_text(size = 18)) +
  ylab("Probability of at least 2 resources changing") + xlab("Exposure type") +
  labs(fill="SSP")
dev.off()

# relationship between metrics
dat_points <- dat %>% 
  filter(ssp =="5.85") %>% 
  pivot_wider(names_from = type, values_from = proba)
dat_points <- left_join(dat_points, regions, by = c("regions" = "SOVEREIGN1"))

png(paste0("figures/figure_2b.png"),
    width = 10*200, height = 6*200, res = 200)
ggplot(dat_points, aes(y = gradual, x = shock)) +
  # geom_rect(xmin = 0.75, xmax = 2, ymin = 0.75, ymax = 2, linetype = "dashed",
  #           fill = "lightgrey") +
  geom_point(alpha = 0.2, size = 3) +
  geom_point(data = dat_points[dat_points$shock>0.75 & dat_points$gradual>0.75,],
             alpha = 0.5, size = 3) +
  geom_point(data = dat_points[dat_points$regions=="global",], 
             aes(y = gradual, x = shock),
             col = "red", size = 3) +
  theme_bw() + xlim(0,1) + ylim(0,1) +
  theme(strip.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(0.75,'lines'),
        strip.text = element_text(size = 14), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 18)) +
  scale_x_continuous(expand=c(0.01,0.01)) + scale_y_continuous(expand=c(0.02,0.02)) +
  ylab("Probability of at least 2 res. changing by >25%") + xlab("Probability of at least 1 year with at least 2 shocks") +
  geom_text_repel(data = dat_points[dat_points$shock>0.75 & dat_points$gradual>0.75,],
                   aes(label = regions), size=5, max.overlaps=30)
  # geom_text(data = dat_points[dat_points$shock>0.75 & dat_points$gradual>0.75,],
  #           aes(label = ISO_SOV1), size = 4)
dev.off()


#### C. correlation between metrics under different SSPs ----
dat_cor <- dat %>% 
  pivot_wider(names_from = type, values_from = proba)

cor(dat_cor$shock[dat_cor$ssp=="1.26"],
    dat_cor$gradual[dat_cor$ssp=="1.26"],
    method = "pearson")
cor.test(dat_cor$shock[dat_cor$ssp=="1.26"],
         dat_cor$gradual[dat_cor$ssp=="1.26"],
         method = "pearson")

cor(dat_cor$shock[dat_cor$ssp=="5.85"],
    dat_cor$gradual[dat_cor$ssp=="5.85"],
    method = "pearson")
cor.test(dat_cor$shock[dat_cor$ssp=="5.85"],
         dat_cor$gradual[dat_cor$ssp=="5.85"],
         method = "pearson")
