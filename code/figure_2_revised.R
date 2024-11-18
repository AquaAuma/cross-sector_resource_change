#### Figure 1 and alternative versions
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
library(ggpubr)


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



#### B. boxplot
dat <- dat %>% 
  select(regions, climates, at_least_two_shocks, at_least_two_change_25) %>% 
  mutate(ssp = ifelse(str_detect(climates, "ssp126"),"1.26","5.85"),
         model = ifelse(str_detect(climates, "gfdl-esm4"),"gfdl-esm4","ipsl-cm6a-lr")) %>% 
  group_by(regions, ssp) %>% 
  summarize(shock = mean(at_least_two_shocks, na.rm=T),
            gradual = mean(at_least_two_change_25, na.rm=T)) %>% 
  pivot_longer(3:4, names_to = "type", values_to = "proba") %>% 
  data.frame()

png(paste0("figures/revised_figures/figure_2.png"),
    width = 4*200, height = 4*200, res = 200)
ggplot(dat) + geom_boxplot(aes(x = type, y = proba, fill = ssp), alpha = 0.5) +
  theme_bw() +
  scale_fill_manual(values = c("white","black")) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = -45, vjust = -0.5),
        text = element_text(size = 10)) +
  ylab("p(X>=2)") + xlab("Exposure type") +
  labs(fill="SSP")
dev.off()
