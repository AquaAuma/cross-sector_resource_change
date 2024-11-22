#### Figure 4 revision
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
library(RColorBrewer)


################################################################################
#### 4. COUNTRY RANKINGS
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

# number of resources per country
pct_diff_ts_r_cs_ag <- read_csv("data/data_processing/countries_time_series_1985-2015_2070-2099_agriculture.csv")
pct_diff_ts_r_cs_fi <- read_csv("data/data_processing/countries_time-series_1985-2015_2070-2099_marine-fishery_global.csv")
pct_diff_ts_r_cs_wa <- read_csv("data/data_processing/countries_time_series_1985-2015_2070-2099_water_global.csv")
pct_diff_ts_r_cs <- rbind(pct_diff_ts_r_cs_ag, pct_diff_ts_r_cs_fi, pct_diff_ts_r_cs_wa)

nres_r <- data.frame(pct_diff_ts_r_cs) %>% 
  filter(!is.na(percent_diff),
         output_variable %in% c("rice", "wheat", "soybean", "maize", "tcblog10", "qtot")) %>% 
  group_by(regions) %>% 
  summarize(n_res = length(unique(output_variable)))

rm(pct_diff_ts_r_cs, pct_diff_ts_r_cs_wa, pct_diff_ts_r_cs_fi, pct_diff_ts_r_cs_ag)

# cbind both datasets
dat <- left_join(dat, dat_mec, by = c("regions", "climates")) %>% 
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

dat <- left_join(dat, nres_r, by = c("regions"))


#### D. Figure 4 ----
dat_plot <- dat %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585"),
         !regions %in% c("Antarctica","global")) %>% 
  mutate(regions = ifelse(regions == "Democratic Republic of the Congo", "D.R. Congo",regions),
         regions = ifelse(regions == "Bosnia and Herzegovina","Bosnia H.",regions),
         regions = ifelse(regions == "Federal Republic of Somalia","Somalia",regions),
         regions = ifelse(regions == "Republic of the Congo","R. Congo",regions),
         regions = ifelse(regions == "Dominican Republic", "Dominican R.",regions),
         regions = ifelse(regions == "Trinidad and Tobago","Trinidad Tobago",regions),
         regions = ifelse(regions == "Central African Republic","C. African R.",regions),
         regions = ifelse(regions == "Grand Duchy of Luxembourg","Luxembourg",regions),
         regions = ifelse(regions == "Papua New Guinea","Papua New G.",regions),
         regions = ifelse(regions == "United Arab Emirates","U. Arab Emirates",regions),
         regions = ifelse(regions == "Sao Tome and Principe","Sao Tome P.",regions),
         regions = ifelse(regions == "Saint Vincent and the Grenadines","St Vincent G.",regions),
         regions = ifelse(regions == "Saint Kitts and Nevis","St. Kitts N.",regions),
         regions = ifelse(regions == "Marshall Islands","Marshall Is.",regions),
         regions = ifelse(regions == "Antigua and Barbuda","Antigua Bar.",regions))
  
col_res <- brewer.pal(9, "Greys")
  
plot_shocks_a <- dat_plot %>% 
  filter(scale == "abrupt",
         n_res %in% c(5,6)) %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = function(x) median(x, na.rm=TRUE)), x = proba)) + 
  geom_boxplot(aes(fill = as.factor(n_res)), alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = c(col_res[7],col_res[9]), name = "# resources") +
  facet_grid(factor(n_res, levels=c("6","5","4","3","2"))~factor(scale, levels=c("abrupt","gradual"),
                    labels = c("Shocks","Gradual")), 
             scales="free", space = "free") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0,'lines'),
        legend.position = "none",
        text = element_text(size = 11)) +
  ylab("") + xlab("Exposure probability")

plot_shocks_b <- dat_plot %>% 
  filter(scale == "abrupt",
         n_res %in% c(4,3,2)) %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = function(x) median(x, na.rm=TRUE)), x = proba)) + 
  geom_boxplot(aes(fill = as.factor(n_res)), alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = c(col_res[1],col_res[3],col_res[5]), name = "# resources") +
  facet_grid(factor(n_res, levels=c("6","5","4","3","2"))~factor(scale, levels=c("abrupt","gradual"),
                                                                 labels = c("Shocks","Gradual")), 
             scale="free", space = "free_y") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0,'lines'),
        legend.position = "none",
        text = element_text(size = 11)) +
  ylab("") + xlab("Exposure probability")

order_countries <- dat_plot %>% 
  filter(scale == "abrupt") %>% 
  group_by(regions, n_res) %>% 
  summarize(proba = median(proba, na.rm=T)) %>% 
  arrange(desc(n_res), desc(proba)) %>% 
  tibble::rownames_to_column() %>% 
  select(-n_res, -proba)

plot_gradual_c <- left_join(dat_plot, order_countries, by = "regions") %>% 
  filter(scale == "gradual",
         n_res %in% c(6,5)) %>% 
  mutate(regions = fct_reorder(as.factor(regions),desc(as.numeric(rowname)))) %>% 
  ggplot(aes(y = regions, x = proba)) + 
  geom_boxplot(aes(fill = as.factor(n_res)), alpha = 0.8, outlier.shape = NA) +
  #geom_point(shape = 4, size = 0.5, color = "grey20", stroke = 2) +
  scale_fill_manual(values = c(col_res[7],col_res[9]), name = "# resources") +
  facet_grid(factor(n_res, levels=c("6","5","4","3","2"))~factor(scale, levels=c("abrupt","gradual"),
                                                                 labels = c("Shocks","Gradual")), 
             scale="free", space = "free_y") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        panel.spacing = unit(0,'lines'),
        legend.position = "none",
        text = element_text(size = 11)) +
  ylab("") + xlab("Exposure probability")

plot_gradual_d <- left_join(dat_plot, order_countries, by = "regions") %>% 
  filter(scale == "gradual",
         n_res %in% c(4,3,2)) %>% 
  mutate(regions = fct_reorder(as.factor(regions),desc(as.numeric(rowname)))) %>% 
  ggplot(aes(y = regions, x = proba)) + 
  geom_boxplot(aes(fill = as.factor(n_res)), alpha = 0.8, outlier.shape = NA) +
  #geom_point(shape = 4, size = 0.5, color = "grey20", stroke = 2) +
  scale_fill_manual(values = c(col_res[1],col_res[3],col_res[5]), name = "# resources") +
  facet_grid(factor(n_res, levels=c("6","5","4","3","2"))~factor(scale, levels=c("abrupt","gradual"),
                                                                 labels = c("Shocks","Gradual")), 
             scale="free", space = "free_y") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        panel.spacing = unit(0,'lines'),
        legend.position = "none",
        text = element_text(size = 11)) +
  ylab("") + xlab("Exposure probability")

png(paste0("figures/revised_figures/figure_4a.png"),
    width = 4*200, height = 16*200, res = 200)
plot_shocks_a
dev.off()

png(paste0("figures/revised_figures/figure_4b.png"),
    width = 4*200, height = 16*200, res = 200)
plot_shocks_b
dev.off()

png(paste0("figures/revised_figures/figure_4c.png"),
    width = 4*200, height = 16*200, res = 200)
plot_gradual_c
dev.off()

png(paste0("figures/revised_figures/figure_4d.png"),
    width = 4*200, height = 16*200, res = 200)
plot_gradual_d
dev.off()


#### E. Figure 4bis ----
plot_shocks_6 <- dat_plot %>% 
  filter(scale == "abrupt",
         n_res == 6) %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = function(x) median(x, na.rm=TRUE)), x = proba)) + 
  geom_boxplot(aes(fill = as.factor(n_res)), alpha = 0.7, outlier.shape = NA) +
  scale_fill_manual(values = c(col_res[9]), name = "# resources") +
  facet_grid(factor(n_res, levels=c("6","5","4","3","2"))~factor(scale, levels=c("abrupt","gradual"),labels = c("Shocks","Gradual")), 
             scales="free", space = "free") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0,'lines'),
        legend.position = "none",
        text = element_text(size = 18)) +
  ylab("") + xlab("Exposure probability")

plot_shocks_5 <- dat_plot %>% 
  filter(scale == "abrupt",
         n_res == 5) %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = function(x) median(x, na.rm=TRUE)), x = proba)) + 
  geom_boxplot(aes(fill = as.factor(n_res)), alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = c(col_res[7]), name = "# resources") +
  facet_grid(factor(n_res, levels=c("6","5","4","3","2"))~factor(scale, levels=c("abrupt","gradual"),labels = c("Shocks","Gradual")), 
             scales="free", space = "free") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0,'lines'),
        legend.position = "none",
        text = element_text(size = 18)) +
  ylab("") + xlab("Exposure probability")

plot_shocks_4 <- dat_plot %>% 
  filter(scale == "abrupt",
         n_res == 4) %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = function(x) median(x, na.rm=TRUE)), x = proba)) + 
  geom_boxplot(aes(fill = as.factor(n_res)), alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = c(col_res[5]), name = "# resources") +
  facet_grid(factor(n_res, levels=c("6","5","4","3","2"))~factor(scale, levels=c("abrupt","gradual"),labels = c("Shocks","Gradual")), 
             scales="free", space = "free") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0,'lines'),
        legend.position = "none",
        text = element_text(size = 18)) +
  ylab("") + xlab("Exposure probability")

plot_shocks_3 <- dat_plot %>% 
  filter(scale == "abrupt",
         n_res == 3) %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = function(x) median(x, na.rm=TRUE)), x = proba)) + 
  geom_boxplot(aes(fill = as.factor(n_res)), alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = c(col_res[3]), name = "# resources") +
  facet_grid(factor(n_res, levels=c("6","5","4","3","2"))~factor(scale, levels=c("abrupt","gradual"),labels = c("Shocks","Gradual")), 
             scales="free", space = "free") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0,'lines'),
        legend.position = "none",
        text = element_text(size = 18)) +
  ylab("") + xlab("Exposure probability")

plot_shocks_b <- dat_plot %>% 
  filter(scale == "abrupt",
         n_res %in% c(4,3,2)) %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = function(x) median(x, na.rm=TRUE)), x = proba)) + 
  geom_boxplot(aes(fill = as.factor(n_res)), alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = c(col_res[1],col_res[3],col_res[5]), name = "# resources") +
  facet_grid(factor(n_res, levels=c("6","5","4","3","2"))~factor(scale, levels=c("abrupt","gradual"),
                                                                 labels = c("Shocks","Gradual")), 
             scale="free", space = "free_y") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0,'lines'),
        legend.position = "none",
        text = element_text(size = 11)) +
  ylab("") + xlab("Exposure probability")

order_countries <- dat_plot %>% 
  filter(scale == "abrupt") %>% 
  group_by(regions, n_res) %>% 
  summarize(proba = median(proba, na.rm=T)) %>% 
  arrange(desc(n_res), desc(proba)) %>% 
  tibble::rownames_to_column() %>% 
  select(-n_res, -proba)

plot_gradual_c <- left_join(dat_plot, order_countries, by = "regions") %>% 
  filter(scale == "gradual",
         n_res %in% c(6,5)) %>% 
  mutate(regions = fct_reorder(as.factor(regions),desc(as.numeric(rowname)))) %>% 
  ggplot(aes(y = regions, x = proba)) + 
  geom_boxplot(aes(fill = as.factor(n_res)), alpha = 0.8, outlier.shape = NA) +
  #geom_point(shape = 4, size = 0.5, color = "grey20", stroke = 2) +
  scale_fill_manual(values = c(col_res[7],col_res[9]), name = "# resources") +
  facet_grid(factor(n_res, levels=c("6","5","4","3","2"))~factor(scale, levels=c("abrupt","gradual"),
                                                                 labels = c("Shocks","Gradual")), 
             scale="free", space = "free_y") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        panel.spacing = unit(0,'lines'),
        legend.position = "none",
        text = element_text(size = 11)) +
  ylab("") + xlab("Exposure probability")

plot_gradual_d <- left_join(dat_plot, order_countries, by = "regions") %>% 
  filter(scale == "gradual",
         n_res %in% c(4,3,2)) %>% 
  mutate(regions = fct_reorder(as.factor(regions),desc(as.numeric(rowname)))) %>% 
  ggplot(aes(y = regions, x = proba)) + 
  geom_boxplot(aes(fill = as.factor(n_res)), alpha = 0.8, outlier.shape = NA) +
  #geom_point(shape = 4, size = 0.5, color = "grey20", stroke = 2) +
  scale_fill_manual(values = c(col_res[1],col_res[3],col_res[5]), name = "# resources") +
  facet_grid(factor(n_res, levels=c("6","5","4","3","2"))~factor(scale, levels=c("abrupt","gradual"),
                                                                 labels = c("Shocks","Gradual")), 
             scale="free", space = "free_y") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        panel.spacing = unit(0,'lines'),
        legend.position = "none",
        text = element_text(size = 11)) +
  ylab("") + xlab("Exposure probability")

png(paste0("figures/revised_figures/figure_4a.png"),
    width = 4*200, height = 16*200, res = 200)
plot_shocks_a
dev.off()

png(paste0("figures/revised_figures/figure_4b.png"),
    width = 4*200, height = 16*200, res = 200)
plot_shocks_b
dev.off()

png(paste0("figures/revised_figures/figure_4c.png"),
    width = 4*200, height = 16*200, res = 200)
plot_gradual_c
dev.off()

png(paste0("figures/revised_figures/figure_4d.png"),
    width = 4*200, height = 16*200, res = 200)
plot_gradual_d
dev.off()