#### Manuscript figures
#### Coding: Aurore A. Maureaud, July 2024

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
#### 1. ARE COUNTRIES EXPERIENCING SHORT- AND LONG-TERM CROSS-SECTOR CHANGES?
################################################################################

## load data
# countries
shock_probas_r_cs <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas.csv")
change_probas_r_cs <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas.csv") %>% 
  dplyr::select(-spatial_scale)

# global
shock_probas_g_cs <- read_csv("data/short-term_change/global_shocks_1985-2015_probas.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(shock_probas_r_cs))
change_probas_g_cs <- read_csv("data/long-term_change/global_long-term_change_1985-2015_probas.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(change_probas_r_cs))

# rbind
shock_probas <- rbind(shock_probas_g_cs, shock_probas_r_cs)
long_term <- rbind(change_probas_g_cs, change_probas_r_cs) %>% 
  pivot_wider(names_from = "type", values_from = "probas")
dat <- left_join(long_term, shock_probas, by = c("climates", "regions", "time_window"))

### Combining all plots possible
# plots combined for at least 1 shock
yrs <- c(10,20,30)
for(i in 1:length(yrs)){
  # at least one resource
  one_5 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_one_change_5, x = at_least_one_shock)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_one_change_5, x = at_least_one_shock),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("one_5")
  
  one_10 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_one_change_10, x = at_least_one_shock)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_one_change_10, x = at_least_one_shock),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("one_10")
  
  one_25 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_one_change_25, x = at_least_one_shock)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_one_change_25, x = at_least_one_shock),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("one_25")
  
  # at least two resources
  two_5 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_two_change_5, x = at_least_one_shock)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_5, x = at_least_one_shock),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("two_5")
  
  two_10 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_two_change_10, x = at_least_one_shock)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_10, x = at_least_one_shock),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("two_10")
  
  two_25 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_two_change_25, x = at_least_one_shock)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_25, x = at_least_one_shock),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("two_25")
  
  # at least three resources
  three_5 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_three_change_5, x = at_least_one_shock)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_5, x = at_least_one_shock),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("three_5")
  
  three_10 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_three_change_10, x = at_least_one_shock)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_10, x = at_least_one_shock),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("three_10")
  
  three_25 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_three_change_25, x = at_least_one_shock)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_three_change_25, x = at_least_one_shock),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    ylab("three_25")
  
  # combined plot
  png(paste0("figures/combined/combined_plots_",yrs[i],"_at_least_one_shock.png"),
      width = 8*200, height = 15*200, res = 200)
  grid.arrange(one_5, one_10, one_25,
               two_5, two_10, two_25,
               three_5, three_10, three_25,
               nrow = 9, 
               name = paste("Time window of ", yrs[i]))
  dev.off()
}


# plots combined for at least 2 shocks
for(i in 1:length(yrs)){
  # at least one resource
  one_5 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_one_change_5, x = at_least_two_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_one_change_5, x = at_least_two_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("one_5")
  
  one_10 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_one_change_10, x = at_least_two_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_one_change_10, x = at_least_two_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("one_10")
  
  one_25 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_one_change_25, x = at_least_two_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_one_change_25, x = at_least_two_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("one_25")
  
  # at least two resources
  two_5 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_two_change_5, x = at_least_two_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_5, x = at_least_two_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("two_5")
  
  two_10 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_two_change_10, x = at_least_two_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_10, x = at_least_two_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("two_10")
  
  two_25 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_two_change_25, x = at_least_two_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_25, x = at_least_two_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("two_25")
  
  # at least three resources
  three_5 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_three_change_5, x = at_least_two_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_5, x = at_least_two_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("three_5")
  
  three_10 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_three_change_10, x = at_least_two_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_10, x = at_least_two_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("three_10")
  
  three_25 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_three_change_25, x = at_least_two_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_three_change_25, x = at_least_two_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    ylab("three_25")
  
  # combined plot
  png(paste0("figures/combined/combined_plots_",yrs[i],"_at_least_two_shocks.png"),
      width = 8*200, height = 15*200, res = 200)
  grid.arrange(one_5, one_10, one_25,
               two_5, two_10, two_25,
               three_5, three_10, three_25,
               nrow = 9, 
               name = paste("Time window of ", yrs[i]))
  dev.off()
}


# plots combined for at least 3 shocks
for(i in 1:length(yrs)){
  # at least one resource
  one_5 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_one_change_5, x = at_least_three_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_one_change_5, x = at_least_three_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("one_5")
  
  one_10 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_one_change_10, x = at_least_three_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_one_change_10, x = at_least_three_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("one_10")
  
  one_25 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_one_change_25, x = at_least_three_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_one_change_25, x = at_least_three_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("one_25")
  
  # at least two resources
  two_5 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_two_change_5, x = at_least_three_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_5, x = at_least_three_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("two_5")
  
  two_10 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_two_change_10, x = at_least_three_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_10, x = at_least_three_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("two_10")
  
  two_25 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_two_change_25, x = at_least_three_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_25, x = at_least_three_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("two_25")
  
  # at least three resources
  three_5 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_three_change_5, x = at_least_three_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_5, x = at_least_three_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("three_5")
  
  three_10 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_three_change_10, x = at_least_three_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_two_change_10, x = at_least_three_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
    ylab("three_10")
  
  three_25 <- dat %>% filter(time_window == yrs[i]) %>% 
    ggplot(aes(y = at_least_three_change_25, x = at_least_three_shocks)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ climates, ncol=4) +
    geom_point(data = dat[dat$regions=="global" & dat$time_window==yrs[i],], 
               aes(y = at_least_three_change_25, x = at_least_three_shocks),
               col = "red") +
    theme_bw() + xlim(0,1) + ylim(0,1) +
    ylab("three_25")
  
  # combined plot
  png(paste0("figures/combined/combined_plots_",yrs[i],"_at_least_three_shocks.png"),
      width = 8*200, height = 15*200, res = 200)
  grid.arrange(one_5, one_10, one_25,
               two_5, two_10, two_25,
               three_5, three_10, three_25,
               nrow = 9, 
               name = paste("Time window of ", yrs[i]))
  dev.off()
}

# example plots for cross-sector conditions
png(paste0("figures/combined/example1_label.png"),
    width = 6*200, height = 4*200, res = 200)
dat %>% filter(time_window == 10) %>% 
  ggplot(aes(y = at_least_two_change_25, x = at_least_two_shocks)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ climates) +
  geom_point(data = dat[dat$regions=="global" & dat$time_window==10,], 
             aes(y = at_least_two_change_25, x = at_least_two_shocks),
             col = "red") +
  theme_bw() + xlim(0,1) + ylim(0,1) +
  ylab("Probability of at least 2 res. changing by >25%") + xlab("Probability of at least 2 shocks") +
  geom_text_repel(data = dat[dat$time_window==10 & dat$at_least_two_shocks>0.5 & dat$at_least_two_change_25>0.5,], aes(label = regions),
                  size=2, max.overlaps=15)
dev.off()

# example plots for cross-sector conditions
png(paste0("figures/combined/figure_1.png"),
    width = 6*200, height = 4*200, res = 200)
dat %>% filter(time_window == 30) %>% 
  ggplot(aes(y = at_least_two_change_25, x = at_least_two_shocks)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ climates) +
  geom_point(data = dat[dat$regions=="global" & dat$time_window==30,], 
             aes(y = at_least_two_change_25, x = at_least_two_shocks),
             col = "red") +
  theme_bw() + xlim(0,1) + ylim(0,1) +
  ylab("Probability of at least 2 res. changing by >25%") + xlab("Probability of at least 2 shocks")
# geom_text_repel(data = dat[dat$time_window==30 & dat$at_least_two_shocks>0.5 & dat$at_least_two_change_10>0.5,], aes(label = regions),
#                 size=2, max.overlaps=20)
dev.off()

png(paste0("figures/combined/example3.png"),
    width = 6*200, height = 4*200, res = 200)
dat %>% filter(time_window == 10) %>% 
  ggplot(aes(y = at_least_two_change_10, x = at_least_one_shock)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ climates) +
  geom_point(data = dat[dat$regions=="global" & dat$time_window==10,], 
             aes(y = at_least_two_change_10, x = at_least_one_shock),
             col = "red") +
  theme_bw() + xlim(0,1) + ylim(0,1) +
  ylab("Probability of at least 2 res. changing by >10%") + xlab("Probability of at least 1 shock") +
  theme(legend.position = "none")
dev.off()

png(paste0("figures/combined/example4_labels.png"),
    width = 6*200, height = 4*200, res = 200)
dat %>% filter(time_window == 10) %>% 
  ggplot(aes(y = at_least_three_change_25, x = at_least_two_shocks)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ climates) +
  geom_point(data = dat[dat$regions=="global" & dat$time_window==10,], 
             aes(y = at_least_three_change_25, x = at_least_two_shocks),
             col = "red") +
  theme_bw() + xlim(0,1) + ylim(0,1) +
  ylab("Probability of at least 3 res. changing by >25%") + xlab("Probability of at least 2 shocks") +
  geom_text_repel(data = dat[dat$time_window==10 & dat$at_least_two_shocks>0.5 & dat$at_least_three_change_25>0.5,], aes(label = regions),
                  size=2, max.overlaps=15)
dev.off()

### Boxplots comparing distributions
# boxplot for shocks
png(paste0("figures/combined/shocks_boxplots.png"),
    width = 7*200, height = 3*200, res = 200)
dat %>% 
  dplyr::select(regions, climates, time_window, at_least_one_shock, at_least_two_shocks, at_least_three_shocks) %>% 
  pivot_longer(4:6, names_to = "type", values_to = "proba") %>% 
  ggplot(aes(x=as.factor(climates), y=proba, fill=as.factor(time_window))) +
  geom_boxplot() +
  facet_wrap(~ factor(type, levels = c("at_least_one_shock","at_least_two_shocks","at_least_three_shocks")), 
             scales = "free_y") +
  theme_bw() + ylab("Probability") + xlab("Future climates") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(breaks = c("10","20","30"),
                    values = c("grey80","grey60","grey40"),
                    name = "Time window")
dev.off()

# boxplot for long-term changes
png(paste0("figures/combined/long-term_change_boxplots.png"),
    width = 9*200, height = 8*200, res = 200)
dat %>% 
  dplyr::select(-at_least_one_shock, -at_least_two_shocks, -at_least_three_shocks) %>% 
  pivot_longer(4:12, names_to = "type", values_to = "proba") %>% 
  mutate(threshold = gsub("\\D","",type),
         at_least = ifelse(str_detect(type, "one"),"one",NA_character_),
         at_least = ifelse(str_detect(type, "two"),"two",at_least),
         at_least = ifelse(str_detect(type, "three"),"three",at_least),
         choices = paste0("at_least_",at_least,"_res. >",threshold, "%")) %>%
  ggplot(aes(x=climates, y = proba, fill = as.factor(time_window))) +
  geom_boxplot() +
  facet_wrap(~ factor(choices, 
                      levels = c("at_least_one_res. >5%","at_least_two_res. >5%","at_least_three_res. >5%",
                                 "at_least_one_res. >10%","at_least_two_res. >10%","at_least_three_res. >10%",
                                 "at_least_one_res. >25%","at_least_two_res. >25%","at_least_three_res. >25%"))) +
  theme_bw() + ylab("Probability") + xlab("Future climates") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(breaks = c("10","20","30"),
                    values = c("grey80","grey60","grey40"),
                    name = "Time window")
dev.off()
  


################################################################################
#### 2. ARE COUNTRIES EXPERIENCING SYNCHRONY AND/OR COMPENSATION?
################################################################################
## load data
# countries
shock_probas_r_cs_mec <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas_mechanisms.csv")
change_probas_r_cs_mec <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas_mechanisms.csv") %>% 
  dplyr::select(-spatial_scale)

# global
shock_probas_g_cs_mec <- read_csv("data/short-term_change/global_shocks_1985-2015_probas_mechanisms.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(shock_probas_r_cs_mec))
change_probas_g_cs_mec <- read_csv("data/long-term_change/global_long-term_change_1985-2015_probas_mechanisms.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(change_probas_r_cs_mec))

# rbind
shock_probas <- rbind(shock_probas_g_cs_mec, shock_probas_r_cs_mec)
long_term <- rbind(change_probas_g_cs_mec, change_probas_r_cs_mec) %>% 
  pivot_wider(names_from = "type", values_from = "probas")
dat_mec <- left_join(long_term, shock_probas, by = c("climates", "regions", "time_window"))

### Comparing individual compensation at least one probabilities ----
# long-term probabilities
windows <- c(10, 20, 30)
for(i in 1:length(windows)){
  png(paste0("figures/combined/compensation_lg_5_probas_correlations_",windows[i],".png"),
      width = 8*200, height = 7*200, res = 200)
  dat_mec %>% 
    dplyr::select(regions, climates, time_window,
                  at_least_one_5_up, at_least_one_10_up, at_least_one_25_up,
                  at_least_one_5_down, at_least_one_10_down, at_least_one_25_down) %>%
    filter(time_window == windows[i]) %>% 
    ggpairs(columns = c(4,7), aes(color = climates, alpha = 0.5))+
    ggtitle(paste0("Time window of ",windows[i]," years")) +
    theme(text = element_text(size = 20))
  dev.off()
}

for(i in 1:length(windows)){
  png(paste0("figures/combined/compensation_lg_10_probas_correlations_",windows[i],".png"),
      width = 8*200, height = 7*200, res = 200)
  dat_mec %>% 
    dplyr::select(regions, climates, time_window,
                  at_least_one_5_up, at_least_one_10_up, at_least_one_25_up,
                  at_least_one_5_down, at_least_one_10_down, at_least_one_25_down) %>%
    filter(time_window == windows[i]) %>% 
    ggpairs(columns = c(5,8), aes(color = climates, alpha = 0.5))+
    ggtitle(paste0("Time window of ",windows[i]," years")) +
    theme(text = element_text(size = 20))
  dev.off()
}

for(i in 1:length(windows)){
  png(paste0("figures/combined/compensation_lg_25_probas_correlations_",windows[i],".png"),
      width = 8*200, height = 7*200, res = 200)
  dat_mec %>% 
    dplyr::select(regions, climates, time_window,
                  at_least_one_5_up, at_least_one_10_up, at_least_one_25_up,
                  at_least_one_5_down, at_least_one_10_down, at_least_one_25_down) %>%
    filter(time_window == windows[i]) %>% 
    ggpairs(columns = c(6,9), aes(color = climates, alpha = 0.5))+
    ggtitle(paste0("Time window of ",windows[i]," years")) +
    theme(text = element_text(size = 20))
  dev.off()
}

# short-term probabilities
windows <- c(10, 20, 30)
for(i in 1:length(windows)){
  png(paste0("figures/combined/compensation_shocks_probas_correlations_",windows[i],".png"),
      width = 8*200, height = 7*200, res = 200)
  dat_mec %>% 
    dplyr::select(regions, climates, time_window,
                  at_least_one_shock_up, at_least_one_shock_down) %>%
    filter(time_window == windows[i]) %>% 
    ggpairs(columns = 4:5, aes(color = climates, alpha = 0.5)) +
    ggtitle(paste0("Time window of ",windows[i]," years")) +
    theme(text = element_text(size = 20))
  dev.off()
}

### A. Boxplots comparing distributions ----
# boxplot for shocks
png(paste0("figures/combined/shocks_boxplots_mec.png"),
    width = 7*200, height = 3*200, res = 200)
dat_mec %>% 
  dplyr::select(regions, climates, time_window, at_least_two_shocks_down, at_least_one_shock_up_down) %>% 
  pivot_longer(4:5, names_to = "type", values_to = "proba") %>% 
  ggplot(aes(x=as.factor(climates), y=proba, fill=as.factor(time_window))) +
  geom_boxplot() +
  facet_wrap(~ factor(type, levels = c("at_least_two_shocks_down","at_least_one_shock_up_down")), 
             scales = "free_y") +
  theme_bw() + ylab("Probability") + xlab("Future climates") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(breaks = c("10","20","30"),
                    values = c("grey80","grey60","grey40"),
                    name = "Time window")
dev.off()

# boxplot for long-term changes
png(paste0("figures/combined/long-term_change_boxplots_mec.png"),
    width = 9*200, height = 8*200, res = 200)
dat_mec %>% 
  dplyr::select(-at_least_two_shocks_down, -at_least_three_shocks_down, -at_least_one_shock_up_down,
                -at_least_one_shock_up, -at_least_one_shock_down,
                -at_least_one_10_down, -at_least_one_10_up,
                -at_least_one_5_down, -at_least_one_5_up,
                -at_least_one_25_down, -at_least_one_25_up) %>% 
  pivot_longer(4:12, names_to = "type", values_to = "proba") %>% 
  mutate(threshold = gsub("\\D","",type),
         at_least = ifelse(str_detect(type, "one"),"one",NA_character_),
         at_least = ifelse(str_detect(type, "two"),"two",at_least),
         at_least = ifelse(str_detect(type, "three"),"three",at_least),
         choices = paste0("at_least_",at_least,"_res. >",threshold, "%")) %>%
  ggplot(aes(x=climates, y = proba, fill = as.factor(time_window))) +
  geom_boxplot() +
  facet_wrap(~ factor(type, levels = c("at_least_one_5_up_down", "at_least_one_10_up_down", "at_least_one_25_up_down",
                                "at_least_two_5_down","at_least_two_10_down","at_least_two_25_down",
                                "at_least_three_5_down","at_least_three_10_down","at_least_three_25_down")),
             dir = "v") +
  theme_bw() + ylab("Probability") + xlab("Future climates") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(breaks = c("10","20","30"),
                    values = c("grey80","grey60","grey40"),
                    name = "Time window")
dev.off()

### B. example plots of cross-sector change mechanisms ----
# Synchrony
png(paste0("figures/combined/synchrony_probas_example2.png"),
    width = 6*200, height = 4*200, res = 200)
dat_mec %>% filter(time_window == 30) %>% 
  ggplot(aes(y = at_least_two_10_down, x = at_least_two_shocks_down)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ climates) +
  geom_point(data = dat_mec[dat_mec$regions=="global" & dat_mec$time_window==30,], 
             aes(y = at_least_two_10_down, x = at_least_two_shocks_down),
             col = "red") +
  theme_bw() + xlim(0,1) + ylim(0,1) +
  ylab("Probability of at least 2 res. decreasing by 10%") + xlab("Probability of at least 2 decreasing shocks") +
  geom_text_repel(data = dat_mec[dat_mec$time_window==30 & dat_mec$at_least_two_10_down>0.5 & dat_mec$at_least_two_shocks_down>0.5,], 
                  aes(label = regions), size=2, max.overlaps=15)
dev.off()

# example plots of cross-sector change: compensation
png(paste0("figures/combined/compensation_probas_example1.png"),
    width = 6*200, height = 4*200, res = 200)
dat_mec %>% filter(time_window == 30) %>% 
  ggplot(aes(y = at_least_one_10_up_down, x = at_least_one_shock_up_down)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ climates) +
  geom_point(data = dat_mec[dat_mec$regions=="global" & dat_mec$time_window==30,], 
             aes(y = at_least_one_10_up_down, x = at_least_two_10_down),
             col = "red") +
  theme_bw() + xlim(0,1) + ylim(0,1) +
  ylab("Probability of at least 2 res. changing by 10% compensate") + xlab("Probability of at least two compensating shocks") +
  geom_text_repel(data = dat_mec[dat_mec$time_window==30 & dat_mec$at_least_one_shock_up_down>0.5 & dat_mec$at_least_one_10_up_down>0.5,], 
                  aes(label = regions), size=2, max.overlaps=15)
dev.off()

# example plots cross-sector synchrony versus compensation long-term
png(paste0("figures/combined/compensation_synchrony_long-term_example1.png"),
    width = 6*200, height = 4*200, res = 200)
dat_mec %>% filter(time_window == 30) %>% 
  ggplot(aes(y = at_least_one_10_up_down, x = at_least_two_10_down)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ climates) +
  geom_point(data = dat_mec[dat_mec$regions=="global" & dat_mec$time_window==30,], 
             aes(y = at_least_one_10_up_down, x = at_least_two_10_down),
             col = "red") +
  theme_bw() + xlim(0,1) + ylim(0,1) +
  ylab("Probability that at least 2 res. changing by 10+% compensate") + xlab("Probability of at least 2 res. decreasing by 10+%") +
  geom_text_repel(data = dat_mec[dat_mec$time_window==30 & dat_mec$at_least_one_shock_up_down>0.5 & dat_mec$at_least_one_10_up_down>0.5,], 
                  aes(label = regions), size=2, max.overlaps=15)
dev.off()

# example plots cross-sector synchrony versus compensation short-term
png(paste0("figures/combined/compensation_synchrony_short-term_example1.png"),
    width = 6*200, height = 4*200, res = 200)
dat_mec %>% filter(time_window == 30) %>% 
  ggplot(aes(y = at_least_two_shocks_down, x = at_least_one_shock_up_down)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ climates) +
  geom_point(data = dat_mec[dat_mec$regions=="global" & dat_mec$time_window==30,], 
             aes(y = at_least_two_shocks_down, x = at_least_one_shock_up_down),
             col = "red") +
  theme_bw() + xlim(0,1) + ylim(0,1) +
  ylab("Probability of at least 2 decreasing shocks") + xlab("Probability of at least 2 compensating shocks") +
  geom_text_repel(data = dat_mec[dat_mec$time_window==30 & dat_mec$at_least_two_shocks_down>0.5 & dat_mec$at_least_one_shock_up_down>0.5,], 
                  aes(label = regions), size=2, max.overlaps=15)
dev.off()

# correlations between metrics
png(paste0("figures/combined/correlation_compensation_synchrony_30years.png"),
    width = 9*200, height = 7*200, res = 200)
dat_mec %>% filter(time_window == 30) %>% 
  dplyr::select(regions, climates, 
                at_least_two_shocks_down, at_least_one_shock_up_down,
                at_least_two_10_down, at_least_one_10_up_down) %>% 
  ggpairs(columns = 3:6, aes(color = climates, alpha = 0.5)) +
  theme(text = element_text(size = 10)) +
  theme_bw()
dev.off()


### C. Maps of synchrony/compensation short-term and long-term ----

# eez-land merge shapefile
regions <- st_read("data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp") %>% 
  filter(POL_TYPE != "Joint regime (EEZ)",
         is.na(SOVEREIGN2),
         SOVEREIGN1 != "Republic of Mauritius") %>% 
  dplyr::select(UNION, SOVEREIGN1)

# average climate models for ssp585 
dat_mec_ssp585 <- dat_mec %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr-ssp585"),
         time_window==30,
         regions != "global") %>% 
  group_by(regions) %>% 
  summarize(at_least_one_10_up_down = mean(at_least_one_10_up_down, na.rm=T),
            at_least_two_10_down = mean(at_least_two_10_down, na.rm=T),
            at_least_one_shock_up_down = mean(at_least_one_shock_up_down, na.rm=T),
            at_least_two_shocks_down = mean(at_least_two_shocks_down, na.rm=T))

regions_dat <- left_join(regions, dat_mec_ssp585, by = c("SOVEREIGN1" = "regions"))

png(paste0("figures/combined/maps_mechanisms_ssp585_climates_averaged.png"),
    width = 6*200, height = 4*200, res = 200)
regions_dat %>% 
  pivot_longer(3:6, names_to = "mechanism", values_to = "probability") %>% 
  ggplot() + geom_sf(aes(fill = probability)) +
  facet_wrap(~ mechanism) + 
  theme_bw() +
  scale_fill_continuous_sequential(palette = "BurgYl") +
  theme(legend.position = "bottom")
dev.off()

# average climate models for ssp585 but 25% threshold
dat_mec_ssp585 <- dat_mec %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr-ssp585"),
         time_window==30,
         regions != "global") %>% 
  group_by(regions) %>% 
  summarize(at_least_one_25_up_down = mean(at_least_one_25_up_down, na.rm=T),
            at_least_two_25_down = mean(at_least_two_25_down, na.rm=T),
            at_least_one_shock_up_down = mean(at_least_one_shock_up_down, na.rm=T),
            at_least_two_shocks_down = mean(at_least_two_shocks_down, na.rm=T))

regions_dat <- left_join(regions, dat_mec_ssp585, by = c("SOVEREIGN1" = "regions"))

png(paste0("figures/combined/maps_mechanisms_ssp585_climates_averaged_25_threshold.png"),
    width = 6*200, height = 4*200, res = 200)
regions_dat %>% 
  pivot_longer(3:6, names_to = "mechanism", values_to = "probability") %>% 
  ggplot() + geom_sf(aes(fill = probability)) +
  facet_wrap(~ mechanism) + 
  theme_bw() +
  scale_fill_continuous_sequential(palette = "BurgYl") +
  theme(legend.position = "bottom")
dev.off()

# average climate models for ssp126
dat_mec_ssp126 <- dat_mec %>% 
  filter(climates %in% c("gfdl-esm4 ssp126","ipsl-cm6a-lr-ssp126"),
         time_window==30,
         regions != "global") %>% 
  group_by(regions) %>% 
  summarize(at_least_one_10_up_down = mean(at_least_one_10_up_down, na.rm=T),
            at_least_two_10_down = mean(at_least_two_10_down, na.rm=T),
            at_least_one_shock_up_down = mean(at_least_one_shock_up_down, na.rm=T),
            at_least_two_shocks_down = mean(at_least_two_shocks_down, na.rm=T))

regions_dat <- left_join(regions, dat_mec_ssp126, by = c("SOVEREIGN1" = "regions"))

png(paste0("figures/combined/maps_mechanisms_ssp126_climates_averaged.png"),
    width = 6*200, height = 4*200, res = 200)
regions_dat %>% 
  pivot_longer(3:6, names_to = "mechanism", values_to = "probability") %>% 
  ggplot() + geom_sf(aes(fill = probability)) +
  facet_wrap(~ mechanism) + 
  theme_bw() +
  scale_fill_continuous_sequential(palette = "BurgYl") +
  theme(legend.position = "bottom")
dev.off()


################################################################################
#### 3. SUMMARIZING ECOLOGICAL METRICS TO FIND WORSE-CASE SCENARIOS
################################################################################

### A. load data for short- and long-term changes ----
# countries
shock_probas_r_cs <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas.csv")
change_probas_r_cs <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas.csv") %>% 
  dplyr::select(-spatial_scale)

# global
shock_probas_g_cs <- read_csv("data/short-term_change/global_shocks_1985-2015_probas.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(shock_probas_r_cs))
change_probas_g_cs <- read_csv("data/long-term_change/global_long-term_change_1985-2015_probas.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(change_probas_r_cs))

# rbind
shock_probas <- rbind(shock_probas_g_cs, shock_probas_r_cs)
long_term <- rbind(change_probas_g_cs, change_probas_r_cs) %>% 
  pivot_wider(names_from = "type", values_from = "probas")
dat <- left_join(long_term, shock_probas, by = c("climates", "regions", "time_window"))

### B. load data for compensatory and synchronous mechanisms ----
# countries
shock_probas_r_cs_mec <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas_mechanisms.csv")
change_probas_r_cs_mec <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas_mechanisms.csv") %>% 
  dplyr::select(-spatial_scale)

# global
shock_probas_g_cs_mec <- read_csv("data/short-term_change/global_shocks_1985-2015_probas_mechanisms.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(shock_probas_r_cs_mec))
change_probas_g_cs_mec <- read_csv("data/long-term_change/global_long-term_change_1985-2015_probas_mechanisms.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(change_probas_r_cs_mec))

# rbind
shock_probas <- rbind(shock_probas_g_cs_mec, shock_probas_r_cs_mec)
long_term <- rbind(change_probas_g_cs_mec, change_probas_r_cs_mec) %>% 
  pivot_wider(names_from = "type", values_from = "probas")
dat_mec <- left_join(long_term, shock_probas, by = c("climates", "regions", "time_window"))

### C. Data across metrics ----
# cbind both datasets
dat_ecology <- left_join(dat, dat_mec, by = c("regions", "climates", "time_window")) %>% 
  filter(time_window == 30) %>% 
  dplyr::select(regions, climates, time_window, 
                at_least_two_change_10, at_least_two_shocks,
                at_least_two_10_down, at_least_one_10_up_down,
                at_least_two_shocks_down, at_least_one_shock_up_down) %>%
  mutate(at_least_one_10_up_down = 1-at_least_one_10_up_down,
         at_least_one_shock_up_down = 1-at_least_one_shock_up_down) %>%
  pivot_longer(4:9, names_to = "type", values_to = "proba") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "shock", "long-term"),
         mechanism = ifelse(str_detect(type, "down")==TRUE, "synchrony", "stability"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation", mechanism))

write.csv(dat_ecology,
          file = "data/ecological_data/cross-sector_climate_exposure.csv",
          row.names = F)

### D. Overview figures ----
png("figures/combined/rank_countries_exposure_probabilities.png",
    width = 10*200, height = 7*200, res = 200)
dat_ecology %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585"),
         regions %in% c("United States","France","China","Madagascar","Sudan","Colombia","Canada",
                        "Australia","Spain","Argentina","Egypt","Germany","Bolivia","Japan",
                        "Ecuador","Mozambique","Russia","Philippines","Mexico",
                        "Norway","Brazil","South Africa","Namibia","India","Indonesia",
                        "Ukraine","Uruguay","Turkey","Sweden","Sri Lanka","Romania",
                        "Portugal","Pakistan","Nigeria","Netherlands","Libya","Liberia")) %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = median), x = proba)) + geom_boxplot() +
  geom_point(aes(shape = scale, color = mechanism), fill = "grey", stroke = 1) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = c("darkorchid2","grey40","forestgreen")) +
  theme_bw() + 
  ylab("regions") + xlab("cross-sector exposure probabilities") +
  facet_wrap(~climates)
dev.off()

png("figures/combined/rank_countries_exposure_probabilities_combined_subset.png",
    width = 7*200, height = 7*200, res = 200)
dat_ecology %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585"),
         regions %in% c("United States","France","China","Madagascar","Sudan","Colombia","Canada",
                        "Australia","Spain","Argentina","Egypt","Germany","Bolivia","Japan",
                        "Ecuador","Mozambique","Russia","Philippines","Mexico",
                        "Norway","Brazil","South Africa","Namibia","India","Indonesia",
                        "Ukraine","Uruguay","Turkey","Sweden","Sri Lanka","Romania",
                        "Portugal","Pakistan","Nigeria","Netherlands","Libya","Liberia")) %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = median), x = proba)) + geom_boxplot() +
  geom_point(aes(shape = scale, color = mechanism), fill = "grey", stroke = 1) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = c("darkorchid2","grey40","forestgreen")) +
  theme_bw() + 
  ylab("regions") + xlab("cross-sector exposure probabilities")
dev.off()

png("figures/combined/rank_countries_exposure_probabilities_shocks_subset.png",
    width = 7*200, height = 7*200, res = 200)
dat_ecology %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585"),
         regions %in% c("United States","France","China","Madagascar","Sudan","Colombia","Canada",
                        "Australia","Spain","Argentina","Egypt","Germany","Bolivia","Japan",
                        "Ecuador","Mozambique","Russia","Philippines","Mexico",
                        "Norway","Brazil","South Africa","Namibia","India","Indonesia",
                        "Ukraine","Uruguay","Turkey","Sweden","Sri Lanka","Romania",
                        "Portugal","Pakistan","Nigeria","Netherlands","Libya","Liberia"),
         scale == "shock") %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = median), x = proba)) + geom_boxplot() +
  geom_point(aes(shape = scale, color = mechanism), fill = "grey", stroke = 1) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = c("darkorchid2","grey40","forestgreen")) +
  theme_bw() +
  theme(axis.text = element_text(size = 5)) +
  ylab("regions") + xlab("cross-sector exposure probabilities")
dev.off()

png("figures/combined/rank_countries_exposure_probabilities_combined.png",
    width = 10*200, height = 15*200, res = 300)
dat_ecology %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = median), x = proba)) + geom_boxplot() +
  geom_point(aes(shape = scale, color = mechanism), fill = "grey", stroke = 1) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = c("darkorchid2","grey40","forestgreen")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("regions") + xlab("cross-sector exposure probabilities")
dev.off()

png("figures/combined/rank_countries_exposure_probabilities_combined_shocks.png",
    width = 10*200, height = 15*200, res = 300)
dat_ecology %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585"),
         scale == "shock") %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = median), x = proba)) + geom_boxplot() +
  geom_point(aes(shape = scale, color = mechanism), fill = "grey", stroke = 1) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = c("darkorchid2","grey40","forestgreen")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("regions") + xlab("cross-sector exposure probabilities")
dev.off()

png("figures/combined/rank_countries_exposure_probabilities_combined_long-term.png",
    width = 10*200, height = 15*200, res = 300)
dat_ecology %>% 
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585"),
         scale == "long-term") %>% 
  ggplot(aes(y = reorder(regions, proba, FUN = median), x = proba)) + geom_boxplot() +
  geom_point(aes(shape = scale, color = mechanism), fill = "grey", stroke = 1) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = c("darkorchid2","grey40","forestgreen")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("regions") + xlab("cross-sector exposure probabilities")
dev.off()


################################################################################
#### 4. EXPOSURE TO CLIMATE CS IMPACTS for CNT versus RTAs
################################################################################

### A. load data for short- and long-term changes ----
# countries
shock_probas_r_cs <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas.csv") %>% 
  mutate(spatial_scale = "regions")
change_probas_r_cs <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas.csv")

# global
shock_probas_g_cs <- read_csv("data/short-term_change/global_shocks_1985-2015_probas.csv") %>% 
  mutate(regions = "global",
         spatial_scale = "global") %>% 
  dplyr::select(names(shock_probas_r_cs))
change_probas_g_cs <- read_csv("data/long-term_change/global_long-term_change_1985-2015_probas.csv") %>% 
  mutate(regions = "global",
         spatial_scale = "global") %>% 
  dplyr::select(names(change_probas_r_cs))

# rtas
shock_probas_rta_cs <- read_csv("data/short-term_change/rta_shocks_1985-2015_probas.csv") %>% 
  mutate(spatial_scale = "rta") %>% 
  dplyr::select(names(shock_probas_r_cs))
change_probas_rta_cs <- read_csv("data/long-term_change/rta_long-term_change_1985-2015_probas.csv") %>% 
  dplyr::select(names(change_probas_r_cs))

# rbind
shock_probas <- rbind(shock_probas_g_cs, shock_probas_r_cs, shock_probas_rta_cs)
long_term <- rbind(change_probas_g_cs, change_probas_r_cs, change_probas_rta_cs) %>% 
  pivot_wider(names_from = "type", values_from = "probas")
dat <- left_join(long_term, shock_probas, by = c("climates", "regions", "time_window", "spatial_scale"))

### B. load data for compensatory and synchronous mechanisms ----
# countries
shock_probas_r_cs_mec <- read_csv("data/short-term_change/countries_shocks_1985-2015_probas_mechanisms.csv") %>% 
  mutate(spatial_scale = "regions")
change_probas_r_cs_mec <- read_csv("data/long-term_change/countries_long-term_change_1985-2015_probas_mechanisms.csv")

# global
shock_probas_g_cs_mec <- read_csv("data/short-term_change/global_shocks_1985-2015_probas_mechanisms.csv") %>% 
  mutate(regions = "global",
         spatial_scale = "global") %>% 
  dplyr::select(names(shock_probas_r_cs_mec))
change_probas_g_cs_mec <- read_csv("data/long-term_change/global_long-term_change_1985-2015_probas_mechanisms.csv") %>% 
  mutate(regions = "global",
         saptial_scale = "global") %>% 
  dplyr::select(names(change_probas_r_cs_mec))

# rta
shock_probas_rta_cs_mec <- read_csv("data/short-term_change/rta_shocks_1985-2015_probas_mechanisms.csv") %>% 
  mutate(spatial_scale = "rta") %>% 
  dplyr::select(names(shock_probas_r_cs_mec))
change_probas_rta_cs_mec <- read_csv("data/long-term_change/rta_long-term_change_1985-2015_probas_mechanisms.csv") %>% 
  dplyr::select(names(change_probas_r_cs_mec))

# rbind
shock_probas <- rbind(shock_probas_g_cs_mec, shock_probas_r_cs_mec, shock_probas_rta_cs_mec)
long_term <- rbind(change_probas_g_cs_mec, change_probas_r_cs_mec, change_probas_rta_cs_mec) %>% 
  pivot_wider(names_from = "type", values_from = "probas")
dat_mec <- left_join(long_term, shock_probas, by = c("climates", "regions", "time_window", "spatial_scale"))


### C. Data across metrics ----
# cbind both datasets
dat_ecology_rta <- left_join(dat, dat_mec, by = c("regions", "climates", "time_window", "spatial_scale")) %>% 
  filter(time_window == 30) %>% 
  dplyr::select(regions, climates, time_window, spatial_scale,
                at_least_two_change_10, at_least_two_shocks,
                at_least_two_10_down, at_least_one_10_up_down,
                at_least_two_shocks_down, at_least_one_shock_up_down) %>%
  mutate(at_least_one_10_up_down = 1-at_least_one_10_up_down,
         at_least_one_shock_up_down = 1-at_least_one_shock_up_down) %>%
  pivot_longer(5:10, names_to = "type", values_to = "proba") %>% 
  mutate(scale = ifelse(str_detect(type, "shock")==TRUE, "shock", "long-term"),
         mechanism = ifelse(str_detect(type, "down")==TRUE, "synchrony", "stability"),
         mechanism = ifelse(str_detect(type, "up_down")==TRUE, "compensation", mechanism))

write.csv(dat_ecology_rta,
          file = "data/ecological_data/cross-sector_climate_exposure_cross_scales.csv",
          row.names = F)


### D. Overview of results ----

# example of the US RTAs
png("figures/combined/US_RTAs_1.png",
    width = 10*200, height = 7*200, res = 200)
dat_ecology_rta %>% 
  filter(regions %in% c("United States", "Canada", "Mexico", "1087"),
         climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
  ggplot(aes(y = regions, x = proba)) + geom_boxplot() +
  geom_point(aes(shape = scale, color = mechanism), fill = "grey", stroke = 1) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = c("darkorchid2","grey40","forestgreen")) +
  theme_bw() + 
  facet_wrap(~scale) +
  ylab("regions") + xlab("cross-sector exposure probabilities")
dev.off()

png("figures/combined/US_RTAs_2.png",
    width = 10*200, height = 7*200, res = 200)
dat_ecology_rta %>% 
    filter(regions %in% c("United States", "Honduras", "Guatemala",
                          "El Salvador", "Nicaragua", "Costa Rica",
                          "Dominican Republic", "27"),
           climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585")) %>% 
    ggplot(aes(y = regions, x = proba)) + geom_boxplot() +
    geom_point(aes(shape = scale, color = mechanism), fill = "grey", stroke = 1) +
    scale_shape_manual(values = c(21, 24)) +
    scale_color_manual(values = c("darkorchid2","grey40","forestgreen")) +
    theme_bw() + 
    facet_wrap(~scale) +
  ylab("regions") + xlab("cross-sector exposure probabilities")
dev.off()
  
# make matrix of country versus RTA
# load RTA database
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
  filter(climates %in% c("gfdl-esm4 ssp585","ipsl-cm6a-lr ssp585"),
         time_window == 30) %>% 
  group_by(scale, spatial_scale, regions) %>% 
  summarize(proba = median(proba, na.rm=T)) %>% 
  filter(spatial_scale!="global")
  
dat_ecology_cnt <- dat_ecology_rta %>% 
  filter(spatial_scale == "regions")
dat_ecology_rta <- dat_ecology_rta %>% 
  filter(spatial_scale == "rta")

dat_matrix <- left_join(dat_rta_list, dat_ecology_cnt, by = c("SOVEREIGN1" = "regions")) %>% 
  mutate(rta_id = as.character(rta_id))
dat_matrix <- left_join(dat_matrix, dat_ecology_rta, by = c("rta_id" = "regions","scale")) %>% 
  mutate(proba_ratio = proba.x/proba.y)

png("figures/combined/rta_exposures_shocks.png",
    width = 10*200, height = 15*200, res = 300)
dat_matrix %>% 
  filter(scale == "shock") %>%
  mutate(proba_ratio = ifelse(proba_ratio>2, 2, proba_ratio)) %>% 
  ggplot(aes(y = SOVEREIGN1, x = rta_id)) +
  geom_point(aes(fill = proba_ratio), shape =21) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 5)) +
  xlab("RTA ID") + ylab("Country")
dev.off()

png("figures/combined/rta_exposures_long-term.png",
    width = 10*200, height = 15*200, res = 300)
dat_matrix %>% 
  filter(scale == "long-term") %>%
  mutate(proba_ratio = ifelse(proba_ratio>2, 2, proba_ratio)) %>% 
  ggplot(aes(y = SOVEREIGN1, x = rta_id)) +
  geom_point(aes(fill = proba_ratio), shape =21) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 1, limits = c(0,2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 5)) +
  xlab("RTA ID") + ylab("Country")
dev.off()

png("figures/combined/rta_exposures_long-term_subset.png",
    width = 10*200, height = 7*200, res = 200)
dat_matrix %>% 
  filter(scale == "long-term",
         SOVEREIGN1 %in% c("United States","France","China","Madagascar","Sudan","Colombia","Canada",
                           "Australia","Spain","Argentina","Egypt","Germany","Bolivia","Japan",
                           "Ecuador","Mozambique","Russia","Philippines","Mexico",
                           "Norway","Brazil","South Africa","Namibia","India","Indonesia",
                           "Ukraine","Uruguay","Turkey","Sweden","Sri Lanka","Romania",
                           "Portugal","Pakistan","Nigeria","Netherlands","Libya","Liberia")) %>%
  mutate(proba_ratio = ifelse(proba_ratio>2, 2, proba_ratio)) %>% 
  ggplot(aes(y = SOVEREIGN1, x = rta_id)) +
  geom_point(aes(fill = proba_ratio), shape =21) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 1, limits = c(0,2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("RTA ID") + ylab("Country")
dev.off()

png("figures/combined/rta_exposures_shocks_subset.png",
    width = 10*200, height = 7*200, res = 200)
dat_matrix %>% 
  filter(scale == "shock",
         SOVEREIGN1 %in% c("United States","France","China","Madagascar","Sudan","Colombia","Canada",
                           "Australia","Spain","Argentina","Egypt","Germany","Bolivia","Japan",
                           "Ecuador","Mozambique","Russia","Philippines","Mexico",
                           "Norway","Brazil","South Africa","Namibia","India","Indonesia",
                           "Ukraine","Uruguay","Turkey","Sweden","Sri Lanka","Romania",
                           "Portugal","Pakistan","Nigeria","Netherlands","Libya","Liberia")) %>%
  mutate(proba_ratio = ifelse(proba_ratio>2, 2, proba_ratio)) %>% 
  ggplot(aes(y = SOVEREIGN1, x = rta_id)) +
  geom_point(aes(fill = proba_ratio), shape =21) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 1, limits = c(0,2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("RTA ID") + ylab("Country")
dev.off()
