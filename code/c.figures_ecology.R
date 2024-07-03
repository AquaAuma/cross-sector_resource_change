#### Manuscript figures
#### Coding: Aurore A. Maureaud, July 2024

# load libraries
library(here)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggExtra)
library(ggrepel)
library(GGally)


################################################################################
#### 1. ARE COUNTRIES EXPERIENCING SHORT- AND LONG-TERM CROSS-SECTOR CHANGES?
################################################################################

## load data
# countries
shock_probas_r_cs <- read_csv("data/short-term_change/countries_shocks_2010-2019_probas.csv")
change_probas_r_cs <- read_csv("data/long-term_change/countries_long-term_change_2010-2019_probas.csv") %>% 
  dplyr::select(-spatial_scale)

# global
shock_probas_g_cs <- read_csv("data/short-term_change/global_shocks_2010-2019_probas.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(shock_probas_r_cs))
change_probas_g_cs <- read_csv("data/long-term_change/global_long-term_change_2010-2019_probas.csv") %>% 
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
png(paste0("figures/combined/example2_label.png"),
    width = 6*200, height = 4*200, res = 200)
dat %>% filter(time_window == 30) %>% 
  ggplot(aes(y = at_least_two_change_25, x = at_least_two_shocks)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ climates) +
  geom_point(data = dat[dat$regions=="global" & dat$time_window==30,], 
             aes(y = at_least_two_change_25, x = at_least_two_shocks),
             col = "red") +
  theme_bw() + xlim(0,1) + ylim(0,1) +
  ylab("Probability of at least 2 res. changing by >25%") + xlab("Probability of at least 2 shocks") +
geom_text_repel(data = dat[dat$time_window==30 & dat$at_least_two_shocks>0.5 & dat$at_least_two_change_25>0.5,], aes(label = regions),
                size=2, max.overlaps=20)
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
shock_probas_r_cs_mec <- read_csv("data/short-term_change/countries_shocks_2010-2019_probas_mechanisms.csv")
change_probas_r_cs_mec <- read_csv("data/long-term_change/countries_long-term_change_2010-2019_probas_mechanisms.csv") %>% 
  dplyr::select(-spatial_scale)

# global
shock_probas_g_cs_mec <- read_csv("data/short-term_change/global_shocks_2010-2019_probas_mechanisms.csv") %>% 
  mutate(regions = "global") %>% 
  dplyr::select(names(shock_probas_r_cs_mec))
change_probas_g_cs_mec <- read_csv("data/long-term_change/global_long-term_change_2010-2019_probas_mechanisms.csv") %>% 
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
  dplyr::select(-at_least_two_shocks_down, -at_least_three_shocks_down, -at_least_one_shock_up_down) %>% 
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
  ylab("Probability of at least 2 res. changing by 10% compensate") + xlab("Probability of at least 2 resources change by 10%") +
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
  ylab("Probability of at least 2 decreasing shocks") + xlab("Probability of at least 2 shocks compensate") +
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

