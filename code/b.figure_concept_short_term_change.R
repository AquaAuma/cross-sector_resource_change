#### Long-term cross-sector resource change figures
#### Coding: Aurore A. Maureaud, April 2024

# load libraries
library(raster)
library(ncdf4)
library(here)
library(tidyverse)
library(ggplot2)
library(sf)
sf_use_s2(FALSE)
library(gridExtra)

# load dataset
dat <- read_csv("data/long-term_change/global_time_series_2010-2019_2090-2099.csv") %>% 
  filter(experiment_climate == "ssp585",
         output_variable != "ptotww",
         climate_model == "gfdl-esm4")

sub_dat <- dat %>% 
  filter(eco_model == "crover",
         output_variable == "maize")

# sub_dat$z_score <- (sub_dat$percent_diff-mean(sub_dat$percent_diff, na.rm=T))/sd(sub_dat$percent_diff, na.rm=T)
# ggplot(sub_dat, aes(x = years, y = percent_diff)) + geom_line() + geom_point()
# ggplot(sub_dat, aes(x = years, y = z_score)) + geom_line() + geom_point()

# detrending time-series
lmd <- lm(sub_dat$percent_diff ~ sub_dat$years)

# residuals from linear model
sub_dat$res <- (resid(lmd)-mean(resid(lmd)))/sd(resid(lmd))
ggplot(sub_dat, aes(x = years, y = res)) + geom_line() + 
  geom_point(data = sub_dat[(sub_dat$res<(-2) | sub_dat$res>2),], aes(x = years, y = res), col = "red") +
  geom_text(data = sub_dat[(sub_dat$res<(-2) | sub_dat$res>2),], aes(label = years), col = "red",
  vjust = -0.1, hjust = -0.1) +
  geom_hline(yintercept = -2, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "grey50") +
  theme_bw() +
  ggtitle(paste0(sub_dat$eco_model[1], "  R2=", round(summary(lmd)$adj.r.squared, 2))) +
  xlab("Years") + ylab("z-score of residuals")
  

