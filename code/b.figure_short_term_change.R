#### Short-term cross-sector resource change figures
#### Coding: Aurore A. Maureaud, June 2024

# load libraries
library(here)
library(tidyverse)
library(ggplot2)


################################################################################
#### 1. GLOBAL SHOCKS
################################################################################

shock_ts_g_cs <- read_csv("data/short-term_change/global_shocks_time_series_2010-2019.csv")

shock_ts_g_cs %>% 
  filter(output_variable != "ptotww",
         output_variable == "maize") %>%
  mutate(shock = ifelse(is.na(shock),0,shock),
         shock_up = ifelse(is.na(shock_up),0,shock_up),
         shock_down = ifelse(is.na(shock_down),0,shock_down)) %>% 
  mutate(climates = paste(climate_model, experiment_climate)) %>% 
  group_by(climates, years, output_variable) %>% 
  summarize(shock = mean(shock),
            shock_down = mean(shock_down),
            shock_up = mean(shock_up)) %>% 
  pivot_longer(c("shock","shock_down","shock_up"), names_to = "shock_type",values_to = "p_shock") %>% 
  ggplot(aes(x = years, y = p_shock, fill = shock_type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  ylim(0,1) + ylab("Shock probability") +
  scale_fill_manual(breaks = c("shock","shock_down","shock_up"), 
                    labels = c("shock","down","up"),
                    values = c("grey50","mediumaquamarine","lightsalmon")) +
  facet_wrap(~ climates)


shock_ts_g_cs %>% 
  filter(output_variable != "ptotww") %>%
  mutate(shock = ifelse(is.na(shock),0,shock),
         shock_up = ifelse(is.na(shock_up),0,shock_up),
         shock_down = ifelse(is.na(shock_down),0,shock_down)) %>% 
  mutate(climates = paste(climate_model, experiment_climate)) %>% 
  filter(climates == "gfdl-esm4 ssp585") %>% 
  group_by(climates, years, output_variable) %>% 
  summarize(shock = mean(shock),
            shock_down = mean(shock_down),
            shock_up = mean(shock_up)) %>% 
  pivot_longer(c("shock","shock_down","shock_up"), names_to = "shock_type",values_to = "p_shock") %>% 
  ggplot(aes(x = years, y = p_shock, fill = shock_type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  ylim(0,1) + ylab("Shock probability") +
  scale_fill_manual(breaks = c("shock","shock_down","shock_up"), 
                    labels = c("shock","down","up"),
                    values = c("grey50","mediumaquamarine","lightsalmon")) +
  facet_wrap(~ output_variable)
