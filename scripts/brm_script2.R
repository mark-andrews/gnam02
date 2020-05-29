## ------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(modelr)
library(magrittr)
library(brms)
theme_set(theme_classic())


# get rent99 data ---------------------------------------------------------

rent_df <- read_csv('data/rent99.csv')

# Exploratory analysis

# rent per area
rent_df %>%
    ggplot(aes(x = area, y = rent)) +
    geom_point()

# rentsqm per area
rent_df %>%
  ggplot(aes(x = area, y = rentsqm)) +
  geom_point()

# rentsqm per area per year
rent_df %>%
  mutate(decade = as.factor(ntile(yearc, 10))) %>% 
  ggplot(aes(x = area, y = rentsqm, colour = decade)) +
  geom_point() +
  stat_smooth(method = 'lm', se = F, fullrange = T) +
  facet_wrap(~decade)



# A model -----------------------------------------------------------------

Mrent <- brm(rentsqm ~ t2(area, yearc) + (1|district),
             data = rent99)
