## ------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(modelr)
library(magrittr)
library(brms)
theme_set(theme_classic())


# get rent99 data ---------------------------------------------------------

rent_df <- read_csv('data/rent99.csv')
