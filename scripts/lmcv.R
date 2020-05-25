library(tidyverse)

housing_df <- read_csv('data/housing.csv')

logprediction_m1 <- function(i){
  m1_not_i <- lm(log(price) ~ 1, 
                 data = slice(housing_df, -i)
  )
  mu <- coef(m1_not_i)
  stdev <- sigma(m1_not_i)
  y_i <- slice(housing_df, i) %>% pull(price)
  dnorm(log(y_i), mean = mu, sd = stdev, log = T)
  
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
n <- nrow(housing_df)
map_dbl(seq(n), logprediction_m1) %>% 
  sum()


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
logprediction_m0 <- function(i){
  m0_not_i <- lm(price ~ 1, 
                 data = slice(housing_df, -i)
  )
  mu <- coef(m0_not_i)
  stdev <- sigma(m0_not_i)
  y_i <- slice(housing_df, i) %>% pull(price)
  dnorm(y_i, mean = mu, sd = stdev, log = T)
  
}

map_dbl(seq(n), logprediction_m0) %>% 
  sum()


