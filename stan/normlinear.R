library(tidyverse)
library(rstan)

math_df <- read_csv('data/MathPlacement.csv')

math_df_2 <- math_df %>% 
  select(SATM, PlcmtScore) %>% 
  na.omit()

x <- pull(math_df_2, SATM)
y <- pull(math_df_2, PlcmtScore)
  
math_data_2 <- list(
  x = x,
  y = y,
  N = length(x),
  tau = 50, omega = mad(y), kappa = 3
)

M_math_2 <- stan('stan/normlinear.stan', data = math_data_2)

summary(M_math_2, pars = c('beta_0'), probs = c(0.05, 0.95))$summary
