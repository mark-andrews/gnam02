library(tidyverse)
library(rstan)

y <- read_csv('data/MathPlacement.csv') %>% select(SATM) %>% na.omit() %>% pull(SATM)

N <- length(y)

math_data <- list(y = y, N = N, nu = 50, tau = 25, phi = 0, omega = 10, kappa = 5)
M_math <- stan('stan/normal.stan', data = math_data)

summary(M_math, pars = 'mu', probs = c(0.05, 0.95))$summary