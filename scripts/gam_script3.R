## ------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(splines)
library(lme4)

# Understanding linear mixed effects

Msleep <- lmer(Reaction ~ Days + (Days|Subject),
               data = sleepstudy)

sleepstudy %>% 
  add_predictions(Msleep) %>% 
  ggplot(aes(x = Days, y = Reaction, col = Subject)) + 
    geom_point() + 
    geom_line(aes(y = pred)) +
    facet_wrap(~Subject)


# Load random effects tanh data sets --------------------------------------
data_df <- read_csv('data/raneftanh.csv')

# plot it -----------------------------------------------------------------
ggplot(data_df,
       aes(x = x, y = y, col = v)) + geom_point() + facet_wrap(~v)

# cubic b-spline expansion ------------------------------------------------

add_basis_functions <- function(x, k = 5){
  bs(x, degree = k) %>%
    as_tibble() %>% 
    rename_all(~paste0('phi_', .)) %>%
    mutate_all(as.numeric)
}
bs_df <- data_df %>% pull(y) %>% add_basis_functions(k = 5)
data_df <- bind_cols(data_df, bs_df)

# look at the basis functions
data_df %>% 
  select(y, starts_with('phi')) %>% 
  pivot_longer(cols = -y,
               names_to = 'phi',
               values_to = 'f') %>% 
  ggplot(aes(x = y, y = f, colour = phi)) + geom_line()

# Use lmer
M <- lmer(y ~ phi_1 + phi_2 + (phi_1 + phi_2|v), 
          data = data_df)

# Use predictions
data_df %>% 
  add_predictions(M) %>% 
  ggplot(aes(x = x, y = pred, colour = v)) +
    geom_point() + facet_wrap(~v)

# The same as above but fixed ---------------------------------------------

data_df %>% 
  add_predictions(lm(y ~ bs(x, degree = 5), data = data_df)) %>% 
  ggplot(aes(x = x, y = pred)) + geom_line()

df_q <- bind_cols(data_df, ns(data_df$x, df=5) %>% as_tibble())

# population average curve (approximately calculated using `lm` not `lmer`)
data_df %>% 
  add_predictions(lm(y ~ ns(x, df = 5), data = data_df)) %>% 
  ggplot(aes(x = x, y = pred)) + geom_line()

# random variations around population average curve
# ignore warnings
df_q %>% 
  add_predictions(lmer(y ~ `1` + `2` + `3` + `4` + `5` + 
                         (`1` + `2` + `3` + `4` + `5`|v), data = df_q)) %>% 
  ggplot(aes(x = x, y = pred, colour = v)) + geom_line() + facet_wrap(~v)

