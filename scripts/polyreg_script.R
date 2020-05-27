library(tidyverse)
library(magrittr)
library(modelr)

# load data ---------------------------------------------------------------
eyefix_df <- read_csv('data/funct_theme_pts.csv')
eyefix_df_avg <- eyefix_df %>% 
  group_by(Time, Object) %>% 
  summarize(mean_fix = mean(meanFix)) %>% 
  ungroup()

eyefix_df_avg_targ <- filter(eyefix_df_avg, Object == 'Target')

ggplot(eyefix_df_avg_targ,
       aes(x = Time, y = mean_fix)) + geom_point()



# Polynomial regression ----------------------------------------------------

# degree 9
M_9 <- lm(mean_fix ~ poly(Time, degree = 9), data = eyefix_df_avg_targ)
eyefix_df_avg_targ %>% 
  add_predictions(M_9) %>% 
  ggplot(aes(x = Time, y = pred)) + geom_line() +
  geom_point(aes(y = mean_fix), colour = 'red')


# degree 5
M_5 <- lm(mean_fix ~ poly(Time, degree = 5), data = eyefix_df_avg_targ)
eyefix_df_avg_targ %>% 
  add_predictions(M_5) %>% 
  ggplot(aes(x = Time, y = pred)) + geom_line() +
  geom_point(aes(y = mean_fix), colour = 'red')


# compare two models
summary(M_9)$r.sq
summary(M_9)$adj.r.squared

summary(M_5)$r.sq
summary(M_5)$adj.r.squared
anova(M_9, M_5)

AIC(M_9)
AIC(M_5)


# Polynomial with multiple predictors -------------------------------------

M_eyefix <- lm(mean_fix ~ poly(Time, 9)*Object, 
               data=eyefix_df_avg)
M_eyefix_null <- lm(mean_fix ~ Object + poly(Time, 9), data=eyefix_df_avg)
anova(M_eyefix_null, M_eyefix)
AIC(M_eyefix, M_eyefix_null)


# Overfitting -------------------------------------------------------------


# Some random data (actual normal linear)
set.seed(101)
Df <- tibble(x = seq(-2, 2, length.out = 20),
             y = 0.5 + 1.5 * x + rnorm(length(x))
) 
Df %>% ggplot(aes(x,y)) + geom_point()

# Fit 9 polynomial models of degree 1 to 9
M_overfits <- map(seq(9), 
                  ~lm(y ~ poly(x, degree = .), data = Df)
)

# Plot this
col_labels <- paste0('degree_', seq(length(M_overfits))) %>% 
  paste0(' (',round(map_dbl(M_overfits, ~summary(.)$r.sq), 2),')')

Df_new <- tibble(x = seq(-2, 2, length.out = 1000))

prediction_df <- map(M_overfits, ~predict(., newdata = Df_new)) %>% 
  as_tibble(.name_repair = 'minimal') %>% 
  set_colnames(col_labels) %>% 
  cbind(Df_new, .) %>% 
  gather(degree, prediction, starts_with('degree_')) %>% 
  mutate(degree = factor(degree, levels = col_labels))

prediction_df %>% 
  ggplot(mapping = aes(x = x)) +
  geom_point(data=Df, aes(x = x, y = y), size = 0.5, alpha = 0.5) +
  geom_line(aes(y = prediction), col='red') +
  facet_wrap(~degree) +
  theme_minimal()

# Let's look at AIC
map_dbl(M_overfits, AIC) %>%
  set_names(paste0('degree_', seq(M_overfits))) %>% 
  round(2)

aic_c <- function(model){
  K <- length(coef(model)) + 1
  N <- nrow(model$model)
  AIC(model) + (2*K*(K+1))/(N-K-1)
}

# Let's look at AIC_c

map_dbl(M_overfits, aic_c) %>%
  set_names(paste0('degree_', seq(M_overfits))) %>% 
  round(2)

# Runge's phenomenon
M <- lm(y ~ poly(x, degree = 16, raw = F), data = Df)
Df_new <- tibble(x = seq(-2, 2, length.out = 1000))
Df_new %>% 
  add_predictions(M) %>% 
  ggplot() +
  geom_line(mapping = aes(x = x, y = pred)) +
  geom_point(Df, mapping= aes(x = x, y = y), col='red') 

