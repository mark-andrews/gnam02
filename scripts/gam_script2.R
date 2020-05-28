## ------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(modelr)
library(magrittr)
library(mgcv)
theme_set(theme_classic())

# Glm gams ----------------------------------------------------------------


# golf data ---------------------------------------------------------------
golf_df <- read_csv('data/golf_putts.csv') %>% 
  mutate(failure = attempts - success,
         p = success/attempts)

M <- glm(cbind(success, failure) ~ distance,
         family = binomial,
         data = golf_df)

golf_df %>% 
  add_predictions(M, type = 'response') %>% 
  ggplot(aes(x = distance)) +
  geom_point(aes(y = p)) +
  geom_line(aes(y = pred))

# With a gam

Mg <- gam(cbind(success, failure) ~ s(distance, ),
          family = binomial,
          data = golf_df,
          method = "REML")

golf_df %>% 
  add_predictions(Mg, type = 'response') %>% 
  ggplot(aes(x = distance)) +
  geom_point(aes(y = p)) +
  geom_line(aes(y = pred))

# affairs data model ------------------------------------------------------

affairs_df <- read_csv('data/affairs.csv') %>% 
  mutate(cheater = affairs > 0,
         gender = factor(gender))

affairs_df %>% 
  group_by(yearsmarried, gender) %>% 
  summarise(ncheat = sum(cheater),
            n = n(), 
            pcheat = ncheat/n) %>% 
  ggplot(aes(x = yearsmarried, y = pcheat, colour = gender)) + geom_point() + stat_smooth(se=F, method='loess')

Mglm <- glm(cheater ~ gender*yearsmarried,
            data = affairs_df,
            family = binomial)

affairs_df %>% 
  add_predictions(Mglm, type='response') %>% 
  ggplot(aes(x = yearsmarried, y = pred, colour=gender)) + geom_line()

Mgam <- gam(cheater ~ gender + s(yearsmarried, by = gender, k = 8),
            data = affairs_df,
            family = binomial,
            method = 'REML')

crossing(yearsmarried = seq(0, 20, by = 0.1), gender = c('male', 'female')) %>% 
  add_predictions(Mgam, type = 'response') %>% 
  ggplot(aes(x = yearsmarried, y = pred, group = gender, colour = gender)) + 
  geom_line()



# Spatial and continuous x continuous interactions -------------------------

meuse <- read_csv('data/meuse.csv')

M_1 <- gam(copper ~ s(x, y), data = meuse)

plot(M_1)
plot(M_1, se=F)
plot(M_1, scheme = 1)
plot(M_1, scheme = 2)
vis.gam(M_1, view = c('x' ,'y'), plot.type = 'persp')
vis.gam(M_1, view = c('x' ,'y'), plot.type = 'contour')
vis.gam(M_1, view = c('x' ,'y'), plot.type = 'contour', too.far = 0.1)
vis.gam(M_1, view = c('x' ,'y'), plot.type = 'persp', se = 1)
vis.gam(M_1, view = c('x' ,'y'), plot.type = 'persp', theta = 100, phi = 0, r = 20.1)
vis.gam(M_1, view = c('x' ,'y'), plot.type = 'contour', nlevels = 3)

M_2 <- gam(copper ~ s(x, y) + s(elev) + s(dist), data = meuse)
plot(M_2)

# Continuous x continuous interactions ------------------------------------

# What is an interaction?

insul_df <- read_csv('data/insulation.csv')

insul_df %>% 
  ggplot(aes(x = Temp, y = Gas, colour = Insul)) + geom_point() + stat_smooth(method = 'lm', se = F)

M <- lm(Temp ~ Gas * Insul, data = insul_df)


# A product surface plot
x <- y <- seq(-1, 1, length= 20)
z <- outer(x, y, function(x, y) x*y)

persp(x, y, z, theta = 120)

# Some data simulated by Jan Vanhove
vanhove_df <- read_csv('data/example_interaction.csv')

vanhove_df %>% 
  mutate(v = ntile(pred2, 4)) %>% 
  ggplot(aes(x = pred1, y = outcome)) + 
    geom_point() + 
    stat_smooth(method = 'lm', se = F, fullrange = T) +
  facet_wrap(~v, nrow = 1)

# With a tensor smooth GAM
Mte <- gam(outcome ~ te(pred1, pred2), data = vanhove_df)
plot(Mte)
vis.gam(Mte, view = c('pred1' ,'pred2'), 
        plot.type = 'persp',
        theta = 120)


# test the interaction with ti
M1 <- gam(outcome ~ s(pred1) + s(pred2) + ti(pred1, pred2), 
          data = vanhove_df)
M2 <- gam(outcome ~ s(pred1) + s(pred2), 
          data = vanhove_df)

AIC(M2, M1)



# mixed effects gamms -----------------------------------------------------

library(gamm4)
library(lme4)

M <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)
Mg <- gamm4(Reaction ~ Days, random = ~(1|Subject), data = sleepstudy)
Mg1 <- gamm4(Reaction ~ s(Days), random = ~(1|Subject), data = sleepstudy)
summary(Mg1$gam)
summary(Mg1$mer)
plot(Mg$gam)

Mg2 <- gamm4(Reaction ~ s(Days), random = ~(1+Days|Subject), data = sleepstudy)
summary(Mg2$gam)
summary(Mg2$mer)
plot(Mg2$gam)

# artificial data
a <- 10
b <- 2.15
x <- seq(-2, 5, length.out = 100)

n <- length(x)
data_df <- map_dfc(seq(12),
                   function(.) {rnorm(1, mean = a) + rnorm(1, mean = b, sd = 0.5) * tanh(x) + rnorm(n, sd = 0.25)}) %>% 
  mutate(x = x) %>% 
  select(x, everything()) %>% 
  pivot_longer(cols = -x,
               names_to = 'v',
               values_to = 'y') %>% 
  mutate(v = as.factor(v))

ggplot(data_df,
       aes(x = x, y = y, col = v)) + geom_point() + facet_wrap(~v)

Mg1 <- gamm4(y ~ s(x), random = ~(x|v), data = data_df)
plot(Mg1$gam)
summary(Mg1$mer)
plot(predict(Mg1$mer))

data_df %>% 
  mutate(p = predict(Mg1$mer),
         q = predict(Mg1$gam)) %>% 
  ggplot(aes(x = x, y = p, col = v)) + geom_point() + facet_wrap(~v)

data_df %>% 
  mutate(q = predict(Mg1$gam)) %>% 
  ggplot(aes(x = x, y = q)) + geom_point()


# splines -----------------------------------------------------------------
# random effects splines
library(splines)
M <- lmer(Reaction ~ bs(Days,degree=3) + (bs(Days,degree=3)|Subject), 
          data = sleepstudy)

sleepstudy %>% 
  add_predictions(M) %>% 
  ggplot(aes(x = Days, y = pred, col = Subject)) + 
    geom_point() + 
    facet_wrap(~Subject)
