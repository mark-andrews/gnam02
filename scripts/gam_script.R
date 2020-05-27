## ------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(modelr)
library(magrittr)
theme_set(theme_classic())



# Load mcycle dat ---------------------------------------------------------
mcycle <- MASS::mcycle

mcycle %>% 
  ggplot(aes(x = times, y = accel)) +
  geom_point()


library(mgcv)

# A linear model
M_0 <- gam(accel ~ times, data = mcycle)


# A thin plate spline basis function model
M_1 <- gam(accel ~ s(times), data = mcycle)


plot(M_1, residuals = T)

summary(M_1)$s.table


M_1$rank


# Set the rank
M_2 <- gam(accel ~ s(times, k = 5), data = mcycle)
M_2$rank

plot(M_2, residuals = T)

# Optimize the rank by AIC
M_k_seq <- map(seq(3, 20) %>% set_names(.,.), 
               ~gam(accel ~ s(times, k = .), data = mcycle))
model_aic <- map_dbl(M_k_seq, AIC)
which.min(model_aic)

# check
gam.check(M_2)
k.check(M_2)

# smoothing penalties

plot(gam(accel ~ s(times, sp = 1e-1), data = mcycle), residuals = T)
plot(gam(accel ~ s(times, sp = 1), data = mcycle), residuals = T)
plot(gam(accel ~ s(times, sp = 1e1), data = mcycle), residuals = T)
plot(gam(accel ~ s(times, sp = 1e2), data = mcycle), residuals = T)

# Optimize it by AIC
sp_seq <- 10^seq(-5, 0)
M_sp_seq <- map(sp_seq,
                ~gam(accel ~ s(times, sp = .), data = mcycle)
)
model_sp_aic <- map_dbl(M_sp_seq, AIC) %>% 
  set_names(sp_seq)

# What mgcv does itself
c(M_1$sp, M_2$sp)




# Eye tracking ------------------------------------------------------------


# Get the data ------------------------------------------------------------

eyefix_df <- read_csv('data/funct_theme_pts.csv')

eyefix_df_avg <- eyefix_df %>% 
  group_by(Time, Object) %>% 
  summarize(mean_fix = mean(meanFix)) %>% 
  ungroup()

eyefix_df_avg_targ <- filter(eyefix_df_avg, Object == 'Target')



# Fit it ------------------------------------------------------------------

# linear model
M <- gam(mean_fix ~ Time, data = eyefix_df_avg_targ)

# spline model
M <- gam(mean_fix ~ s(Time), data = eyefix_df_avg_targ)
plot(M, residuals = T)

# spline model with 3 basis functions
M <- gam(mean_fix ~ s(Time, k = 3), data = eyefix_df_avg_targ)

# spline model with penalty = 0.1
M <- gam(mean_fix ~ s(Time, sp = 1.1), data = eyefix_df_avg_targ)




# Varying intercepts ??? ---------------------------------------------------

M_additive <- gam(mean_fix ~ s(Time) + Object, data = eyefix_df_avg)

crossing(Time = seq(-1000, 3000, by = 100), 
         Object = c('Target', 'Competitor', 'Unrelated')) %>% 
  add_predictions(M) %>% 
  ggplot(aes(x = Time, y = pred, group = Object, colour = Object)) +
  geom_line()


eyefix_df_avg %>% 
  ggplot(aes(x = Time, y = mean_fix)) +geom_point()

# Understanding interactions

insul_df <- read_csv('data/insulation.csv')

insul_df %>% 
  ggplot(aes(x = Temp, y = Gas, colour = Insul)) + geom_point() + stat_smooth(method = 'lm', se = F)

M <- lm(Temp ~ Gas * Insul, data = insul_df)


# The right way -----------------------------------------------------------
eyefix_df_avg %<>% mutate(Object = factor(Object))
M_interaction <- gam(mean_fix ~ Object + s(Time, by =Object), data = eyefix_df_avg)
# or
M <- gam(mean_fix ~ s(Time, Object, bs = 'fs'), data = eyefix_df_avg)

eyefix_df_avg %>% 
  add_predictions(M) %>% 
  ggplot(aes(x = Time, group = Object, colour = Object)) +
  geom_point(aes(y = mean_fix), size = 0.5, alpha = 0.5) +
  geom_line(aes(y = pred))

# Visualize it
vis.gam(M, plot.type = 'persp', theta = -200)

# Check fit -------------------------------------------------------------

M <- gam(mean_fix ~ Object + s(Time, by =Object, k = 30), data = eyefix_df_avg)
gam.check(M)

# Check concurvity --------------------------------------------------------------

concurvity(M)



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

Mg <- gam(cbind(success, failure) ~ s(distance, sp = 10),
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

M <- glm(cheater ~ gender*yearsmarried,
         data = affairs_df,
         family = binomial)

affairs_df %>% 
  add_predictions(M, type='response') %>% 
  ggplot(aes(x = yearsmarried, y = pred, colour=gender)) + geom_line()

M <- gam(cheater ~ gender + s(yearsmarried, by = gender, k = 8, sp = 1),
         data = affairs_df,
         family = binomial,
         method = 'REML')

crossing(yearsmarried = seq(0, 20, by = 0.1), gender = c('male', 'female')) %>% 
  add_predictions(M, type = 'response') %>% 
  ggplot(aes(x = yearsmarried, y = pred, group = gender, colour = gender)) + 
  geom_line()



# Spatial and continuous x continuous interactions -------------------------

meuse <- read_csv('data/meuse.csv')

M_1 <- gam(copper ~ s(x, y), data = meuse)

M_2 <- gam(copper ~ s(x, y) + s(elev) + s(dist), data = meuse)

# Continuous x continuous interactions....
x <- y <- seq(-1, 1, length= 20)
z <- outer(x, y, function(x, y) x*y)

persp(x, y, z, theta = 120)


# visualization -----------------------------------------------------------

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



# tensor smooths for interactions ----------------------------------------------------------

M <- gam(copper ~ te(x, y, elev), data = meuse)
plot(M)

M <- gam(copper ~ s(x,y) + s(elev) + ti(x, y, elev), data = meuse)
plot(M)
