library(tidyverse)
library(splines)
library(magrittr)

# Looking at splines ---------------------------------------------------

b_spline <- function(x, knots, show_piece = F){
  
  stopifnot(length(knots) == 5)
  
  .b_spline <- function(x){
    if (x >= knots[1] & x < knots[2]) {
      piece <- 1
      u <- (x-knots[1])/(knots[2] - knots[1])
      y <- 1/6 * u^3 
      
    } else if (x >= knots[2] & x < knots[3]) {
      piece <- 2
      u <- (x-knots[2])/(knots[3] - knots[2])
      y <- 1/6 * (1 + 3*u + 3*u^2 - 3*u^3)
      
    } else if (x >= knots[3] & x < knots[4]) {
      piece <- 3
      u <- (x-knots[3])/(knots[4] - knots[3])
      y <- 1/6 * (4 - 6*u^2 + 3*u^3)
      
    } else if (x >= knots[4] & x <= knots[5]) {
      piece <- 4
      u <- (x-knots[4])/(knots[5] - knots[4])
      y <- 1/6 * (1 - 3*u + 3*u^2 - u^3)
    }
    else {
      piece <- 0
      y <- 0 
    } 
    
    if (!show_piece) return(y)
    
    c(y, piece)
    
  }
  
  if (!show_piece){
    tibble(x = x, 
           y = map_dbl(x, .b_spline)
    )
  } else {
    map(x, .b_spline) %>% 
      do.call(rbind, .)%>%
      set_colnames(c('y', 'segment')) %>% 
      as_tibble() %>% 
      mutate(x = x) %>% 
      mutate_at(vars(segment), as.factor) %>% 
      select(x, everything())
  }
  
}

x <- seq(-1, 1, length.out = 1000)
knots <- seq(-0.5, 0.5, length.out = 5)
b_spline(x, knots = knots, show_piece = T) %>%
  ggplot(mapping = aes(x = x, 
                       y = y, 
                       colour = segment)) +
  geom_point(size = 0.5) +
  theme(legend.position="none")

# a range of splines
x <- seq(-2, 2, length.out = 1000)
knots <- seq(-2, 2, by = 0.5)

imap_dfr(seq(1,length(knots)-4),
         ~b_spline(x, knots = knots[.x:(.x+4)]) %>% 
           mutate(k = .y)
) %>% ggplot(mapping = aes(x = x, y = y, group = k)) + geom_line()

# Using splines::bs
bs(x, knots  = knots) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  mutate(x=x) %>% 
  gather(k, y, -x) %>% ggplot(mapping = aes(x = x, y = y, group = k)) + geom_line()

# a random spline functions
rbspline <- function(x, knots){
  tibble(x = x,
         y = bs(x, knots = knots) %*% rnorm(length(knots) + 3) %>% 
           as.vector()
  )
  
}

rbspline_examples <- function(i){
  set.seed(i)
  Df <- imap(rerun(5, rbspline(x, knots)), 
             ~mutate(., example = .y)) %>% 
    bind_rows() %>% 
    mutate_at(vars(example), as.character)
  
  p <- Df %>% ggplot(mapping = aes(x = x, y = y, colour = example)) + 
    geom_line() +
    theme(legend.position="none")
  
  p
}

rbspline_examples(102)


# Splines regression ------------------------------------------------------

# evenly spaced knots 
knots <- seq(-500, 2500, by = 500)
M_bs <- lm(mean_fix ~ bs(Time, knots = knots)*Object, 
           data=eyefix_df_avg)

# plot the model
eyefix_df_avg%>%
  add_predictions(M_bs) %>%
  ggplot(mapping = aes(x = Time, group = Object, colour = Object)) +
  geom_point(aes(y = mean_fix), size = 0.5, alpha = 0.5) +
  geom_line(aes(y = pred))


# Allow knots to be chosen
M_bs_df10 <- lm(mean_fix ~ bs(Time, df = 10)*Object, 
                data=eyefix_df_avg)

with(eyefix_df_avg,
     attr(bs(Time, df = 10), 'knots')
)



# rbf ---------------------------------------------------------------------
rbf <- function(x, centres, sigma = 1.0){
  map_dfc(centres, 
          ~exp(-(x-.)^2/(2*sigma^2))
  ) %>% as.matrix()
}

random_rbf <- function(x, centres, sigma = 1.0){
  rbf(x, centres, sigma) %>% 
    multiply_by_matrix(rnorm(length(centres))) %>% 
    as.data.frame() %>% 
    set_names('y') %>% 
    mutate(x = x)
}

x <- seq(-3, 3, length.out = 1000)

random_rbf_examples <- function(i, sigma){
  set.seed(i)
  Df <- imap(rerun(5, random_rbf(x, centres = centres, sigma = sigma)), 
             ~mutate(.x, example = .y)) %>% 
    bind_rows() %>% 
    mutate_at(vars(example), as.character)
  
  p <- Df %>% ggplot(mapping = aes(x = x, y = y, colour = example)) + 
    geom_line() +
    theme(legend.position="none")
  
  p
}

centres <- seq(-2, 2, by = 0.5)
random_rbf_examples(1010, 0.25)


# RBF regression ----------------------------------------------------------
rbf <- function(x, centres, sigma = 1.0){
  map(centres,
      ~exp(-(x-.)^2/(2*sigma^2))
  ) %>% do.call(cbind, .)
}

centres <- seq(-1000, 3000, by = 500)
M <-lm(mean_fix ~ rbf(Time, centres = centres, sigma = 500)*Object, 
       data=eyefix_df_avg)
eyefix_df_avg %>% 
  add_predictions(M) %>% 
  ggplot(mapping = aes(x = Time, colour = Object, group = Object)) +
  geom_point(aes(y = mean_fix), size = 0.5, alpha = 0.5) +
  geom_line(aes(y = pred)) + 
  theme_minimal()



# GSSVocab ----------------------------------------------------------------

GSSvocab <- read_csv('data/GSSvocab.csv')
gssvocab <- GSSvocab %>%
  group_by(age) %>%
  summarize(vocab = mean(vocab, na.rm=T)) %>%
  ungroup() %>%
  drop_na()

ggplot(gssvocab,
       aes(x = age, y = vocab)
) + geom_point()

df_seq <- seq(3, 30) %>% set_names(.,.)

M_gssvocab <- map(df_seq,
                  ~lm(vocab ~ ns(age, df = .),
                      data = gssvocab)
)

aic_results <- map_dbl(M_gssvocab, aic_c) %>%
  enframe(name = 'df', value = 'aic')

aic_results %>%
  mutate(aic = aic - min(aic)) %>% 
  ggplot(aes(x = df, y = aic)) + 
  geom_point() +
  geom_segment(aes(x=df, xend=df, y=0, yend=aic)) +
  scale_x_discrete(limits=names(df_seq)) + 
  xlab('Number of knots') +
  ylab('AIC - min(AIC)') + 
  theme(axis.text.x = element_text(size=5),
        axis.text.y = element_text(size = 8),
        axis.title.y =element_text(size=8)) 


model_set <- c('3', '4', '5', '6', '10', '15')
imap(M_gssvocab[model_set],
     function(m,y) {gssvocab %>% add_predictions(m) %>% mutate(df=y)}
) %>% bind_rows() %>% 
  mutate(df = factor(df, levels = model_set)) %>% 
  ggplot(aes(x = age)) +
  geom_point(aes(y = vocab), size = 0.5, alpha = 0.5) +
  geom_line(aes(y = pred), col='red') +
  facet_wrap(~df) +
  theme_minimal()
