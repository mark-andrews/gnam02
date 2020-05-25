library(tidyverse)
library(modelr)
library(magrittr)
library(splines)

# Some normal-nonlinear models -----------------------------------------

N <- 50
Df <- tibble(x = seq(-2, 2, length.out = N))

plotter <- function(f, sigma){
  Df %>% mutate(y = f(x) + rnorm(N, sd = sigma)) %>% 
    ggplot(Df, mapping = aes(x = x, y = y)) +
    geom_point(size = 0.5) +
    theme_classic() +
    theme(aspect.ratio = 1/2)  +  ylab('y') +
    xlab('x') 
}
exponential_f <- function(sigma = 1){
  plotter(f = exp, sigma = sigma)
}

logistic_f <- function(L = 1, sigma = 1, b = 1, a = 0){
  ilogit <- function(x) L/(1 + exp(-(b*x + a)))
  plotter(f = ilogit, sigma = sigma)
}

sinusoidal_f <- function(b = 1, sigma = 1){
  plotter(f = function(x) sin(b*x), sigma = sigma)
}

quadratic_f <- function(sigma = 1){
  plotter(f = function(x) -x^2, sigma = sigma)
}

mog_f <- function(sigma = 1){
  f <- function(x){
    x <- x * 2
    dnorm(x, mean = -1.5) + 1.5 * dnorm(x, mean = 1.5)
  }
  plotter(f = f, sigma = sigma)
}

exponential_f(sigma = 0.5)
logistic_f(sigma = 0.5, b = 3, L = 10)
sinusoidal_f(b = 1.5, sigma=0.15)
mog_f(sigma = 0.04)


# tanh functions ----------------------------------------------------------

Df <- local({
  b <- 3.0
  alpha <- 0.75
  beta <- 0.25
  tibble(x = seq(-10, 10, length.out = 50),
         y = b * tanh(alpha + beta*x) + rnorm(length(x), sd = 0.5))
})

tibble(x = seq(-3, 3, length.out = 1000),
       y = tanh(x)) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_line() + ylab('y') + xlab('x') 

ggplot(Df, aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  ylab('y') + xlab('x') 


# Polynomials --------------------------------------------------------------

get_polynomial_design_matrix <- function(K=3, xmin=-10, xmax=10, N=100, rescale = T){
  
  # Produce a matrix of x, and x raised to the power of 0 to K
  # If `rescale, the values of the powers of x are scaled to be between -2 and 2
  
  rescale_f <- function(x, new_min=-1, new_max=1){
    
    new_range <- new_max - new_min
    original_range <- max(x) - min(x)
    
    x <- x * new_range/original_range
    x - (min(x) - new_min)
  }
  
  x <- seq(xmin, xmax, length.out = N)
  Df <- map(x, ~.^seq(0, K)) %>%
    do.call(rbind, .) %>% 
    set_colnames(paste0('degree_', seq(0, K))) %>% 
    as_tibble() %>% 
    mutate(x = degree_1) %>% 
    select(x, everything())
  
  if (rescale){
    Df %>% mutate_at(vars(matches('^degree_[^0]$')), 
                     ~rescale_f(., new_min = -2, new_max = 2))
  } else {
    Df
  }
}

rpolynomial <- function(K = 5){
  beta <- rnorm(K + 1)
  beta <- beta/sum(beta)
  get_polynomial_design_matrix(K = K) %>%
    mutate(y = select(., starts_with('degree')) %>% 
             apply(1, function(x) sum(x*beta))
    ) %>% select(x, y) 
}


get_polynomial_design_matrix(K=5) %>% 
  pivot_longer(cols = starts_with('degree'), names_to = 'degree', values_to = 'y') %>% 
  ggplot(mapping = aes(x = x, y = y, colour = degree)) + geom_line()


rpolynomial_examples <- function(i){
  set.seed(i)
  Df <- imap(rerun(5, rpolynomial(K = 5)), 
             ~mutate(., example = .y)) %>% 
    bind_rows() %>% 
    mutate_at(vars(example), as.character)
  
  p <- Df %>% ggplot(mapping = aes(x = x, y = y, colour = example)) + 
    geom_line() +
    theme(legend.position="none")
  
  p
}


rpolynomial_examples(110)
rpolynomial_examples(114)
rpolynomial_examples(116)
rpolynomial_examples(117)



# Orthogonal ------------------------------------------------------------

x <- seq(-1, 1, length.out = 100)
y <- poly(x, degree = 5) # orthogonal
y %>%
  set_colnames(paste0('degree_', seq(ncol(y)))) %>% 
  as_tibble() %>% 
  mutate(x = x) %>% 
  gather(degree, y, starts_with('degree')) %>% 
  ggplot(mapping = aes(x = x, y = y, colour = degree)) + geom_line()



# Overfitting -------------------------------------------------------------

set.seed(101)
Df <- tibble(x = seq(-2, 2, length.out = 20),
             y = 0.5 + 1.5 * x + rnorm(length(x))
) 
Df %>% ggplot(aes(x,y)) + geom_point()

M_overfits <- map(seq(9), 
                  ~lm(y ~ poly(x, degree = .), data = Df)
)
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


map_dbl(M_overfits, AIC) %>%
  set_names(paste0('degree_', seq(M_overfits))) %>% 
  round(2)

M <- lm(y ~ poly(x, degree = 16, raw = F), data = Df)
Df_new <- tibble(x = seq(-2, 2, length.out = 1000))
Df_new %>% 
  add_predictions(M) %>% 
  ggplot() +
  geom_line(mapping = aes(x = x, y = pred)) +
  geom_point(Df, mapping= aes(x = x, y = y), col='red') 



# Spline ------------------------------------------------------------------

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

x <- seq(-2, 2, length.out = 1000)
knots <- seq(-2, 2, by = 0.5)

imap_dfr(seq(1,length(knots)-4),
         ~b_spline(x, knots = knots[.x:(.x+4)]) %>% 
           mutate(k = .y)
) %>% ggplot(mapping = aes(x = x, y = y, group = k)) + geom_line()

bs(x, knots  = knots) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  mutate(x=x) %>% 
  gather(k, y, -x) %>% ggplot(mapping = aes(x = x, y = y, group = k)) + geom_line()

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

knots <- seq(-500, 2500, by = 500)
M_bs <- lm(mean_fix ~ bs(Time, knots = knots)*Object, 
           data=eyefix_df_avg)

eyefix_df_avg%>%
  add_predictions(M_bs) %>%
  ggplot(mapping = aes(x = Time, group = Object, colour = Object)) +
  geom_point(aes(y = mean_fix), size = 0.5, alpha = 0.5) +
  geom_line(aes(y = pred))


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


