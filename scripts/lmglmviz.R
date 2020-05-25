library(tidyverse)
library(ggridges)

weight_df <- read_csv("data/weight.csv")

weight_male_df <- weight_df %>% 
  filter(gender == 'male')

# Histogram of the distribution of weights (kg) in a sample of men. The bin width is 5kg.', out.width='0.5\\textwidth'----
weight_male_df %>% 
  ggplot(aes(x = weight)) + 
  geom_histogram(binwidth = 5, colour = 'white')


## The histograms (a) and density plots (b) of the weights in a sample of men who are subdivided according to the quintile of their heights. In (c), we plot the mean weight against the mean height in each quintile.
weight_male_df %>% 
  mutate(height_decile = ntile(height, 5)) %>% 
  ggplot(aes(x = weight)) + 
  geom_histogram(binwidth = 5, colour = 'white') +
  facet_wrap(~height_decile) +
  theme_minimal()

weight_male_df %>% 
  mutate(height_decile = ntile(height, 5)) %>% 
  ggplot(aes(x = weight, y = height_decile, group = height_decile)) + 
  geom_density_ridges(bandwidth = 10) + 
  ylab('Height quintile')

weight_male_df %>% 
  mutate(height_decile = ntile(height, 5)) %>%
  group_by(height_decile) %>% 
  summarize(height = mean(height),
            weight = mean(weight)) %>% 
  ggplot(aes(x = height, y = weight)) + geom_point()



affairs_df <- read_csv('data/affairs.csv') %>%
  mutate(cheater = affairs > 0)

## Each bar in each plot shows the proportion of people in the relevant group who have had an affair or not in the past year. (a) The proportions for female and males. (b) The proportions according to the rating of the happiness of the marriage. (c) The proportions according to the marriage rating for females and males.'----

affairs_df %>%
  ggplot(aes(x = gender, fill = cheater)) + 
  geom_bar(stat = 'count', position = 'fill') +
  theme_minimal() + 
  theme(legend.position = 'none')

affairs_df %>% 
  ggplot(aes(x = rating, fill = cheater)) + 
  geom_bar(stat = 'count', position = 'fill') + 
  theme_minimal() + 
  theme(legend.position = 'none')

affairs_df %>% 
  ggplot(aes(x = rating, fill = cheater)) +
  geom_bar(stat = 'count', position = 'fill') +
  facet_wrap(~gender) + 
  theme_minimal()

