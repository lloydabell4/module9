library(tidyverse)
library(modelr)
library(broom)

options(na.action = na.warn)

board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

ggplot(board_games, aes(year_published, average_rating)) +
  geom_point()

#create models based on lm
board_mod <- lm(average_rating ~ year_published, data = board_games)
coef(board_mod)

summary(board_mod)

#use broom
board_gamesa <- augment(board_mod)

glance(board_mod)

#find residuals
board_games1 <- board_games %>%
  add_residuals(board_mod)

#visual distribution of residuals - is distribution skewed? normal?
ggplot(board_games1, aes(resid)) + 
  geom_freqpoly(binwidth = .5)

#is there a pattern to the residuals across x? if so, model probably not great
ggplot(board_games1, aes(year_published, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

#################
board_games <- filter(board_games, min_age > 2 & min_age < 18)
board_games$min_age <- as.factor(board_games$min_age)

ggplot(board_games, aes(year_published, average_rating)) + 
  geom_point(aes(color = min_age))+
  geom_smooth(method = "lm")+
  facet_wrap(~min_age)

board_mod1 <- lm(average_rating ~ year_published + min_age, data = board_games)
board_mod2 <- lm(average_rating ~ year_published * min_age, data = board_games)

grid <- board_games %>% 
  data_grid(year_published, min_age) %>% 
  gather_predictions(board_mod1, board_mod2)
grid

ggplot(board_games, aes(year_published, average_rating, colour = min_age)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

board_games <- board_games %>% 
  gather_residuals(board_mod1, board_mod2)

ggplot(board_games, aes(year_published, resid, colour = min_age)) + 
  geom_point() + 
  facet_grid(model ~ min_age)

summary(board_mod1)
summary(board_mod2)
