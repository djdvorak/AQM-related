#library(ggplot2)
#library(plyr)
#library(dplyr)
#library(gapminder)
#install.packages("gapminder")
pop.delta <- gapminder %>%
  select(year, lifeExp, pop) %>%
  mutate(pop_change = 100*((pop - lag(pop)))/pop) %>%
  filter(year > 1952) %>%
  group_by(year) %>%
  summarise(pop_delta = mean(pop_change))

x <- select(gapminder, continent, year, lifeExp, pop)
x2 <- mutate(x, pop_change = 100*((pop - lag(pop)))/pop)
x3 <- filter(x2, year > 1952)
qplot(lifeExp,pop_change, data = x3, color = continent)