## Code to compute solution to quadratic equation of the form ax^2 + bx + c
## define the variables
a <- 3
b <- 2
c <- -1
## now compute the solution
(-b + sqrt(b^2 - 4*a*c)) / (2*a)
(-b - sqrt(b^2 - 4*a*c)) / (2*a)

##DATA VISUALIZATION##
##------------------------------------------------------------------------------------##
## Data Visualization in Practice ##
##................................##

gapminder %>% as_tibble()
gapminder %>% filter(year == 2015 & country %in% c("Sri Lanka" , "Turkey")) %>% select(country , infant_mortality)

gapminder %>% filter(year == 1962) %>% ggplot(aes(fertility , life_expectancy , color = continent)) + geom_point()

gapminder %>% filter(year %in% c(1962 , 2012)) %>% ggplot(aes(fertility , life_expectancy , col = continent)) + geom_point() + facet_grid(.~year)

years <- c(1962,1970,1980,1990,2000,2012)
continents <- c("Europe" , "Asia")
gapminder %>% filter(year %in% years & continent %in% continents) %>% ggplot(aes(fertility , life_expectancy , col = continent)) + geom_point() + facet_wrap(~year)

gapminder %>% filter(year %in% years & continent %in% continents) %>% ggplot(aes(fertility , life_expectancy , col = continent)) + geom_point() + facet_wrap(~year, scales = "free")

gapminder %>% filter(country == "United States") %>% ggplot(aes(year,fertility)) + geom_point()

countries <- c("South Korea","Germany")
gapminder %>% filter(country %in% countries & !is.na(fertility)) %>% ggplot(aes(year, fertility, col = country)) + geom_line()

countries <- c("South Korea","Germany")
labels <- data.frame(country = countries, x = c(1975,1965), y = c(60,72))
gapminder %>%  filter(country %in% countries) %>%  ggplot(aes(year, life_expectancy, col = country)) +  geom_line() +  geom_text(data = labels, aes(x, y, label = country), size = 5) +  theme(legend.position = "none")

## Data Transformations ##
##................................##

gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

gapminder

## 9.5.1 Log Transformation ##
##................................##

past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black")

gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")
