
install.packages("dplyr")
library(readr)
library(dplyr)

gapminder <- read_csv("Documents/Jeongyeon/Data_handling_course/3.3 Data handling with dplyr/gapminder_data.csv")
View(gapminder)


#Select()

gapminder <- select(gapminder, continent, country, year, pop, lifeExp, gdpPercap)
View(gapminder)

year_country_gdp <- select(gapminder, year, country, gdpPercap)
View(year_country_gdp)

#same way as the upper code, but if I wanna apply more pipes later, this is better
year_country_gdp <- gapminder %>% select(year, country, gdpPercap)
View(year_country_gdp)

smaller_gapminder <- select(gapminder, -continent)
View(smaller_gapminder)


tidy_gdp <- year_country_gdp %>% rename(gdp_per_capita = gdpPercap)
head(tidy_gdp)

# If I wanna change all the dataset name at once, click "command+F"

#Filter()

year_country_gdp_euro <- gapminder %>%
  filter(continent == "Europe") %>%
  select(year, country, gdpPercap)
View(year_country_gdp_euro)

europe_lifeExp_2007 <- gapminder %>%
  filter(continent == "Europe", year == 2007) %>%
  select(country, lifeExp)
View(europe_lifeExp_2007)


#Challenge 1
Challenge_1_Africa <- gapminder %>% 
  filter(continent == "Africa") %>% 
  select(year, country, lifeExp)

View(Challenge_1_Africa)
count(Challenge_1_Africa) 

#Question. When is it required to have "" mark? when it comes to variable, no need of quotation mark, but for character/string, I need to add ""

#Group_by() 
str(gapminder)
str(gapminder %>% group_by(continent))


#Summarize()
gdp_by_continents <- gapminder %>%
  group_by(continent) %>%
  summarize(mean_gdpPercap = mean(gdpPercap))
View(gdp_by_continents)


#Challenge 2. Calculate the average life expectancy per country. Which has the longest average life expectancy and which has the shortest average life expectancy?

Avg_lifeExp_country <- gapminder %>% 
  group_by(country) %>% 
  summarise(mean_lifeExp = mean(lifeExp), long_lifeExp = max(lifeExp), Short_lifeExp = min(lifeExp))
View(Avg_lifeExp_country)  

gdp_pop_bycontinents_byyear <- gapminder %>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            mean_pop = mean(pop),
            sd_pop = sd(pop))
View(gdp_pop_bycontinents_byyear)


#Count() and n()
gapminder %>%
  filter(year == 2002) %>%
  count(continent, sort = TRUE)

gapminder %>%
  group_by(year == 2002) %>%
  count(continent, sort = TRUE)

gapminder %>%
  group_by(continent) %>%
  summarize(se_le = sd(lifeExp)/sqrt(n()))

gapminder %>%
  group_by(continent) %>%
  summarize(
    mean_le = mean(lifeExp),
    min_le = min(lifeExp),
    max_le = max(lifeExp),
    se_le = sd(lifeExp)/sqrt(n()))


## keeping all data but "filtering" after a certain condition
# calculate GDP only for people with a life expectation above 25
gdp_pop_bycontinents_byyear_above25 <- gapminder %>%
  mutate(gdp_billion = ifelse(lifeExp > 25, gdpPercap * pop / 10^9, NA)) %>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            mean_pop = mean(pop),
            sd_pop = sd(pop),
            mean_gdp_billion = mean(gdp_billion),
            sd_gdp_billion = sd(gdp_billion))
View(gdp_pop_bycontinents_byyear_above25)


