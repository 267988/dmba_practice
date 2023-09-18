install.packages("tidyverse")
library("tidyverse")
install.packages("gapminder")
library("gapminder")
df <- gapminder
# Exercises:
# 1. Which country had the lowest life expectancy in the year you were born?
gapminder
df1 <- gapminder %>% filter(gapminder$year == 1992) %>% arrange(lifeExp)
df1
# 2. Which continent had the lowest average life expectancy in the year you were born?
df2 <- gapminder %>% filter(gapminder$year == 1992) %>% arrange(lifeExp) %>%
  group_by(continent) %>% summarise(meanLifeExp=mean(lifeExp))
df2

# 3. ADVANCED: Count the number of countries in each continent. https://dplyr.tidyverse.org/reference/count.html
df3 <- gapminder %>% group_by(continent,country) %>% 
  summarise(NrOfCountries=1) %>% 
  summarise(NrOfCountries=sum(NrOfCountries))
df3                                    
#df3 <- gapminder %>% group_by(continent) %>% count(country) 
# Den her fikk jeg ikke til å fungere med count

# 4. ADVANCED: Find the five most populous countries in the last year of the dataset. 
df4 <- gapminder %>% filter(year == max(year)) %>% arrange(desc(pop)) %>%
  slice_head(n=5)
df4

# Maybe use  https://dplyr.tidyverse.org/reference/row_number.html
# 5. HARD: https://dplyr.tidyverse.org/reference/lead-lag.html
# a. Which country had the largest positive change in life expectancy from one year to another?

df5 <- gapminder %>% mutate(life_exp_change = lifeExp - lag(lifeExp)) %>%
  filter(life_exp_change == max(life_exp_change, na.rm = TRUE))
df5

# b. Which country had the largest positive change in life expectancy in a ten year period?

df5b <- gapminder %>% mutate(life_exp_change = lifeExp - lag(lifeExp, n=10)) %>%
  filter(life_exp_change == max(life_exp_change, na.rm = TRUE))
df5b

# 6. Tidy the world_bank_pop dataset
install.packages("world_bank_pop")
view(world_bank_pop)
wb_tidy <- world_bank_pop %>%
  pivot_longer("2000" : "2017", names_to = "year") %>%
  pivot_wider(id_cols = c("country", "year"), names_from = indicator, values_from = value)
view(wb_tidy)

# 7. ADVANCED: enrich the above dataset with https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv

countries <- read.csv("/Users/nikolaydimov/Documents/Master i e-businessteknologi og Cybersikkerhet/BTS4210-1 23H Data Mining and Business Analytics/Practice/rpractice/all.csv")
wb_tidy <- wb_tidy %>% rename(alpha.3 = country)
wb_tidy

wb_enriched <- wb_tidy %>% left_join(select(countries, alpha.3, name)) %>% select(name, everything())
wb_enriched

#Moving name column to the most left by adding "select(name, everything())"

# Enriching everything
wb_enriched2 <- wb_tidy %>% inner_join(countries)
wb_enriched2



