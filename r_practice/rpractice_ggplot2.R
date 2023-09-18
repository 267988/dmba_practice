library(tidyverse)
tilsyn <- read_delim("/Users/nikolaydimov/Documents/Master i e-businessteknologi og Cybersikkerhet/BTS4210-1 23H Data Mining and Business Analytics/Practice/rpractice/tilsyn.csv", delim = ";")
view(tilsyn)



# 3. Read in the tilsyn-data from last week, and create some interesting plots:
df1 <- tilsyn[c("navn","poststed","total_karakter","dato")]
df1 <- df1 %>% mutate(dato=parse_datetime(dato, format="%d%m%Y")) %>%
  mutate(year=year(dato), month=month(dato), day=day(dato)) %>%
  filter(year != 2015)
df1
# a. Total number of tilsyn per week across the period?
library(lubridate)
#df1$date_combined <- make_date(df1$year, df1$month, df1$day)
#df1 <- df1 %>% select(-date_combined) --deleting the columnd date_combined
#The above is not necessary because we have the dato column before parsing
df1$week_nr <- week(df1$dato)
df1

#"Grouping" by using table(df1$week_nr) function
count_by_week <- table(df1$week_nr)
print(count_by_week)
#Creating a dataframe from the table
count_by_week_df <- as.data.frame(count_by_week)
count_by_week_df

#Another way to do without using lubridate function date(df1$week_nr)
#Simply by grouping by the week number and summarizing how many times the rows occur
count_by_week_df2 <- df1 %>%
  group_by(week_nr) %>%
  summarize(number_of_tilsyn = n())

count_by_week_df2

#Creating a visualisation
p <- ggplot(count_by_week_df2, aes(x=week_nr, y=number_of_tilsyn)) +
  geom_line() +
  scale_y_continuous("Number of tilsyn") +
  scale_x_continuous("Week number of the year")
plot(p)

# b. Number of tilsyn of with each total_karakter across time.
dfb <-df1 %>% group_by(total_karakter, year) %>% count(name = "count_tilsyn") %>%
  arrange(total_karakter, year)
dfb

#alternatively

dfb2 <- df1 %>% group_by(total_karakter, year) %>% summarise(count_tilsyn = n()) %>%
  arrange(total_karakter, year)
dfb2

#Visualising it
p2 <- ggplot(dfb2, aes(x=year, y=total_karakter)) +
  geom_jitter(aes(size=count_tilsyn), width=1) +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Result from the control") +
  scale_size_continuous(name="Amount of controls")
p2

# c. Comparing cities with regards to number of controls across weeks in a given year.
# Using "df1$week_nr <- week(df1$dato)" from 3a
df1$week_nr <- week(df1$dato)

dfb3 <- df1 %>% group_by(week_nr, year, poststed) %>%
  summarize(number_of_controls = n())
dfb3

p3 <- ggplot(dfb3 %>% filter(poststed %in% c("STAVANGER","TRONDHEIM")),
             aes(x=week_nr, y=number_of_controls, color = poststed)) +
  geom_line() +
  scale_x_continuous(name="Week") +
  scale_y_continuous(name="Number of Controls") +
  scale_color_discrete(name="City") +
  facet_wrap(~year)
p3

# d. Visualise how the number of controls for a given city change in some chosen years

dfb4 <- df1 %>% group_by(week_nr, year, poststed) %>%
  summarize(number_of_controls = n())
dfb4
# Have to use factor(year) in order to convert it to categorical data
p4 <- ggplot(dfb4 %>% filter(poststed == "DRAMMEN" & year %in% 2017:2019),
             aes(x=week_nr, y=number_of_controls, color = factor(year))) +
  geom_line() +
  scale_x_continuous(name="Week") +
  scale_y_continuous(name="Number of Controls") +
  scale_color_discrete(name="Year")
p4

# Lets try do it per month and hope the graph is easier to read
#Couldnt read the months as Months instead of numbers

dfb5 <- df1 %>% group_by(month, year, poststed) %>%
  summarize(number_of_controls = n())
dfb5

p5 <- ggplot(dfb5 %>% filter(poststed == "DRAMMEN" & year %in% 2020:2022),
             aes(x=month, y=number_of_controls, color = factor(year))) +
  geom_line() +
  scale_x_continuous(name = "Month", breaks = 1:12,
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(name="Number of Controls") +
  scale_color_discrete(name="Year")
p5






