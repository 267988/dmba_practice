rbinom(n=100, size=10, prob=0.5)

# Sannsynlighet for et gitt utfall. Size = nr of trials

dbinom(x=3, size=10, prob=0.5)

# Man bruker qbinom
# cdf = cumulative distributive function
qbinom(p=0.3, size=10, prob=0.5, lower.tail = FALSE)
# FALSE - man begynner fra høyre grense av grafen.
# Svaret er 6. Man må ha 6 med for å få minst 30% sannsynlighet.
# I dette eksempelet: Man får 30% sansynlighet for å få 0,1,2,3,4

qnorm(p=0.9, mean=100, sd=5, lower.tail = TRUE)
# Nøyaktig det samme som ovenfor: qnorm(p=0.1, mean=100, sd=5, lower.tail = FALSE)


#Generate 1000 values from the standard normal distribution mu=0, sigma=1.
#Plot them in a histogram
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
svar <- rnorm(1000, mean = 0, sd = 1)
svar #tibble
svar_df <- data_frame(svar)
svar_df #data frame
plot <- ggplot(svar_df, aes(x = svar)) +
  geom_histogram(binwidth = 0.2) +
  labs(title = "Statistics", x = "Normal distribution?", y = "Numbers")
plot

#Generate 1000 values from the binomial distribution (n=10, p=0.6)
#Plot them in a histogram

svar2 <-rbinom(n=1000, size=10, prob=0.6)
svar_df2 <- data.frame(svar2)

plot2 <- ggplot(svar_df2, aes(x = svar2)) +
  geom_histogram
plot2

# Oppgave. Måte 1 å svare på oppgaven
x <- rnorm(100000, mean = 10, sd = sqrt(10))
x
library("tidyverse")
df = tibble(x=x)
df
df <- df %>% mutate(ind=(x<=0))
df
sum(df$ind)/100000
#Sannsynligheten for at vi får tall mindre eller lik 0.
#Måte 2 å svare på oppgaven. 
pnorm(q=0, mean=10, sd=sqrt(10), lower.tail = FALSE)
