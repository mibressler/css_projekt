

library(tidyverse)
library(readr)
coronaNet <- read.csv('data/coronanet_release.csv')
casedata <- read.csv('data/owid-covid-data.csv')

germanycase <- casedata %>% filter (location == "Germany")
germany <- coronaNet %>% filter (country == "Germany")
x <- table(germany$type)

plot(x)


d1 <- as.Date("2020-11-11")
d2 <- as.Date("2020-11-22")
germanyd <- subset(germanycase, date>d1 & date<d2)
ggplot(data=germanyd, aes(date, total_cases))+
  geom_bar(fill="steelblue", stat='identity')
