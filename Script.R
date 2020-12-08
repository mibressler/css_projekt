

library(tidyverse)
library(readr)
coronaNet <- read.csv('data/coronanet_release.csv')
casedata <- read.csv('data/owid-covid-data.csv')

germanycase <- casedata %>% filter (location == "Germany")
germany <- coronaNet %>% filter (country == "Germany")
x <- table(germany$type)

plot(x)
