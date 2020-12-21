library(tidyverse)
library(readr)
library(zoo)
library(xts)
library(tidyquant)

## ---- all
coronaNet <- read.csv('data/coronanet_release.csv')
casedata_rki <- read.csv('data/rki_basic.csv')
casedata <- read.csv('data/owid-covid-data.csv')

cNetBay <- coronaNet[coronaNet$country=="Germany",]
cNetBay <- cNetBay[cNetBay$province=="Bavaria",]

germanycase <- casedata %>% filter (location == "Germany")
germany <- coronaNet %>% filter (country == "Germany")
x <- table(germany$type)

plot(x)


d1 <- as.Date("2020-11-11")
d2 <- as.Date("2020-11-22")
germanyd <- subset(germanycase, date>d1 & date<d2)
ggplot(data=germanyd, aes(date, total_cases))+
  geom_bar(fill="steelblue", stat='identity')

# täglich gemeldete Fälle ~ Zeit
cases <- casedata_rki %>%     group_by(Meldedatum) %>%
  mutate(TagFall = sum(AnzahlFall)) %>%
  distinct(TagFall, Meldedatum)
cases <- xts(cases, order.by = as.POSIXct(cases$Meldedatum))

# ggplot(cases, aes(x=as.Date(Meldedatum),y=TagFall)) + geom_line()

