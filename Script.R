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

# nach Bundesland
SchlesHols <- subset(casedata_rki, IdBundesland == "1")
Hamb <- subset(casedata_rki, IdBundesland == "2")
Nieder <- subset(casedata_rki, IdBundesland == "3")
Bremen <- subset(casedata_rki, IdBundesland == "4")
NordWest <- subset(casedata_rki, IdBundesland == "5")
Hessen <- subset(casedata_rki, IdBundesland == "6")
RhePfalz <- subset(casedata_rki, IdBundesland == "7")
BadWuert <- subset(casedata_rki, IdBundesland == "8")
Bay <- subset(casedata_rki, IdBundesland == "9")
Saarl <- subset(casedata_rki, IdBundesland == "10")
Berlin <- subset(casedata_rki, IdBundesland == "11")
Brand <- subset(casedata_rki, IdBundesland == "12")
MeckVor <- subset(casedata_rki, IdBundesland == "13")
Sachsen <- subset(casedata_rki, IdBundesland == "14")
SachAnh <- subset(casedata_rki, IdBundesland == "15")
Thuer <- subset(casedata_rki, IdBundesland == "16")

bundeslaender <- list(SchlesHols,Hamb,Nieder,Bremen)

# täglich gemeldete Fälle ~ Zeit
 
SchlesHols_G <- SchlesHols %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  SchlesHols_XT <- xts(SchlesHols_G, order.by = as.POSIXct(SchlesHols_G$Meldedatum))



# ggplot(cases, aes(x=as.Date(Meldedatum),y=TagFall)) + geom_line()

