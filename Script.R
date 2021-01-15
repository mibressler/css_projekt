library(tidyverse)
library(readr)
library(zoo)
library(xts)
library(tidyquant)
library(plyr)

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
#
BadWuert <- subset(casedata_rki, IdBundesland == "8")
Bay <- subset(casedata_rki, IdBundesland == "9")
Saarl <- subset(casedata_rki, IdBundesland == "10")
Berlin <- subset(casedata_rki, IdBundesland == "11")
Brand <- subset(casedata_rki, IdBundesland == "12")
MeckVor <- subset(casedata_rki, IdBundesland == "13")
#
Sachsen <- subset(casedata_rki, IdBundesland == "14")
SachAnh <- subset(casedata_rki, IdBundesland == "15")
Thuer <- subset(casedata_rki, IdBundesland == "16")

bundeslaender <- list(SchlesHols,Hamb,Nieder,Bremen)

# täglich gemeldete Fälle ~ Zeit
 
SchlesHols_G <- SchlesHols %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  SchlesHols_XT <- xts(SchlesHols_G, order.by = as.POSIXct(SchlesHols_G$Meldedatum))
  
  SchlesHols_G$week <- format(SchlesHols_G$TagFall, format="%Y-%U")
  siebentage   <-   ddply(SchlesHols_G, .(week), summarize, sum=sum(TagFall))
  SchlesHols_Einwohner <- 2903773
siebentage <- siebentage %>% mutate(inzidenz =(sum / SchlesHols_Einwohner)*100000)

  
  Hamb_G <- Hamb %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Hamb_XT <- xts(Hamb_G, order.by = as.POSIXct(Hamb_G$Meldedatum))
  
  Nieder_G <- Nieder %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Nieder_XT <- xts(Nieder_G, order.by = as.POSIXct(Nieder_G$Meldedatum))
  
  Bremen_G <- Bremen %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Bremen_XT <- xts(Bremen_G, order.by = as.POSIXct(Bremen_G$Meldedatum))
  
  NordWest_G <- NordWest %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  NordWest_XT <- xts(NordWest_G, order.by = as.POSIXct(NordWest_G$Meldedatum))
  
  Hessen_G <- Hessen %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Hessen_XT <- xts(Hessen_G, order.by = as.POSIXct(Hessen_G$Meldedatum))

  RhePfalz_G <- RhePfalz %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  RhePfalz_XT <- xts(RhePfalz_G, order.by = as.POSIXct(RhePfalz_G$Meldedatum))
  
BadWuert_G <- BadWuert %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  BadWuert_XT <- xts(BadWuert_G, order.by = as.POSIXct(BadWuert_G$Meldedatum))

Bay_G <- Bay %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Bay_XT <- xts(Bay_G, order.by = as.POSIXct(Bay_G$Meldedatum))
  
Saarl_G <- Saarl %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Saarl_XT <- xts(Saarl_G, order.by = as.POSIXct(Saarl_G$Meldedatum))
  
Berlin_G <- Berlin %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Berlin_XT <- xts(Berlin_G, order.by = as.POSIXct(Berlin_G$Meldedatum))
  
Brand_G <- Brand %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Brand_XT <- xts(Brand_G, order.by = as.POSIXct(Brand_G$Meldedatum))
  
MeckVor_G <- MeckVor %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  MeckVor_XT <- xts(MeckVor_G, order.by = as.POSIXct(MeckVor_G$Meldedatum))
  
  Sachsen_G <- Sachsen %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Sachsen_XT <- xts(Sachsen_G, order.by = as.POSIXct(Sachsen_G$Meldedatum))
  
  SachAnh_G <- SachAnh %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  SachAnh_XT <- xts(SachAnh_G, order.by = as.POSIXct(SachAnh_G$Meldedatum))
  
  Thuer_G <- Thuer %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Thuer_XT <- xts(Thuer_G, order.by = as.POSIXct(Thuer_G$Meldedatum))
# ggplot(cases, aes(x=as.Date(Meldedatum),y=TagFall)) + geom_line()

