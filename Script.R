

library(tidyverse)
library(readr)
coronaNet <- read.csv('data/coronanet_release.csv')
casedata <- read.csv('data/rki_basic.csv')

germany <- coronaNet %>% filter (country == "Germany")
x <- table(germany$type)

plot(x)

as.Date(casedata$Meldedatum)


qplot(Meldedatum,y=AnzahlFall, data=casedata)