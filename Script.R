

library(tidyverse)
library(readr)
coronaNet <- read.csv('data/coronanet_release.csv')
casedata <- read.csv('data/rki_basic.csv')

germany <- coronaNet %>% filter (country == "Germany")
x <- table(germany$type)

plot(x)

as.Date(casedata$Meldedatum)

ggplot(data=casedata[,c(7,9), aes(x="Meldedatum", y="AnzahlFall")]) + geom_point()