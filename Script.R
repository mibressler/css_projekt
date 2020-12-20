

library(tidyverse)
library(readr)
coronaNet <- read.csv('data/coronanet_release.csv')
casedata <- read.csv('data/rki_basic.csv')

germany <- coronaNet %>% filter (country == "Germany")
x <- table(germany$type)

plot(x)

as.Date(casedata$Meldedatum)

<<<<<<< HEAD
<<<<<<< HEAD

qplot(Meldedatum,y=AnzahlFall, data=casedata)
=======
ggplot(data=casedata[,c(7,9), aes(x="Meldedatum", y="AnzahlFall")]) + geom_point()
>>>>>>> parent of 997bc3d... Update Script.R
=======
ggplot(data=casedata[,c(7,9), aes(x="Meldedatum", y="AnzahlFall")]) + geom_point()
>>>>>>> parent of 997bc3d... Update Script.R
