library(tidyverse)
library(readr)
library(zoo)
library(xts)
library(tidyquant)
library(pROC)
library(quantreg)


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
  
 SchlesHols_G <- SchlesHols_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  SchlesHols_G <- SchlesHols_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  SchlesHols_Einwohner <- 2903773
 SchlesHols_G <- SchlesHols_G%>% mutate(inzidenz = (weekFall / SchlesHols_Einwohner)*100000)
 SchlesHols_G 
 
 SchlesHols_G <- SchlesHols_G %>% mutate(weekmean = weekFall / 7)
  
  Hamb_G <- Hamb %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Hamb_XT <- xts(Hamb_G, order.by = as.POSIXct(Hamb_G$Meldedatum))
  
  Hamb_G <- Hamb_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  Hamb_G <- Hamb_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  Hamb_Einwohner <- 1847253
  Hamb_G <- Hamb_G%>% mutate(inzidenz = (weekFall / Ham_Einwohner)*100000)
  
  Hamb_G <- Hamb_G %>% mutate(weekmean = weekFall / 7)
    
  Nieder_G <- Nieder %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Nieder_XT <- xts(Nieder_G, order.by = as.POSIXct(Nieder_G$Meldedatum))
  
  Nieder_G <- Nieder_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  Nieder_G <- Nieder_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  Nieder_Einwohner <- 7993608
  Nieder_G <- Nieder_G%>% mutate(inzidenz = (weekFall / Nieder_Einwohner)*100000)
  
  Nieder_G <- Nieder_G %>% mutate(weekmean = weekFall / 7)
  
  Bremen_G <- Bremen %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Bremen_XT <- xts(Bremen_G, order.by = as.POSIXct(Bremen_G$Meldedatum))
  
  Bremen_G <- Bremen_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  Bremen_G <- Bremen_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  Bremen_Einwohner <- 681202
  Bremen_G <- Bremen_G%>% mutate(inzidenz = (weekFall / Bremen_Einwohner)*100000)
  
  Bremen_G <- Bremen_G %>% mutate(weekmean = weekFall / 7)
  
  NordWest_G <- NordWest %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  NordWest_XT <- xts(NordWest_G, order.by = as.POSIXct(NordWest_G$Meldedatum))
  
  NordWest_G <- NordWest_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  NordWest_G <- NordWest_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  NordWest_Einwohner <- 17947221
  NordWest_G <- NordWest_G%>% mutate(inzidenz = (weekFall / NordWest_Einwohner)*100000)
  
  NordWest_G <- NordWest_G %>% mutate(weekmean = weekFall / 7)
  
  Hessen_G <- Hessen %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Hessen_XT <- xts(Hessen_G, order.by = as.POSIXct(Hessen_G$Meldedatum))

  Hessen_G <- Hessen_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  Hessen_G <- Hessen_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  Hessen_Einwohner <- 6288080
  Hessen_G <- Hessen_G%>% mutate(inzidenz = (weekFall / Hessen_Einwohner)*100000)
  
  Hessen_G <- Hessen_G %>% mutate(weekmean = weekFall / 7)
  
  RhePfalz_G <- RhePfalz %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  RhePfalz_XT <- xts(RhePfalz_G, order.by = as.POSIXct(RhePfalz_G$Meldedatum))
  
  RhePfalz_G <- RhePfalz_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  RhePfalz_G <- RhePfalz_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  RhePfalz_Einwohner <- 4093903
  RhePfalz_G <- RhePfalz_G%>% mutate(inzidenz = (weekFall / RhePfalz_Einwohner)*100000)
  
  RhePfalz_G <- RhePfalz_G %>% mutate(weekmean = weekFall / 7)
  
BadWuert_G <- BadWuert %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  BadWuert_XT <- xts(BadWuert_G, order.by = as.POSIXct(BadWuert_G$Meldedatum))

  BadWuert_G <- BadWuert_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  BadWuert_G <- BadWuert_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  BadWuert_Einwohner <- 11100394
  BadWuert_G <- BadWuert_G%>% mutate(inzidenz = (weekFall / BadWuert_Einwohner)*100000)
  
  BadWuert_G <- BadWuert_G %>% mutate(weekmean = weekFall / 7)
  
Bay_G <- Bay %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Bay_XT <- xts(Bay_G, order.by = as.POSIXct(Bay_G$Meldedatum))
  
  Bay_G <- Bay_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  Bay_G <- Bay_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  Bay_Einwohner <- 13124737
  Bay_G <- Bay_G%>% mutate(inzidenz = (weekFall / Bay_Einwohner)*100000)
  
  Bay_G <- Bay_G %>% mutate(weekmean = weekFall / 7)
  
Saarl_G <- Saarl %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Saarl_XT <- xts(Saarl_G, order.by = as.POSIXct(Saarl_G$Meldedatum))
  
  Saarl_G <- Saarl_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  Saarl_G <- Saarl_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  Saarl_Einwohner <- 986887
  Saarl_G <- Saarl_G%>% mutate(inzidenz = (weekFall / Saarl_Einwohner)*100000)
  
  Saarl_G <- Saarl_G %>% mutate(weekmean = weekFall / 7)
  
Berlin_G <- Berlin %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Berlin_XT <- xts(Berlin_G, order.by = as.POSIXct(Berlin_G$Meldedatum))
  
  Berlin_G <- Berlin_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  Berlin_G <- Berlin_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  Berlin_Einwohner <- 3669491
  Berlin_G <- Berlin_G%>% mutate(inzidenz = (weekFall / Berlin_Einwohner)*100000)
  
  Berlin_G <- Berlin_G %>% mutate(weekmean = weekFall / 7)
  
Brand_G <- Brand %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Brand_XT <- xts(Brand_G, order.by = as.POSIXct(Brand_G$Meldedatum))
  
  Brand_G <- Brand_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  Brand_G <- Brand_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  Brand_Einwohner <- 2521893
  Brand_G <- Brand_G%>% mutate(inzidenz = (weekFall / Brand_Einwohner)*100000)
  
  Brand_G <- Brand_G %>% mutate(weekmean = weekFall / 7)
  
MeckVor_G <- MeckVor %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  MeckVor_XT <- xts(MeckVor_G, order.by = as.POSIXct(MeckVor_G$Meldedatum))
  
  MeckVor_G <- MeckVor_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  MeckVor_G <- MeckVor_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  MeckVor_Einwohner <- 1608138
  MeckVor_G <- MeckVor_G%>% mutate(inzidenz = (weekFall / MeckVor_Einwohner)*100000)
  
  MeckVor_G <- MeckVor_G %>% mutate(weekmean = weekFall / 7)
  
  Sachsen_G <- Sachsen %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Sachsen_XT <- xts(Sachsen_G, order.by = as.POSIXct(Sachsen_G$Meldedatum))
  
  Sachsen_G <- Sachsen_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  Sachsen_G <- Sachsen_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  Sachsen_Einwohner <- 4071971
  Sachsen_G <- Sachsen_G%>% mutate(inzidenz = (weekFall / Sachsen_Einwohner)*100000)
  
  Sachsen_G <- Sachsen_G %>% mutate(weekmean = weekFall / 7)
  
  SachAnh_G <- SachAnh %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  SachAnh_XT <- xts(SachAnh_G, order.by = as.POSIXct(SachAnh_G$Meldedatum))
  
  SachAnh_G <- SachAnh_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  SachAnh_G <- SachAnh_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  SachAnh_Einwohner <- 2194782
  SachAnh_G <- SachAnh_G%>% mutate(inzidenz = (weekFall / SachAnh_Einwohner)*100000)
  
  SachAnh_G <- SachAnh_G %>% mutate(weekmean = weekFall / 7)
  
  Thuer_G <- Thuer %>% group_by(Meldedatum) %>%
    mutate(TagFall = sum(AnzahlFall)) %>%
    distinct(TagFall, Meldedatum)
  Thuer_XT <- xts(Thuer_G, order.by = as.POSIXct(Thuer_G$Meldedatum))
  
  Thuer_G <- Thuer_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
  Thuer_G <- Thuer_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
  Thuer_Einwohner <- 2133378
  Thuer_G <- Thuer_G%>% mutate(inzidenz = (weekFall / Thuer_Einwohner)*100000)
  
  Thuer_G <- Thuer_G %>% mutate(weekmean = weekFall / 7)
  
# ggplot(cases, aes(x=as.Date(Meldedatum),y=TagFall)) + geom_line()


# Frequenz der Verordnungen
testgeltung <- read.csv("geltung1.csv")  
holstest <- as.Date(testgeltung[c(1:15),3], '%d.%m.%Y')
holssverordn <- data.frame(date=holstest,verordn=1)

holslm <- SchlesHols_G %>% select(weekmean,Meldedatum)
holslm <- holslm %>% group_by(week) %>% summarise(weekmean=weekmean,Meldedatum=Meldedatum)
holslm$Meldedatum <- as.Date(holslm$Meldedatum)


holscount <- data.frame(Geltungsstart=holstest,weeknr=week(holstest))
holsfreq <- holscount %>% group_by(weeknr) %>% summarise(verordfreq = length(Geltungsstart),Meldedatum=Geltungsstart)

holsfinal <- merge(holslm,holsfreq,by="Meldedatum",all=T)
holsfinal[is.na(holsfinal)] <- 0

holsfinal2 <- holsfinal %>% select(Meldedatum,weekmean,verordfreq)


glm <- glm(holsfinal2$verordfreq ~ holsfinal2$weekmean, family="binomial")
library(popbio)
logi.hist.plot(holsfinal2$weekmean,holsfinal2$verordfreq,boxp=F,type="hist",col="gray")

# Geltungsstarts

# Presets
MakeDate <- function(date) {
  date[date==""] <- NA
  date <- as.Date(date,'%d.%m.%Y')
  date <- na.omit(date)
  return(date)
}

# Geltungen
geltung <- read.csv("data/geltung_18_01.csv")

geltung_erststarts <- geltung$Geltung.START

geltung_erststarts <- MakeDate(geltung_erststarts)

geltung_aenderungen <- as.vector(as.matrix(geltung[,c(5:26)]))

geltung_aenderungen <- MakeDate(geltung_aenderungen)

geltung_alle <- c(geltung_erststarts,geltung_aenderungen)

geltung_alle <- data.frame(Meldedatum=geltung_alle,Geltungsstart=geltung_alle)

# Weekmeans
bundes_weekmean <- c(BadWuert_G$weekmean,Bay_G$weekmean,Berlin_G$weekmean,Brand_G$weekmean,Bremen_G$weekmean,Hamb_G$weekmean,Hessen_G$weekmean,MeckVor_G$weekmean,Nieder_G$weekmean,NordWest_G$weekmean,RhePfalz_G$weekmean,Saarl_G$weekmean,SachAnh_G$weekmean,Sachsen_G$weekmean,SchlesHols_G$weekmean,Thuer_G$weekmean)

# Fälle pro Meldedatum

fpm <- casedata_rki[,c("AnzahlFall","Meldedatum")]
fpm <- fpm %>% group_by(Meldedatum) %>% summarise(FaelleproTag=sum(AnzahlFall))
fpm$Meldedatum <- as.Date(fpm$Meldedatum)

# Model

modeldata <- merge(fpm,geltung_alle,by="Meldedatum", all=T)
model <- glm(geltung_alle)   
    # Geltungsstart 0<->1
modeldata$Geltungsstart <- as.character(modeldata$Geltungsstart)
modeldata$Geltungsstart[!is.na(modeldata$Geltungsstart)] <- 1
modeldata[is.na(modeldata)] <- 0
modeldata$Geltungsstart <- as.numeric(modeldata$Geltungsstart)

   # group by day
gmodeldata <- modeldata %>% group_by(Meldedatum,FaelleproTag) %>% summarise(Geltungsstart=sum(Geltungsstart))

# Analytics ungrouped data (binary response)
model <- glm(modeldata$Geltungsstart ~ modeldata$FaelleproTag, family="binomial")
    # binary response mit diff
# dgmodel <- glm(modeldata$Geltungsstart ~ dmodeldata$diff, family="binomial")

ggplot(modeldata, aes(x=Meldedatum,y=FaelleproTag)) + geom_line()

logi.hist.plot(modeldata$FaelleproTag,modeldata$Geltungsstart,boxp=F,type="hist",col="gray")

boxplot(FaelleproTag~Geltungsstart, ylab="Am Tag gemeldete Fälle", xlab= "Geltungsbeginn einer Veordnung", col="light blue",data = modeldata)

summary(model)
summary(model$fitted.values)

hist(model$fitted.values, main = " Histogram ",xlab = "Wahrscheinlichkeit eines Geltungsbeginns", col = 'light green')

roc(Geltungsstart~model$fitted.values, data = modeldata, plot = TRUE, main = "ROC CURVE", col= "blue")

auc(Geltungsstart~model$fitted.values, data = modeldata)

# Analytics grouped data (continous response)
ggplot(data=gmodeldata, aes(x=Meldedatum))+
  geom_area(aes(y=FaelleproTag),fill="red") +
  geom_density2d(aes(y=Geltungsstart*1500),color="blue")+
  scale_y_continuous(sec.axis=sec_axis(trans~./1500,name="Geltungsstarts am Tag"))

ggplot(data=gmodeldata, aes(x=Meldedatum))+
  geom_area(aes(y=FaelleproTag),fill="red") +
  geom_jitter(aes(y=Geltungsstart*1500),color="blue")+
  scale_y_continuous(sec.axis=sec_axis(trans~./1500,name="Geltungsstarts am Tag"))

ggplot(data=gmodeldata, aes(x=Meldedatum))+
  geom_area(aes(y=FaelleproTag),fill="red") +
  geom_smooth(aes(y=Geltungsstart*1500),color="blue",span=0.1)+
  scale_y_continuous(sec.axis=sec_axis(trans~./1500,name="Geltungsstarts am Tag"))

gmodel <- lm(gmodeldata$Geltungsstart ~ gmodeldata$FaelleproTag)
summary(gmodel)

cor <- cor.test(gmodeldata$Geltungsstart,gmodeldata$FaelleproTag)
# Ansatz mit Differenz der Fallzahlen
g2modeldata <- gmodeldata %>% mutate(test="test")
dmodeldata <- g2modeldata %>% group_by(test) %>% mutate(diff= FaelleproTag - lag(FaelleproTag))

dmodel <- lm(dmodeldata$Geltungsstart ~ dmodeldata$diff)
summary(dmodel)

boxplot(dmodel[['residuals']],main='Boxplot: Residuals',ylab='residual value')


dcor <- cor.test(dmodeldata$Geltungsstart,dmodeldata$diff)

ggplot(data=dmodeldata, aes(x=Meldedatum))+
  geom_line(aes(y=diff),color="red") +
  geom_smooth(aes(y=Geltungsstart*1500),color="blue",span=0.1)+
  scale_y_continuous(sec.axis=sec_axis(trans~./1500,name="Geltungsstarts am Tag"))

# verordnungen <- data.frame(Bundesland = SchleswigHolstein, Geltungsstart = 