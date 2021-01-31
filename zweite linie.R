library(dygraphs)

setwd("/Users/isa/Documents/POLITIKWISSENSCHAFT/5. SEMESTER/Computational Social Science/css_projekt")


#######
Thuer_G <- Thuer %>% group_by(Meldedatum) %>%
  mutate(TagFall = sum(AnzahlFall)) %>%
  distinct(TagFall, Meldedatum)
Thuer_XT <- xts(Thuer_G, order.by = as.POSIXct(Thuer_G$Meldedatum))

Thuer_G <- Thuer_G %>% mutate(week = strftime(Meldedatum, format ="%V"))
Thuer_G <- Thuer_G  %>% group_by(week) %>% mutate(weekFall = sum(TagFall))
Thuer_Einwohner <- 2133378
Thuer_G <- Thuer_G%>% mutate(inzidenz = (weekFall / Thuer_Einwohner)*100000)

Thuer_G <- Thuer_G %>% mutate(weekmean = weekFall / 7)



########
Thuer_G2 <- Thuer_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
Thuer_XT2 <- xts(Thuer_G2, order.by = as.POSIXct(Thuer_G2$Meldedatum))

Thuer_all <- cbind(Thuer_XT, Thuer_XT2)

dygraph(Thuer_all, main = "thueringen") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 
  




########
dygraph(Thuer_all, main = "Infektionsfälle über Zeit", ylab = "Gemeldete Infektionsfälle") %>% 
  dyEvent("2020-03-25","Corona EindämmungsVO", labelLoc = "bottom") %>%
  dyEvent("2020-03-27","ThürSARS-CoV-2-EindmaßnVO", labelLoc = "bottom") %>%
  dyEvent("2020-04-08","2.ThürSARS-CoV-2-EindmaßnVO", labelLoc = "bottom") %>%
  dyEvent("2020-04-20","3.ThürSARS-CoV-2-EindmaßnVO", labelLoc = "bottom") %>%
  dyEvent("2020-05-13","ThürSARS-CoV-2-MaßnFortentwVO", labelLoc = "bottom") %>%
  dyEvent("2020-05-26","2.Thüringer Quarantäneverordnung", labelLoc = "bottom") %>%
  dyEvent("2020-06-16","3.Thüringer Quarantäneverordnung", labelLoc = "bottom") %>%
  dyEvent("2020-07-16","4.Thüringer Quarantäneverordnung", labelLoc = "bottom") %>%
  dyEvent("2020-11-02","ThürSARS-CoV-2-SonderEindmaßnVO", labelLoc = "bottom") %>%
  dyEvent("2020-11-08","5.Thüringer Quarantäneverordnung", labelLoc = "bottom") 
