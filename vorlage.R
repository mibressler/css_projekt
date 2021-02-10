laud <- dygraph(laud_XT2, main ="bland") %>% dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = "y2", color = "rgb(0,101,189)") %>%
  dySeries("TagFall", label = "Fälle/Tag", color = "rgb(227,114,34)", fillGraph = TRUE)
for (i in 1:length(geltung_ae_laud)) {
  laud <- laud %>% dyEvent(geltung_ae_laud[i], labelLoc = "bottom", color = "gray")
}
for (i in 1:length(geltung_st_laud)) {
  laud <- laud %>% dyEvent(geltung_st_laud[i],label = geltung$Veordnung[i],labelLoc = "bottom", color = "black", strokePattern = "solid")
}
laud
