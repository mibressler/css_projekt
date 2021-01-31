library(dygraphs)


# Schleswig-Holstein

SchlesHols_G2 <- SchlesHols_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
SchlesHols_XT2 <- xts(SchlesHols_G2, order.by = as.POSIXct(Schles_Hols_G2$Meldedatum))

SchlesHols_all <- cbind(SchlesHols_XT, SchlesHols_XT2)

dygraph(SchlesHols_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Hamburg

Hamb_G2 <- Hamb_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
Hamb_XT2 <- xts(Hamb_G2, order.by = as.POSIXct(Hamb_G2$Meldedatum))

Hamb_all <- cbind(Hamb_XT, Hamb_XT2)

dygraph(Hamb_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Niedersachsen

Nieder_G2 <- Nieder_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
Nieder_XT2 <- xts(Nieder_G2, order.by = as.POSIXct(Nieder_G2$Meldedatum))

Nieder_all <- cbind(Nieder_XT, Nieder_XT2)


dygraph(Nieder_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Bremen 

Bremen_G2 <- Bremen_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
Bremen_XT2 <- xts(Bremen_G2, order.by = as.POSIXct(Bremen_G2$Meldedatum))

Bremen_all <- cbind(Bremen_XT, Bremen_XT2)


dygraph(Bremen_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Nordrhein-Westfalen

NordWest_G2 <- NordWest_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
NordWest_XT2 <- xts(NordWest_G2, order.by = as.POSIXct(NordWest_G2$Meldedatum))

NordWest_all <- cbind(NordWest_XT, NordWest_XT2)


dygraph(NordWest_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Hessen

Hessen_G2 <- Hessen_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
Hessen_XT2 <- xts(Hessen_G2, order.by = as.POSIXct(Hessen_G2$Meldedatum))

Hessen_all <- cbind(Hessen_XT, Hessen_XT2)


dygraph(Hessen_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Rheinland-Pfalz

RhePfalz_G2 <- RhePfalz_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
RhePfalz_XT2 <- xts(RhePfalz_G2, order.by = as.POSIXct(RhePfalz_G2$Meldedatum))

RhePfalz_all <- cbind(RhePfalz_XT, RhePfalz_XT2)


dygraph(RhePfalz_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Baden-Wuerttemberg

BadWuert_G2 <- BadWuert_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
BadWuert_XT2 <- xts(BadWuert_G2, order.by = as.POSIXct(BadWuert_G2$Meldedatum))

BadWuert_all <- cbind(BadWuert_XT, BadWuert_XT2)


dygraph(BadWuert_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Bayern

Bay_G2 <- Bay_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
Bay_XT2 <- xts(Bay_G2, order.by = as.POSIXct(Bay_G2$Meldedatum))

Bay_all <- cbind(Bay_XT, Bay_XT2)


dygraph(Bay_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Saarland

Saarl_G2 <- Saarl_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
Saarl_XT2 <- xts(Saarl_G2, order.by = as.POSIXct(Saarl_G2$Meldedatum))

Saarl_all <- cbind(Saarl_XT, Saarl_XT2)


dygraph(Saarl_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Berlin

Berlin_G2 <- Berlin_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
Berlin_XT2 <- xts(Berlin_G2, order.by = as.POSIXct(Berlin_G2$Meldedatum))

Berlin_all <- cbind(Berlin_XT, Berlin_XT2)


dygraph(Berlin_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Brandenburg

Brand_G2 <- Brand_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
Brand_XT2 <- xts(Brand_G2, order.by = as.POSIXct(Brand_G2$Meldedatum))

Brand_all <- cbind(Brand_XT, Brand_XT2)

# Mecklenburg-Vorpommern

MeckVor_G2 <- MeckVor_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
MeckVor_XT2 <- xts(MeckVor_G2, order.by = as.POSIXct(MeckVor_G2$Meldedatum))

MeckVor_all <- cbind(MeckVor_XT, MeckVor_XT2)


dygraph(MeckVor_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Sachsen

Sachsen_G2 <- Sachsen_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
Sachsen_XT2 <- xts(Sachsen_G2, order.by = as.POSIXct(Sachsen_G2$Meldedatum))

Sachsen_all <- cbind(Sachsen_XT, Sachsen_XT2)


dygraph(Sachsen_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Sachsen-Anhalt

SachAnh_G2 <- SachAnh_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
SachAnh_XT2 <- xts(SachAnh_G2, order.by = as.POSIXct(SachAnh_G2$Meldedatum))

SachAnh_all <- cbind(SachAnh_XT, SachAnh_XT2)


dygraph(SachAnh_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 

# Thueringen

Thuer_G2 <- Thuer_G %>% group_by(Meldedatum) %>%
  distinct(inzidenz, TagFall)
Thuer_XT2 <- xts(Thuer_G2, order.by = as.POSIXct(Thuer_G2$Meldedatum))

Thuer_all <- cbind(Thuer_XT, Thuer_XT2)

dygraph(Thuer_all, main = "Infektionsfälle über Zeit") %>%
  dyAxis("y", label = "Gemeldete Infektionsfälle") %>%
  dyAxis("y2", label = "7-Tage-Inzidenzwert", independentTicks = TRUE) %>%
  dySeries("inzidenz", axis = 'y2') 


