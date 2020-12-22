casedata_rki <- read.csv('data/rki_basic.csv')

# aus der Website
casedata_rki <- read.csv("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv")



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



# täglich gemeldete Fälle ~ Zeit

cases <- Berlin %>%     group_by(Meldedatum) %>%
  mutate(TagFall = sum(AnzahlFall)) %>%
  distinct(TagFall, Meldedatum)
cases <- xts(cases, order.by = as.POSIXct(cases$Meldedatum))

ggplot(cases, aes(x=as.Date(Meldedatum),y=TagFall)) + geom_line()


