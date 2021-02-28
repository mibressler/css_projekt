Computational Social Science Project
================

Um unseren Code zu einzusehen und zu replizieren, könnten Sie dieses Repository z.B. klonen oder als zip-Datei herunterladen und dann ggf. die R-Projektdatei öffnen.

Quelldaten
--------------
**Meldezahlen des Robert-Koch-Instituts** (müssen erst noch manuell im git-ignorierten Ordner "data" als "rki_basic.csv" hinterlegt werden)
Quelle:https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0
**Geltungszeiträume der Verordnungen und Verordnungsänderungen für jedes Bundesland**
Da die Einträge zu den Maßnahmen im [CoronaNet Datensatz](https://www.coronanet-project.org/) leider nicht ganz mit den rechtlich festgelegten Geltungszeiträumen der spezifischen Corona-Schutz-Verordnungen der einzelnen Länder korrespondieren, haben wir uns dazu entschieden die Daten der Geltungsstarts der Verordnungen und Verordnungsänderungen selbst zu erheben und haben dafür Rechtsportale (wie etwa Juris) aufgesucht. (Leider keine öffentlich zugängliche API)
(muss auch noch manuell im "data"-Ordner als "geltung_18_01.csv" hinterlegt werden)
Quelle: https://syncandshare.lrz.de/getlink/fiAAZaT9sPkL4GmAKoLqBJyp/geltung_18_01.csv


R Analyse
------------
Alle unsere weiteren Datenumwandlungen, statistischen Tests und Plots haben wir in dem R Dokument **Skript.R** zusammengefasst.
Die verwendeten Packages sind Tidyverse (generelle Funktionen), readr, zoo, xts, tidyquant (u.a. für bessere Datums-transformationen), pROC (ROC-Kurven für logistische Regression), quantreg und car (Unterstützung bei statistischen Tests).

Zunächst werden die Meldezahlen des Robert-Koch-Instituts aus einer lokal gespeicherten (aus Speicherplatzgründen im git-ignorierten "data" Ordner) CSV-Datei importiert. (Aktuelle Version kann hier (https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0) heruntergeladen werden.)

Die Meldedaten werden nun auf, für die einzelnen Bundesländer stehende Variablen aufgeteilt. Für jedes Bundesland wird mittels group_by() eine Summe der Fallzahlen pro Tag gebildet. Außerdem wird mithilfe der Einwohnerzahl jedes Bundelandes eine 7-Tagesinzidenz für jede Woche gebildet. (keine "rolling Sum".)

Die eigene Funktion MakeDate (Zeile 280ff) soll die Umwandlung von Datums-Strings im euorpäischen Format in ein Date-Format erleichtern.

Auch die Geltungen werden importiert und für die weitere Verarbeitung diversifiziert nach Bundesländern und Erstgeltungen oder Änderungen. Dann wird eine Logistische Regression mit glm() mit der independent Variable "Meldesumme pro Tag" und Response Variable eines "Geltungsstarts-Ereignisses" (binär, Erststarts & Änderungen) zusammen mit unterstützenden Plots (logi.hist.plot, boxplot, hist, roc, summary) durchgeführt.

Als Nächstes werden die vorher gruppierten Daten & die Response Variable "Summe Geltungsstarts pro Tag" mithilfe einer linearen Regression lm() und cor.test() analysiert. Unterstütztend werden zahlreiche Plots mit dem ggplot2-Package gemalt. 
Um ein besseres Verständnis von dem Verhältnis von Verschärfungen, Lockerungen und neutralen Aktualisierungen in Bayern zu bekommen haben wir die bayerischen Verordnungen in ihrem Wortlaut und Kontext qualitativ analysiert und der dementsprechend passendsten Kategorie zugeordnet. Das entstandene Verhältnis haben wir mitt ggplot() & geom_bar() visualisiert.


Webseite
------------
Das Dashboard, in dem wir die Starts der Geltung der Verodnungen (oder Verordnungsänderungen) mit den Meldezahlen pro Bundesland vergleichen kann auf doc.ax/css eingesehen werden. Realisiert ist die Webseite mit einem R-Makrdown Dokument (**RWeb.Rmd**), welches als ShinyApp auf shiny.io gehostet wird. Die zudem weiter verwendeten Packages sind flexdashboard (Visualisierung der Webseite) & dygraphs (interaktive Zeit-Graphen). 

Zunächst wird mit dem Source() Befehl das komplette Skript.R geladen, so dass alle Variablen in der Umgebung geladen & verfügbar sind. Die Darstellung der einzelnen Graphen funktioniert über das dygraph() Package. Neben den Fallzahlen und der 7-Tages Inzidenz werden für jedes Bundesland die In-Kraft-Tretungs-Events der Verordnungen dargestellt. Letztere werden aus unserem Datensatz mithilfe der dyEvent() Funktion und einem For-Loop importiert und angezeigt.

