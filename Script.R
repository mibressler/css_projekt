setwd("/Users/isa/Documents/POLITIKWISSENSCHAFT/5. SEMESTER/Computational Social Science/css_projekt")

library(tidyverse)
library(readr)
coronaNet <- read.csv('data/coronanet_release.csv')
germany <- coronaNet %>% filter (country == "Germany")
x <- table(germany$type)

plot(x)
