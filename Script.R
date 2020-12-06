setwd("/Users/isa/Documents/POLITIKWISSENSCHAFT/5. SEMESTER/Computational Social Science/css_projekt")

library(tidyverse)
library(readr)
coronaNet <- read.csv('data/coronanet_release.csv')
germany <- coronaNet %>% filter (country == "Germany")
x <- table(germany$type)

plot(x)
<<<<<<< HEAD
=======
               
y=5
>>>>>>> f1d95ab4dab6a55f5dcd291d327b1801374460d6
