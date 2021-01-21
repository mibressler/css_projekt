Computational Social Science Project MICHAEL HAT KEIN HANDY UND BRAUCHT ZOOM LINK AUF SLACK!
================

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.4     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readr)

coronaNet <- read.csv('data/coronanet_release.csv')
germany <- coronaNet %>% filter (country == "Germany")
x <- table(germany$type)

plot(x)
```

![](README_figs/README-unnamed-chunk-1-1.png)<!-- -->
