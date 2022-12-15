#E Rooney
#12 15 2022
#neon load

# load packages

library(neonUtilities)
library(tidyverse)

#load NEON data files

Precip_2021 = loadByProduct(dpID="DP1.00006.001",
                             package="basic",
                             startdate="2021-06",
                             enddate="2021-09")


save(Precip_2021, file="raw/Precip_2021.RData")
