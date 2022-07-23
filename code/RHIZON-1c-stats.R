#7 22 2022
#STATS
#EC Rooney

#load all packages

source("code/0-packages.R")
library("Hmisc")

#load data

rhizon_means = read.csv("processed/rhizon_forelements.csv")
rhizon_meta_combine = read.csv("processed/rhizon_2021.csv")


#prepare for stats

rhizon_corr = 
  rhizon_stats %>% 
  pivot_wider(names_from = "")