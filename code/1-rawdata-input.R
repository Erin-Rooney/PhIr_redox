#Raw data input
#rhizons
#E C Rooney

#bringing in data from google drive
#only install once
#From r-bloggers: The command above should prompt the installation process, 
#but it can also throw errors if you have some outdated packages in your 
#environment (e.g. the incompatible version of ‘rlang’). 
#Update required packages, and rerun the installation process. 
#You may need to restart the R session afterward.

install.packages("googlesheets4")

##
#start here if googlesheets4 is already installed

library(googlesheets4)

#load all packages

source("code/0-packages.R")

#grant access to google account and authenticate
# Answer 1 for yes to question "Is it OK to cache OAuth access credential in the folder..."

gs4_auth()

#select desired user
#Connect new users by entering 0 in command line

#From r-bloggers:
#Note: If you do not need to access any private Google spreadsheets use gs4_deauth().

rhizon_raw = read_sheet("https://docs.google.com/spreadsheets/d/1n0ou-nkpoE2qw9Xbl9lsVGQ_g6LUT2xbLnxQdxjdGD0/edit#gid=0")
metadata_rhizon = read_sheet("https://docs.google.com/spreadsheets/d/1IuW1DstoXZJPFrmtL8RpOGGeoLgQnxetv-v6RO8Pg-k/edit#gid=0")
sipper_raw = read_sheet("https://docs.google.com/spreadsheets/d/1XXRUo2oagEGhlQ9JKWsxrZyseAyfA5sHAkZqjs3PsBA/edit#gid=0")


metadata_rhizon_withdate = 
  metadata_rhizon %>%
  mutate(Betterdate = as.Date(date, format = ("%m-%d-%Y"))) 

#capitalize Y for four number year, lowercase y for two number year.

rhizon_meta_combine = 
  rhizon_raw %>% 
  left_join(metadata_rhizon_withdate) %>% 
  pivot_longer(-c(ID, Sample, date, Area, Site, Plot, Betterdate), names_to = 'ICP', values_to = 'concentration') %>% 
  na.omit() 

###write file

write.csv(rhizon_meta_combine, "processed/rhizon_2021.csv")
write.csv(sipper_raw, "processed/sipper_2021.csv")

#there is weird double data
#all doubles look identical except for the blank
#grouping, will check with Beth/Sumant later
#delete and revise once checking is complete


############
#Data logger data input

westhydric_dlraw = read.csv("raw/CR1000_15min_DL6_WestHydric_cumulative_01122022.csv")
easthydric_dlraw = read.csv("raw/CR300_15min_cumulative_DL5 East Hydric_03212022.csv")

westhydric_dlname = 
  westhydric_dlraw %>% 
  mutate(site = 'west')

easthydric_dlname = 
  easthydric_dlraw %>% 
  mutate(site = 'east')

hydric_combine = 
  westhydric_dlname %>% 
  vctrs::vec_c(easthydric_dlname)

write.csv(hydric_combine, "processed/hydric_combine.csv")
