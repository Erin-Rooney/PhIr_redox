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

#install.packages("googlesheets4")

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
rhizon_raw2022 = read.csv("raw/2023_LIME_ICPAES_223R_Herndon_UnivTN_0907_rhizons_summer2022.csv")

metadata_rhizon_withdate = 
  metadata_rhizon %>%
  mutate(Betterdate = as.Date(date, format = ("%m-%d-%Y"))) 

#capitalize Y for four number year, lowercase y for two number year.

rhizon_meta_combine = 
  rhizon_raw %>% 
  left_join(metadata_rhizon_withdate) %>% 
  pivot_longer(-c(ID, Sample, date, Area, Site, Plot, Betterdate), names_to = 'ICP', values_to = 'concentration') %>% 
  na.omit() %>% 
  dplyr::mutate(month = case_when(grepl("Jun", Sample)~"june",
                                  grepl("Jul", Sample)~"july",
                                  grepl("Aug", Sample)~"august"))

#####


metadata_rhizon_withdate_2022 = 
  rhizon_raw2022 %>%
  separate(PhIr_ID, sep = " - ", into = c("num", "ID")) %>% 
  separate(ID, sep = "-2022_", into = c("date", "site")) %>% 
  separate(site, sep = "_rhizon_", into = c("site2", "hcl")) %>% 
  separate(site2, sep = "_", into = c("sitearea", "rep")) %>% 
  na.omit() %>% 
  dplyr::mutate(site = case_when(grepl("dry", sitearea)~"dry",
                                      grepl("mesic", sitearea)~"mesic",
                                      grepl("hydric", sitearea)~"hydric")) %>% 
  dplyr::mutate(area = case_when(grepl("east", sitearea)~"east",
                                    grepl("west", sitearea)~"west")) %>% 
  replace(is.na(.),"transect") %>% 
  separate(date, sep = "-", into = c("day", "month")) %>% 
  mutate(month = recode(month, "June" = "06", "July" = "07", "August" = "08", 
                        "September" = "09")) %>% 
  mutate(year = 2022) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
  dplyr::select(-c(penn_state_ID, num, sitearea, hcl, day, month, year)) %>% 
  pivot_longer(-c(site, area, date, rep), names_to = 'ICP', values_to = 'concentration') %>% 
  mutate(concentration_undo_dilution = (concentration* 10))
  

#capitalize Y for four number year, lowercase y for two number year.

rhizon_meta_combine = 
  rhizon_raw %>% 
  left_join(metadata_rhizon_withdate) %>% 
  pivot_longer(-c(ID, Sample, date, Area, Site, Plot, Betterdate), names_to = 'ICP', values_to = 'concentration') %>% 
  na.omit() %>% 
  dplyr::mutate(month = case_when(grepl("Jun", Sample)~"june",
                                  grepl("Jul", Sample)~"july",
                                  grepl("Aug", Sample)~"august"))


############

#not working
#issue with word month for betterdate

# sipper_raw_withdate = 
#   sipper_raw %>%
#   mutate(Betterdate = as.Date(Collection_Date), format = ("%d-%m-%Y"))

#capitalize Y for four number year, lowercase y for two number year.

sipper_data = 
  sipper_raw %>% 
  pivot_longer(-c(Sample_ID, Plot_ID, Collection_Date, Depth_cm, Filter_Type), names_to = 'elements', values_to = 'concentration') %>% 
  na.omit() %>% 
  dplyr::mutate(month = case_when(grepl("Jun", Collection_Date)~"june",
                                  grepl("Jul", Collection_Date)~"july",
                                  grepl("Aug", Collection_Date)~"august"))

                                  

###write file

write.csv(rhizon_meta_combine, "processed/rhizon_2021.csv")
write.csv(sipper_data, "processed/sipper_2021.csv")

write.csv(metadata_rhizon_withdate_2022, "processed/metadata_rhizon_withdate_2022.csv")
