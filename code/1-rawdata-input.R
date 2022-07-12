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
westmesic_dlraw = read.csv("raw/CR300_15min_cumulative_DL2 West Mesic_03182022.csv")
eastmesic_dlraw = read.csv("raw/CR300_15min_DL1_EastMesic_03102022.csv")
westdry_dlraw = read.csv("raw/DL4_WestDrysite_15 Min.csv")
eastdry_dlraw = read.csv("raw/CR300_15min_DL3_EastDry_02012022.csv")


#remove ptemp column, add site, position, and row numbers "X"


westhydric_dlname = 
  westhydric_dlraw %>% 
  mutate(site = 'west', position = "hydric") %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 5988))  

easthydric_dlname = 
  easthydric_dlraw %>% 
  mutate(site = 'east', position = "hydric")%>% 
  dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 3780))  


westmesic_dlname = 
  westmesic_dlraw %>% 
  mutate(site = 'west', position = "mesic")%>% 
  dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 3565))  


eastmesic_dlname = 
  eastmesic_dlraw %>% 
  mutate(site = 'east', position = "mesic")%>% 
  dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 4959))  


westdry_dlname = 
  westdry_dlraw %>% 
  mutate(site = 'west', position = "dry") %>% 
  dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 3525))  


eastdry_dlname = 
  eastdry_dlraw %>% 
  mutate(site = 'east', position = "dry") %>% 
  dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 7014))  

#Create metadata

metadata_function <- function(dat){
  dat %>% 
  dplyr::select('X', 'TIMESTAMP', 'RECORD', 'site', 'position')%>% 
    mutate(Betterdate = strptime(TIMESTAMP, format = ("%m/%d/%Y %H:%M"))) %>% 
    mutate(redox_NUM_Avg = X) %>% 
    mutate(redox_NUM_Std = X) %>%  
    dplyr::select(-X)
}

#run function on all 6 data frames

westhydric_metadata = 
  metadata_function(westhydric_dlname)

westmesic_metadata = 
  metadata_function(westmesic_dlname)

westdry_metadata = 
  metadata_function(westdry_dlname)

easthydric_metadata = 
  metadata_function(easthydric_dlname)

eastmesic_metadata = 
  metadata_function(eastmesic_dlname)

eastdry_metadata = 
  metadata_function(eastdry_dlname)

#separate data columns only

#average
#X is being changed to an ID number that can used to join the pivoted data with the metadata later on
#named with "Avg" or "Std" at the end so it will be included in the next lines of code


avg_separate <- function(dat){
  dat %>% 
    mutate(redox_NUM_Avg = X) %>%
    dplyr::select(-X) %>% 
    # dplyr::select(c('X') & 
    dplyr::select(starts_with('redox')) %>% 
    dplyr::select(ends_with('Avg')) %>% 
    #if needed, get rid of NA rows then columns. Not needed.
   # filter(redox_NUM_Avg < 5989) %>%
   # select_if(~ !any(is.na(.))) %>% 
    
    force()
}

# run all 6 data frames

westhydric_avg = 
  avg_separate(westhydric_dlname) %>% 
  left_join(westhydric_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_avg", values_to = "avg_values") 

westmesic_avg = 
  avg_separate(westmesic_dlname) %>% 
  left_join(westmesic_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_avg", values_to = "avg_values") 

westdry_avg = 
  avg_separate(westdry_dlname) %>% 
  left_join(westdry_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_avg", values_to = "avg_values") 

easthydric_avg = 
  avg_separate(easthydric_dlname) %>% 
  left_join(easthydric_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_avg", values_to = "avg_values") 

eastmesic_avg = 
  avg_separate(eastmesic_dlname) %>% 
  left_join(eastmesic_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_avg", values_to = "avg_values") 

eastdry_avg = 
  avg_separate(eastdry_dlname) %>% 
  left_join(eastdry_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_avg", values_to = "avg_values") 


#Std 

avg_separate <- function(dat){
  dat %>% 
    mutate(redox_NUM_Avg = X) %>%
    dplyr::select(-X) %>% 
    # dplyr::select(c('X') & 
    dplyr::select(starts_with('redox')) %>% 
    dplyr::select(ends_with('Avg')) %>% 
    #if needed, get rid of NA rows then columns. Not needed.
    # filter(redox_NUM_Avg < 5989) %>%
    # select_if(~ !any(is.na(.)))
    
    force()
}


# apply to six dataframes


sensor_depths_prename = read.csv("raw/datalogger_depths.csv") 

sensor_depths = 
  sensor_depths_prename %>% 
  dplyr::rename(position = Site,
                site = Area) %>% 
  mutate(site = recode(site, "West" = "west",
                       "East" = "east")) %>% 
  mutate(position = recode(position, "Hydric" = "hydric",
                           "Mesic" = "mesic",
                           "Dry" = "dry"),
         Sensor = recode(Sensor, "redox_5-2-3_fromtip_Avg" = "redox_5_2-3_fromtip_Avg")) %>% 
  mutate(Sensor = str_remove(Sensor, "Redox_")) %>% 
  mutate(Sensor = str_remove(Sensor, "redox_")) %>% 
  mutate(Sensor = str_remove(Sensor, "_fromtip_Avg")) %>% 
  separate(Sensor, sep = "_", into = c("datalogger", "probe_sensor")) %>% 
  separate(probe_sensor, sep = "-", into = c("probe", "sensor")) %>% 
  mutate(sensor = as.numeric(sensor)) %>% 
  mutate(depth_cm = str_replace(depth_cm, "n.a.", "NA")) %>%
  mutate(depth_cm = as.numeric(depth_cm)) %>% 
  na.omit() 
  
  


depths_function <- function(dat){
  dat %>% 
    mutate(redox_avg = recode(redox_avg, 'redox_5.2.3_fromtip_Avg' = "redox_5_2_3_fromtip_Avg")) %>% 
    mutate(redox_avg = str_remove(redox_avg, "Redox_")) %>% 
    mutate(redox_avg = str_remove(redox_avg, "redox_")) %>% 
    mutate(redox_avg = str_remove(redox_avg, "_fromtip_Avg")) %>% 
    separate(redox_avg, sep = "_", into = c("datalogger", "probe_sensor")) %>%
    filter(datalogger != "NUM") %>% 
    dplyr::select(-redox_NUM_Avg) %>% 
    mutate(probe_sensor = recode(probe_sensor, "1.8" = "1_8", "1.7" = "1_7", "1.6" = "1_6", "1.5" = "1_5", "1.4" = "1_4", "1.3" = "1_3", "1.2" = "1_2", "1.1" = "1_1",
                                 "2.8" = "2_8", "2.7" = "2_7", "2.6" = "2_6", "2.5" = "2_5", "2.4" = "2_4", "2.3" = "2_3", "2.2" = "2_2", "2.1" = "2_1",
                                 "3.8" = "3_8", "3.7" = "3_7", "3.6" = "3_6", "3.5" = "3_5", "3.4" = "3_4", "3.3" = "3_3", "3.2" = "3_2", "3.1" = "3_1")) %>% 
    separate(probe_sensor, sep = "_", into = c("probe", "sensor")) %>% 
    mutate(sensor = as.numeric(sensor)) %>%
    left_join(sensor_depths) %>% 
    group_by(site, position, depth_cm, Betterdate) %>%
    dplyr::summarize(avg_values_summarised = mean(avg_values),
                     std_values_summarised = sd(avg_values)/sqrt(n())) %>%
    mutate(depth_cm = as.numeric(depth_cm)) %>% 
    force()
  
  
}


#below code already doesn't work
# no probe or sensor data

westhydric_depths = 
  depths_function(westhydric_avg)

easthydric_depths = 
  depths_function(easthydric_avg) %>% 
  na.omit()

westmesic_depths = 
  depths_function(westmesic_avg)

eastmesic_depths = 
  depths_function(eastmesic_avg)

westdry_depths = 
  depths_function(westdry_avg)

eastdry_depths = 
  depths_function(eastdry_avg) %>% 
  na.omit()



#####

all_combine_depthbins = 
  westhydric_depths %>% 
  bind_rows(easthydric_depths, westmesic_depths, eastmesic_depths, westdry_depths, eastdry_depths) %>% 
  mutate(depth_bins = case_when(depth_cm <= 100 ~ cut_width(depth_cm, width = 1, center = 1))) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]","")) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[","")) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
  # now separate this into two different columns
  separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm")) %>% 
  mutate(depth_start_cm = as.integer(depth_start_cm),
         depth_stop_cm = as.integer(depth_stop_cm)) %>% 
  mutate(depth2 = depth_stop_cm - depth_start_cm)


all_combine = 
  westhydric_depths %>% 
  bind_rows(easthydric_depths, westmesic_depths, eastmesic_depths, westdry_depths, eastdry_depths)  
  

# mesic_combine = 
#   westmesic_dlname %>% 
#   vctrs::vec_c(eastmesic_dlname) 
# 
# dry_combine = 
#   westdry_dlname %>% 
#   vctrs::vec_c(eastdry_dlname)
# 
# all_combine = 
#   hydric_combine %>% 
#   vctrs::vec_c(mesic_combine, dry_combine)

#write.csv(hydric_combine, "processed/hydric_combine.csv")

write.csv(all_combine, "processed/all_combine.csv")

write.csv(all_combine_depthbins, "processed/all_combine_depthbins.csv")
