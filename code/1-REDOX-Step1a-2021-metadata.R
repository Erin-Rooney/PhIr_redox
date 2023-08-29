#ECROONEY
#July 12 2022

#load all packages

source("code/0-packages.R")

#load data


#Data logger data input

westhydric_dlraw = read.csv("raw/CR1000_15min_DL6_WestHydric_cumulative_01122022.csv")
easthydric_dlraw = read.csv("raw/CR300_15min_cumulative_DL5 East Hydric_03212022.csv")
westmesic_dlraw = read.csv("raw/CR300_15min_cumulative_DL2 West Mesic_03182022.csv") %>% 
  filter(RECORD > 205) 
eastmesic_dlraw = read.csv("raw/CR300_15min_DL1_EastMesic_03102022.csv")
westdry_dlraw = read.csv("raw/DL4_WestDrysite_15 Min.csv") %>% 
  filter(RECORD > 185) 
eastdry_dlraw = read.csv("raw/CR300_15min_DL3_EastDry_02012022.csv")

#remove ptemp column, add site, position, and row numbers "X"


#################

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
  mutate("X" = seq.int(1, by = 1, length.out = 3359))  


eastmesic_dlname = 
  eastmesic_dlraw %>% 
  mutate(site = 'east', position = "mesic")%>% 
  dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 4959))  


westdry_dlname = 
  westdry_dlraw %>% 
  mutate(site = 'west', position = "dry") %>% 
  dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 3339))  


eastdry_dlname = 
  eastdry_dlraw %>% 
  mutate(site = 'east', position = "dry") %>% 
  dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 7014))

write.csv(westhydric_dlname, "processed/westhydric_dlname.csv")
write.csv(easthydric_dlname, "processed/easthydric_dlname.csv")
write.csv(westmesic_dlname, "processed/westmesic_dlname.csv")
write.csv(eastmesic_dlname, "processed/eastmesic_dlname.csv")
write.csv(westdry_dlname, "processed/westdry_dlname.csv")
write.csv(eastdry_dlname, "processed/eastdry_dlname.csv")



#Create metadata

########################

#fix timezone issue

# library(lubridate)
# 
# westmesic_UT =
#   westmesic_dlname %>% 
#   filter(RECORD < 205) 
# 
# westmesic_AK =
#   westmesic_dlname %>% 
#   filter(RECORD > 204) 
# 
# 
# westdry_UT =
#   westdry_dlname %>% 
#   filter(RECORD < 186) 
# 
# westdry_AK =
#   westdry_dlname %>% 
#   filter(RECORD > 185) 



#####################

metadata_function <- function(dat){
  dat %>% 
    dplyr::select('X', 'TIMESTAMP', 'RECORD', 'site', 'position')%>% #pair down columns to what's needed for metadata
    mutate(Betterdate = strptime(TIMESTAMP, format("%m/%d/%Y %H:%M", tzone = "America/Anchorage", usetz = TRUE))) %>% #change date formatting, attempt to set timezone, fail
    mutate(redox_NUM_Avg = X) %>% #rename so that X column will stay when I later 
    mutate(redox_NUM_Std = X) %>% #rename
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

###

#run for UT timezones

# metadata_function <- function(dat){
#   dat %>% 
#     dplyr::select('X', 'TIMESTAMP', 'RECORD', 'site', 'position')%>% 
#     mutate(Betterdate = strptime(TIMESTAMP, format("%m/%d/%Y %H:%M", tz = "US/Denver", usetz = TRUE))) %>% 
#     mutate(redox_NUM_Avg = X) %>% 
#     mutate(redox_NUM_Std = X) %>%  
#     dplyr::select(-X)
#     
# }
# 
# metadata_function <- function(dat){
#   dat %>% 
#     dplyr::select('X', 'TIMESTAMP', 'RECORD', 'site', 'position')%>% 
#     mutate(Betterdate = strptime(TIMESTAMP, format("%m/%d/%Y %H:%M"))) %>% 
#     mutate(redox_NUM_Avg = X) %>% 
#     mutate(redox_NUM_Std = X) %>%  
#     dplyr::select(-X)
#   
# }



# #not working
# westmesic_metadata_tocombine$Betterdate <- force_tz(westmesic_metadata_tocombine$Betterdate, "MST")
# 
# 
# #not working
# westmesic_metadata_tocombine$Betterdate <- force_tz(westmesic_metadata_tocombine$Betterdate, "AKST")
# 
# #not working
# westmesic_metadata_tocombine = 
#   metadata_function(westmesic_UT) %>% 
#   lubridate::force_tz(Betterdate, tzone = "America/Anchorage")
# 
# 
# #not working
# westdry_metadata_tocombine = 
#   metadata_function(westdry_UT) %>% 
#   lubridate::force_tz(Betterdate, tzone = "America/Anchorage")




write.csv(westhydric_metadata, "processed/westhydric_metadata.csv")
write.csv(easthydric_metadata, "processed/easthydric_metadata.csv")
write.csv(westmesic_metadata, "processed/westmesic_metadata.csv")
write.csv(eastmesic_metadata, "processed/eastmesic_metadata.csv")
write.csv(westdry_metadata, "processed/westdry_metadata.csv")
write.csv(eastdry_metadata, "processed/eastdry_metadata.csv")

