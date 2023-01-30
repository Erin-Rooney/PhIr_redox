#ECROONEY
#Jan 3 2023

#load all packages

source("code/0-packages.R")

#load data


#Data logger data input

westhydric_dlraw = read.csv("raw/west_hydric_2022.csv")
easthydric_dlraw = read.csv("raw/east_hydric_2022.csv")
westmesic_dlraw = read.csv("raw/west_mesic_2022.csv") 
eastmesic_dlraw = read.csv("raw/east_mesic_2022.csv")
westdry_dlraw = read.csv("raw/west_dry_2022.csv") 
eastdry_dlraw = read.csv("raw/east_dry_2022.csv")

#remove ptemp column, add site, position, and row numbers "X"


#################

westhydric_dlname = 
  westhydric_dlraw %>% 
  mutate(site = 'west', position = "hydric") %>% 
  #dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 4134)) 


easthydric_dlname = 
  easthydric_dlraw %>% 
  mutate(site = 'east', position = "hydric")%>% 
  dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 2261))  


westmesic_dlname = 
  westmesic_dlraw %>% 
  mutate(site = 'west', position = "mesic")%>% 
  dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 1630))  


eastmesic_dlname = 
  eastmesic_dlraw %>% 
  mutate(site = 'east', position = "mesic")%>% 
  dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 4322))  


westdry_dlname = 
  westdry_dlraw %>% 
  mutate(site = 'west', position = "dry") %>% 
  dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 3487))  


eastdry_dlname = 
  eastdry_dlraw %>% 
  mutate(site = 'east', position = "dry") %>% 
  dplyr::select(!ptemp) %>% 
  mutate("X" = seq.int(1, by = 1, length.out = 4234))

write.csv(westhydric_dlname, "processed/westhydric_dlname2022.csv")
write.csv(easthydric_dlname, "processed/easthydric_dlname2022.csv")
write.csv(westmesic_dlname, "processed/westmesic_dlname2022.csv")
write.csv(eastmesic_dlname, "processed/eastmesic_dlname2022.csv")
write.csv(westdry_dlname, "processed/westdry_dlname2022.csv")
write.csv(eastdry_dlname, "processed/eastdry_dlname2022.csv")



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



write.csv(westhydric_metadata, "processed/westhydric_metadata2022.csv")
write.csv(easthydric_metadata, "processed/easthydric_metadata2022.csv")
write.csv(westmesic_metadata, "processed/westmesic_metadata2022.csv")
write.csv(eastmesic_metadata, "processed/eastmesic_metadata2022.csv")
write.csv(westdry_metadata, "processed/westdry_metadata2022.csv")
write.csv(eastdry_metadata, "processed/eastdry_metadata2022.csv")

