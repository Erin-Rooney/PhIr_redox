#ECROONEY
#July 12 2022

#load all packages

source("code/0-packages.R")

#load data


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

write.csv(westhydric_dlname, "processed/westhydric_dlname.csv")
write.csv(easthydric_dlname, "processed/easthydric_dlname.csv")
write.csv(westmesic_dlname, "processed/westmesic_dlname.csv")
write.csv(eastmesic_dlname, "processed/eastmesic_dlname.csv")
write.csv(westdry_dlname, "processed/westdry_dlname.csv")
write.csv(eastdry_dlname, "processed/eastdry_dlname.csv")



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


write.csv(westhydric_metadata, "processed/westhydric_metadata.csv")
write.csv(easthydric_metadata, "processed/easthydric_metadata.csv")
write.csv(westmesic_metadata, "processed/westmesic_metadata.csv")
write.csv(eastmesic_metadata, "processed/eastmesic_metadata.csv")
write.csv(westdry_metadata, "processed/westdry_metadata.csv")
write.csv(eastdry_metadata, "processed/eastdry_metadata.csv")

