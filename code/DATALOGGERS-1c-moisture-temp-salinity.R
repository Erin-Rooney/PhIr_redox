#ECROONEY
#July 12 2022

#load all packages

source("code/0-packages.R")

#load data

westhydric_dlname = read.csv("processed/westhydric_dlname.csv")
easthydric_dlname = read.csv("processed/easthydric_dlname.csv")
westmesic_dlname = read.csv("processed/westmesic_dlname.csv")
eastmesic_dlname = read.csv("processed/eastmesic_dlname.csv")
westdry_dlname = read.csv("processed/westdry_dlname.csv")
eastdry_dlname = read.csv("processed/eastdry_dlname.csv")

westhydric_metadata = read.csv("processed/westhydric_metadata.csv")
easthydric_metadata = read.csv("processed/easthydric_metadata.csv")
westmesic_metadata = read.csv("processed/westmesic_metadata.csv")
eastmesic_metadata = read.csv("processed/eastmesic_metadata.csv")
westdry_metadata = read.csv("processed/westdry_metadata.csv")
eastdry_metadata = read.csv("processed/eastdry_metadata.csv")

#separate data columns only

#average
#X is being changed to an ID number that can used to join the pivoted data with the metadata later on
#named with "Avg" or "Std" at the end so it will be included in the next lines of code


moisture_separate <- function(dat){
  dat %>% 
    dplyr::rename('soilmoisture_NUM_Avg' = 'X.1') %>%
    # dplyr::select(c('X') & 
    dplyr::select(starts_with('soilmoisture'),
                  starts_with('soiltemperature'),
                  starts_with("soilsalinity")) %>% 
    dplyr::select(ends_with('Avg')) %>% 
    dplyr::rename(redox_NUM_Avg = soilmoisture_NUM_Avg,
                  moisture_5 = soilmoisture5cm_Avg,
                  moisture_15 = soilmoisture15cm_Avg,
                  moisture_25 = soilmoisture25cm_Avg,
                  temp_5 = soiltemperature5cm_Avg,
                  temp_15 = soiltemperature15cm_Avg,
                  temp_25 = soiltemperature25cm_Avg,
                  salinity_5 = soilsalinity5cm_Avg,
                  salinity_15 = soilsalinity15cm_Avg,
                  salinity_25 = soilsalinity25cm_Avg
    ) %>%
    force()
}

# run all 6 data frames



westhydric_moisture = 
  moisture_separate(westhydric_dlname) %>% 
  #dplyr::select(-soiltemperature_Avg) %>% 
  left_join(westhydric_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'avg_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "avg_values")


westmesic_moisture = 
  moisture_separate(westmesic_dlname) %>% 
  dplyr::select(-soiltemperature_Avg) %>% 
  left_join(westhydric_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'avg_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "avg_values")

westdry_moisture = 
  moisture_separate(westdry_dlname) %>% 
  dplyr::select(-soiltemperature_Avg) %>% 
  left_join(westhydric_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'avg_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "avg_values")

easthydric_moisture = 
  moisture_separate(easthydric_dlname) %>% 
  dplyr::select(-soiltemperature_Avg) %>% 
  left_join(westhydric_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'avg_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "avg_values")

eastmesic_moisture = 
  moisture_separate(eastmesic_dlname) %>% 
  dplyr::select(-soiltemperature_Avg) %>% 
  left_join(westhydric_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'avg_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "avg_values")

eastdry_moisture = 
  moisture_separate(eastdry_dlname) %>%
  dplyr::select(-soiltemperature_Avg) %>% 
  left_join(westhydric_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'avg_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "avg_values")


#####

# moisture_combine_depthbins = 
#   westhydric_depths %>% 
#   bind_rows(easthydric_depths, westmesic_depths, eastmesic_depths, westdry_depths, eastdry_depths) %>% 
#   mutate(depth_bins = case_when(depth_cm <= 100 ~ cut_width(depth_cm, width = 1, center = 1))) %>% 
#   mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]","")) %>% 
#   mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[","")) %>% 
#   mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
#   # now separate this into two different columns
#   separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm")) %>% 
#   mutate(depth_start_cm = as.integer(depth_start_cm),
#          depth_stop_cm = as.integer(depth_stop_cm)) %>% 
#   mutate(depth2 = depth_stop_cm - depth_start_cm)


moisture_combine = 
  westhydric_moisture %>% 
  bind_rows(easthydric_moisture, westmesic_moisture, eastmesic_moisture, westdry_moisture, eastdry_moisture) %>% 
  na.omit()

#write csv

write.csv(moisture_combine, "processed/moisture_temp_salinity_avgs_combine.csv")

#write.csv(all_combine_depthbins, "processed/all_combine_depthbins.csv")

