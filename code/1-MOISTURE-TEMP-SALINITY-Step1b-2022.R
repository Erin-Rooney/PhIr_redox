#ECROONEY
#July 12 2022

#load all packages


source("code/0-packages.R")

#load data

westhydric_dlname = read.csv("processed/westhydric_dlname2022.csv") 
easthydric_dlname = read.csv("processed/easthydric_dlname2022.csv")
westmesic_dlname = read.csv("processed/westmesic_dlname2022.csv")
eastmesic_dlname = read.csv("processed/eastmesic_dlname2022.csv")
westdry_dlname = read.csv("processed/westdry_dlname2022.csv")
eastdry_dlname = read.csv("processed/eastdry_dlname2022.csv")

westhydric_metadata = read.csv("processed/westhydric_metadata2022.csv")
easthydric_metadata = read.csv("processed/easthydric_metadata2022.csv")
westmesic_metadata = read.csv("processed/westmesic_metadata2022.csv")
eastmesic_metadata = read.csv("processed/eastmesic_metadata2022.csv")
westdry_metadata = read.csv("processed/westdry_metadata2022.csv")
eastdry_metadata = read.csv("processed/eastdry_metadata2022.csv")

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

moisture_separate_std <- function(dat){
  dat %>% 
    dplyr::rename('soilmoisture_NUM_Std' = 'X.1') %>%
    # dplyr::select(c('X') & 
    dplyr::select(starts_with('soilmoisture'),
                  starts_with('soiltemperature'),
                  starts_with("soilsalinity")) %>% 
    dplyr::select(ends_with('Std')) %>% 
    dplyr::rename(redox_NUM_Std = soilmoisture_NUM_Std,
                  moisture_5 = soilmoisture5cm_Std,
                  moisture_15 = soilmoisture15cm_Std,
                  moisture_25 = soilmoisture25cm_Std,
                  temp_5 = soiltemperature5cm_Std,
                  temp_15 = soiltemperature15cm_Std,
                  temp_25 = soiltemperature25cm_Std,
                  salinity_5 = soilsalinity5cm_Std,
                  salinity_15 = soilsalinity15cm_Std,
                  salinity_25 = soilsalinity25cm_Std
    ) %>%
    force()
}


# run all 6 data frames

westhydric_moisture = 
  moisture_separate(westhydric_dlname) %>% 
  #dplyr::select(-soiltemperature_Avg) %>% 
  left_join(westhydric_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "X", "redox_NUM_Std", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'avg_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "avg_values") %>% 
  # group_by(RECORD, TIMESTAMP, depth) %>%
  # dplyr::mutate(n = n()) %>%
  # ungroup() %>%
  dplyr::select(-c(redox_NUM_Avg, X)) %>% 
  mutate(moisture = as.numeric(moisture),
         temp = as.numeric(temp),
         salinity = as.numeric(salinity)) %>% 
  force()

westhydric_moisture_std = 
  moisture_separate_std(westhydric_dlname) %>% 
  left_join(westhydric_metadata, by = 'redox_NUM_Std') %>% 
  filter(redox_NUM_Std > 2304) %>% 
  dplyr::select(-redox_NUM_Avg) %>% 
  pivot_longer(-c("redox_NUM_Std", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'std_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "std_values") %>% 
  # group_by(RECORD, TIMESTAMP, depth) %>%
  # dplyr::mutate(n = n()) %>%
  # ungroup() %>%
  dplyr::select(-c(redox_NUM_Std, X)) %>% 
  mutate(moisture = as.numeric(moisture),
         temp = as.numeric(temp),
         salinity = as.numeric(salinity)) %>% 
  force()

westmesic_moisture = 
  moisture_separate(westmesic_dlname) %>% 
  dplyr::select(-soiltemperature_Avg) %>% 
  left_join(westmesic_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'avg_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "avg_values") %>% 
  dplyr::select(-c(redox_NUM_Avg, X)) %>% 
  force()

westmesic_moisture_std = 
  moisture_separate_std(westmesic_dlname) %>% 
  left_join(westmesic_metadata, by = 'redox_NUM_Std') %>% 
  #filter(redox_NUM_Std > 2304) %>% 
  dplyr::select(-redox_NUM_Avg) %>% 
  pivot_longer(-c("redox_NUM_Std", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'std_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "std_values") %>% 
  # group_by(RECORD, TIMESTAMP, depth) %>%
  # dplyr::mutate(n = n()) %>%
  # ungroup() %>%
  dplyr::select(-c(redox_NUM_Std, X)) %>% 
  mutate(moisture = as.numeric(moisture),
         temp = as.numeric(temp),
         salinity = as.numeric(salinity)) %>% 
  force()

westdry_moisture = 
  moisture_separate(westdry_dlname) %>% 
  dplyr::select(-soiltemperature_Avg) %>% 
  left_join(westdry_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "X", "TIMESTAMP", "redox_NUM_Std", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'avg_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "avg_values") %>% 
  dplyr::select(-c(redox_NUM_Avg, X)) %>% 
  mutate(moisture = as.numeric(moisture),
         temp = as.numeric(temp),
         salinity = as.numeric(salinity)) %>% 
  force()


westdry_moisture_std = 
  moisture_separate_std(westdry_dlname) %>% 
  left_join(westdry_metadata, by = 'redox_NUM_Std') %>% 
  filter(redox_NUM_Std > 1981) %>% 
  dplyr::select(-redox_NUM_Avg, -soiltemperature_Std) %>% 
  pivot_longer(-c("redox_NUM_Std", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'std_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "std_values") %>% 
  # group_by(RECORD, TIMESTAMP, depth) %>%
  # dplyr::mutate(n = n()) %>%
  # ungroup() %>%
  dplyr::select(-c(redox_NUM_Std, X)) %>% 
  mutate(moisture = as.numeric(moisture),
         temp = as.numeric(temp),
         salinity = as.numeric(salinity)) %>% 
  force()

easthydric_moisture = 
  moisture_separate(easthydric_dlname) %>% 
  dplyr::select(-soiltemperature_Avg) %>% 
  left_join(easthydric_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'avg_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "avg_values") %>% 
  dplyr::select(-c(redox_NUM_Avg, X)) %>% 
  filter(RECORD != c(0)) %>% 
  force()

easthydric_moisture_std = 
  moisture_separate_std(easthydric_dlname) %>% 
  left_join(easthydric_metadata, by = 'redox_NUM_Std') %>% 
  #filter(redox_NUM_Std > 2304) %>% 
  dplyr::select(-redox_NUM_Avg) %>% 
  pivot_longer(-c("redox_NUM_Std", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'std_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "std_values") %>% 
  # group_by(RECORD, TIMESTAMP, depth) %>%
  # dplyr::mutate(n = n()) %>%
  # ungroup() %>%
  dplyr::select(-c(redox_NUM_Std, X)) %>% 
  mutate(moisture = as.numeric(moisture),
         temp = as.numeric(temp),
         salinity = as.numeric(salinity)) %>% 
  force()

eastmesic_moisture = 
  moisture_separate(eastmesic_dlname) %>% 
  dplyr::select(-soiltemperature_Avg) %>% 
  left_join(eastmesic_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'avg_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "avg_values")%>% 
  dplyr::select(-c(redox_NUM_Avg, X)) %>%
  filter(RECORD != c(1628, 766)) %>% 
  force() 

eastmesic_moisture_std = 
  moisture_separate_std(eastmesic_dlname) %>% 
  left_join(eastmesic_metadata, by = 'redox_NUM_Std') %>% 
  #filter(redox_NUM_Std > 2304) %>% 
  dplyr::select(-redox_NUM_Avg) %>% 
  pivot_longer(-c("redox_NUM_Std", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'std_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "std_values") %>% 
  # group_by(RECORD, TIMESTAMP, depth) %>%
  # dplyr::mutate(n = n()) %>%
  # ungroup() %>%
  dplyr::select(-c(redox_NUM_Std, X)) %>% 
  mutate(moisture = as.numeric(moisture),
         temp = as.numeric(temp),
         salinity = as.numeric(salinity)) %>% 
  force()

eastdry_moisture = 
  moisture_separate(eastdry_dlname) %>%
  dplyr::select(-soiltemperature_Avg) %>% 
  left_join(eastdry_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'avg_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "avg_values")%>% 
  dplyr::select(-c(redox_NUM_Avg, X)) %>% 
  filter(RECORD != c(7019)) %>% 
  force()

eastdry_moisture_std = 
  moisture_separate_std(eastdry_dlname) %>% 
  left_join(eastdry_metadata, by = 'redox_NUM_Std') %>% 
  #filter(redox_NUM_Std > 2304) %>% 
  dplyr::select(-redox_NUM_Avg) %>% 
  pivot_longer(-c("redox_NUM_Std", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'std_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "std_values") %>% 
  # group_by(RECORD, TIMESTAMP, depth) %>%
  # dplyr::mutate(n = n()) %>%
  # ungroup() %>%
  dplyr::select(-c(redox_NUM_Std, X)) %>% 
  mutate(moisture = as.numeric(moisture),
         temp = as.numeric(temp),
         salinity = as.numeric(salinity)) %>% 
  force()

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
  dplyr::select(-c(redox_NUM_Std)) %>% 
  na.omit() %>% 
  group_by(site, position, TIMESTAMP, depth) %>%
  dplyr::mutate(n = n()) %>%
  ungroup() %>% 
  force()


#
#write.csv(moisture_combine, "processed/moisture_temp_salinity_avgs_combine.csv")
# 
#write.csv(moisture_combine, "processed/2022moisture_temp_salinity_avgs_combine.csv")
write.csv(moisture_combine, "processed/2022final_temp_salinity_avgs.csv")

