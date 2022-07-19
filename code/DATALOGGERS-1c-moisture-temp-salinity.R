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
  pivot_wider(names_from = "type", values_from = "avg_values") %>% 
  # group_by(RECORD, TIMESTAMP, depth) %>%
  # dplyr::mutate(n = n()) %>%
  # ungroup() %>%
  dplyr::select(-c(redox_NUM_Avg, X)) %>% 
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

westdry_moisture = 
  moisture_separate(westdry_dlname) %>% 
  dplyr::select(-soiltemperature_Avg) %>% 
  left_join(westdry_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "X", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = c("type", "depth"), names_sep= "_", values_to = 'avg_values') %>% 
  filter(type != 'redox') %>% 
  pivot_wider(names_from = "type", values_from = "avg_values") %>% 
  dplyr::select(-c(redox_NUM_Avg, X)) %>% 
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
  na.omit() %>% 
  group_by(site, position, TIMESTAMP, depth) %>%
  dplyr::mutate(n = n()) %>%
  ungroup() %>% 
  force()

##### Now there are mostly N = 1, with some N = 2 (the dupes)
#
#get rid of duplicates

#the below test varifies that only west dry and west mesic have duplicates with diverging data.

# moisture_combine_dupes_test = 
#   moisture_combine %>% 
#   filter(n == 2) %>% 
#   group_by(TIMESTAMP, site, position, depth) %>% 
#   dplyr::summarise(sd = sd(salinity)/sqrt(n())) %>% 
#   ungroup() %>% 
#   filter(sd > 0)


#this is not a real averaging below, the data is identical, so this is 
#the clunky path I'm taking to get rid of one row.
#   

moisture_combine_dupes_hydric = 
  moisture_combine %>%
  filter(position == 'hydric' & n == 2) %>% 
  group_by(TIMESTAMP, site, position, Betterdate, depth) %>% 
  dplyr::summarise(moisture2 = mean(moisture),
                   temp2 = mean(temp),
                   salinity2 = mean(salinity)) %>% 
  rename(moisture = moisture2,
         temp = temp2,
         salinity = salinity2) %>% 
  group_by(site, position, TIMESTAMP, depth) %>%
  dplyr::mutate(n = n()) %>%
  ungroup() 
  
#now the weirder part
moisture_combine_dupes_nothydric_pretest = 
  moisture_combine %>%
  filter(position != 'hydric' & n == 2) %>% 
  group_by(TIMESTAMP, site, position, Betterdate, depth) %>% 
  group_by(site, position, TIMESTAMP, depth) %>%
  dplyr::mutate(n = n()) %>%
  ungroup()


moisture_combine_dupes_nothydric_test =
  moisture_combine_dupes_nothydric %>%
  group_by(TIMESTAMP, site, position, depth) %>%
  # dplyr::summarise(sdsal = sd(salinity)/sqrt(n()),
  #                  sdtemp = sd(temp)/sqrt(n()),
  #                  sdmoist = sd(moisture)/sqrt(n())) %>% 
  dplyr::summarise(sdsal = sd(salinity)/sqrt(n()),
                   sdtemp = sd(temp)/sqrt(n()),
                   sdmoist = sd(moisture)/sqrt(n())) 

#okay. Here we go. I am going through and picking out the specific timestamps (before and after dupe)
#then I am picking the dupe that is most similar and removing the other


dupes_westdry_5cm_628 = 
  moisture_combine %>%
  mutate(dupe = case_when(grepl("6/28/2021", TIMESTAMP)~ "keep")) %>% 
  filter(site == 'west' & position == "dry" & depth == 5)

dupes_westdry_westmesic_628 = 
  moisture_combine %>%
  mutate(dupe = case_when(grepl("6/28/2021", TIMESTAMP)~ "keep")) %>% 
  filter(site == 'west' & position != "hydric")


moisture_combine_nodupes =
  moisture_combine %>% 
  dplyr::select(-RECORD) %>% 
  filter(n != 2) %>% 
  bind_rows(moisture_combine_dupes_hydric) %>% 
  
  


#calculate n to see which timestamps were duplicates 



write.csv(moisture_combine_dupes_nothydric_test, "processed/dupe_test.csv")



#write csv

write.csv(moisture_combine, "processed/moisture_temp_salinity_avgs_combine.csv")

write.csv(moisture_combine, "processed/moisture_temp_salinity_avgs_combine.csv")

#write.csv(all_combine_depthbins, "processed/all_combine_depthbins.csv")

