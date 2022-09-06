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

# std_separate <- function(dat){
#   dat %>% 
#     mutate(redox_NUM_Avg = X) %>%
#     dplyr::select(-X) %>% 
#     # dplyr::select(c('X') & 
#     dplyr::select(starts_with('redox')) %>% 
#     dplyr::select(ends_with('Avg')) %>% 
#     #if needed, get rid of NA rows then columns. Not needed.
#     # filter(redox_NUM_Avg < 5989) %>%
#     # select_if(~ !any(is.na(.)))
#     
#     force()
# }


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
    #add 197 to all redox potentials to report data relative to the standard hydrogen electrode
    dplyr::mutate(avg_values_fixed = avg_values + 197) %>% 
    dplyr::summarize(avg_values_summarised = mean(avg_values_fixed),
                     std_values_summarised = sd(avg_values_fixed)/sqrt(n())) %>%
    mutate(depth_cm = as.numeric(depth_cm)) %>% 
    force()
  
  
}

#without summarising

depths_function_nosummary <- function(dat){
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
    #add 197 to all redox potentials to report data relative to the standard hydrogen electrode
    dplyr::mutate(avg_values_fixed = avg_values + 197) %>% 
    # dplyr::summarize(avg_values_summarised = mean(avg_values_fixed),
    #                  std_values_summarised = sd(avg_values_fixed)/sqrt(n())) %>%
    mutate(depth_cm = as.numeric(depth_cm)) %>% 
    force()
  
  
}
#run on all six dataframes to get site, position, depth, date, avg, std


##no summarise

westhydric_depths_nosummary = 
  depths_function_nosummary(westhydric_avg)%>% 
  na.omit()

easthydric_depths_nosummary = 
  depths_function_nosummary(easthydric_avg) %>% 
  na.omit()

westmesic_depths_nosummary = 
  depths_function_nosummary(westmesic_avg)%>% 
  na.omit()

eastmesic_depths_nosummary = 
  depths_function_nosummary(eastmesic_avg)%>% 
  na.omit()

westdry_depths_nosummary = 
  depths_function_nosummary(westdry_avg)%>% 
  na.omit()

eastdry_depths_nosummary = 
  depths_function_nosummary(eastdry_avg) %>% 
  na.omit()


##summarise

westhydric_depths = 
  depths_function(westhydric_avg)%>% 
  na.omit()

easthydric_depths = 
  depths_function(easthydric_avg) %>% 
  na.omit()

westmesic_depths = 
  depths_function(westmesic_avg)%>% 
  na.omit()

eastmesic_depths = 
  depths_function(eastmesic_avg)%>% 
  na.omit()

westdry_depths = 
  depths_function(westdry_avg)%>% 
  na.omit()

eastdry_depths = 
  depths_function(eastdry_avg) %>% 
  na.omit()


#####

all_combine_depthbins = 
  westhydric_depths %>% 
  bind_rows(easthydric_depths, westmesic_depths, eastmesic_depths, westdry_depths, eastdry_depths) %>% 
  #mutate(depth_bins = case_when(depth_cm <= 100 ~ cut_width(depth_cm, width = 1, center = 1))) %>% 
  mutate(depth_bins = cut_width(depth_cm, width = 5, center=2.5)) %>% 
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

#write csv

write.csv(all_combine, "processed/all_combine.csv")

write.csv(all_combine_depthbins, "processed/all_combine_depthbins.csv")


#######no summarise

all_combine_depthbins_nosummmary = 
  westhydric_depths_nosummary %>% 
  bind_rows(easthydric_depths_nosummary, westmesic_depths_nosummary, eastmesic_depths_nosummary, 
            westdry_depths_nosummary, eastdry_depths_nosummary) %>% 
  #mutate(depth_bins = case_when(depth_cm <= 100 ~ cut_width(depth_cm, width = 1, center = 1))) %>% 
  mutate(depth_bins = cut_width(depth_cm, width = 5, center=2.5)) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]","")) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[","")) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
  # now separate this into two different columns
  separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm")) %>% 
  mutate(depth_start_cm = as.integer(depth_start_cm),
         depth_stop_cm = as.integer(depth_stop_cm)) %>% 
  mutate(depth2 = depth_stop_cm - depth_start_cm)


all_combine_nosummary = 
  westhydric_depths_nosummary %>% 
  bind_rows(easthydric_depths_nosummary, westmesic_depths_nosummary, 
            eastmesic_depths_nosummary, westdry_depths_nosummary, eastdry_depths_nosummary)  

#write csv

write.csv(all_combine_nosummary, "processed/all_combine_nosummary.csv")

write.csv(all_combine_depthbins_nosummmary, "processed/all_combine_depthbins_nosummary.csv")

