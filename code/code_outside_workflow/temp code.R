

eastmesic_depths =
  eastmesic_avg %>% 
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
  # dplyr::rename(position = Site,
  #               site = Area) %>% 
  # mutate(site = recode(site, "West" = "west",
  #                      "East" = "east")) %>% 
  # mutate(position = recode(position, "Hydric" = "hydric",
  #                          "Mesic" = "mesic",
  #                          "Dry" = "dry")) %>% 
  mutate(sensor = str_remove(sensor, "Redox_")) %>% 
  mutate(sensor = str_remove(sensor, "redox_")) %>% 
  mutate(sensor = str_remove(sensor, "_fromtip_Avg")) %>% 
  separate(sensor, sep = "_", into = c("datalogger", "probe_sensor")) %>% 
  separate(probe_sensor, sep = "-", into = c("probe", "sensor")) %>% 
  mutate(sensor = as.numeric(sensor))


combo_metadata =
  combo_dat %>% 
  dplyr::select('X', 'TIMESTAMP', 'RECORD', 'site', 'position')


combo_betterdate = 
  combo_metadata %>% 
  mutate(Betterdate = strptime(TIMESTAMP, format = ("%m/%d/%Y %H:%M"))) %>% 
  mutate(redox_NUM_Avg = X) %>% 
  mutate(redox_NUM_Std = X) %>%  
  dplyr::select(-X)


# hydric_redox = 
#   hydric_dat %>% 
#   mutate(redox_NUM_Avg = X) %>% 
#   mutate(redox_NUM_Std = X) %>% 
#   dplyr::select(-X) %>% 
#   # dplyr::select(c('X') & 
#   dplyr::select(starts_with('redox')) 
#   #rename(X = redox_NUM_Avg) 

combo_redox = 
  combo_dat %>% 
  mutate(redox_NUM_Avg = X) %>% 
  mutate(redox_NUM_Std = X) %>% 
  dplyr::select(-X) %>% 
  # dplyr::select(c('X') & 
  dplyr::select(starts_with('redox')) 
#rename(X = redox_NUM_Avg) %>% 

#X is being changed to an ID number that can used to join the pivoted data with the metadata later on
#named with "Avg" or "Std" at the end so it will be included in the next lines of code

# hydric_redox_avg = 
#   hydric_redox %>% 
#   dplyr::select(ends_with('Avg')) 

#okay, get rid of NA rows then columns. Left with only data frame datalogger (no NAs)

combo_redox_avg = 
  combo_redox %>% 
  dplyr::select(ends_with('Avg')) %>% 
  filter(redox_NUM_Avg < 5989) %>%
  select_if(~ !any(is.na(.)))

# hydric_redox_std = 
#   hydric_redox %>% 
#   dplyr::select(ends_with('Std')) 

combo_redox_std = 
  combo_redox %>% 
  dplyr::select(ends_with('Std')) %>% 
  filter(redox_NUM_Std < 5989) %>%
  select_if(~ !any(is.na(.)))

# hydric_redox_longer_avg =
#   hydric_redox_avg %>%
#   left_join(hydric_betterdate, by = 'redox_NUM_Avg') %>% 
#   pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "Betterdate"),
#                names_to = "redox_avg", values_to = "avg_values") %>% 
#   mutate(redox_avg = recode(redox_avg, 'redox_5.2.3_fromtip_Avg' = "redox_5_2_3_fromtip_Avg")) %>% 
#   mutate(redox_avg = str_remove(redox_avg, "Redox_")) %>% 
#   mutate(redox_avg = str_remove(redox_avg, "redox_")) %>% 
#   mutate(redox_avg = str_remove(redox_avg, "_fromtip_Avg")) %>% 
#   separate(redox_avg, sep = "_", into = c("datalogger", "probe_sensor")) %>%
#   mutate(probe_sensor = recode(probe_sensor, "1.8" = "1_8", "1.7" = "1_7", "1.6" = "1_6", "1.5" = "1_5", "1.4" = "1_4", "1.3" = "1_3", "1.2" = "1_2", "1.1" = "1_1",
#                                "2.8" = "2_8", "2.7" = "2_7", "2.6" = "2_6", "2.5" = "2_5", "2.4" = "2_4", "2.3" = "2_3", "2.2" = "2_2", "2.1" = "2_1",
#                                "3.8" = "3_8", "3.7" = "3_7", "3.6" = "3_6", "3.5" = "3_5", "3.4" = "3_4", "3.3" = "3_3", "3.2" = "3_2", "3.1" = "3_1")) %>% 
#   separate(probe_sensor, sep = "_", into = c("probe", "sensor")) 

combo_redox_longer_avg =
  combo_redox_avg %>%
  left_join(combo_betterdate, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_avg", values_to = "avg_values") %>% 
  mutate(redox_avg = recode(redox_avg, 'redox_5.2.3_fromtip_Avg' = "redox_5_2_3_fromtip_Avg")) %>% 
  mutate(redox_avg = str_remove(redox_avg, "Redox_")) %>% 
  mutate(redox_avg = str_remove(redox_avg, "redox_")) %>% 
  mutate(redox_avg = str_remove(redox_avg, "_fromtip_Avg")) %>% 
  separate(redox_avg, sep = "_", into = c("datalogger", "probe_sensor")) %>%
  mutate(probe_sensor = recode(probe_sensor, "1.8" = "1_8", "1.7" = "1_7", "1.6" = "1_6", "1.5" = "1_5", "1.4" = "1_4", "1.3" = "1_3", "1.2" = "1_2", "1.1" = "1_1",
                               "2.8" = "2_8", "2.7" = "2_7", "2.6" = "2_6", "2.5" = "2_5", "2.4" = "2_4", "2.3" = "2_3", "2.2" = "2_2", "2.1" = "2_1",
                               "3.8" = "3_8", "3.7" = "3_7", "3.6" = "3_6", "3.5" = "3_5", "3.4" = "3_4", "3.3" = "3_3", "3.2" = "3_2", "3.1" = "3_1")) %>% 
  separate(probe_sensor, sep = "_", into = c("probe", "sensor")) 


# hydric_redox_longer_std =
#   hydric_redox_std %>%
#   left_join(hydric_betterdate, by = 'redox_NUM_Std') %>% 
#   pivot_longer(-c("TIMESTAMP", "RECORD", "site", "Betterdate"),
#                names_to = "redox_std", values_to = "std_values") %>% 
#   mutate(redox_std = recode(redox_std, 'redox_5.2.3_fromtip_Std' = "redox_5_2_3_fromtip_Std")) %>% 
#   mutate(redox_std = str_remove(redox_std, "Redox_")) %>% 
#   mutate(redox_std = str_remove(redox_std, "redox_")) %>% 
#   mutate(redox_std = str_remove(redox_std, "_fromtip_Std")) %>% 
#   separate(redox_std, sep = "_", into = c("datalogger", "probe_sensor")) %>%
#   mutate(probe_sensor = recode(probe_sensor, "1.8" = "1_8", "1.7" = "1_7", "1.6" = "1_6", "1.5" = "1_5", "1.4" = "1_4", "1.3" = "1_3", "1.2" = "1_2", "1.1" = "1_1",
#                                "2.8" = "2_8", "2.7" = "2_7", "2.6" = "2_6", "2.5" = "2_5", "2.4" = "2_4", "2.3" = "2_3", "2.2" = "2_2", "2.1" = "2_1",
#                                "3.8" = "3_8", "3.7" = "3_7", "3.6" = "3_6", "3.5" = "3_5", "3.4" = "3_4", "3.3" = "3_3", "3.2" = "3_2", "3.1" = "3_1")) %>% 
#   separate(probe_sensor, sep = "_", into = c("probe", "sensor")) 

combo_redox_longer_std =
  combo_redox_std %>%
  left_join(combo_betterdate, by = 'redox_NUM_Std') %>% 
  pivot_longer(-c("TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_std", values_to = "std_values") %>% 
  mutate(redox_std = recode(redox_std, 'redox_5.2.3_fromtip_Std' = "redox_5_2_3_fromtip_Std")) %>% 
  mutate(redox_std = str_remove(redox_std, "Redox_")) %>% 
  mutate(redox_std = str_remove(redox_std, "redox_")) %>% 
  mutate(redox_std = str_remove(redox_std, "_fromtip_Std")) %>% 
  separate(redox_std, sep = "_", into = c("datalogger", "probe_sensor")) %>%
  mutate(probe_sensor = recode(probe_sensor, "1.8" = "1_8", "1.7" = "1_7", "1.6" = "1_6", "1.5" = "1_5", "1.4" = "1_4", "1.3" = "1_3", "1.2" = "1_2", "1.1" = "1_1",
                               "2.8" = "2_8", "2.7" = "2_7", "2.6" = "2_6", "2.5" = "2_5", "2.4" = "2_4", "2.3" = "2_3", "2.2" = "2_2", "2.1" = "2_1",
                               "3.8" = "3_8", "3.7" = "3_7", "3.6" = "3_6", "3.5" = "3_5", "3.4" = "3_4", "3.3" = "3_3", "3.2" = "3_2", "3.1" = "3_1")) %>% 
  separate(probe_sensor, sep = "_", into = c("probe", "sensor")) 


# hydric_redox_avgstd =
#   hydric_redox_longer_avg %>% 
#   left_join(hydric_redox_longer_std)
# 
# write.csv(hydric_redox_avgstd, "processed/hydric_redox_avgstd.csv")



# hydric_redox_avgstd = read.csv("processed/hydric_redox_avgstd.csv")

combo_redox_avgstd = read.csv("processed/combo_redox_avgstd.csv")

sensor_depths_prename = read.csv("raw/datalogger_depths.csv") 

sensor_depths = 
  sensor_depths_prename %>% 
  dplyr::rename(position = Site,
                site = Area) %>% 
  mutate(site = recode(site, "West" = "west",
                       "East" = "east")) %>% 
  mutate(position = recode(position, "Hydric" = "hydric",
                           "Mesic" = "mesic",
                           "Dry" = "dry"))


# hydric_redox_betterdates =
#   hydric_redox_avgstd %>%  
#   dplyr::select(X, site, Betterdate, sensor)

combo_redox_betterdates =
  combo_redox_avgstd %>%  
  dplyr::select(X, site, Betterdate, sensor)

sensor_depths_processed = 
  sensor_depths %>% 
  mutate(Sensor = str_remove(Sensor, "Redox_")) %>% 
  mutate(Sensor = str_remove(Sensor, "redox_")) %>% 
  mutate(Sensor = str_remove(Sensor, "_fromtip_Avg")) %>% 
  separate(Sensor, sep = "_", into = c("datalogger", "probe_sensor")) %>% 
  separate(probe_sensor, sep = "-", into = c("probe", "sensor")) %>% 
  mutate(sensor = as.numeric(sensor))  
#dplyr::select(-Plot)


# hydric_redox_depths_summarised = 
#   hydric_redox_avgstd %>% 
#   mutate(sensor = as.numeric(sensor)) %>% 
#   left_join(sensor_depths_processed) %>% 
#   group_by(X, site, sensor, depth_cm) %>%
#   dplyr::summarize(avg_values_summarised = mean(avg_values)) %>% 
#   left_join(hydric_redox_betterdates) %>% 
#   mutate(Betterdate = as.Date(Betterdate, format = ("%Y-%m-%d %H:%M:%S"))) 
# 
# hydric_redox_forggplot =
#   hydric_redox_depths_summarised %>% 
#   mutate(depth_cm = as.numeric(depth_cm)) 

combo_redox_depths_summarised = 
  combo_redox_avgstd %>% 
  mutate(sensor = as.numeric(sensor)) %>% 
  left_join(sensor_depths_processed) %>% 
  group_by(X, site, position, sensor, depth_cm) %>%
  dplyr::summarize(avg_values_summarised = mean(avg_values)) %>% 
  left_join(combo_redox_betterdates) %>% 
  mutate(Betterdate = as.Date(Betterdate, format = ("%Y-%m-%d %H:%M:%S"))) 

combo_redox_forggplot =
  combo_redox_depths_summarised %>% 
  mutate(depth_cm = as.numeric(depth_cm)) 

write.csv(combo_redox_forggplot, "processed/combo_redox_withdepths.csv")


library(scales)

# hydric_redox_forggplot %>% 
#   na.omit() %>% 
#   ggplot(aes(x = Betterdate, y = depth_cm, fill = avg_values_summarised))+
#   geom_bar(position = "stack", stat= "identity")+
#     scale_x_date(date_breaks = "1 week" , date_labels = "%m-%d-%Y")+
#   scale_y_reverse()+
#   scale_fill_gradientn(colors = (PNWColors::pnw_palette("Bay")))+  
#   theme_er1()+
#   theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90))+
#   facet_grid(site~.)

combo_redox_forggplot %>% 
  na.omit() %>% 
  ggplot(aes(x = Betterdate, y = depth_cm, fill = avg_values_summarised))+
  geom_bar(position = "stack", stat= "identity")+
  scale_x_date(date_breaks = "1 week" , date_labels = "%m-%d-%Y")+
  scale_y_reverse()+
  scale_fill_gradientn(colors = (PNWColors::pnw_palette("Bay")))+  
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(site~position)



#June 1 2021
#ECRooney
#Data logger data

#load all packages

source("code/0-packages.R")

#load data

# hydric_dat = read.csv("processed/hydric_combine.csv")

combo_dat = read.csv("processed/all_combine.csv")


#ggplot having issues

combo_dat_forggplot =
  combo_dat %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>% 
  mutate(depth_cm = as.numeric(depth_cm)) %>% 
  na.omit()



combo_dat_forggplot %>% 
  na.omit() %>% 
  ggplot(aes(x = as.Date(Betterdate), y=depth_start_cm, yend=depth_stop_cm, fill = avg_values_summarised))+
  geom_bar(position = "stack", stat = "identity")+
  scale_x_date(date_breaks = "1 week" , date_labels = "%m-%d-%Y")+
  scale_y_reverse()+
  
  scale_fill_gradientn(colors = (PNWColors::pnw_palette("Bay")))+  
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(position~site, scales = 'free_y') 







combo_redox_avgstd =
  combo_redox_longer_avg %>% 
  left_join(combo_redox_longer_std)

write.csv(combo_redox_avgstd, "processed/combo_redox_avgstd.csv")

