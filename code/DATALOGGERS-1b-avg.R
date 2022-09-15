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

#install data cleaning package

# install.packages("fable")
# install.packages("forecast")


westhydric_avg = 
  avg_separate(westhydric_dlname) %>% 
  left_join(westhydric_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_avg", values_to = "avg_values") %>% 
  #add 197 to all redox potentials to report data relative to the standard hydrogen electrode
  dplyr::mutate(avg_values_fixed = avg_values + 197) %>% 
  filter(avg_values_fixed < 1000) %>% 
  filter(avg_values_fixed > -600) 

westmesic_avg = 
  avg_separate(westmesic_dlname) %>% 
  left_join(westmesic_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_avg", values_to = "avg_values") %>% 
  dplyr::mutate(avg_values_fixed = avg_values + 197) %>% 
  filter(avg_values_fixed < 1000) %>% 
  filter(avg_values_fixed > -600)

westdry_avg = 
  avg_separate(westdry_dlname) %>% 
  left_join(westdry_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_avg", values_to = "avg_values") %>% 
  dplyr::mutate(avg_values_fixed = avg_values + 197) %>% 
  filter(avg_values_fixed < 1000) %>% 
  filter(avg_values_fixed > -600) 

easthydric_avg = 
  avg_separate(easthydric_dlname) %>% 
  left_join(easthydric_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_avg", values_to = "avg_values") %>% 
  dplyr::mutate(avg_values_fixed = avg_values + 197) %>% 
  filter(avg_values_fixed < 1000) %>% 
  filter(avg_values_fixed > -600)

eastmesic_avg = 
  avg_separate(eastmesic_dlname) %>% 
  left_join(eastmesic_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_avg", values_to = "avg_values") %>% 
  dplyr::mutate(avg_values_fixed = avg_values + 197) %>% 
  filter(avg_values_fixed < 1000) %>% 
  filter(avg_values_fixed > -600)

eastdry_avg = 
  avg_separate(eastdry_dlname) %>% 
  left_join(eastdry_metadata, by = 'redox_NUM_Avg') %>% 
  pivot_longer(-c("redox_NUM_Avg", "TIMESTAMP", "RECORD", "site", "position", "Betterdate"),
               names_to = "redox_avg", values_to = "avg_values") %>%
  dplyr::mutate(avg_values_fixed = avg_values + 197) %>% 
  filter(avg_values_fixed < 1000) %>% 
  filter(avg_values_fixed > -600)


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


#separate poorly labeled columns into meaningful metadata

 depths_function <- function(dat){
  dat  %>% 
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
    

    force()
  
  
}

 
##depths
 
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
 
 #data cleaning bonanza
 
 datacleaning_function <- function(dat){
   dat %>% 
     mutate(lag = lag(avg_values_fixed)-avg_values_fixed,
            lead = lead(avg_values_fixed)-avg_values_fixed) %>% 
     mutate(laglead = case_when(lag > 100 & lead < -100 ~ 'artifact',
                                lag < -100 & lead > 100 ~ 'artifact'),
            laglead = if_else(is.na(laglead), "keep", laglead)) %>% 
     filter(laglead == "keep")
   
 }
 
 
#WEST HYDRIC PROBE 1 SENSORS 1-8 
 
westhydric_depths_outliers_1_1 = 
   westhydric_depths %>% 
   filter(probe == "1" & sensor == "1") 
 
westhydric_depths_outliers_1_2 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "2") 


westhydric_depths_outliers_1_3 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "3") 


westhydric_depths_outliers_1_4 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "4") 

westhydric_depths_outliers_1_5 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "5") 

westhydric_depths_outliers_1_6 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "6") 


westhydric_depths_outliers_1_7 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "7") 


westhydric_depths_outliers_1_8 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "8") 
 
 
westhydric_1_1_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_1)   
 
westhydric_1_2_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_2)   

westhydric_1_3_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_3)   

westhydric_1_4_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_4)   

westhydric_1_5_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_5)   

westhydric_1_6_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_6)  

westhydric_1_7_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_7)   

westhydric_1_8_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_8) 

westhydric_probe1_cleaned =
  westhydric_1_1_forcombine %>% 
  vctrs::vec_c(westhydric_1_2_forcombine, westhydric_1_3_forcombine,
             westhydric_1_4_forcombine, westhydric_1_5_forcombine,
             westhydric_1_6_forcombine, westhydric_1_7_forcombine,
             westhydric_1_8_forcombine)
  
#test for Beth, delete later

westhydric_probe1_notcleaned =
  westhydric_depths %>% 
  filter(probe == "1")

temporary_fig =
  westhydric_probe1_cleaned %>% 
  #filter(type == "temp_avg" & depth > 20) %>% 
  ggplot(aes(x = Betterdate, y = avg_values_fixed))+
  geom_point(aes(color = depth_cm), alpha = 0.5)+
  geom_line(aes(group = depth_cm, color = depth_cm, orientation = "x"))+
  scale_color_gradientn(colors = natparks.pals(name = "KingsCanyon", 8))+
  #scale_x_date(date_breaks = "1 day" , date_labels = "%Y-%m-%d")+
  #scale_x_discrete(breaks = seq(-1,31,2))+
  labs(y = "redox potential, mV")+
  #facet_wrap(month_name~.)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))

ggsave("output/temporary_fig_cleaned.png", plot = temporary_fig, width = 20, height = 8)

temporary_fig_notcleaned =
  westhydric_probe1_notcleaned %>% 
  #filter(type == "temp_avg" & depth > 20) %>% 
  ggplot(aes(x = Betterdate, y = avg_values_fixed))+
  geom_point(aes(color = depth_cm), alpha = 0.5)+
  geom_line(aes(group = depth_cm, color = depth_cm, orientation = "x"))+
  scale_color_gradientn(colors = natparks.pals(name = "KingsCanyon", 8))+
  #scale_x_date(date_breaks = "1 day" , date_labels = "%Y-%m-%d")+
  #scale_x_discrete(breaks = seq(-1,31,2))+
  labs(y = "redox potential, mV")+
  #facet_wrap(month_name~.)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))

ggsave("output/temporary_fig_notcleaned.png", plot = temporary_fig_notcleaned, width = 20, height = 8)

#I need to find a way to group_by so that this cleaning is happening within plots/sensors/depths/dataloggers

  
#  eastdry_depths_probe1 =
#   eastdry_depths %>%
#   mutate(sensor = recode(sensor, "1" = "S1", "2" = "S2", "3" = "S3", "4" = "S4")) %>% 
#   filter(probe == 1) %>%
#   dplyr::select(-c("depth_cm", 'RECORD', 'avg_values')) %>% 
#   pivot_wider(names_from = 'sensor', values_from = 'avg_values_fixed') %>%
#   mutate(lagS1 = lag(S1)-S1,
#           leadS1 = lead(S1)-S1,
#          lagS2 = lag(S2)-S2,
#          leadS2 = lead(S2)-S2,
#          lagS3 = lag(S3)-S3,
#          leadS3 = lead(S3)-S3,
#          lagS4 = lag(S4)-S4,
#          leadS4 = lead(S4)-S4,
#          ) %>% 
#    mutate(laglead = case_when(lagS1 > 200 & leadS1 < -200 ~ 'artifact',
#                               lagS1 < -200 & leadS1 > 200 ~ 'artifact',
#                               lagS2 > 200 & leadS2 < -200 ~ 'artifact',
#                               lagS2 < -200 & leadS2 > 200 ~ 'artifact',
#                               lagS3 > 200 & leadS3 < -200 ~ 'artifact',
#                               lagS3 < -200 & leadS3 > 200 ~ 'artifact',
#                               lagS4 > 200 & leadS4 < -200 ~ 'artifact',
#                               lagS4 < -200 & leadS4 > 200 ~ 'artifact'))
#  
#   mutate(checknum = row_number())
# 
# eastdry_depths_probe1_dataonly =
#   eastdry_depths_probe1 %>% 
#   dplyr::select(-c("lagS1", 'leadS1', 'lagS2', "leadS2",
#                    "lagS3", "leadS3", 'lagS4', "leadS4")) %>% 
#   pivot_longer(-c("TIMESTAMP", "site", "position", "Betterdate",
#                   "datalogger", "probe", "Plot"),
#                names_to = "sensor", values_to = "avg_values_fixed") %>% 
#   mutate(sensor = recode(sensor, "S1" = 1, "S2" = 2, "S3" = 3, "S4" = 4)) %>% 
#   na.omit() %>% 
#   left_join(eastdry_depths)
  

###currently lag/lead are being impossible. 

eastdry_depths_probe1_artifacts =
  eastdry_depths_probe1 %>% 
  dplyr::select(-c("S1", 'S2', 'S3', "S4")) %>% 
  # pivot_longer(-c("TIMESTAMP", "site", "position", "Betterdate",
  #                 "datalogger", "probe", "Plot"),
  #              names_to = "Lags", values_to = "") %>% 
     mutate(laglead = case_when(lagS1 > 200 & leadS1 < -200 ~ 'artifact',
                                lagS1 < -200 & leadS1 > 200 ~ 'artifact',
                                lagS2 > 200 & leadS2 < -200 ~ 'artifact',
                                lagS2 < -200 & leadS2 > 200 ~ 'artifact',
                                lagS3 > 200 & leadS3 < -200 ~ 'artifact',
                                lagS3 < -200 & leadS3 > 200 ~ 'artifact',
                                lagS4 > 200 & leadS4 < -200 ~ 'artifact',
                                lagS4 < -200 & leadS4 > 200 ~ 'artifact'))
   

 
 
 
 
 
 #summarise so that all plots are combined
 group_by(site, position, depth_cm, Betterdate) %>%
   dplyr::summarize(avg_values_summarised = mean(avg_values_fixed),
                    std_values_summarised = sd(avg_values_fixed)/sqrt(n())) %>%
   mutate(depth_cm = as.numeric(depth_cm)) %>% 
   
   
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
    #add 197 to all redox potentials to report data relative to the standard hydrogen electrode
    dplyr::mutate(avg_values_fixed = avg_values + 197) %>% 
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

#change names since I copy and pasted these to go above for the data cleaning.

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

