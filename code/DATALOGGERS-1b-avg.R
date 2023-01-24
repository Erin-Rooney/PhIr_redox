#ECROONEY
#July 12 2022

#load all packages

source("code/0-packages.R")


#####
#load metadata and data from each site. 
#eventually we will combine everything, but we're keeping it separate for now.

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

#####
#Now we isolate so that only the "avg" measurements are selected 
#since I am only taking the data (not the metadata) I need a "label" column
#all the data I want is the "avg" data, so I am creating a label column with avg at the end
#X is being changed to an ID number that can used to join the pivoted data with the metadata later on
#named with "Avg" or "Std" at the end so it will be included in the next lines of code

avg_separate <- function(dat){
  dat %>% 
    #create the "label" column. It needs to start with redox and end with avg in order to be
    #selected in the dplyr::select arguments below. If your data has a different labeling pattern
    #adjust the starts_with and ends_with arguments and the name of the X column
    mutate(redox_NUM_Avg = X) %>%
    #we made a new column, so X is still present. The below is a little unnecessary since 
    #X will be deselected after dplyr::select, but I don't want it floating around
    dplyr::select(-X) %>% 
    dplyr::select(starts_with('redox')) %>% 
    dplyr::select(ends_with('Avg')) %>%
    #if needed, get rid of NA rows then columns. Not needed.
    
    force()
}

# run the function on all 6 dataframes (for each site)
# after running the function, we will pivot_longer so that the data is in two columns
# data label including depth and probe
# then we will "fix" the redox values by adding 197 
# then we will filter the data so that only fixed average values between -600 and 1000 are kept

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

#####

#next we need to make the data labels from the depth metadata csv 
#meaningful and separate them into specific columns
#the below can be modified to separate labels for other datasets. In this situation
#we are separating into datalogger and probe_sensor by "_" and then 
#separating into "probe" and "sensor" by "-"
#after we fix the metadata from the datalogger_depths.csv sheet, we will create a new function
#that allows us to left_join that data to the actual site data frames

#load metadata
sensor_depths_prename = read.csv("raw/datalogger_depths.csv") 

#when we left_join, we want columns with the same information to combine into one column
#this menas we needd to fix some stuff for sensor_depth
#rename Site as position
#rename Area as site
#recode so that there are no capital words in site and position


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
#unfortunately "." are really problematic in R, so we are going to replace "." with "_"
#unfortunately "." are so problematic that I can't even do a stringi replace, so I have to 
#individually recode each label. Please don't include "." whenever possible

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

 
##now run the depths function on all six dataframes (now site_avg)
 
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
 #data cleaning bonanza
 
 datacleaning_function <- function(dat){
   dat %>% 
     group_by(probe, sensor) %>% 
     mutate(lag = lag(avg_values_fixed)-avg_values_fixed,
            lead = lead(avg_values_fixed)-avg_values_fixed) %>% 
     mutate(laglead = case_when(lag > 25 & lead < -25 ~ 'artifact',
                                lag < -25 & lead > 25 ~ 'artifact',
                                lag > 25 & lead > 25 ~ 'artifact',
                                lag < -25 & lead < -25 ~ 'artifact'),
            laglead = if_else(is.na(laglead), "keep", laglead)) %>% 
     filter(laglead == "keep") %>% 
     filter(avg_values_fixed < 800) %>% 
     filter(avg_values_fixed > -250) %>% 
     
     force()
   
 }

#################
 
#WEST HYDRIC  
 
westhydric_depths_outliers = 
   datacleaning_function(westhydric_depths)   
 
easthydric_depths_outliers = 
   datacleaning_function(easthydric_depths)   

westmesic_depths_outliers = 
  datacleaning_function(westmesic_depths)   

eastmesic_depths_outliers = 
  datacleaning_function(eastmesic_depths)  

westdry_depths_outliers = 
  datacleaning_function(westdry_depths)   

eastdry_depths_outliers = 
  datacleaning_function(eastdry_depths) 

#####

#final data cleaning combine

alldata_cleaned =
  westhydric_depths_outliers %>% 
  vctrs::vec_c(easthydric_depths_outliers, westmesic_depths_outliers,
               eastmesic_depths_outliers, westdry_depths_outliers,
               eastdry_depths_outliers)

alldata_notcleaned =
  westhydric_depths %>% 
  vctrs::vec_c(easthydric_depths, westmesic_depths,
               eastmesic_depths, westdry_depths,
               eastdry_depths)




 
#####

###go through and make sure artifacts parameters are okay.


temporary_fig_eastmesic =
  alldata_cleaned %>%
  filter(position == "mesic" & site == "east") %>%
  ggplot(aes(x = Betterdate, y = avg_values_fixed))+
  geom_point(aes(color = depth_cm), alpha = 0.5)+
  geom_line(aes(group = depth_cm, color = depth_cm, orientation = "x"))+
  scale_color_gradientn(colors = natparks.pals(name = "KingsCanyon", 8))+
  #scale_x_date(date_breaks = "1 day" , date_labels = "%Y-%m-%d")+
  #scale_x_discrete(breaks = seq(-1,31,2))+
  labs(y = "redox potential, mV")+
  facet_wrap(site~position)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))

ggsave("output/temporary_fig_cleaned_eastmesic.png", plot = temporary_fig_eastmesic, width = 20, height = 10)

alldata_cleaned %>%
  filter(position == "mesic" & site == "west") %>%
  ggplot(aes(x = Betterdate, y = avg_values_fixed))+
  geom_point(aes(color = depth_cm), alpha = 0.5)+
  geom_line(aes(group = depth_cm, color = depth_cm, orientation = "x"))+
  scale_color_gradientn(colors = natparks.pals(name = "KingsCanyon", 8))+
  #scale_x_date(date_breaks = "1 day" , date_labels = "%Y-%m-%d")+
  #scale_x_discrete(breaks = seq(-1,31,2))+
  labs(y = "redox potential, mV")+
  facet_wrap(site~position)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))


artifact_fig_2021 =
  alldata_cleaned %>%
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(probe == 3) %>%
  ggplot(aes(x = as_datetime(Betterdate), y = avg_values_fixed))+
  geom_point(aes(color = depth_cm), alpha = 0.5)+
  #geom_line(aes(group = depth_cm, color = depth_cm, orientation = "x"))+
  scale_color_gradientn(colors = natparks.pals(name = "KingsCanyon", 8))+
  scale_x_datetime(date_breaks = "2 weeks" , date_labels = "%Y-%m-%d")+
  #scale_x_discrete(breaks = seq(-1,31,2))+
  labs(y = "redox potential, mV",
       x = "Post-cleaning",
       color = "depth, cm")+
  facet_grid(position~site)+
  theme_er1()+
  theme(legend.position = "top", axis.text.x = element_text (size = 10, vjust = 0.5, angle = 45))

ggsave("output/temporary_fig_cleaned_2021_25.png", plot = artifact_fig_2021, width = 6, height = 7.5)

artifact_fig_2021_uncleaned =
  alldata_notcleaned %>%
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(probe == 3) %>%
  ggplot(aes(x = as_datetime(Betterdate), y = avg_values_fixed))+
  geom_point(aes(color = depth_cm), alpha = 0.5)+
  #geom_line(aes(group = depth_cm, color = depth_cm, orientation = "x"))+
  scale_color_gradientn(colors = natparks.pals(name = "KingsCanyon", 8))+
  scale_x_datetime(date_breaks = "2 weeks" , date_labels = "%Y-%m-%d")+
  #scale_x_discrete(breaks = seq(-1,31,2))+
  labs(y = "redox potential, mV",
       x = "Pre-cleaning",
       color = "depth, cm")+
  facet_grid(position~site)+
  theme_er1()+
  theme(legend.position = "top", axis.text.x = element_text (size = 10 , vjust = 0.5, angle = 45))

ggsave("output/temporary_fig_uncleaned_2021.png", plot = artifact_fig_2021_uncleaned, width = 6, height = 7.5)


  
#write csv

#comment in if running for the first time
#write.csv(alldata_cleaned, "processed/all_combine.csv")

