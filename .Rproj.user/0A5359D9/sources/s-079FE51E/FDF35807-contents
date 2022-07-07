#ECROONEY
#June 3 2022

#combining redox data with sensor depths

#load all packages

source("code/0-packages.R")

#load data

hydric_redox_avgstd = read.csv("processed/hydric_redox_avgstd.csv")

sensor_depths = read.csv("raw/datalogger_depths.csv")

hydric_redox_betterdates =
  hydric_redox_avgstd %>%  
  dplyr::select(X, site, Betterdate, sensor)

sensor_depths_processed = 
  sensor_depths %>% 
  mutate(Sensor = str_remove(Sensor, "Redox_")) %>% 
  mutate(Sensor = str_remove(Sensor, "redox_")) %>% 
  mutate(Sensor = str_remove(Sensor, "_fromtip_Avg")) %>% 
  separate(Sensor, sep = "_", into = c("datalogger", "probe_sensor")) %>% 
  separate(probe_sensor, sep = "-", into = c("probe", "sensor")) %>% 
  filter(Site == "Hydric") %>% 
  mutate(sensor = as.numeric(sensor))


hydric_redox_depths_summarised = 
  hydric_redox_avgstd %>% 
  mutate(sensor = as.numeric(sensor)) %>% 
  left_join(sensor_depths_processed) %>% 
  group_by(X, site, sensor, depth_cm) %>%
  dplyr::summarize(avg_values_summarised = mean(avg_values)) %>% 
  left_join(hydric_redox_betterdates) %>% 
  mutate(Betterdate = as.Date(Betterdate, format = ("%Y-%m-%d %H:%M:%S"))) 

hydric_redox_forggplot =
  hydric_redox_depths_summarised %>% 
  mutate(depth_cm = as.numeric(depth_cm)) 
  

library(scales)

hydric_redox_forggplot %>% 
  na.omit() %>% 
  ggplot(aes(x = Betterdate, y = depth_cm, fill = avg_values_summarised))+
  geom_bar(position = "stack", stat= "identity")+
    scale_x_date(date_breaks = "1 week" , date_labels = "%m-%d-%Y")+
  scale_y_reverse()+
  scale_fill_gradientn(colors = (PNWColors::pnw_palette("Bay")))+  
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(site~.)
  
