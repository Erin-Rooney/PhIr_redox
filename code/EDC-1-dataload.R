#ECROONEY 
#September 30 2022

#load all packages

source("code/0-packages.R")

#load data

climate_1hr_data = read.csv("raw/1-hour_data.csv")
climate_1hr_data_airtemp = read.csv("raw/1-hour_data_airtemp.csv")
climate_3hr_data = read.csv("raw/3-hour_data.csv")
climate_24hr_data = read.csv("raw/24-hour_data.csv")


#processing data

climate_1hr_data_cleaned =
  climate_1hr_data %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>%
  mutate(date = as.Date(paste(year,month,day, sep = "-"))) %>%
  group_by(date) %>% 
  dplyr::summarise(air_temp_1m_avg = mean(air_temp_1m),
                   snow_depth_1hr_avg = mean(snow_depth_1hr),
                   rain_avg = mean(rain)) 

climate_1hr_data_airtemp_cleaned =
  climate_1hr_data_airtemp %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>%
  mutate(date = as.Date(paste(year,month,day, sep = "-"))) %>%
  group_by(date) %>% 
  dplyr::summarise(air_temp_1m_avg = mean(air_temp_1m),
                   air_temp_3m_avg = mean(air_temp_3m),
                   air_temp_5m_avg = mean(air_temp_5m)) 

#ggplots

airtemp_EDC =
  climate_1hr_data_airtemp_cleaned %>% 
  dplyr::select(date, air_temp_3m_avg) %>% 
  filter(date > "2020-03-01") %>% 
  na.omit() %>% 
  ggplot() + 
  geom_point(aes(y = air_temp_3m_avg, x = date))+
  scale_x_date(date_breaks = "2 weeks")+
  labs(y = 'EDC daily air temperature averages, C')+
  theme_er1()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

snow_EDC =
  climate_1hr_data_cleaned %>% 
  dplyr::select(date, snow_depth_1hr_avg) %>% 
  filter(date > "2020-03-01") %>% 
  na.omit() %>% 
  ggplot() + 
  geom_point(aes(y = snow_depth_1hr_avg, x = date))+
  scale_x_date(date_breaks = "2 weeks")+
  labs(y = 'EDC daily snow depth average, cm')+
  theme_er1()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

rain_EDC =
  climate_1hr_data_cleaned %>% 
  dplyr::select(date, rain_avg) %>% 
  filter(date > "2020-03-01") %>% 
  na.omit() %>% 
  ggplot() + 
  labs(y = 'EDC daily rain average, mm')+
  geom_point(aes(y = rain_avg, x = date))+
  scale_x_date(date_breaks = "2 weeks")+
  theme_er1()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  ggsave("output/rain_EDC.png", plot = rain_EDC, height = 4.5, width = 10)
  ggsave("output/snow_EDC.png", plot = snow_EDC, height = 4.5, width = 10)
  ggsave("output/air_EDC.png", plot = airtemp_EDC, height = 4.5, width = 10)

  
  
  #ggplots
  
  airtemp_EDC_subset =
    climate_1hr_data_airtemp_cleaned %>% 
    dplyr::select(date, air_temp_3m_avg) %>% 
    filter(date > "2021-05-01" & date < "2021-10-01") %>% 
    na.omit() %>% 
    ggplot() + 
    geom_point(aes(y = air_temp_3m_avg, x = date))+
    scale_x_date(date_breaks = "7 days")+
    labs(y = 'EDC daily air temperature averages, C')+
    theme_er1()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  snow_EDC_subset =
    climate_1hr_data_cleaned %>% 
    dplyr::select(date, snow_depth_1hr_avg) %>% 
    filter(date > "2021-05-01" & date < "2021-10-01") %>% 
    na.omit() %>% 
    ggplot() + 
    geom_point(aes(y = snow_depth_1hr_avg, x = date))+
    scale_x_date(date_breaks = "1 week")+
    labs(y = 'EDC daily snow depth average, cm')+
    theme_er1()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  rain_EDC_subset =
    climate_1hr_data_cleaned %>% 
    dplyr::select(date, rain_avg) %>% 
    filter(date > "2021-05-01" & date < "2021-10-01") %>% 
    na.omit() %>% 
    ggplot() + 
    labs(y = 'EDC daily rain average, mm')+
    geom_point(aes(y = rain_avg, x = date))+
    scale_x_date(date_breaks = "1 week")+
    theme_er1()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ggsave("output/rain_EDC_subset.png", plot = rain_EDC_subset, height = 4.5, width = 10)
  ggsave("output/snow_EDC_subset.png", plot = snow_EDC_subset, height = 4.5, width = 10)
  ggsave("output/air_EDC_subset.png", plot = airtemp_EDC_subset, height = 4.5, width = 10)
  
  
  # library(cowplot)  
  # library(patchwork)
  #   
  #   airtemp_EDC/snow_EDC/rain_EDC+ #combines the two plots
  #     plot_layout(guides = "collect") # sets a common legend  
  