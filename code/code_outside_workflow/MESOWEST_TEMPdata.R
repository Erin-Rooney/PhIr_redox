#MesoWest temperature data for toolik


#load all packages

source("code/0-packages.R")

#load data

August_temp = read.csv("raw/TUAA2_August_temp_2022.csv")
July_temp = read.csv("raw/TUAA2_July_temp_2022.csv")
June_temp = read.csv("raw/TUAA2_June_temp_2022.csv")



all_temps = 
  June_temp %>% 
  vctrs::vec_c(July_temp, August_temp) %>% 
  mutate(airtemp_C = round((air_temp_set_1_F - 32) *(5/9),2)) %>% 
  separate(Date_Time, sep = " ", into = c("date2", "time", "UTC")) %>% 
  separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(datetime = ymd_hm(paste(date, time)),
         date = ymd(date))

airtemp_2022 =
  all_temps %>% 
  ggplot(aes(y = airtemp_C, x = datetime))+
  geom_point(size = 1, alpha = 0.4)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  #geom_line(orientation = "x", show.legend = FALSE)+
  labs(y = "Air temperature, celsius", x = "")+
  #scale_x_continuous(position="top")+
  #facet_grid(.~site, switch = "x")  +
  theme_er1()

ggsave("output/airtemp_2022.png", plot = airtemp_2022, height = 4, width = 11)
