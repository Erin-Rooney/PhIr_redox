#ECROONEY 
#September 30 2022

#load all packages

#Datasets were provided by the Toolik Field Station Environmental Data Center. 
#This material is based upon work supported by the National Science Foundation under grant #1623461.



#dual plots
#EDC data
#PhIr data
#redox contour plots
#redox line/dot plots

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
  filter(date > "2021-05-01") %>% 
  na.omit() %>% 
  ggplot() + 
  geom_col(aes(y = snow_depth_1hr_avg, x = date))+
  scale_x_date(date_breaks = "2 weeks")+
  labs(y = 'EDC daily snow depth average, cm')+
  theme_er1()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

accumulation_EDC =
  climate_1hr_data_cleaned %>% 
  dplyr::select(date, rain_avg, snow_depth_1hr_avg) %>% 
  mutate(rain_avg_cm = (rain_avg/10)) %>% 
  dplyr::select(-rain_avg) %>% 
  pivot_longer(-c(date), names_to = "precip_type", values_to = "accumulation") %>% 
  mutate(precip_type = recode(precip_type, "rain_avg_cm" = "rain",
                              "snow_depth_1hr_avg" = "snow"))

accumulation_fig =
  accumulation_EDC %>% 
  #dplyr::select(date, rain_avg) %>% 
  filter(date > "2021-06-21" & date < "2021-09-20") %>% 
  na.omit() %>% 
  ggplot() + 
  labs(y = 'EDC accumulation average, cm', x = "",
       fill = "")+
  geom_col(aes(y = accumulation, x = date, fill = precip_type))+
  scale_y_reverse()+
  scale_x_date(date_breaks = "1 weeks", date_labels = "%b-%d", position = "top")+
  scale_fill_manual(values = c("#64a8a8", "#cde5f9"))+
  #scale_color_manual(values = c("#64a8a8", "#cde5f9"))+
  theme_er1()+
  theme(axis.text.x = element_text(size = 9), legend.position = "NONE")+
  facet_grid(precip_type~., scales = "free_y")+
NULL

accumulationrain_fig =
  accumulation_EDC %>% 
  #dplyr::select(date, rain_avg) %>% 
  filter(date > "2021-06-14" & date < "2021-09-20" & precip_type == "rain") %>% 
  na.omit() %>% 
  ggplot() + 
  labs(y = 'EDC rain, cm', x = "",
       fill = "")+
  geom_col(aes(y = accumulation, x = date, fill = precip_type))+
  scale_y_reverse()+
  scale_x_date(date_breaks = "1 weeks", date_labels = "%b-%d", position = "top")+
  scale_fill_manual(values = c("#64a8a8", "#cde5f9"))+
  #scale_color_manual(values = c("#64a8a8", "#cde5f9"))+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 9), legend.position = "NONE")+
  #facet_grid(precip_type~., scales = "free_y")+
  NULL
 

  ggsave("output/rain_EDC.png", plot = rain_EDC, height = 4.5, width = 10)
  ggsave("output/snow_EDC.png", plot = snow_EDC, height = 4.5, width = 10)
  ggsave("output/air_EDC.png", plot = airtemp_EDC, height = 4.5, width = 10)
  ggsave("output/accumulation_EDC.png", plot = accumulation_fig, height = 4.5, width = 7.5)
  ggsave("formanuscript/accumulationrain_EDC.png", plot = accumulationrain_fig, height = 2, width = 7.5)
  

  
  
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
  
###add in the redox data------------
  
dailyredoxunbinned = read.csv('processed/dailyredox_unbinned.csv')
combo_redox_withdepths = read.csv("processed/all_combine.csv")
  

  
grouped_redox_forfigs = 
  combo_redox_withdepths %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  group_by(site, position, depth_cm) %>% 
  dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
                   redox_sd = round(sd(avg_values_fixed),2),
                   redox_se = round(sd(avg_values_fixed)/sqrt(n()),2),
                   redox_sd2 = (redox_sd)/2)


ungrouped_redox_forfigs_nonacidichydric = 
  combo_redox_withdepths %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  filter(position == "hydric") %>% 
  separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
  separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(datetime = ymd_hm(paste(date, time)),
         date = ymd(date)) %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "7", "9", "11", "13", "23", "33", "53")))   %>% 
  #filter(probe == 1) %>%
  dplyr::rename(redox_avg_mV = avg_values_fixed) 
  # group_by(site, position, depth_2, datetime) %>%
  # dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
  #                  redox_sd = round(sd(avg_values_fixed),2))

ungrouped_redox_forfigs_acidichydric = 
  combo_redox_withdepths %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  filter(position == "hydric") %>% 
  separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
  separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(datetime = ymd_hm(paste(date, time)),
         date = ymd(date)) %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("1", "3", "5", "7", "9", "11", "13", "15", "19", "23", "25", "29", "30", "33", "35", "43", "49", "53", "55")))   %>% 
  #filter(probe == 1) %>%
  dplyr::rename(redox_avg_mV = avg_values_fixed) 
# group_by(site, position, depth_2, datetime) %>%
  # dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
  #                  redox_sd = round(sd(avg_values_fixed),2))

ungrouped_redox_forfigs_nonhydric = 
  combo_redox_withdepths %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  filter(position != "hydric") %>% 
  separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
  separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(datetime = ymd_hm(paste(date, time)),
         date = ymd(date)) %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("1", "5", "6", "10", "15", "16", "20", "25", "26", "30"))) %>%
  #filter(probe == 1) %>%
  dplyr::rename(redox_avg_mV = avg_values_fixed) 
  # group_by(site, position, depth_2, datetime) %>%
  # dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
  #                  redox_sd = round(sd(avg_values_fixed),2))


redoxfig_depth_sd =
  grouped_redox_forfigs %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = position, fill = position), group = 'position')+
  geom_point(size = 3, alpha = 0.4, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE)+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  scale_color_manual(values = c("#9a031e", "#40916c", "#118ab2"))+
  scale_fill_manual(values = c("#9a031e", "#40916c", "#118ab2"))+
  ylim(60, 0)+
    labs(x = 'redox potential (mV)',
         y = "depth (cm)",
         color = "", fill = "")+
  scale_x_continuous(position="top")+
  facet_grid(.~site, switch = "x")+
  theme_er1()


nonacidic_hydric_redox =
  ungrouped_redox_forfigs_nonacidichydric %>% 
  filter(site == "non-acidic tundra" & probe == 1) %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 2.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(natparks.pals(name = "Olympic")))+
  scale_fill_manual(values = rev(natparks.pals(name = "Olympic")))+
  ylim(-250, 700)+
  labs(x = "", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(.~position)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 9),
        legend.position = "bottom")

nonacidic_mesic_redox =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "mesic" & site == "non-acidic tundra" & probe == "3") %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 2.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(natparks.pals(name = "Olympic")))+
  scale_fill_manual(values = rev(natparks.pals(name = "Olympic")))+
  ylim(-250, 700)+
  labs(x = "", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(.~position)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 9),
        legend.position = "bottom")

nonacidic_dry_redox =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "dry" & site == "non-acidic tundra" & probe == 1) %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  geom_rect(aes(xmin=as_datetime('2021-06-21 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 2.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(natparks.pals(name = "Olympic")))+
  scale_fill_manual(values = rev(natparks.pals(name = "Olympic")))+
  ylim(-250, 700)+
  labs(x = "", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(.~position)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 9),
        legend.position = "bottom")

acidichydric =
combo_redox_withdepths %>% 
  filter(site == "east" & position == "hydric") %>% 
  group_by(depth_cm) %>% 
  mutate(depth_cm = factor(depth_cm))

print(acidichydric$depth_cm)

acidicmesic =
  combo_redox_withdepths %>% 
  filter(site == "east" & position == "mesic") %>% 
  group_by(depth_cm) %>% 
  mutate(depth_cm = factor(depth_cm))

print(acidicmesic$depth_cm)


acidicdry =
  combo_redox_withdepths %>% 
  filter(site == "east" & position == "dry") %>% 
  group_by(depth_cm) %>% 
  mutate(depth_cm = factor(depth_cm))

print(acidicdry$depth_cm)


acidic_hydric_redox =
  ungrouped_redox_forfigs_acidichydric %>% 
  filter(site == "acidic tundra" & position == "hydric" & probe == 3) %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 2.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(natparks.pals(name = "Olympic")))+
  scale_fill_manual(values = rev(natparks.pals(name = "Olympic")))+
  ylim(-250, 700)+
  labs(x = "", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(.~position)+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "bottom")

acidic_mesic_redox =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "mesic" & site == "acidic tundra" & probe == 1) %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 2.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(natparks.pals(name = "Olympic")))+
  scale_fill_manual(values = rev(natparks.pals(name = "Olympic")))+
  ylim(-250, 700)+
  labs(x = "", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)",
       subtitle = "acidic mesic")+
  #facet_grid(position~site)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90),
        legend.position = "bottom")

acidic_dry_redox =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "dry" & site == "acidic tundra" & probe == 1) %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 2.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(natparks.pals(name = "Olympic")))+
  scale_fill_manual(values = rev(natparks.pals(name = "Olympic")))+
  ylim(-250, 700)+
  labs(x = "", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90),
        legend.position = "bottom")

ggsave("figures_finalized/redox_groupdepth.tiff", plot = redoxfig_depth_sd, height = 7, width = 5)
ggsave("figures_finalized/redox_groupdepth.png", plot = redoxfig_depth_sd, height = 7, width = 5)
ggsave("figures_finalized/nonacidic_hydric_redox.tiff", plot = nonacidic_hydric_redox, height = 5.75, width = 9.5)
ggsave("figures_finalized/nonacidic_hydric_redox.png", plot = nonacidic_hydric_redox, height = 6.5, width = 4.5)
ggsave("figures_finalized/nonacidic_mesic_redox.png", plot = nonacidic_mesic_redox, height = 6, width = 4.5)
ggsave("figures_finalized/nonacidic_dry_redox.png", plot = nonacidic_dry_redox, height = 6, width = 4.5)
ggsave("figures_finalized/acidic_hydric_redox.png", plot = acidic_hydric_redox, height = 6.5, width = 4.5)
ggsave("figures_finalized/acidic_mesic_redox.png", plot = acidic_mesic_redox, height = 5.75, width = 9.5)
ggsave("figures_finalized/acidic_dry_redox.png", plot = acidic_dry_redox, height = 6, width = 4.5)

ggsave("figures_finalized/redox_groupdepth.tiff", plot = redoxfig_depth_sd, height = 7, width = 5)
ggsave("formanuscript/nonacidic_hydric_redox.tiff", plot = nonacidic_hydric_redox, height = 3, width = 7.5)
ggsave("formanuscript/nonacidic_mesic_redox.tiff", plot = nonacidic_mesic_redox, height = 3, width = 7.5)
ggsave("formanuscript/nonacidic_dry_redox.tiff", plot = nonacidic_dry_redox, height = 3, width = 7.5)
ggsave("formanuscript/acidic_hydric_redox.tiff", plot = acidic_hydric_redox, height = 3, width = 7.5)
ggsave("formanuscript/acidic_mesic_redox.tiff", plot = acidic_mesic_redox, height = 3, width = 7.5)
ggsave("formanuscript/acidic_dry_redox.tiff", plot = acidic_dry_redox, height = 3, width = 7.5)




####contour plots----



contour_nonacidic_hydric =
  ungrouped_redox_forfigs_nonacidichydric %>%
  filter(site == "non-acidic tundra" & probe == 1) %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(55, 5)+
  labs(x = "", y = "depth, cm",
       fill = "redox potential, mV")+
  scale_fill_manual(values=pnw_palette("Bay", 11))+
  facet_grid(position~site)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90),
        legend.position = "bottom")

contour_acidic_hydric =
  ungrouped_redox_forfigs_acidichydric %>% 
  filter(site == "acidic tundra" & position == "hydric" & probe == 3) %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(55, 5)+
  labs(x = "", y = "depth, cm",
        fill = "redox potential, mV")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=pnw_palette("Bay", 11))+
  facet_grid(position~site)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90),
        legend.position = "bottom")

contour_nonacidic_dry =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "dry" & site == "acidic tundra" & probe == 1) %>% 
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(30, 5)+
  labs(x = "", y = "depth, cm",
       fill = "redox potential, mV")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=pnw_palette("Bay", 11))+
  facet_grid(position~site)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90),
        legend.position = "bottom")

contour_acidic_dry =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "dry" & site == "non-acidic tundra" & probe == 1) %>% 
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(30, 5)+
  labs(x = "", y = "depth, cm",
       fill = "redox potential, mV")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=pnw_palette("Bay", 11))+
  facet_grid(position~site)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90),
        legend.position = "bottom")

ggsave("figures_finalized/contour_acidic_dry.png", plot = contour_acidic_dry, height = 5.5, width = 8)
ggsave("figures_finalized/contour_nonacidic_dry.png", plot = contour_nonacidic_dry, height = 5.5, width = 8)
ggsave("figures_finalized/contour_acidic_hydric.png", plot = contour_acidic_hydric, height = 5.5, width = 8)
ggsave("figures_finalized/contour_nonacidic_hydric.png", plot = contour_nonacidic_hydric, height = 5.5, width = 8)

  
###temp----

final_temp_sal_moist = read.csv("processed/final_temp_salinity_avgs.csv")

library(lubridate)

final_temp_sal_moist_forfig =
  final_temp_sal_moist %>% 
  separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
  separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(datetime = ymd_hm(paste(date, time)),
         date = ymd(date)) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  dplyr::rename(depth_cm = depth) %>% 
  na.omit() 


  
daily_redox_wred =
daily_redox_forfigs %>% 
  filter(date < "2021-07-15" & date > "2021-06-01") %>% 
    ggplot(aes(x = date, y = depth_cm))+
    geom_point(aes(color = depth_avg))+
    geom_point(data = final_temp_sal_moist_forfig %>% filter(temp < 1 & date < "2021-07-05" & date > "2021-06-01"),
             size = 2, color = "Red"
             )+
    labs(y = "depth, cm", colors = "redox potential, mV")+
    scale_x_date(date_breaks = "1 day" , date_labels = "%Y-%m-%d")+
  scale_y_reverse()+
    scale_color_gradientn(colors = (PNWColors::pnw_palette("Anemone")))+
    theme_er1()+
    theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
    facet_grid(position~site) 

ggsave("output/daily_redox_wred.png", plot = daily_redox_wred, width = 8, height = 5.5)
  
coeff <- 7.5

allsoilmoisture_fig =
final_temp_sal_moist_forfig %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  #filter(position == "hydric" & site == "non-acidic tundra") %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = moisture, x = datetime, group = 'depth_cm'))+
  #geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 0.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  scale_fill_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  ylim(0, 60)+
  labs(x = "", y = "soil moisture (%)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "bottom", axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9))

acidic_moisture_contour=
  final_temp_sal_moist_forfig %>% 
  filter(site == "acidic tundra") %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = moisture))+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(25, 5)+
  labs(x = "", y = "depth, cm",
       fill = "redox potential, mV")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=rev(pnw_palette("Bay", 11)))+
  facet_grid(position~.)+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "bottom")

nonacidic_moisture_contour=
  final_temp_sal_moist_forfig %>% 
  filter(site == "non-acidic tundra") %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = moisture))+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(25, 5)+
  labs(x = "", y = "depth, cm",
       fill = "redox potential, mV")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=rev(pnw_palette("Bay", 11)))+
  facet_grid(position~.)+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "bottom")


ggsave("formanuscript/acidic_moisture_contour.tiff", plot = acidic_moisture_contour, width = 7.5, height = 5.5)
ggsave("formanuscript/nonacidic_moisture_contour.tiff", plot = nonacidic_moisture_contour, width = 7.5, height = 5.5)



allsoiltemperature_fig =
  final_temp_sal_moist_forfig %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  #filter(position == "hydric" & site == "non-acidic tundra") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = temp, x = datetime, group = 'depth_cm'))+
  #geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 0.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  scale_fill_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  ylim(0, 25)+
  labs(x = "", y = "soil temperature (C)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "bottom", axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9))

ggsave("figures_finalized/allsoiltemp_fig.tiff", plot = allsoiltemperature_fig, width = 6, height = 5.5)

nonacidic_hydric_soiltemperature_fig =
  final_temp_sal_moist_forfig %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  filter(position == "hydric" & site == "non-acidic tundra") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = temp, x = datetime, group = 'depth_cm'))+
  #geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  scale_fill_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  ylim(0, 25)+
  labs(x = "", y = "soil temperature (C)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "bottom", axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9))

ggsave("figures_finalized/nonacidic_hydric_soiltemperature_fig.png", plot = nonacidic_hydric_soiltemperature_fig, width = 6, height = 4)

nonacidic_dry_soiltemperature_fig =
  final_temp_sal_moist_forfig %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  filter(position == "dry" & site == "non-acidic tundra") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = temp, x = datetime, group = 'depth_cm'))+
  #geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  scale_fill_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  ylim(0, 25)+
  labs(x = "", y = "soil temperature (C)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "bottom", axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9))

ggsave("figures_finalized/nonacidic_dry_soiltemperature_fig.png", plot = nonacidic_dry_soiltemperature_fig, width = 6, height = 4)



moisture_depth_lineplot_hydric =
  final_temp_sal_moist_forfig %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  filter(position == "hydric" & site == "non-acidic tundra") %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = moisture, x = datetime, group = 'depth_cm'))+
  #geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 1.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  scale_fill_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  ylim(0, 60)+
  labs(x = "", y = "soil moisture (%)",
       color = "depth (cm)", fill = "depth (cm)")+
  #facet_grid(position~site)+
  theme_er1()+
  theme(legend.position = "left", axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9))

moisture_depth_lineplot_dry =
  final_temp_sal_moist_forfig %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  filter(position == "dry" & site == "non-acidic tundra") %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = moisture, x = datetime), group = 'depth_cm')+
  #geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 1.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  scale_fill_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  ylim(0, 60)+
  labs(x = "", y = "soil moisture (%)",
       color = "depth (cm)", fill = "depth (cm)")+
  #facet_grid(position~site)+
  theme_er1()+
  theme(legend.position = "left", axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9))

ggsave("figures_finalized/moisture_depth_lineplothydric.png", plot = moisture_depth_lineplot_hydric, width = 6, height = 3)
ggsave("figures_finalized/moisture_depth_lineplotdry.png", plot = moisture_depth_lineplot_dry, width = 6, height = 3)


hydric_dual =
  final_temp_sal_moist_forfig %>% 
  filter(depth_cm == 25 & position == "hydric") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = moisture), color = c("#c8b6ff"), size = 1)+
  geom_line(aes(y = temp*10), size = 0.8)+
  labs(subtitle = "Hydric")+
  scale_x_datetime(date_breaks = "1 week" , date_labels = "%b-%d")+
  scale_y_continuous(name = "Soil Moisture, (purple line)",
                     sec.axis = sec_axis(~./10, name = "Soil Temperature"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(.~site, scales="free")  


mesic_dual =
  final_temp_sal_moist_forfig %>% 
  filter(depth_cm == 25 & position == "mesic") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = moisture), color = c("#c8b6ff"), size = 1)+
  geom_line(aes(y = temp*10), size = 0.8)+
  labs(subtitle = "Mesic")+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_y_continuous(name = "Soil Moisture (purple line)",
                     sec.axis = sec_axis(~./10, name = "Soil Temperature"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(.~site, scales="free")    

dry_dual =
  final_temp_sal_moist_forfig %>% 
  filter(depth_cm == 25 & position == "dry") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = moisture), color = c("#c8b6ff"), size = 1)+
  geom_line(aes(y = temp*10), size = 0.8)+
  labs(subtitle = "Dry")+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_y_continuous(name = "Soil Moisture (purple line)",
                     sec.axis = sec_axis(~./10, name = "Soil Temperature"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(.~site, scales="free") 

combo_dual = dry_dual + mesic_dual + hydric_dual


ggsave("output/combo_dual.png", plot = combo_dual, width = 15, height = 4)
ggsave("output/dry_dual.png", plot = dry_dual, width = 8, height = 5)
ggsave("output/mesic_dual.png", plot = mesic_dual, width = 8, height = 5)
ggsave("output/hydric_dual.png", plot = hydric_dual, width = 8, height = 5)
ggsave("figures_finalized/combo_dual.png", plot = combo_dual, width = 15, height = 4)
ggsave("figures_finalized/dry_dual.png", plot = dry_dual, width = 8, height = 5)
ggsave("figures_finalized/mesic_dual.png", plot = mesic_dual, width = 8, height = 5)
ggsave("figures_finalized/hydric_dual.png", plot = hydric_dual, width = 8, height = 5)



final_temp_sal_moist_forfig %>% 
  filter(depth_cm == 25) %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = moisture))+
  scale_x_datetime(date_breaks = "1 week")+
   theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(position~site)    


final_temp_sal_moist_forfig %>% 
  filter(depth_cm == 25 & position == "hydric") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = temp, color = site))+
  scale_x_datetime(date_breaks = "1 day")+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")
 

#############dual plots for EDC x dataloggers-----

###SOMETHING IS WRONG WHEN DATA IS JOINED

dualplot_temp_moist_airtemp =
  final_temp_sal_moist_forfig %>% 
  left_join(climate_1hr_data_airtemp_cleaned)

dualplot_temp_moist =
  final_temp_sal_moist_forfig %>% 
  left_join(climate_1hr_data_cleaned)

coeff <- 2

hydric_dual_temps =
  dualplot_temp_moist_airtemp %>% 
  filter(depth_cm == 25 & position == "hydric") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = air_temp_3m_avg), color = c("#4cc9f0"), size = 1)+
  geom_line(aes(y = temp*coeff), size = 0.8)+
  labs(subtitle = "Hydric", x = "Date",
       caption = "depth = 25 cm")+
  scale_x_datetime(date_breaks = "1 week")+
  scale_y_continuous(name = "Air Temperature (light blue line)",
                     sec.axis = sec_axis(~./coeff, name = "Soil Temperature"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(.~site, scales="free")   

dualplot_temp_moist_airtemp %>% 
  filter(depth_cm == 25 & position == "hydric") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = air_temp_3m_avg), color = c("#4cc9f0"), size = 1)+
  #geom_line(aes(y = temp*coeff), size = 0.8)+
  labs(subtitle = "Hydric", x = "Date",
       caption = "depth = 25 cm")+
  scale_x_datetime(date_breaks = "1 week")+
  # scale_y_continuous(name = "Air Temperature (light blue line)",
  #                    sec.axis = sec_axis(~./coeff, name = "Soil Temperature"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(.~site, scales="free")    


non_acidic_hydric_dual_temps =
  dualplot_temp_moist_airtemp %>% 
  filter(depth_cm == 25 & position == "hydric" & site == "non-acidic tundra") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = air_temp_3m_avg), color = c("#4cc9f0"), size = 1)+
  geom_line(aes(y = temp*coeff), size = 0.8)+
  labs(x = "Date",
       caption = "depth = 25 cm")+
  scale_x_datetime(date_breaks = "1 week")+
  scale_y_continuous(name = "Air Temperature (light blue line)",
                     sec.axis = sec_axis(~./coeff, name = "Soil Temperature (black line)"))+
  theme_er1()+
  #theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(position~site, scales="free") +
  theme(legend.position = "bottom", axis.text.x = element_text(vjust = 0.5, hjust=1, angle = 90, size = 9),
        strip.placement = "outside")

ggsave("output/non_acidic_hydric_dual_temps.png", plot = non_acidic_hydric_dual_temps, width = 8, height = 5)

non_acidic_dry_dual_temps =
  dualplot_temp_moist_airtemp %>% 
  filter(depth_cm == 25 & position == "dry" & site == "non-acidic tundra") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = air_temp_3m_avg), color = c("#4cc9f0"), size = 1)+
  geom_line(aes(y = temp*coeff), size = 0.8)+
  labs(x = "Date",
       caption = "depth = 25 cm")+
  scale_x_datetime(date_breaks = "1 week")+
  scale_y_continuous(name = "Air Temperature (light blue line)",
                     sec.axis = sec_axis(~./coeff, name = "Soil Temperature (black line)"))+
  theme_er1()+
  #theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(position~site, scales="free") +
  theme(legend.position = "bottom", axis.text.x = element_text(vjust = 0.5, hjust=1, angle = 90, size = 9),
        strip.placement = "outside")

ggsave("output/non_acidic_dry_dual_temps.png", plot = non_acidic_dry_dual_temps, width = 8, height = 5)


coeff1 <- 10


hydric_dual_temps_air =
  dualplot_temp_moist_airtemp %>% 
  filter(position == "hydric") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = air_temp_3m_avg), color = c("#ffd60a"), size = 1)+
  geom_line(aes(y = temp*2, color = site), size = 0.65)+
  labs(subtitle = "Hydric", color = "site")+
  scale_x_datetime(date_breaks = "1 week")+
  scale_y_continuous(name = "Air Temperature (gold line)",
                     sec.axis = sec_axis(~./2, name = "Soil Temperature"))+
  scale_color_manual(values = c("#0a9396", "#ee9b00"))+
  labs(x = "Date")+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(depth_cm~., scales="free")    



hydric_dual_rain_temp =
  dualplot_temp_moist %>% 
  filter(position == "hydric") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = temp, color = site), size = 0.6)+
  geom_line(aes(y = rain_avg*20), color = c("#001219"), size = 1)+
  labs(subtitle = "Hydric", color = "site")+
  scale_x_datetime(date_breaks = "1 week")+
  scale_y_continuous(name = "Soil Temperature, C",
                     sec.axis = sec_axis(~./20, name = "Rain, mm (black line)"))+
  labs(x = "Date")+
  scale_color_manual(values = c("#0a9396", "#ee9b00"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(depth_cm~., scales="free")    


hydric_dual_airtemp_moisture =
  dualplot_temp_moist_airtemp %>% 
  filter(depth_cm == 25 & position == "hydric") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = air_temp_3m_avg), color = c("#f28482"), size = 1)+
  geom_line(aes(y = moisture/coeff), size = 0.8)+
  labs(subtitle = "Hydric",
       x = "Date")+
  scale_x_datetime(date_breaks = "1 week")+
  scale_y_continuous(name = "Air Temperature, C (orange line)",
                     sec.axis = sec_axis(~.*coeff, name = "Soil Moisture, %"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(.~site, scales="free")    


hydric_dual_rain_moisture =
dualplot_temp_moist %>% 
  filter(position == "hydric") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = rain_avg), color = c("#001219"), size = 1)+
  geom_line(aes(y = (moisture/100), color = site), size = 0.8)+
  labs(subtitle = "Hydric",
       x = "Date")+
  scale_x_datetime(date_breaks = "1 week")+
  scale_y_continuous(name = "Rain, mm (black line)",
                     sec.axis = sec_axis(~.*100, name = "Soil Moisture, %"))+
  scale_color_manual(values = c("#0a9396", "#ee9b00"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(depth_cm~.)


ggsave("figures_finalized/hydric_dual_tempsair.png", plot = hydric_dual_temps_air, width = 8, height = 5)
ggsave("figures_finalized/hydric_dual_temps.png", plot = hydric_dual_temps, width = 8, height = 5)
ggsave("figures_finalized/hydric_dual_rain_temp.png", plot = hydric_dual_rain_temp, width = 6, height = 6)
ggsave("figures_finalized/hydric_dual_airtemp_moisture.png", plot = hydric_dual_airtemp_moisture, width = 8, height = 5)
ggsave("figures_finalized/hydric_dual_rain_moisture.png", plot = hydric_dual_rain_moisture, width = 6, height = 6)

ggsave("output/hydric_dual_temps.png", plot = hydric_dual_temps, width = 8, height = 5)
ggsave("output/hydric_dual_rain_temp.png", plot = hydric_dual_rain_temp, width = 6, height = 6)
ggsave("output/hydric_dual_airtemp_moisture.png", plot = hydric_dual_airtemp_moisture, width = 8, height = 5)
ggsave("output/hydric_dual_rain_moisture.png", plot = hydric_dual_rain_moisture, width = 6, height = 6)
ggsave("output/hydric_dual_tempsair.png", plot = hydric_dual_temps_air, width = 8, height = 5)

###stats-----

library(nlme)

soiltemplme = lme(temp ~ rain_avg*moisture*snow_depth_1hr_avg, random = ~1|date, na.action = na.omit, data = dualplot_temp_moist %>% filter(position == 'hydric'))
                                                                                                   
summary(soiltemplme)
print(soiltemplme)
fortable = anova(soiltemplme)

fortable %>% 
  broom::tidy()

write.csv(fortable, "output/lme_soiltemperature.csv")


soiltemplme_mesic = lme(temp ~ rain_avg*moisture*snow_depth_1hr_avg, random = ~1|date, na.action = na.omit, data = dualplot_temp_moist %>% filter(position == 'mesic'))

summary(soiltemplme_mesic)
print(soiltemplme_mesic)
fortable_mesic = anova(soiltemplme_mesic)

fortable_mesic %>% 
  broom::tidy()

write.csv(fortable_mesic, "output/lme_soiltemperature_mesic.csv")


dualplot_temp_moist_datefilter =
  dualplot_temp_moist %>% 
  dplyr::select(-c(air_temp_1m_avg)) %>% 
  filter(date < '2021-09-16')

soiltemplme_dry = lme(temp ~ rain_avg*moisture*snow_depth_1hr_avg, random = ~1|date, na.action = na.omit, data = dualplot_temp_moist_datefilter %>% filter(position == 'dry'))

summary(soiltemplme_dry)
print(soiltemplme_dry)
fortable_dry = anova(soiltemplme_dry)

fortable_dry %>% 
  broom::tidy()

write.csv(fortable_dry, "output/lme_soiltemperature_dry.csv")

