#ECROONEY
#July 8 2022

#combining redox data with sensor depths

#load all packages

source("code/0-packages.R")

#load data

combo_redox_withdepths = read.csv("processed/all_combine.csv")
final_temp_sal_moist = read.csv("processed/final_temp_salinity_avgs.csv")

final_temp_sal_moist_bins =
  final_temp_sal_moist %>% 
  # create bins of 5 cm depth increments
  mutate(depth_bins = cut_width(depth, width = 10, center=5)) %>% 
  # mutate(depth_bins = case_when(depth_cm = 5 ~ cut_width(depth_cm, width = 1, center = 5))) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]","")) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[","")) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
  # now separate this into two different columns
  separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm")) %>% 
  mutate(depth_start_cm = as.integer(depth_start_cm),
         depth_stop_cm = as.integer(depth_stop_cm)) %>% 
  mutate(depth2 = depth_stop_cm - depth_start_cm)


combo_redox_withdepths_bins =
  combo_redox_withdepths %>% 
  mutate(depth_bins = cut_width(depth_cm, width = 4, center=2)) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]","")) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[","")) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
  # now separate this into two different columns
  separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm")) %>% 
  mutate(depth_start_cm = as.integer(depth_start_cm),
         depth_stop_cm = as.integer(depth_stop_cm)) %>% 
  mutate(depth2 = depth_stop_cm - depth_start_cm)

final_temp_sal_moist_dailynumbers =
  final_temp_sal_moist %>% 
  separate(Betterdate, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>%
  dplyr::mutate(month_name = case_when(grepl("06", month)~"june",
                                  grepl("07", month)~"july",
                                  grepl("08", month)~"august",
                                  grepl("09", month)~"september")) %>% 
  mutate(month_name = factor(month_name, levels = c("june", "july", "august", "september"))) %>% 
  dplyr::mutate(site_pos = (paste0(site,"-", position))) %>% 
  mutate(site_pos = factor(site_pos, levels = c("east-dry", "west-dry", "east-mesic", "west-mesic",
                                                "east-hydric", "west-hydric"))) %>% 
  group_by(site_pos, month_name, day, depth) %>% 
  dplyr::summarise(temp_avg = round(mean(temp),2),
                   temp_se = round(sd(temp)/sqrt(n()),2),
                   moisture_avg = round(mean(moisture),2),
                   moisture_se = round(sd(moisture)/sqrt(n()),2),
                   salinity_avg = round(mean(salinity),2),
                   salinity_se = round(sd(salinity)/sqrt(n()),2))

##############

twentyfivecm_peaktemps =
  final_temp_sal_moist_dailynumbers %>% 
  filter(depth == 25)

twentyfivecm_peaktemps_westmesic =
  final_temp_sal_moist_dailynumbers %>% 
  filter(depth == 25 & site_pos == "west-mesic")

fifteencm_peaktemps_easthydric =
  final_temp_sal_moist_dailynumbers %>% 
  filter(depth == 15 & site_pos == "east-hydric")

fifteencm_peaktemps_westhydric =
  final_temp_sal_moist_dailynumbers %>% 
  filter(depth == 15 & site_pos == "west-hydric")

fifteencm_peaktemps_eastmesic =
  final_temp_sal_moist_dailynumbers %>% 
  filter(depth == 15 & site_pos == "east-mesic")

fifteencm_peaktemps_westmesic =
  final_temp_sal_moist_dailynumbers %>% 
  filter(depth == 15 & site_pos == "west-mesic")

fifteencm_peaktemps_eastdry =
  final_temp_sal_moist_dailynumbers %>% 
  filter(depth == 15 & site_pos == "east-dry")

fifteencm_peaktemps_westdry =
  final_temp_sal_moist_dailynumbers %>% 
  filter(depth == 15 & site_pos == "west-dry")
  
###############

#patterns associated with temp peaks

twentyfivecm_dailypeaktemps =
  final_temp_sal_moist_dailynumbers %>% 
  dplyr::select(-temp_se, -moisture_se, -salinity_se) %>% 
  pivot_longer(-c('site_pos', 'month_name', 'day', 'depth'), names_to = "type", values_to = "value")  
  

temp_point =
  twentyfivecm_dailypeaktemps %>% 
  filter(type == "temp_avg" & depth > 20) %>% 
  ggplot(aes(x = day, y = value))+
  geom_point(aes(color = site_pos), alpha = 0.5)+
  geom_line(aes(group = site_pos, color = site_pos, orientation = "x"))+
  scale_color_manual(values = natparks.pals(name = "KingsCanyon", 6))+
  #scale_x_discrete(breaks = seq(-1,31,2))+
  labs(y = "temperature, C")+
  facet_wrap(month_name~.)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))
  
moisture_point =
  twentyfivecm_dailypeaktemps %>% 
  filter(type == "moisture_avg" & depth > 20) %>% 
  ggplot(aes(x = day, y = value))+
  geom_point(aes(color = site_pos), alpha = 0.5)+
  geom_line(aes(group = site_pos, color = site_pos, orientation = "x"))+
  scale_color_manual(values = natparks.pals(name = "KingsCanyon", 6))+
  labs(y = "moisture %")+
  #scale_x_discrete(breaks = seq(-1,31,2))+
  facet_wrap(month_name~.)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))

salinity_point =
  twentyfivecm_dailypeaktemps %>% 
  filter(type == "salinity_avg" & depth > 20) %>% 
  ggplot(aes(x = day, y = value))+
  geom_point(aes(color = site_pos), alpha = 0.5)+
  geom_line(aes(group = site_pos, color = site_pos, orientation = "x"))+
  scale_color_manual(values = natparks.pals(name = "KingsCanyon", 6))+
  labs(y = "salinity, uS/cm")+
  #scale_x_discrete(breaks = seq(-1,31,2))+
  facet_wrap(month_name~.)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))

ggsave(plot = temp_point, "output/temp_25.tiff", width = 8.5, height = 5.5)
ggsave(plot = moisture_point, "output/moisture_25.tiff", width = 8.5, height = 5.5)
ggsave(plot = salinity_point, "output/salinity_25.tiff", width = 8.5, height = 5.5)

###############

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

pal <- pnw_palette("Bay",100)

combo_redox_withdepths %>% 
  na.omit() %>% 
  ggplot(aes(x = as.Date(Betterdate), y = depth_cm, fill = avg_values_summarised))+
  geom_bar(position = "stack", stat= "identity")+
  scale_x_date(date_breaks = "1 week" , date_labels = "%Y-%m-%d")+
  scale_y_reverse()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "yellow", midpoint = 0)+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(position~site)

combo_redox_withdepths_bins %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  na.omit() %>% 
  ggplot(aes(xmin = (as.Date(Betterdate))-0.4, xmax = (as.Date(Betterdate))+0.4, ymin = depth_start_cm, ymax = depth_stop_cm, fill = avg_values_summarised))+
  geom_rect()+
  labs(y = "depth, cm",
       fill = "redox potential")+
  scale_x_date(date_breaks = "1 week" , date_labels = "%Y-%m-%d")+
  scale_y_reverse()+
  scale_fill_gradientn(colors = natparks.pals("Banff"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(position~site)

redox_fig =
  combo_redox_withdepths_bins %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  na.omit() %>% 
  ggplot(aes(xmin = (as.Date(Betterdate))-0.4, xmax = (as.Date(Betterdate))+0.4, ymin = depth_start_cm, ymax = depth_stop_cm, fill = avg_values_summarised))+
  geom_rect()+
  labs(y = "depth, cm",
       fill = "redox potential, volts")+
  scale_x_date(date_breaks = "1 week" , date_labels = "%Y-%m-%d")+
  scale_y_reverse()+
  scale_fill_gradientn(colors = rev(PNWColors::pnw_palette("Sunset2")))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90), legend.position = "bottom")+
  facet_grid(position~site)
  

moisture_fig =
  final_temp_sal_moist_bins %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  na.omit() %>% 
  ggplot(aes(xmin = (as.Date(Betterdate))-0.4, xmax = (as.Date(Betterdate))+0.4, ymin = depth_start_cm, ymax = depth_stop_cm, fill = moisture))+
  geom_rect()+
  labs(y = "depth, cm",
       fill = "soil moisture (%)")+
  scale_x_date(date_breaks = "1 week" , date_labels = "%Y-%m-%d")+
  ylim(30, 0)+
  scale_fill_gradientn(colors = rev(PNWColors::pnw_palette("Bay")))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90), legend.position = "bottom")+
  facet_grid(position~site)


temp_fig =
  final_temp_sal_moist_bins %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  na.omit() %>% 
  ggplot(aes(xmin = (as.Date(Betterdate))-0.4, xmax = (as.Date(Betterdate))+0.4, ymin = depth_start_cm, ymax = depth_stop_cm, fill = temp))+
  geom_rect()+
  labs(y = "depth, cm",
       fill = "soil temperature, Celsius")+
  scale_x_date(date_breaks = "1 week" , date_labels = "%Y-%m-%d")+
  ylim(30, 0)+
  scale_fill_gradientn(colors = PNWColors::pnw_palette("Sailboat"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90), legend.position = "bottom")+
  facet_grid(position~site)

sal_fig =
  final_temp_sal_moist_bins %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  na.omit() %>% 
  ggplot(aes(xmin = (as.Date(Betterdate))-0.4, xmax = (as.Date(Betterdate))+0.4, ymin = depth_start_cm, ymax = depth_stop_cm, fill = salinity))+
  geom_rect()+
  labs(y = "depth, cm",
       fill = "salinity, uS")+
  scale_x_date(date_breaks = "1 week" , date_labels = "%Y-%m-%d")+
  ylim(30, 0)+
  scale_fill_gradientn(colors = PNWColors::pnw_palette("Shuksan2"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90), legend.position = "bottom")+
  facet_grid(position~site)

ggsave("output/moisture.tiff", plot = moisture_fig, height = 6, width = 8)
ggsave("output/temperature.tiff", plot = temp_fig, height = 6, width = 8)
ggsave("output/salinity.tiff", plot = sal_fig, height = 6, width = 8)
ggsave("output/redox.tiff", plot = redox_fig, height = 6, width = 8)
