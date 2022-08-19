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
