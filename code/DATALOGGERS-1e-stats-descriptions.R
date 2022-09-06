#ECROONEY
#September 1 2022

#combining redox data with sensor depths

#load all packages

source("code/0-packages.R")

#load data

#combo_redox_withdepths = read.csv("processed/all_combine.csv")

redox_nosummary = read.csv('processed/all_combine_depthbins_nosummary.csv')

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


###### earliest thaw 

fallfreeze_dat =
  final_temp_sal_moist %>% 
  filter(temp < 0) %>% #isolate "frozen" only, frozen and max TIMESTAMP should go with latest, thaw should go with earliest
  group_by(site, position, depth) %>% 
  dplyr::summarise(max = max(TIMESTAMP),
                   max_x = max(X))

springthaw_dat =
  final_temp_sal_moist %>% 
  filter(temp > 1) %>% #isolate "thaw" only, thaw and min TIMESTAMP should go with earliest spring thaw
  group_by(site, position, depth) %>% 
  dplyr::summarise(min = min(TIMESTAMP),
                   min_x = min(X))

springall_dat =
  final_temp_sal_moist %>% 
  #compare all min TIMESTAMP to identify earliest spring thaw
  group_by(site, position, depth) %>% 
  dplyr::summarise(min_all = min(TIMESTAMP),
                   min_x_all = min(X))


fallall_dat =
  final_temp_sal_moist %>% 
  #compare all min TIMESTAMP to identify earliest spring thaw
  group_by(site, position, depth) %>% 
  dplyr::summarise(max_all = max(TIMESTAMP),
                   max_x_all = max(X)) 


##now combine within spring and fall, find which all TIMESTAMP mins don't match all > 1 temp TIMESTAMP mins.

spring_combo =
  springthaw_dat %>% 
  left_join(springall_dat) %>% 
  mutate(unique = (min_x/min_x_all))%>% 
  filter(unique > 1)


#fall is not working for some reason :(
#fall all TIMESTAMP maxs are earlier than < 0 temp TIMESTAMP maxs, which doesn't make sense.
fall_combo =
  fallfreeze_dat %>% 
  left_join(fallall_dat) %>% 
  mutate(unique = (max_x/max_x_all))%>% 
  filter(unique > 1)


######

redox_for_leftjoin =
  redox_nosummary %>% 
  rename(depth = depth_cm) %>% 
  dplyr::select(TIMESTAMP, site, position, Betterdate, avg_values_fixed)

for_boxplot = 
  final_temp_sal_moist %>% 
  left_join(redox_for_leftjoin) %>% 
  dplyr::mutate(month = case_when(grepl("2021-06", Betterdate)~"june",
                                  grepl("2021-07", Betterdate)~"july",
                                  grepl("2021-08", Betterdate)~"august",
                                  grepl("2021-09", Betterdate)~"september")) %>% 
  mutate(month = factor(month, levels = c("june", "july", "august", "september"))) %>% 
  dplyr::mutate(site_pos = (paste0(site,"-", position))) %>% 
  mutate(site_pos = factor(site_pos, levels = c("east-dry", "west-dry", "east-mesic", "west-mesic",
  
                                                                                              "east-hydric", "west-hydric"))) 
  
  
temp_dat =
  final_temp_sal_moist %>% 
  dplyr::mutate(month = case_when(grepl("2021-06", Betterdate)~"june",
                                  grepl("2021-07", Betterdate)~"july",
                                  grepl("2021-08", Betterdate)~"august",
                                  grepl("2021-09", Betterdate)~"september")) %>%
  mutate(month = factor(month, levels = c("june", "july", "august", "september"))) %>% 
  group_by(site, position, depth, month) %>% 
  dplyr::summarise(temp_max = round(max(temp),2),
                   temp_min = round(min(temp),2),
                   temp_avg = round(mean(temp),2),
                   temp_se = round(sd(temp),2)) %>% 
  dplyr::mutate(site_pos = (paste0(site,"-", position))) %>% 
  mutate(site_pos = factor(site_pos, levels = c("east-dry", "west-dry", "east-mesic", "west-mesic",
                                                "east-hydric", "west-hydric"))) 
  
###

#temp bloxplot

temp_boxplot = 
  for_boxplot %>% 
  ggplot(aes(x = site_pos, y = temp, fill = site_pos))+
  geom_boxplot(alpha = 0.7)+
  labs(x = "",
       y = 'Temperature, C')+
  scale_fill_manual(values = natparks.pals(name = "KingsCanyon", 6))+
  facet_grid(depth~month, scales = "free")+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90), legend.position = 'NONE')


ggsave(plot = temp_boxplot, "output/temp_boxplot.tiff", width = 8, height = 7)
  
###
  
# temp_dat %>% 
#   ggplot(aes(x = site_pos, y = temp, fill = site_pos))+
#   geom_bar(alpha = 0.7)+
#   labs(x = "",
#        y = 'Temperature, C')+
#   scale_fill_manual(values = natparks.pals(name = "Yellowstone", 6))+
#   facet_grid(depth~month, scales = "free")+
#   theme_er1()+
#   theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))

##redox

#redox bloxplot

redox_boxplot = 
  for_boxplot %>% 
  ggplot(aes(x = site_pos, y = avg_values_fixed, fill = site_pos))+
  geom_boxplot(alpha = 0.7)+
  labs(x = "",
       y = 'redox potential')+
  scale_fill_manual(values = natparks.pals(name = "KingsCanyon", 6))+
  facet_grid(depth~month, scales = "free")+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90), legend.position = 'NONE')


ggsave(plot = redox_boxplot, "output/redox_boxplot.tiff", width = 8, height = 7)
ggsave(plot = redox_boxplot, "output/redox_boxplot.png", width = 8, height = 7)

#redox line plot

for_lineplot =
  for_boxplot %>% 
  ungroup() %>% 
  group_by(depth, site_pos, Betterdate, month) %>%
  dplyr::summarise(temp_avg = round(mean(temp),2),
                   temp_sd = round(sd(temp)/sqrt(n()),2),
                   moist_avg = round(mean(moisture),2),
                   moist_sd = round(sd(moisture)/sqrt(n()),2),
                   sal_avg = round(max(salinity),2),
                   sal_sd = round(sd(salinity)/sqrt(n()),2),
                   redox_avg = round(mean(avg_values_fixed),2),
                   redox_sd = round(sd(avg_values_fixed)/sqrt(n()),2)) 

redox_lineplot = 
  for_lineplot %>%
  filter(month == "july") %>% 
  #dplyr::mutate(Betterdate = as.numeric(Betterdate)) %>% 
  ggplot(aes(x = as.Date(Betterdate), y = redox_avg, fill = depth, group = depth, color = depth))+
  geom_point(size = 3, shape = 21, alpha = 0.6)+
  #geom_line(size = 0.5, linetype = "solid", orientation = "x", group = 'site_pos')+
  scale_x_date(date_breaks = "1 week" , date_labels = "%Y-%m-%d")+
  
  labs(x = "",
       y = 'redox potential')+
  # scale_fill_gradientn(colors = natparks.pals(name = "Arches", 3))+
  # scale_color_manual(values = natparks.pals(name = "Arches", 3))+
  facet_grid(site_pos~.)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90), legend.position = 'Bottom')


  
  
  
