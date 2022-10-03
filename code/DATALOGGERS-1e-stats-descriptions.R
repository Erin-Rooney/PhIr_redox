#ECROONEY
#September 1 2022

#combining redox data with sensor depths

#load all packages

source("code/0-packages.R")

#load data

combo_redox_withdepths = read.csv("processed/all_combine.csv")

#redox_nosummary = read.csv('processed/all_combine_depthbins_nosummary.csv')

final_temp_sal_moist = read.csv("processed/final_temp_salinity_avgs.csv")

#add depth bins for figures

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

final_temp_sal_moist_bins_forjoin =
  final_temp_sal_moist_bins %>% 
  dplyr::select(site, position, Betterdate, depth, moisture, temp, salinity)


combo_redox_threedepths_avg =
  combo_redox_withdepths %>% 
  rename(depth_redox = depth_cm) %>% 
  mutate(depth = case_when(depth_redox <= 10 ~ 5,
                              depth_redox <= 20 ~ 15,
                              depth_redox >= 21 ~ 25)) %>% 
  dplyr::select(site, position, Betterdate, Plot, avg_values_fixed, depth, depth_redox) %>%
  group_by(Betterdate, site, position, depth) %>% 
  dplyr::summarise(depth_avg = round(mean(avg_values_fixed),2),
                   depth_avg_se = round(sd(avg_values_fixed)/sqrt(n()),2),
                   n = n()) %>% 
  left_join(final_temp_sal_moist_bins_forjoin) %>% 
  mutate(Betterdate2 = Betterdate) %>% 
  separate(Betterdate, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>%
  dplyr::mutate(month_name = case_when(grepl("06", month)~"june",
                                       grepl("07", month)~"july",
                                       grepl("08", month)~"august",
                                       grepl("09", month)~"september")) %>% 
  mutate(month_name = factor(month_name, levels = c("june", "july", "august", "september"))) %>% 
  rename(redox_avg = depth_avg) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) 


final_temp_sal_moist_bins_forjoin_daily =
  final_temp_sal_moist_bins_forjoin %>% 
  separate(Betterdate, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>% 
  group_by(site, position, month, day, depth) %>% 
  dplyr::summarise(temp_avg = round(mean(temp),2),
                   temp_se = round(sd(temp)/sqrt(n()),2),
                   moisture_avg = round(mean(moisture),2),
                   moisture_se = round(sd(moisture)/sqrt(n()),2),
                   salinity_avg = round(mean(salinity),2),
                   salinity_se = round(sd(salinity)/sqrt(n()),2))

  
combo_redox_threedepths_avgdaily =
  combo_redox_withdepths %>% 
  rename(depth_redox = depth_cm) %>% 
  mutate(depth = case_when(depth_redox <= 10 ~ 5,
                           depth_redox <= 20 ~ 15,
                           depth_redox >= 21 ~ 25)) %>% 
  separate(Betterdate, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>%
  dplyr::select(site, position, Plot, avg_values_fixed, month, day, depth, depth_redox) %>%
  group_by(site, position, month, day, depth) %>% 
  dplyr::summarise(depth_avg = round(mean(avg_values_fixed),2),
                   depth_avg_se = round(sd(avg_values_fixed)/sqrt(n()),2),
                   n = n()) %>% 
  left_join(final_temp_sal_moist_bins_forjoin_daily) %>% 
  rename(redox_avg = depth_avg) %>% 
  dplyr::mutate(month_name = case_when(grepl("06", month)~"june",
                                       grepl("07", month)~"july",
                                       grepl("08", month)~"august",
                                       grepl("09", month)~"september")) %>%
  mutate(month_name = factor(month_name, levels = c("june", "july", "august", "september"))) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) 

combo_redox_unbinned_avgdaily =
  combo_redox_withdepths %>% 
  separate(Betterdate, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>%
  dplyr::select(site, position, Plot, avg_values_fixed, month, day, depth_cm) %>%
  group_by(site, position, month, day, depth_cm) %>% 
  dplyr::summarise(depth_avg = round(mean(avg_values_fixed),2),
                   depth_avg_se = round(sd(avg_values_fixed)/sqrt(n()),2),
                   n = n()) %>% 
  dplyr::mutate(month_name = case_when(grepl("06", month)~"june",
                                       grepl("07", month)~"july",
                                       grepl("08", month)~"august",
                                       grepl("09", month)~"september")) %>%
  mutate(month_name = factor(month_name, levels = c("june", "july", "august", "september"))) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) 

write.csv(combo_redox_unbinned_avgdaily, "processed/dailyredox_unbinned.csv") 


#####ggplots-----













moistXredox =
  combo_redox_threedepths_avg %>% 
  ggplot()+
  geom_point(aes(x = moisture, y = redox_avg, color = month_name),
             alpha = 0.7)+
  labs(fill = "month",
       y = "redox potential, mV",
       x = "soil moisture, %")+
  scale_color_manual(values = natparks.pals(name = "SmokyMtns", 4))+
  facet_grid(site~position)+
  theme_er1()

tempXredox =
  combo_redox_threedepths_avg %>% 
  ggplot()+
  geom_point(aes(x = temp, y = redox_avg, color = month_name),
             alpha = 0.7)+
  labs(fill = "month",
       y = "redox potential, mV",
       x = "soil temperature, C")+
  scale_color_manual(values = natparks.pals(name = "SmokyMtns", 4))+
  facet_grid(site~position)+
  theme_er1()

salXredox =
  combo_redox_threedepths_avg %>% 
  ggplot()+
  geom_point(aes(x = salinity, y = redox_avg, color = month_name),
             alpha = 0.7)+
  labs(fill = "month",
       y = "redox potential, mV",
       x = "salinity, uS")+
  scale_color_manual(values = natparks.pals(name = "SmokyMtns", 4))+
  facet_grid(site~position)+
  theme_er1()

ggsave(plot = tempXredox, "output/tempXredox.tiff", width = 8.5, height = 5.5)
ggsave(plot = moistXredox, "output/moistXredox.tiff", width = 8.5, height = 5.5)
ggsave(plot = salXredox, "output/salXredox.tiff", width = 8.5, height = 5.5)

#####ggplots by depth------

moistXredoxeast =
  combo_redox_threedepths_avg %>% 
  filter(site == "east" & month_name == "july") %>% 
  mutate(depth = factor(depth, levels = c("5", "15", "25"))) %>% 
  ggplot()+
  geom_point(aes(x = moisture, y = redox_avg, color = depth),
             alpha = 0.5)+
  labs(fill = "month",
       y = "redox potential, mV",
       x = "soil moisture, %")+
  scale_color_manual(values = natparks.pals(name = "Banff"))+
  facet_grid(.~position)+
  theme_er1()


nonacidic_salmoistredox =
  combo_redox_threedepths_avg %>% 
  filter(site == "west" & month_name == "july") %>% 
  mutate(depth = factor(depth, levels = c("5", "15", "25"))) %>% 
  ggplot()+
  geom_point(aes(x = moisture, y = salinity, color = redox_avg),
             alpha = 0.5)+
  labs(color = "redox potenital, mV",
       subtitle = "non-acidic tundra",
       y = "salinity, uS",
       x = "soil moisture, %")+
  scale_color_gradientn(colors = (PNWColors::pnw_palette("Anemone")))+
  facet_grid(depth~position)+
  theme_er1()

acidic_salmoistredox =
  combo_redox_threedepths_avg %>% 
  filter(site == "east" & month_name == "july") %>% 
  mutate(depth = factor(depth, levels = c("5", "15", "25"))) %>% 
  ggplot()+
  geom_point(aes(x = moisture, y = salinity, color = redox_avg), alpha = 0.5)+
  labs(color = "redox potential, mV",
       subtitle = "acidic tundra",
       y = "salinity, uS",
       x = "soil moisture, %")+
  scale_color_gradientn(colors = (PNWColors::pnw_palette("Anemone")))+
  facet_grid(depth~position)+
  theme_er1()

ggsave(plot = acidic_salmoistredox, "output/acidic_salmoistredox.tiff", width = 6.75, height = 5.75)
ggsave(plot = nonacidic_salmoistredox, "output/nonacidic_salmoistredox.tiff", width = 6.75, height = 5.75)



hydricacidic_salmoistredox =
  combo_redox_threedepths_avg %>% 
  filter(site == "east" & month_name == "july" & position == "hydric" & depth == "5") %>% 
  mutate(depth = factor(depth, levels = c("5", "15", "25"))) %>% 
  ggplot()+
  geom_point(aes(x = moisture, y = salinity, fill = redox_avg, color = redox_avg),
             shape = c(21), alpha = 0.6, size = 4)+
  labs(color = "redox potential, mV",
       fill = "redox potential, mV",
       subtitle = "acidic tundra, hydric 5 cm",
       y = "salinity, uS",
       x = "soil moisture, %")+
  scale_fill_gradientn(colors = (PNWColors::pnw_palette("Anemone")))+
  scale_color_gradientn(colors = (PNWColors::pnw_palette("Anemone")))+
  facet_grid(month_name~.)+
  theme_er1()+
  theme(legend.position = "right")


hydricacidic_salmoistredox =
  combo_redox_threedepths_avg %>% 
  filter(site == "east" & month_name == "august" & position == "hydric" & depth == "5") %>% 
  mutate(depth = factor(depth, levels = c("5", "15", "25"))) %>% 
  ggplot()+
  geom_point(aes(x = moisture, y = salinity, fill = redox_avg, color = redox_avg),
             shape = c(21), alpha = 0.6, size = 4)+
  labs(color = "redox potential, mV",
       fill = "redox potential, mV",
       subtitle = "acidic tundra, hydric 5 cm",
       y = "salinity, uS",
       x = "soil moisture, %")+
  scale_fill_gradientn(colors = (PNWColors::pnw_palette("Anemone")))+
  scale_color_gradientn(colors = (PNWColors::pnw_palette("Anemone")))+
  facet_grid(month_name~.)+
  theme_er1()+
  theme(legend.position = "right")

drynonacidic15_salmoistredox =
  combo_redox_threedepths_avg %>% 
  filter(site == "west" & month_name == "july" & position == "dry" & depth == "15") %>% 
  mutate(depth = factor(depth, levels = c("5", "15", "25"))) %>% 
  ggplot()+
  geom_point(aes(x = moisture, y = salinity, fill = redox_avg, color = redox_avg),
             shape = c(21), alpha = 0.6, size = 4)+
  labs(color = "redox potential, mV",
       fill = "redox potential, mV",
       subtitle = "non-acidic tundra, dry 15 cm",
       y = "salinity, uS",
       x = "soil moisture, %")+
  scale_fill_gradientn(colors = (PNWColors::pnw_palette("Anemone")))+
  scale_color_gradientn(colors = (PNWColors::pnw_palette("Anemone")))+
  #facet_grid(depth~position)+
  theme_er1()+
  theme(legend.position = "right")

drynonacidic25_salmoistredox =
  combo_redox_threedepths_avg %>% 
  filter(site == "west" & month_name == "july" & position == "dry" & depth == "25") %>% 
  mutate(depth = factor(depth, levels = c("5", "15", "25"))) %>% 
  ggplot()+
  geom_point(aes(x = moisture, y = salinity, fill = redox_avg, color = redox_avg),
             shape = c(21), alpha = 0.6, size = 4)+
  labs(color = "redox potential, mV",
       fill = "redox potential, mV",
       subtitle = "non-acidic tundra, dry 25 cm",
       y = "salinity, uS",
       x = "soil moisture, %")+
  scale_fill_gradientn(colors = (PNWColors::pnw_palette("Anemone")))+
  scale_color_gradientn(colors = (PNWColors::pnw_palette("Anemone")))+
  #facet_grid(depth~position)+
  theme_er1()+
  theme(legend.position = "right")

ggsave(plot = hydricacidic_salmoistredox, "output/hydricacidic_salmoistredox.tiff", width = 6, height = 4)
ggsave(plot = drynonacidic15_salmoistredox, "output/drynonacidic15_salmoistredox.tiff", width = 6, height = 4)
ggsave(plot = drynonacidic25_salmoistredox, "output/drynonacidic25_salmoistredox.tiff", width = 6, height = 4)

######

hydricacidic_salmoistredox_DATA =
  combo_redox_threedepths_avg %>% 
  filter(site == "east" & month_name == "july" & position == "hydric" & depth == "5") 


#all data but only for Fe important redox

combo_redox_threedepths_avg %>% 
  filter(redox_avg > 200 & redox_avg < 300) %>% 
  mutate(depth = factor(depth, levels = c("5", "15", "25"))) %>% 
  ggplot()+
  geom_point(aes(x = moisture, y = salinity, fill = position, color = position, shape = site),
             alpha = 0.6, size = 4)+
  labs(color = "redox potential, mV",
       fill = "redox potential, mV",
       subtitle = "Redox potentials 200-300 mV",
       y = "salinity, uS",
       x = "soil moisture, %")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
  scale_color_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
  facet_grid(depth~.)+
  theme_er1()+
  theme(legend.position = "right")


combo_redox_threedepths_avg %>% 
  filter(redox_avg < 200) %>% 
  mutate(depth = factor(depth, levels = c("5", "15", "25"))) %>% 
  ggplot()+
  geom_point(aes(x = moisture, y = salinity, fill = position, color = position, shape = site),
             alpha = 0.6, size = 2.5)+
  labs(color = "redox potential, mV",
       fill = "redox potential, mV",
       subtitle = "Redox potentials < 300 mV",
       y = "salinity, uS",
       x = "soil moisture, %")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
  scale_color_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
  facet_grid(depth~.)+
  theme_er1()+
  theme(legend.position = "right")


  combo_redox_threedepths_avg %>% 
  filter(redox_avg > 300) %>% 
  mutate(depth = factor(depth, levels = c("5", "15", "25"))) %>% 
  ggplot()+
  geom_point(aes(x = moisture, y = salinity, fill = position, color = position, shape = site),
             alpha = 0.6, size = 2.5)+
  labs(color = "redox potential, mV",
       fill = "redox potential, mV",
       subtitle = "Redox potentials > 300 mV",
       y = "salinity, uS",
       x = "soil moisture, %")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
  scale_color_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
  facet_grid(depth~.)+
  theme_er1()+
  theme(legend.position = "right")

lessthan300redox =
  combo_redox_threedepths_avgdaily %>% 
  filter(redox_avg < 300) %>% 
  mutate(depth = factor(depth, levels = c("5", "15", "25"))) %>% 
  ggplot()+
  geom_point(aes(x = moisture_avg, y = salinity_avg, fill = position, color = position, shape = site),
             alpha = 0.6, size = 4)+
  labs(color = "redox potential, mV",
       fill = "redox potential, mV",
       subtitle = "Redox potentials < 300 mV",
       y = "salinity, uS",
       x = "soil moisture, %")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
  scale_color_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
  facet_grid(depth~.)+
  theme_er1()+
  theme(legend.position = "right")

greaterthan300redox =
  combo_redox_threedepths_avgdaily %>% 
  filter(redox_avg > 300) %>% 
  mutate(depth = factor(depth, levels = c("5", "15", "25"))) %>% 
  ggplot()+
  geom_point(aes(x = moisture_avg, y = salinity_avg, fill = position, color = position, shape = site),
             alpha = 0.6, size = 4)+
  labs(color = "redox potential, mV",
       fill = "redox potential, mV",
       subtitle = "Redox potentials > 300 mV",
       y = "salinity, uS",
       x = "soil moisture, %")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
  scale_color_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
  facet_grid(depth~.)+
  theme_er1()+
  theme(legend.position = "right")

ggsave(plot = lessthan300redox, "output/lessthan300redox.tiff", width = 4.5, height = 6)
ggsave(plot = greaterthan300redox, "output/greaterthan300redox.tiff", width = 4.5, height = 6)

#############







###### earliest thaw -----




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

redox_for_plots =
  redox_nosummary %>% 
  rename(depth = depth_cm) %>% 
  dplyr::select(TIMESTAMP, site, depth, position, Betterdate, avg_values_fixed)

for_barplot = 
  redox_for_plots %>% 
  dplyr::mutate(month = case_when(grepl("2021-06", Betterdate)~"june",
                                  grepl("2021-07", Betterdate)~"july",
                                  grepl("2021-08", Betterdate)~"august",
                                  grepl("2021-09", Betterdate)~"september")) %>% 
  mutate(month = factor(month, levels = c("june", "july", "august", "september"))) %>% 
  dplyr::mutate(site_pos = (paste0(site,"-", position))) %>% 
  mutate(site_pos = factor(site_pos, levels = c("east-dry", "west-dry", "east-mesic", "west-mesic",
   "east-hydric", "west-hydric"))) %>% 
  group_by(site_pos, depth, month) %>% 
  dplyr::summarise(redox_avg = round(mean(avg_values_fixed),2),
                   redox_se = round(sd(avg_values_fixed)/sqrt(n()),2))
  
  
temp_moisture_sal_dat =
  final_temp_sal_moist %>% 
  dplyr::mutate(month = case_when(grepl("2021-06", Betterdate)~"june",
                                  grepl("2021-07", Betterdate)~"july",
                                  grepl("2021-08", Betterdate)~"august",
                                  grepl("2021-09", Betterdate)~"september")) %>%
  mutate(month = factor(month, levels = c("june", "july", "august", "september"))) %>% 
  # group_by(site, position, depth, month) %>% 
  # dplyr::summarise(temp_max = round(max(temp),2),
  #                  temp_min = round(min(temp),2),
  #                  temp_avg = round(mean(temp),2),
  #                  temp_se = round(sd(temp)/sqrt(n()),2),
  #                  moisture_avg = round(mean(moisture),2),
  #                  moisture_se = round(sd(moisture)/sqrt(n()),2),
  #                  salinity_avg = round(mean(salinity),2),
  #                  salinity_se = round(sd(salinity)/sqrt(n()),2)) %>% 
  dplyr::mutate(site_pos = (paste0(site,"-", position))) %>% 
  mutate(site_pos = factor(site_pos, levels = c("east-dry", "west-dry", "east-mesic", "west-mesic",
                                                "east-hydric", "west-hydric"))) 
  

###

#temp bloxplot

temp_boxplot = 
  temp_moisture_sal_dat %>% 
  ggplot(aes(x = site_pos, y = temp, fill = site_pos))+
  geom_boxplot(alpha = 0.7)+
  labs(x = "",
       y = 'Temperature, C')+
  scale_fill_manual(values = natparks.pals(name = "KingsCanyon", 6))+
  facet_grid(depth~month, scales = "free")+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90), legend.position = 'NONE')


ggsave(plot = temp_boxplot, "output/temp_boxplot.tiff", width = 8, height = 7)


#moisture bloxplot

moisture_boxplot = 
  temp_moisture_sal_dat %>% 
  ggplot(aes(x = site_pos, y = moisture, fill = site_pos))+
  geom_boxplot(alpha = 0.7)+
  labs(x = "",
       y = 'Moisture, %')+
  scale_fill_manual(values = natparks.pals(name = "KingsCanyon", 6))+
  facet_grid(depth~month, scales = "free")+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90), legend.position = 'NONE')


ggsave(plot = moisture_boxplot, "output/moisture_boxplot.tiff", width = 8, height = 7)


#salinity bloxplot

salinity_boxplot = 
  temp_moisture_sal_dat %>% 
  ggplot(aes(x = site_pos, y = salinity, fill = site_pos))+
  geom_boxplot(alpha = 0.7)+
  labs(x = "",
       y = 'salinity, uS/cm')+
  scale_fill_manual(values = natparks.pals(name = "KingsCanyon", 6))+
  facet_grid(depth~month, scales = "free")+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90), legend.position = 'NONE')


ggsave(plot = salinity_boxplot, "output/salinity_boxplot.tiff", width = 8, height = 7)

  
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

#redox barplot

redox_point = 
  for_barplot %>% 
  ggplot()+
  geom_point(aes(y = depth, x = month, fill = redox_avg, color = redox_avg), alpha = 0.6, size = 3)+
  # geom_errorbar(aes(x = depth, ymin = (avg_values_fixed - ), ymax = (avg_values_fixed + redox_sd)),
  #                 position = position_dodge(), stat = "identity", color = "gray50")+
  labs(x = "",
       y = 'depth, cm')+
  scale_fill_gradientn(colors = natparks.pals(name = "Banff"))+
  scale_color_gradientn(colors = natparks.pals(name = "Banff"))+
  #coord_flip()+
  scale_y_reverse()+
  facet_wrap(site_pos~.)+
  theme_er2()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90), legend.position = 'bottom')


ggsave(plot = redox_boxplot, "output/redox_boxplot.tiff", width = 8, height = 7)
ggsave(plot = redox_boxplot, "output/redox_boxplot.png", width = 8, height = 7)

#redox line plot

for_lineplot =
  for_boxplot %>% 
  ungroup() %>% 
  group_by(depth, site_pos, Betterdate, month) %>%
  mutate(depth = as.factor(depth)) %>% 
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
  scale_x_date(date_breaks = "1 day" , date_labels = "%Y-%m-%d")+
  
  labs(x = "",
       y = 'redox potential')+
  scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3)))+
  scale_color_manual(values = rev(natparks.pals(name = "Banff", 3)))+
  facet_grid(site_pos~month)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90), legend.position = 'bottom')


ggsave(plot = redox_lineplot, "output/redox_lineplotjuly.tiff", width = 6, height = 12)
ggsave(plot = redox_lineplot, "output/redox_lineplotjuly.png", width = 6, height = 12)

  

######

#redox summaries

redox_summary =
  combo_redox_withdepths  %>% 
  dplyr::mutate(month = case_when(grepl("2021-06", Betterdate)~"june",
                                  grepl("2021-07", Betterdate)~"july",
                                  grepl("2021-08", Betterdate)~"august",
                                  grepl("2021-09", Betterdate)~"september")) %>% 
  mutate(month = factor(month, levels = c("june", "july", "august", "september"))) %>% 
  dplyr::mutate(site_pos = (paste0(site,"-", position))) %>% 
  mutate(site_pos = factor(site_pos, levels = c("east-dry", "west-dry", "east-mesic", "west-mesic",
                                                "east-hydric", "west-hydric"))) %>% 
  group_by(site_pos, month, depth_cm) %>%
  dplyr::summarise(redox_avg = round(mean(avg_values_summarised),2),
                   redox_sd = round(sd(avg_values_summarised)/sqrt(n()),2))


redox_summary %>%
  filter(month == "july") %>% 
  ggplot()+
  geom_bar(aes(y = redox_avg, x = depth_cm, fill = site_pos, color = site_pos),position=position_dodge(.9),
           stat = "identity", width = 2, alpha = 0.6)+
  geom_errorbar(aes(x = depth_cm, ymin = redox_avg - redox_sd, ymax = redox_avg + redox_sd),
  position = position_dodge(), stat = "identity", color = "gray50")+
  coord_flip()+
  scale_fill_manual(values = natparks.pals(name = "KingsCanyon", 6))+
  scale_color_manual(values = natparks.pals(name = "KingsCanyon", 6))+
  facet_grid(site_pos~.)+
  scale_x_reverse()+
  theme_er1()


#not working, something weird about summary

# for_barplot =
#   for_boxplot %>%
#   ungroup() %>%
#   group_by(site_pos, month, depth) %>%
#   dplyr::summarise(temp_avg = round(mean(temp),2),
#                    temp_sd = round(sd(temp)/sqrt(n()),2),
#                    moist_avg = round(mean(moisture),2),
#                    moist_sd = round(sd(moisture)/sqrt(n()),2),
#                    sal_avg = round(max(salinity),2),
#                    sal_sd = round(sd(salinity)/sqrt(n()),2),
#                    redox_avg = round(mean(avg_values_fixed),2),
#                    redox_sd = round(sd(avg_values_fixed)/sqrt(n()),2))
# 
# for_barplot %>%
#   mutate(depth = as.numeric(depth)) %>% 
#   filter(month == "july") %>% 
#   ggplot()+
#   geom_bar(aes(y = redox_avg, x = depth, fill = site_pos, color = site_pos), position = "dodge",
#            stat = "identity", width = 2, alpha = 0.6)+
#   geom_errorbar(aes(x = depth, ymin = (redox_avg - redox_sd), ymax = (redox_avg + redox_sd)),
#                 position = position_dodge(), stat = "identity", color = "gray50")+
#   coord_flip()+
#   facet_wrap(site_pos~month)+
#   scale_x_reverse()+
#   theme_er1()

  
