#June 1 2021
#ECRooney
#Data logger data

#load all packages

source("code/0-packages.R")

#load data

# hydric_dat = read.csv("processed/hydric_combine.csv")

combo_dat = read.csv("processed/all_combine.csv")


#ggplot having issues

combo_dat_forggplot =
  combo_dat %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>% 
  mutate(depth_cm = as.numeric(depth_cm)) %>% 
  na.omit()
  


combo_dat_forggplot %>% 
  na.omit() %>% 
  ggplot(aes(x = as.Date(Betterdate), y=depth_start_cm, yend=depth_stop_cm, fill = avg_values_summarised))+
  geom_bar(position = "stack", stat = "identity")+
 scale_x_date(date_breaks = "1 week" , date_labels = "%m-%d-%Y")+
  scale_y_reverse()+
  
  scale_fill_gradientn(colors = (PNWColors::pnw_palette("Bay")))+  
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(position~site, scales = 'free_y') 







combo_redox_avgstd =
  combo_redox_longer_avg %>% 
  left_join(combo_redox_longer_std)

write.csv(combo_redox_avgstd, "processed/combo_redox_avgstd.csv")
