#ECROONEY
#July 8 2022

#combining redox data with sensor depths

#load all packages

source("code/0-packages.R")

#load data

combo_redox_withdepths = read.csv("processed/combo_redox_withdepths.csv")


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

combo_redox_withdepths %>% 
  na.omit() %>% 
  ggplot(aes(x = as.Date(Betterdate), y = depth_cm, fill = avg_values_summarised))+
  geom_bar(position = "stack", stat= "identity")+
  scale_x_date(date_breaks = "1 week" , date_labels = "%Y-%m-%d")+
  scale_y_reverse()+
  scale_fill_gradientn(colors = (PNWColors::pnw_palette("Bay")))+  
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(position~site)

