#6 1 2022
#Data processing/ggplots

#load all packages

source("code/0-packages.R")

###write file

rhizon_meta_combine = read.csv("processed/rhizon_2021.csv")
sipper_data = read.csv("processed/sipper_2021.csv")

#there is weird double data
#all doubles look identical except for the blank
#grouping, will check with Beth/Sumant later
#delete and revise once checking is complete

rhizon_meta_combine_notransect =
  rhizon_meta_combine %>% 
  filter(Site != 'Transect')

East_rhizon_notransect_2021 = 
  rhizon_meta_combine_notransect %>% 
  filter(Area == 'East') %>% 
  ggplot(aes(x = Betterdate, y = as.numeric(concentration), color = Site, shape = Area))+
  geom_point(size = 2.5, alpha = 0.8)+
  #geom_line(orientation = "x", group = 'Plot')+
  labs(x = "Date",
       y = 'concentration ug/mL')+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  facet_wrap(ICP~., scales = "free")+
  theme_er1()

West_rhizon_notransect_2021 = 
  rhizon_meta_combine_notransect %>% 
  filter(Area == 'West') %>% 
  ggplot(aes(x = Betterdate, y = as.numeric(concentration), color = Site, shape = Area))+
  geom_point(size = 2.5, alpha = 0.8)+
  #geom_line(orientation = "x", group = 'Plot')+
  labs(x = "Date",
       y = 'concentration ug/mL')+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  facet_wrap(ICP~., scales = "free")+
  theme_er1()

ggsave("output/East_rhizon_notransect_2021.tiff", plot = East_rhizon_notransect_2021, height = 5.7, width = 6.7)
ggsave("output/West_rhizon_notransect_2021.tiff", plot = West_rhizon_notransect_2021, height = 5.7, width = 6.7)


