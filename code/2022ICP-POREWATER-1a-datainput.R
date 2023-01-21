#Raw data input
#rhizons
#E C Rooney
#1 21 2023

#load packages

source("code/0-packages.R")

#load document

raw_ICP = read.csv("raw/2023_LIME_ICPAES_002R_Herndon_0118.csv")

processed_ICP = 
  raw_ICP %>% 
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  dplyr::select(c(SampleID, Date, Time, Area, Site, Plot, Depth_cm, Al_ug_mL, Ca_ug_mL, Fe_ug_mL, 
           K_ug_mL, Mg_ug_mL, Mn_ug_mL, Na_ug_mL, P_ug_mL)) %>% 
  na.omit() %>% 
  mutate(Al_ug_mL = recode(Al_ug_mL, "<0.01" = "0.005")) %>% 
  mutate(Mn_ug_mL = recode(Mn_ug_mL, "<0.005" = '0.003')) %>% 
  mutate(P_ug_mL = recode(P_ug_mL, "<0.01" = "0.005",
                          "<0.1" = "0.05")) %>%  #this seems like an error...need to check
  separate(Date, sep = "-", into =c('day', 'month', 'year')) %>% 
  mutate(month = recode(month, "Jun" = "06", "Jul" = "07", "Aug" = "08", "Sep" = "09")) %>% 
  mutate(year = recode(year, "22" = "2022")) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(Al_ug_mL = as.numeric(Al_ug_mL)) %>% 
  mutate(Mn_ug_mL = as.numeric(Mn_ug_mL)) %>% 
  mutate(P_ug_mL = as.numeric(P_ug_mL)) %>%
  mutate(Fe_ug_mL = as.numeric(Fe_ug_mL)) %>% 
  pivot_longer(-c(SampleID, date, Time, Area, Site, Plot, day, month, year, Depth_cm), names_to = 'ICP', values_to = 'concentration') 

Fe_2022_fig =
  processed_ICP %>% 
  filter(ICP == c("Fe_ug_mL")) %>% 
  ggplot(aes(x = date, y = Depth_cm, fill = concentration, group = date)) +
  geom_point(size = 4, shape = c(21))+
  #geom_line(orientation = "y")+
  scale_fill_gradientn(colors = rev(natparks.pals(name = "Arches")))+
  labs(fill = "Fe ug/mL",
       y = "Depth, cm")+
  scale_y_reverse()+
  facet_grid(Site ~ Area)+
  theme_er1()

 
P_2022_fig =
  processed_ICP %>% 
  filter(ICP == c("P_ug_mL")) %>% 
  ggplot(aes(x = date, y = Depth_cm, fill = concentration, group = date)) +
  geom_point(size = 4, shape = c(21))+
  #geom_line(orientation = "y")+
  scale_fill_gradientn(colors = rev(natparks.pals(name = "Olympic")))+
  labs(fill = "P ug/mL",
       y = "Depth, cm")+
  scale_y_reverse()+
  facet_grid(Site ~ Area)+
  theme_er1() 


Mn_2022_fig =
  processed_ICP %>% 
  filter(ICP == c("Mn_ug_mL")) %>% 
  ggplot(aes(x = date, y = Depth_cm, fill = concentration, group = date)) +
  geom_point(size = 4, shape = c(21))+
  labs(fill = "Mn ug/mL",
       y = "Depth, cm")+
  #geom_line(orientation = "y")+
  scale_fill_gradientn(colors = rev(natparks.pals(name = "Acadia")))+
  scale_y_reverse()+
  facet_grid(Site ~ Area)+
  theme_er1() 

ggsave("output/Mn_2022_fig.png", plot = Mn_2022_fig, height = 6.5, width = 5.75)
ggsave("output/Fe_2022_fig.png", plot = Fe_2022_fig, height = 6.5, width = 5.75)
ggsave("output/P_2022_fig.png", plot = P_2022_fig, height = 6.5, width = 5.75)
