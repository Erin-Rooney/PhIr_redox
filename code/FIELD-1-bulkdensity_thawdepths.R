#ECROONEY
#September 27 2022

#combining redox data with sensor depths

#load all packages

source("code/0-packages.R")

#load data

thaw_depths = read.csv("raw/thaw_depth_2021.csv")

bulk_density = read.csv("raw/PhIr2021_Soil_Inventory_bd.csv") %>% 
  dplyr::select(-c(X)) %>% 
  na.omit() %>% 
  mutate(Area = recode(Area, "East" = "acidic tundra",
                       "West" = "non-acidic tundra")) %>% 
  dplyr::mutate(soil_material = case_when(grepl("O",Horizon)~"organic",
                                      grepl("M",Horizon)~"mineral"))

thaw_depths_cleaned =
  thaw_depths %>% 
  dplyr::select(Date, Area, Site, Plot, Plot_ID, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10,
                X11, X12, X13, X14, X15) %>% 
  na.omit() %>% 
  pivot_longer(-c(Date, Area, Site, Plot, Plot_ID), names_to = "rep", values_to = "thaw_depth_cm") %>% 
  separate(Date, sep = "-", into = c("Day", "Month_num", "Year")) %>% 
  mutate(Month = recode(Month_num, "Jun" = "06", "Jul" = "07", "Aug" = "08")) %>% 
  mutate(date2 = as.Date(paste(Year,Month,Day, sep = "-"))) %>% 
  mutate(Area = recode(Area, "East" = "acidic tundra",
                       "West" = "non-acidic tundra")) 


  


####ggplots----

thaw_depths_fig =
  thaw_depths_cleaned %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  
  ggplot()+
  geom_point(aes(x = as.Date(date2), y = thaw_depth_cm), fill = "black", shape = c(21), alpha = 0.4)+
  labs(x = "Date",
       y = "Thaw Depth, cm")+
  scale_y_reverse()+
  facet_grid(Site~Area)+
  theme_er1()

ggsave("output/thawdepthsfig.TIFF", plot = thaw_depths_fig, height = 6, width = 5.75)


bulk_density_fig =
  bulk_density %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  ggplot()+
  geom_point(aes(y = grav_moist_perc, x = soil_bulk_density_g_cm3, fill = Horizon), shape = c(21), 
             size = 3, alpha = 0.4)+
  labs(y = "gravimetric moisture, %",
       x = "soil bulk density, g/cm3")+
  facet_grid(Site~Area)+
  theme_er1()

bulk_density_fig3 =
  bulk_density %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(soil_material = factor(soil_material, levels = c("organic", "mineral"))) %>% 
  ggplot()+
  geom_point(aes(y = grav_moist_perc, x = soil_bulk_density_g_cm3, fill = soil_material), shape = c(21), 
             size = 3, alpha = 0.4)+
  labs(y = "gravimetric moisture, %",
       x = "soil bulk density, g/cm3")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Sunset2", 2)))+
  facet_grid(Site~Area)+
  theme_er1()

ggsave("output/bulk_density_fig3.TIFF", plot = bulk_density_fig3, height = 6, width = 5)


bulk_density_fig2 =
  bulk_density %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(soil_material = factor(soil_material, levels = c("organic", "mineral"))) %>% 
  ggplot()+
  geom_point(aes(y = Average_Depth_cm, x = soil_bulk_density_g_cm3, fill = soil_material), shape = c(21), 
             size = 3, alpha = 0.4)+
  labs(y = "depth, cm",
       x = "soil bulk density, g/cm3",
       fill = " ")+
  scale_y_reverse()+
  scale_fill_manual(values = (PNWColors::pnw_palette("Sunset2", 2)))+
  facet_grid(Site~Area)+
  theme_er1()

ggsave("output/bulk_density_fig2.TIFF", plot = bulk_density_fig2, height = 6, width = 5)



#STATS
