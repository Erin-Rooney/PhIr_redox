#ECROONEY
#September 27 2022

#combining redox data with sensor depths

#load all packages

source("code/0-packages.R")

#load data

thaw_depths = read.csv("raw/thaw_depth_2021.csv")

bd_grav_cleaned = read.csv("raw/PhIr2021_Soil_Inventory_bd.csv") %>% 
  #dplyr::select(-c(X)) %>% 
  mutate(Area = recode(Area, "East" = "acidic tundra",
                       "West" = "non-acidic tundra")) %>% 
  dplyr::mutate(soil_material = case_when(grepl("O",Horizon)~"organic",
                                      grepl("M",Horizon)~"mineral")) 

#nope! Did it all manually, ignore below code. Leaving in for now

# real_depth_fix =
#   bulk_density %>% 
#   dplyr::select(c(Core_ID, Average_Depth_cm, Plot_ID, Horizon)) %>% 
#   group_by(Core_ID) %>% 
#   mutate(real_depth_cm = lag(Average_Depth_cm)+Average_Depth_cm) %>% 
#   ungroup() %>% 
#   mutate(anychange = case_when(grepl("2", Horizon)~"change",
#                                grepl("M", Horizon)~"change",
#                             grepl("1", Horizon)~"nochange",
#                             Horizon == "O" ~ "nochange",
#                             Horizon == "M1" ~ "change"))

#write.csv(real_depth_fix, "processed/realdepthfix.csv")


#had to delete East Hydric 3 horizon 2 because it's impossible to know the depth since O1 wasn't recorded.
#manually changed 8/7 E-M1 O2 to "change" because it wasn't working due to line 28
#manually added in 8/7 W-M2 O1 because it was getting kicked out for some reason

#real_depth_fix = read.csv("processed/realdepthfix.csv")

  
# change =
#   real_depth_fix %>% 
#   filter(anychange == "change") %>% 
#   dplyr::select(-c(Average_Depth_cm))  
#   
#   
# bd_grav_cleaned =
#   real_depth_fix %>% 
#   filter(anychange == "nochange") %>% 
#   dplyr::select(-c(real_depth_cm)) %>% 
#   rename('real_depth_cm' = 'Average_Depth_cm') %>% 
#   vctrs::vec_c(change) %>% 
#   left_join(bulk_density)



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

thaw_depths_fig_30 =
  thaw_depths_cleaned %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  ggplot()+
    geom_point(aes(x = as.Date(date2), y = thaw_depth_cm), fill = "black", shape = c(21), alpha = 0.4)+
    labs(x = "Date",
       y = "Thaw Depth, cm")+
  scale_y_reverse(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
  facet_grid(Site~Area)+
  theme_er1()

ggsave("output/thawdepthsfig30.TIFF", plot = thaw_depths_fig_30, height = 6, width = 5.75)


bulk_density_fig =
  bd_grav_cleaned %>% 
  na.omit() %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  ggplot()+
  geom_point(aes(y = grav_moist_perc, x = soil_bulk_density_g_cm3, fill = Horizon), shape = c(21), 
             size = 3, alpha = 0.4)+
  labs(y = "gravimetric moisture, %",
       x = "soil bulk density, g/cm3")+
  facet_grid(Site~Area)+
  theme_er1()

bulk_density_fig3 =
  bd_grav_cleaned %>% 
  na.omit() %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(soil_material = factor(soil_material, levels = c("organic", "mineral"))) %>% 
  ggplot()+
  geom_point(aes(y = grav_moist_perc, x = soil_bulk_density_g_cm3, fill = soil_material), shape = c(21), 
             size = 3, alpha = 0.4)+
  labs(y = "gravimetric moisture, %",
       x = "soil bulk density, g/cm3",
       fill = " ")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Sunset2", 2)))+
  facet_grid(Site~Area)+
  theme_er1()+
  theme(legend.position = "top")

ggsave("output/bulk_density_fig3.TIFF", plot = bulk_density_fig3, height = 6, width = 5)


bulk_density_fig2 =
  bd_grav_cleaned %>% 
  na.omit() %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(soil_material = factor(soil_material, levels = c("organic", "mineral"))) %>% 
  ggplot()+
  geom_point(aes(y = real_depth_cm, x = soil_bulk_density_g_cm3, fill = soil_material), shape = c(21), 
             size = 3, alpha = 0.4)+
  labs(y = "depth, cm",
       x = "soil bulk density, g/cm3",
       fill = " ")+
  scale_y_reverse()+
  scale_fill_manual(values = (PNWColors::pnw_palette("Sunset2", 2)))+
  facet_grid(Site~Area)+
  theme_er1()

ggsave("output/bulk_density_fig2.TIFF", plot = bulk_density_fig2, height = 6, width = 5)


soilmoisture_fig1=
  bd_grav_cleaned %>% 
  na.omit() %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(soil_material = factor(soil_material, levels = c("organic", "mineral"))) %>% 
  ggplot()+
  geom_point(aes(y = real_depth_cm, x = grav_moist_perc, fill = soil_material), shape = c(21), 
             size = 3, alpha = 0.4)+
  labs(y = "depth, cm",
       x = "gravimetric moisture, %",
       fill = " ")+
  scale_y_reverse()+
  scale_fill_manual(values = (PNWColors::pnw_palette("Sunset2", 2)))+
  facet_grid(Site~Area)+
  theme_er1()+
  theme(legend.position = "top")

ggsave("output/soilmoisture_fig1.TIFF", plot = soilmoisture_fig1, height = 6, width = 5)


combo_soilmoisture_bulkdensity = 
  soilmoisture_fig1+bulk_density_fig3+ #combines the two plots
  plot_layout(guides = "collect") &
  theme(legend.position='bottom') # sets a common legend

ggsave("output/combo_soilmoisture_bulkdensity.TIFF", plot = combo_soilmoisture_bulkdensity, height = 5, width = 8)
ggsave("output/combo_soilmoisture_bulkdensity.png", plot = combo_soilmoisture_bulkdensity, height = 5, width = 8)


#STATS
