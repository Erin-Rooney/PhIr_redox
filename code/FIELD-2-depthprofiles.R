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
                                          grepl("M",Horizon)~"mineral")) %>% 
  mutate(volumetric_water_content_gcm3 = soil_bulk_density_g_cm3 * grav_water_gh20_per_gdrysoil) 

bd_select = 
  bd_grav_cleaned %>% 
  dplyr::select(Sample_ID, Core_ID, Date_collected, Area, Site, Plot_num, Plot_ID, 
                Horizon, Depth_1_cm, Depth_2_cm, Depth_3_cm, Depth_4_cm, Average_Depth_cm, real_depth_cm, 
                soil_bulk_density_g_cm3, volumetric_water_content_gcm3, soil_material) 
  
horizons_acidic =
  bd_select %>% 
  filter(Site != "Transect" & Area == "acidic tundra") %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(Horizon = factor(Horizon, levels = c("M2", "M1", "M", "O3", "O2", "O1", "O"))) %>% 
  ggplot()+
  geom_col(aes(y = Average_Depth_cm, x = Core_ID, fill = Horizon), color = "white", position = 'stack', width = 0.7)+
  scale_y_reverse()+
  labs(fill = "", color = "",
       y = "depth, cm",
       x = "plot")+
  scale_fill_manual(values = c("#D6AB7D", "#B3895D", "#B3895D", "#734F38", "#553725", "#482919", "#482919"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "right")+
  facet_grid(Area~Site, scales="free") 

horizons_nonacidic =
  bd_select %>% 
  filter(Site != "Transect" & Area == "non-acidic tundra") %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(Horizon = factor(Horizon, levels = c("M2", "M1", "M", "O3", "O2", "O1", "O"))) %>% 
  ggplot()+
  geom_col(aes(y = Average_Depth_cm, x = Core_ID, fill = Horizon), color = "white", position = 'stack', width = 0.7)+
  scale_y_reverse()+
  labs(fill = "", color = "",
       y = "depth, cm",
       x = "plot")+
  scale_fill_manual(values = c("#B3895D", "#553725", "#482919", "#482919"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "right")+
  facet_grid(Area~Site, scales="free") 

ggsave("output/horizons_nonacidic.png", plot = horizons_nonacidic, height = 4.5, width = 9.5)
ggsave("output/horizons_acidic.png", plot = horizons_acidic, height = 4.5, width = 9.5)


#Bulk density

bd_nonacidic = 
  bd_select %>% 
  filter(Site != "Transect" & Area == "non-acidic tundra") %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(Horizon = factor(Horizon, levels = c("M2", "M1", "M", "O3", "O2", "O1", "O"))) %>% 
  ggplot()+
  geom_line(aes(y = real_depth_cm, x = soil_bulk_density_g_cm3, group = Core_ID))+
  geom_point(aes(y = real_depth_cm, x = soil_bulk_density_g_cm3, color = Horizon, group = Core_ID), size = 3)+
  scale_y_reverse()+
  labs(fill = "", color = "",
       y = "depth, cm",
       x = "bulk density")+
  scale_color_manual(values = c("#B3895D", "#553725", "#482919", "#482919"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "right")+
  facet_grid(Area~Site) 

bd_acidic =
bd_select %>% 
  filter(Site != "Transect" & Area == "acidic tundra") %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(Horizon = factor(Horizon, levels = c("M2", "M1", "M", "O3", "O2", "O1", "O"))) %>% 
  ggplot()+
  geom_line(aes(y = real_depth_cm, x = soil_bulk_density_g_cm3, group = Core_ID))+
  geom_point(aes(y = real_depth_cm, x = soil_bulk_density_g_cm3, color = Horizon, group = Core_ID), size = 3)+
  scale_y_reverse()+
  labs(fill = "", color = "",
       y = "depth, cm",
       x = "bulk density")+
  scale_color_manual(values = c("#D6AB7D", "#B3895D", "#B3895D", "#734F38", "#553725", "#482919", "#482919"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "right")+
  facet_grid(Area~Site) 

ggsave("output/bd_nonacidic.png", plot = bd_nonacidic, height = 4.5, width = 8.5)
ggsave("output/bd_acidic.png", plot = bd_acidic, height = 4.5, width = 8.5)

bd_grouped =
  bd_select %>% 
  mutate(horizon_simplified = recode(Horizon, "O" = "Organic Surface", "O1" = "Organic Surface",
                                     "O2" = "Organic Subsurface", "O3" = "Organic Subsurface", "M" = "Mineral Subsurface",
                                     "M1" = "Mineral Subsurface", "M2" = "Mineral Subsurface")) %>% 
  group_by(Area, Site, Plot_num, Core_ID, horizon_simplified) %>% 
  dplyr::summarise(mean_bd = mean(soil_bulk_density_g_cm3),
                   mean_vwc = mean(volumetric_water_content_gcm3),
                   mean_depth = mean(real_depth_cm)) %>% 
  na.omit()


bulkdensity_simplified =
  bd_grouped %>% 
  filter(Site != "Transect") %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(horizon_simplified = factor(horizon_simplified, levels = c("Mineral Subsurface", "Organic Subsurface", "Organic Surface"))) %>% 
  ggplot()+
  #geom_line(aes(y = mean_depth, x = soil_bulk_density_g_cm3, group = Core_ID))+
  geom_point(aes(y = mean_depth, x = mean_bd, color = horizon_simplified, group = Core_ID), size = 3)+
  scale_y_reverse()+
  labs(fill = "", color = "",
       y = "depth, cm",
       x = "bulk density (g/cm3)")+
  scale_color_manual(values = c("#D6AB7D", "#8A5A44", "#482919"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(Area~Site) 

vwc_simplified =
bd_grouped %>% 
  filter(Site != "Transect") %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(horizon_simplified = factor(horizon_simplified, levels = c("Mineral Subsurface", "Organic Subsurface", "Organic Surface"))) %>% 
  ggplot()+
  #geom_line(aes(y = mean_depth, x = soil_bulk_density_g_cm3, group = Core_ID))+
  geom_point(aes(y = mean_depth, x = mean_vwc, color = horizon_simplified, group = Core_ID), size = 3)+
  scale_y_reverse()+
  labs(fill = "", color = "",
       y = "depth, cm",
       x = "Volumetric Water Content (g/cm3)")+
  scale_color_manual(values = c("#D6AB7D", "#8A5A44", "#482919"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(Area~Site) 

ggsave("output/bd_simplified.png", plot = bulkdensity_simplified, height = 4.5, width = 6)
ggsave("output/vwc_simplified.png", plot = vwc_simplified, height = 4.5, width = 6)
