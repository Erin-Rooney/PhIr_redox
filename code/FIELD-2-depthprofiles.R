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
  mutate(volumetric_water_content_cm3_cm3 = soil_bulk_density_g_cm3 * grav_water_gh20_per_gdrysoil) 

bd_select = 
  bd_grav_cleaned %>% 
  dplyr::select(Sample_ID, Core_ID, Date_collected, Area, Site, Plot_num, Plot_ID, 
                Horizon, Depth_1_cm, Depth_2_cm, Depth_3_cm, Depth_4_cm, Average_Depth_cm, real_depth_cm, 
                soil_bulk_density_g_cm3, volumetric_water_content_cm3_cm3, soil_material) %>% 
  mutate(label = Horizon) %>% 
  mutate(label = factor(label, levels = c("O", "O1", "O2", "O3", "M", "M1", "M2"))) %>% 
  mutate(date_simple = recode(Date_collected, "6-Jul-21" = "Plot-1", "7-Jul-21" = "Plot-1",
                              "13-Jul-21" = "Plot-2", "14-Jul-21" = "Plot-2",
                              "24-Jul-21" = "Plot-3",
                              "30-Jul-21" = "Plot-4", "31-Jul-21" = "Plot-4",
                              "7-Aug-21" = "Plot-5")) %>% 
  mutate(label = paste0(date_simple, "-", Plot_num))  
  
  
  
  
horizons_acidic =
  bd_select %>% 
  filter(Site != "Transect" & Area == "acidic tundra") %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(Horizon = factor(Horizon, levels = c("M2", "M1", "M", "O3", "O2", "O1", "O"))) %>% 
  ggplot()+
  geom_col(aes(y = Average_Depth_cm, x = label, fill = Horizon), color = "white", position = 'stack', width = 0.7)+
  scale_y_reverse()+
  labs(title = "acidic tundra",
       fill = "", color = "",
       y = "depth, cm",
       x = " ")+
  scale_fill_manual(values = c("#BFAFA6", "#AA968A", "#AA968A", "#734F38", "#553725", "#482919", "#482919"))+
  facet_grid(Site~., scales="free_x") +
  guides(fill = guide_legend(reverse = TRUE))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "right",
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

horizons_nonacidic =
  bd_select %>% 
  filter(Site != "Transect" & Area == "non-acidic tundra") %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(Horizon = factor(Horizon, levels = c("M2", "M1", "M", "O3", "O2", "O1", "O"))) %>% 
  ggplot()+
  geom_col(aes(y = Average_Depth_cm, x = label, fill = Horizon), color = "white", position = 'stack', width = 0.7)+
  scale_y_reverse()+
  labs(title = "non-acidic tundra",
         fill = "", color = "",
       y = "depth, cm",
       x = " ")+
  scale_fill_manual(values = c("#AA968A", "#553725", "#482919", "#482919"))+
  facet_grid(Site~., scales="free_x") +
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

horizons_all = horizons_nonacidic + horizons_acidic

ggsave("formanuscript/horizons_all.png", plot = horizons_all, height = 6, width = 10)

 

horizons_dry_acidic =
  bd_select %>% 
  filter(Site == "Dry" & Area == "acidic tundra") %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(Horizon = factor(Horizon, levels = c("M2", "M1", "M", "O3", "O2", "O1", "O"))) %>% 
  ggplot()+
  geom_col(aes(y = Average_Depth_cm, x = Core_ID, fill = Horizon), color = "white", position = 'stack', width = 0.7)+
  scale_y_reverse()+
  labs(fill = "", color = "",
       y = "depth, cm",
       x = "plot")+
  # scale_fill_manual(values = c("#D6AB7D", "#B3895D", "#B3895D", "#734F38", "#553725", "#482919", "#482919"))+
  scale_fill_manual(values = c("#D6AB7D", "#734F38", "#553725", "#482919"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  facet_grid(Site~Area, scales="free_x") +
  guides(fill = guide_legend(reverse = TRUE))

horizons_dry_nonacidic =
  bd_select %>% 
  filter(Site == "Dry" & Area == "non-acidic tundra") %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(Horizon = factor(Horizon, levels = c("M2", "M1", "M", "O3", "O2", "O1", "O"))) %>% 
  ggplot()+
  geom_col(aes(y = Average_Depth_cm, x = Core_ID, fill = Horizon), color = "white", position = 'stack', width = 0.7)+
  scale_y_reverse()+
  labs(fill = "", color = "",
       y = "depth, cm",
       x = "plot")+
  # scale_fill_manual(values = c("#D6AB7D", "#B3895D", "#B3895D", "#734F38", "#553725", "#482919", "#482919"))+
  scale_fill_manual(values = c( "#734F38", "#553725", "#482919", "#482919"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "right")+
  facet_grid(Site~Area, scales="free_x") +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("output/horizons_nonacidic.png", plot = horizons_nonacidic, height = 4.5, width = 9.5)
ggsave("output/horizons_acidic.png", plot = horizons_acidic, height = 4.5, width = 9.5)

ggsave("formanuscript/horizons_nonacidic_dry.png", plot = horizons_dry_nonacidic, height = 4, width = 7)
ggsave("formanuscript/horizons_acidic_dry.png", plot = horizons_dry_acidic, height = 4, width = 7)


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
  filter(Site != "Transect") %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(Horizon = factor(Horizon, levels = c("O", "O1", "O2", "O3", "M", "M1", "M2"))) %>% 
  dplyr::select(c(Area, Site, Horizon, soil_bulk_density_g_cm3, volumetric_water_content_cm3_cm3)) %>% 
  na.omit() %>% 
  group_by(Area, Site, Horizon) %>% 
  dplyr::summarise(mean_bd = round(mean(soil_bulk_density_g_cm3),2),
                   sd_bd = round(sd(soil_bulk_density_g_cm3),2),
                   mean_vwc = round(mean(volumetric_water_content_cm3_cm3),2),
                   sd_vwc = round(sd(volumetric_water_content_cm3_cm3),2)) %>% 
  mutate(bd_summary = paste(mean_bd, "\u00b1", sd_bd),
         vwc_summary = paste(mean_vwc, "\u00b1", sd_vwc)) %>% 
  dplyr::select(c(Area, Site, Horizon, bd_summary, vwc_summary)) 
  

bd_grouped %>% knitr::kable()

write.csv(bd_grouped, "output/bd_grouped.csv")

bd_forfigs =
  bd_select %>% 
  filter(Site != "Transect") %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(Horizon = factor(Horizon, levels = c("O", "O1", "O2", "O3", "M", "M1", "M2"))) %>% 
  #dplyr::select(c(Area, Site, Core_ID, Horizon, soil_bulk_density_g_cm3, volumetric_water_content_cm3_cm3)) %>% 
  na.omit() %>% 
  mutate(horizon_simplified = recode(Horizon, "O" = "Organic Surface", "O1" = "Organic Surface",
                                     "O2" = "Organic Subsurface", "O3" = "Organic Subsurface", "M" = "Mineral Subsurface",
                                     "M1" = "Mineral Subsurface", "M2" = "Mineral Subsurface")) %>% 
  mutate(depth_color = case_when(real_depth_cm >= 21 ~ "c",
                                 real_depth_cm <= 10 ~ "a",
                                 TRUE ~ "b")) %>% 
  separate(Date_collected, sep = "-", into = c("Day", "Month", "Year")) %>% 
  mutate(Month = recode(Month, "Jun" = "06", "Jul" = "07", "Aug" = "08")) %>% 
  mutate(Year = recode(Year, "21" = "2021")) %>% 
  mutate(date2 = as.Date(paste(Year,Month,Day, sep = "-"))) 


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
  facet_grid(Area~Site) +
  guides(color = guide_legend(reverse = TRUE))

vwc_fig =
  bd_forfigs %>% 
  filter(Site != "Transect") %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(horizon_simplified = factor(horizon_simplified, levels = c("Mineral Subsurface", "Organic Subsurface", "Organic Surface"))) %>% 
  ggplot()+
  #geom_line(aes(y = mean_depth, x = soil_bulk_density_g_cm3, group = Core_ID))+
  geom_point(aes(y = (volumetric_water_content_cm3_cm3*100), x = date2, color = depth_color), size = 3)+
  labs(fill = "", color = "",
       y = "volumetric water content (cm3 water/ cm3 soil)",
       x = "date")+
  #scale_color_manual(values = c("#D6AB7D", "#8A5A44", "#482919"))+
  scale_color_manual(values=(pnw_palette("Lake", 3)))+
  facet_grid(Site~Area, scales = "free_x") +
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  guides(color = guide_legend(reverse = TRUE))

ggsave("output/bd_simplified.png", plot = bulkdensity_simplified, height = 4.5, width = 6)
ggsave("formanuscript/vwc_fig.png", plot = vwc_fig, height = 5, width = 5)
