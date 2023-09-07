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
  

ncount =
  bd_select %>% 
  replace(is.na(.),"not analyzed")  %>% 
  mutate(data = case_when(grepl("not analyzed", soil_bulk_density_g_cm3) ~ "not analyzed", TRUE ~ "analyzed")) %>% 
  group_by(Area, Site, Date_collected, Core_ID, data) %>%
  dplyr::summarise(n = n())

ncount2 =
  bd_select %>% 
  replace(is.na(.),"not analyzed")  %>% 
  mutate(data = case_when(grepl("not analyzed", soil_bulk_density_g_cm3) ~ "not analyzed", TRUE ~ "analyzed")) 

write.csv(ncount2, "output/ncount2.csv")

ncount_bd_vol =
ncount %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric", "Transect"))) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(Date_collected = factor(Date_collected, levels = c("6-Jul-21", "7-Jul-21", "13-Jul-21", "14-Jul-21", "24-Jul-21", "30-Jul-21", 
                                                            "31-Jul-21", "7-Aug-21"))) %>% 
  ggplot()+
  geom_col(aes(x = Date_collected, y = n, fill = data))+
  scale_fill_manual(values = c("#efc3e6", "#b8bedd"))+
  labs(x = "date collected",
       y = "number of samples",
       fill = "bulk density 
& volumetric water")+
  facet_grid(Site~Area, scales = "free")+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "top",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave("output/ncount_bd_vol.png", plot = ncount_bd_vol, height = 7, width = 4.5)

  
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

####

singleprofilethaw =
  thaw_depths %>% 
  filter(Area == "West" & Site == "Hydric" & Plot == "3" & Date == "7-Aug-2021")

library(tibble)
tocombine = tribble(
  ~Area, ~Site, ~Horizon, ~Average_Depth_cm, ~soil_material,
  'non-acidic tundra', 'Hydric', "O/M", 32, "active layer",
  'non-acidic tundra', 'Hydric', "permafrost", 8, "permafrost")


singleprofile_nonacidic_hydric =
  bd_select %>% 
  filter(Site != "Transect" & Area == "non-acidic tundra" & Site == "Hydric" & label == "Plot-1-3") %>% 
  dplyr::select(c(Area, Site, Horizon, Average_Depth_cm, soil_material)) %>% 
  rbind(tocombine)
  





gglabel = tribble(
   ~Area, ~Site, ~x, ~y, ~label,
  'non-acidic tundra', 'Hydric', 1, 3.7, 'low density 
  organic soil',        
  'non-acidic tundra', 'Hydric', 1, 15, 'high density 
  organic soil',
  'non-acidic tundra', 'Hydric', 1, 30, 'active layer
  organic + mineral soil',
  'non-acidic tundra', 'Hydric', 1, 58, 'permafrost',
  )  

horizons_nonacidic_hydric_singleprofile =
  singleprofile_nonacidic_hydric %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(Horizon = factor(Horizon, levels = c("permafrost", "O/M", "O2", "O1"))) %>% 
  ggplot()+
  geom_col(aes(y = Average_Depth_cm, x = Site, fill = Horizon), color = "white", position = 'stack', width = 0.7)+
    geom_text(data = gglabel, aes(x = x, y = y, label = label), color = 'white', size = 7)+
    scale_y_reverse()+
  labs(title = " ",
       fill = "", color = "",
       y = "depth (cm)",
       x = "Soil Profile")+
  scale_fill_manual(values = c("#b8c0ff", "#9a8c98", "#532C1E", "#2F0E07"))+
  #facet_grid(Site~., scales="free_x") +
  theme_er1()+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="white",size=0.25, fill = NA))

ggsave("formanuscript/horizons_nonacidic_hydric_singleprofile.png", plot = horizons_nonacidic_hydric_singleprofile, height = 6.4, width = 5.6)

# bd_nonacidic_hydric_singleprofile =
#   bd_select %>% 
#   filter(Site != "Transect" & Area == "non-acidic tundra" & Site == "Hydric" & label == "Plot-1-3") %>% 
#   mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
#   mutate(Horizon = factor(Horizon, levels = c("M2", "M1", "M", "O3", "O2", "O1", "O"))) %>% 
#   ggplot()+
#   geom_col(aes(y = Average_Depth_cm, x = Site, fill = Horizon), color = "white", position = 'stack', width = 0.7)+
#   geom_text(data = gglabel, aes(x = x, y = y, label = label), color = 'white', size = 6)+
#   scale_y_reverse()+
#   labs(title = " ",
#        fill = "", color = "",
#        y = "depth (cm)",
#        x = "Soil Profile")+
#   scale_fill_manual(values = c("#532C1E", "#2F0E07"))+
#   #facet_grid(Site~., scales="free_x") +
#   theme_er1()+
#   theme(axis.text.x = element_blank(), legend.position = "none",
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), panel.border = element_rect(color="white",size=0.25, fill = NA))
# 
# ggsave("formanuscript/horizons_nonacidic_hydric_singleprofile.png", plot = horizons_nonacidic_hydric_singleprofile, height = 4, width = 3.25)

 

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
  dplyr::select(c(Area, Site, Horizon, soil_bulk_density_g_cm3, volumetric_water_content_cm3_cm3, real_depth_cm)) %>% 
  na.omit() %>% 
  group_by(Area, Site, Horizon) %>% 
  dplyr::summarise(mean_depth = round(mean(real_depth_cm),2),
                   sd_depth = round(sd(real_depth_cm),2),
                  mean_bd = round(mean(soil_bulk_density_g_cm3),2),
                   sd_bd = round(sd(soil_bulk_density_g_cm3),2),
                   mean_vwc = round(mean(volumetric_water_content_cm3_cm3),2),
                   sd_vwc = round(sd(volumetric_water_content_cm3_cm3),2)) %>% 
  mutate(depth_summary = paste(mean_depth, "\u00b1", sd_depth),
         bd_summary = paste(mean_bd, "\u00b1", sd_bd),
         vwc_summary = paste(mean_vwc, "\u00b1", sd_vwc)
         ) %>% 
  dplyr::select(c(Area, Site, Horizon, depth_summary, bd_summary, vwc_summary, mean_bd, sd_bd, mean_vwc, sd_vwc)) 
  

bd_grouped %>% knitr::kable()

write.csv(bd_grouped, "output/bd_grouped.csv")


bd_ungrouped =
  bd_select %>% 
  filter(Site != "Transect") %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(Horizon = factor(Horizon, levels = c("O", "O1", "O2", "O3", "M", "M1", "M2"))) %>% 
  dplyr::select(c(Date_collected, Core_ID, Area, Site, Horizon, soil_bulk_density_g_cm3, volumetric_water_content_cm3_cm3, real_depth_cm)) %>% 
  na.omit() 

bd_ungrouped %>% knitr::kable()

write.csv(bd_ungrouped, "output/bd_ungrouped.csv")

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

bd_summarized_forfigs =
  bd_forfigs %>% 
  group_by(Area, Site, horizon_simplified) %>% 
  dplyr::summarize(mean_bd = round(mean(soil_bulk_density_g_cm3), 3),
                   se_bd = round(sd(soil_bulk_density_g_cm3), 2),
                   mean_depth = round(mean(real_depth_cm, 3))
  )


#bulkdensity_simplified =
  bd_summarized_forfigs %>% 
  filter(Site != "Transect") %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(horizon_simplified = factor(horizon_simplified, levels = c("Mineral Subsurface", "Organic Subsurface", "Organic Surface"))) %>% 
  ggplot()+
  geom_line(aes(y = mean_depth, x = mean_bd), orientation = "y")+
  geom_point(aes(y = mean_depth, x = mean_bd, color = horizon_simplified), size = 4)+
  scale_y_reverse()+
  labs(fill = "", color = "",
       y = "depth (cm)")+
    xlab(bquote("bulk density " (g/cm^3)))+
  scale_color_manual(values = c("#D6AB7D", "#8A5A44", "#2F0E07"))+
    facet_grid(Area~Site) +
    guides(color = guide_legend(reverse = TRUE))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 12), 
        legend.position = "bottom",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA)
        )
  
  BD_linefig =
  bd_summarized_forfigs %>% 
    ggplot()+
    geom_line(aes(x = mean_bd, y = mean_depth, color = Area, group = Area), orientation = "y", size = 1)+
    geom_ribbon(aes(xmin = mean_bd-se_bd, xmax = mean_bd+se_bd, 
                    y = mean_depth, fill = Area, color = Area, group = Area), alpha = 0.4, size = 0.2)+
    scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
    scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
    ylim(40, 0)+
    # xlim(0, 50)+
    labs(y = "Depth (cm)",
         color = " ",
         fill = " ")+
    xlab(bquote("bulk density " (g/cm^3)))+
    facet_grid(Site ~ .)+
    guides(color = guide_legend(nrow = 2))+
    theme_er1()+
    theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
          strip.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
  
  ggsave("output/BD_linefig.png", plot = BD_linefig, height = 6.5, width = 1.5)
  

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


########correlations


###dataloggersmoisture----

final_temp_sal_moist = read.csv("processed/final_temp_salinity_avgs.csv")

library(lubridate)

final_temp_sal_moist_forfig =
  final_temp_sal_moist %>% 
  mutate(forsep = TIMESTAMP) %>% 
  separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
  separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(datetime = ymd_hm(paste(date, time)),
         date = ymd(date)) %>% 
  separate(forsep, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "/", into = c("month", "day", "year")) %>% 
  mutate(date2 = as.Date(paste(year,month,day, sep = "-"))) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  dplyr::rename(depth_cm = depth) %>% 
  na.omit() %>% 
  mutate(frozen = case_when(temp < -1 ~ "frozen", TRUE ~ "unfrozen")) %>% 
  dplyr::select(site, position, depth_cm, moisture, date2) %>% 
  filter(date2 == c("2021-07-06", "2021-07-07", "2021-07-13", "2021-07-14",
                    "2021-07-30", "2021-07-31", "2021-08-07")) %>% 
  mutate(depth_cm = as.numeric(depth_cm)) %>%
  group_by(site, position, date2, depth_cm) %>%
  summarise(moisture_avg = mean(moisture),
            moisture_sd = sd(moisture))
  
vwc_forcombo =
  bd_forfigs %>%
  janitor::clean_names() %>% 
  dplyr::rename(position = site, site = area) %>% 
  dplyr::select(site, position, depth_color, volumetric_water_content_cm3_cm3, date2) %>% 
  mutate(depth_cm = recode(depth_color, "a" = "5",
                           "b" = "15",
                           "c" = "25")) %>% 
  mutate(depth_cm = as.numeric(depth_cm)) %>% 
  mutate(position = recode(position, "Dry" = "dry",
                           "Mesic" = 'mesic',
                           "Hydric" = 'hydric')) %>%
  dplyr::select(-depth_color) %>% 
  group_by(site, position, date2, depth_cm) %>%
  summarise(vwc_avg = mean(volumetric_water_content_cm3_cm3),
            vwc_sd = sd(volumetric_water_content_cm3_cm3))
  

moisture_correlations_summarized = 
  vwc_forcombo %>% 
  left_join(final_temp_sal_moist_forfig)

write.csv(moisture_correlations_summarized, 'output/moisture_correlations_summarized.csv')

moisture_correlations %>% 
  mutate(depth_cm = factor(depth_cm, levels = c("5", "15", "25"))) %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot()+
  geom_point(aes(x = (volumetric_water_content_cm3_cm3*100), y = moisture, color = depth_cm))+
  scale_color_manual(values=(pnw_palette("Lake", 3)))+
  facet_grid(position~site, scales = "free_x") +
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  guides(color = guide_legend(reverse = TRUE))

library(ggpmisc)
library(ggpubr)


moisturecorrelations_fig =
moisture_correlations_summarized %>% 
  mutate(depth_cm = recode(depth_cm, "5" = "0-10", "15" = "10-20", "25" = "20-30")) %>% 
  mutate(depth_cm = factor(depth_cm, levels = c("0-10", "10-20", "20-30"))) %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(x = (vwc_avg*100), y = moisture_avg))+
  #geom_smooth(method = "lm", se=FALSE)+
  # stat_cor(method = "pearson", label.x = 10, label.y = 55)+
  # stat_poly_line() +
  # stat_poly_eq(aes(label = after_stat(eq.label))) +
  # stat_poly_eq(label.y = 0.9) +
  geom_smooth(method = "lm", color="grey", formula = y ~ 0 + x, se = FALSE)+
  stat_regline_equation(formula = y ~ 0 + x, label.y = 45, aes(label = ..eq.label..)) +
  stat_regline_equation(formula = y ~ 0 + x, label.y = 40, aes(label = ..rr.label..))+
  geom_point(aes(color = depth_cm, fill = depth_cm), size = 3.5, shape = c(21))+
  ylim(0, 60)+
  xlim(0, 60)+
  scale_color_manual(values=(pnw_palette("Lake", 3)))+
  scale_fill_manual(values=(pnw_palette("Lake", 3)))+
  labs(x = bquote('measured volumetric water content'~(cm^3/cm^3)),
       y = "datalogger moisture %",
       fill = "depth, cm", color = "depth, cm")+
  #facet_grid(position~site, scales = "free_x") +
  theme_er1()+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

# moisturecorrelations_fig =
# moisture_correlations %>% 
#   mutate(depth_cm = factor(depth_cm, levels = c("5", "15", "25"))) %>% 
#   #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   ggplot()+
#   geom_point(aes(x = (volumetric_water_content_cm3_cm3*100), y = moisture, fill = depth_cm, color = depth_cm), size = 3.5, shape = c(21),
#              alpha = 0.8)+
#   labs(x = "measured volumetric water content, cm3/cm3",
#        y = "datalogger moisture %")+
#   scale_fill_manual(values=(pnw_palette("Lake", 3)))+
#   scale_color_manual(values=(pnw_palette("Lake", 3)))+
#   #facet_grid(position~site, scales = "free_x") +
#   theme_er1()+
#   theme(legend.position = "none",
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank())+
#   guides(color = guide_legend(reverse = TRUE))

# 
# moisturecorrelations_fig =
#   moisture_correlations_summarized %>% 
#   mutate(depth_cm = recode(depth_cm, "5" = "0-10", "15" = "10-20", "25" = "20-30")) %>% 
#   mutate(depth_cm = factor(depth_cm, levels = c("0-10", "10-20", "20-30"))) %>%
#   mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   #ggscatter(x = , y = , color = depth_cm, fill = depth_cm) size = 3.5, shape = c(21))
#   ggscatter(x = "vwc_avg", y = "moisture_avg", color = "depth_cm", fill = "depth_cm", size = 3.5,
#           add = "reg.line",  # Add regressin line
#           add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
#           conf.int = TRUE)+ # Add confidence interval
#   # stat_cor(label.y = 50) +
#   # stat_regline_equation(label.y = 55)+
#   stat_regline_equation(label.y = 55, aes(label = ..eq.label..)) +
#   stat_regline_equation(label.y = 50, aes(label = ..rr.label..))+
# # Add correlation coefficient
#   ylim(0, 60)+
#   #xlim(0, 60)+
#   scale_color_manual(values=(pnw_palette("Lake", 3)))+
#   scale_fill_manual(values=(pnw_palette("Lake", 3)))+
#   labs(x = bquote('measured volumetric water content'~(cm^3/cm^3)),
#        y = "datalogger moisture %",
#        fill = "depth, cm", color = "depth, cm")+
#   #facet_grid(position~site, scales = "free_x") +
#   theme_er1()+
#   theme(legend.position = "bottom",
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank())


ggsave("output/moisturecorrelations_fig.png", plot = moisturecorrelations_fig, height = 5.5, width = 5)
