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

bulk_density_fig2 =
  bd_grav_cleaned %>% 
  na.omit() %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  ggplot()+
  geom_point(aes(y = volumetric_water_content_gcm3, x = soil_bulk_density_g_cm3, fill = Horizon), shape = c(21), 
             size = 3, alpha = 0.4)+
  labs(y = "volumetric soil water (g/g of dry soil)",
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

volumetricwater_fig=
  bd_grav_cleaned %>% 
  na.omit() %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(soil_material = factor(soil_material, levels = c("organic", "mineral"))) %>% 
  ggplot()+
  geom_point(aes(y = real_depth_cm, x = grav_water_gh20_per_gdrysoil, fill = soil_material), shape = c(21), 
             size = 3, alpha = 0.4)+
  labs(y = "depth, cm",
       x = "volumetric water content (g per g of dry soil)",
       fill = " ")+
  scale_y_reverse()+
  scale_fill_manual(values = (PNWColors::pnw_palette("Sunset2", 2)))+
  facet_grid(Site~Area)+
  theme_er1()+
  theme(legend.position = "top")

ggsave("output/soilmoisture_fig1.TIFF", plot = soilmoisture_fig1, height = 6, width = 5)

ggsave("output/volumetricwater_fig.TIFF", plot = volumetricwater_fig, height = 6, width = 5)
ggsave("output/volumetricwater_fig.png", plot = volumetricwater_fig, height = 6, width = 5)

combo_volumetricwater_bulkdensity = 
  volumetricwater_fig+bulk_density_fig3+ #combines the two plots
  plot_layout(guides = "collect") &
  theme(legend.position='bottom') # sets a common legend

combo_soilmoisture_bulkdensity = 
  soilmoisture_fig1+bulk_density_fig3+ #combines the two plots
  plot_layout(guides = "collect") &
  theme(legend.position='bottom') # sets a common legend


ggsave("output/combo_soilmoisture_bulkdensity.TIFF", plot = combo_soilmoisture_bulkdensity, height = 5, width = 8)
ggsave("output/combo_soilmoisture_bulkdensity.png", plot = combo_soilmoisture_bulkdensity, height = 5, width = 8)

ggsave("output/combo_volumetricwater_bulkdensity.TIFF", plot = combo_volumetricwater_bulkdensity, height = 5, width = 8)
ggsave("output/combo_volumetricwater_bulkdensity.png", plot = combo_volumetricwater_bulkdensity, height = 5, width = 8)

#General soil properties figure----

thaw_depths_cleaned_forspfig =
  thaw_depths_cleaned %>% 
  filter(Month_num == 'Aug') %>% 
  group_by(Area, Site) %>% 
  dplyr::summarise(thawmax = max(thaw_depth_cm),
                   thawmin = min(thaw_depth_cm),
                   thawavg = mean(thaw_depth_cm),
                   thawse = sd(thaw_depth_cm)/sqrt(n()))


bd_grav_cleaned_forspfig =
  bd_grav_cleaned %>% 
  group_by(Area, Site, soil_material) %>% 
  na.omit() %>% 
  dplyr::summarise(depth_avg = round(mean(real_depth_cm),2),
                   depth_se = round(sd(real_depth_cm)/sqrt(n()),2),
                   depth_forstack_avg = round(mean(Average_Depth_cm),2),
                   depth_forstack_se = round(sd(Average_Depth_cm)/sqrt(n()),2),
                   bd_avg = round(mean(soil_bulk_density_g_cm3),2),
                   bd_se = round(sd(soil_bulk_density_g_cm3)/sqrt(n()),2),
                   vwc_avg = round(mean(volumetric_water_content_gcm3),2),
                   vwc_se = round(sd(volumetric_water_content_gcm3)/sqrt(n()),2),
                   grav_avg = round(mean(grav_moist_perc),2),
                   grav_se = round(sd(grav_moist_perc)/sqrt(n()),2))




spfig =
  bd_grav_cleaned_forspfig %>% 
  left_join(thaw_depths_cleaned_forspfig) %>% 
  mutate(soil_material = factor(soil_material, levels = c("organic", "mineral"))) %>% 
  mutate(site_num = recode(Site, "Dry" = "1",
                           "Mesic" = "2",
                           "Hydric" = "3")) %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(site_num = as.numeric(site_num))
  
  

vol_fig_2 =
  spfig %>% 
  ggplot(aes(x = Site))+
  geom_point(aes(y = vwc_avg, fill = soil_material), shape = c(21), size = 4)+
  geom_errorbar(aes(ymin=vwc_avg-vwc_se, ymax=vwc_avg+vwc_se, color = soil_material), width=.2)+
  # geom_point(aes(y = bd_avg*100, fill = soil_material), shape = c(21), size = 3)+
  # scale_y_continuous(name = "volumetric water content, g/cm3 (square)",
  #                    sec.axis = sec_axis(~./100, name = "bulk density, g/cm3 (circle)"))+
  labs(fill = "", color = "",
       y = "volumetric water content, g/cm3")+
  scale_fill_manual(values = c("#6d6875", "#b5838d"))+
  scale_color_manual(values = c("#6d6875", "#b5838d"))+
  ylim(0, 0.6)+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(.~Area, scales="free") 

grav_bd =
spfig %>% 
  ggplot(aes(x = Site))+
  geom_col(aes(y = grav_avg, fill = soil_material), color = c("#3a86ff"), alpha = 0.7, position = "dodge", width = 0.4)+
  geom_point(aes(y = bd_avg*75), fill = c("#ffbd00"), shape = c(21), size = 4)+
  scale_y_continuous(name = "gravimetric water content, % (bar)",
                      sec.axis = sec_axis(~./75, name = "bulk density, g/cm3 (gold point)"))+
  labs(fill = "")+
  scale_fill_manual(values = c("#6d6875", "#b5838d"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(soil_material~Area) 

ggsave("figures_finalized/grav_bd.png", plot = grav_bd, width = 9, height = 5)

vwc_1 =
  spfig %>% 
  ggplot(aes(x = Site))+
  geom_col(aes(y = vwc_avg, fill = soil_material, color = soil_material), alpha = 0.7, position = position_dodge2(preserve = "single"), width = 0.4)+
  geom_errorbar(aes(ymin=vwc_avg-vwc_se, ymax=vwc_avg+vwc_se, color = soil_material), position = position_dodge2(preserve = "single"), width=.4)+
  #geom_point(aes(y = bd_avg/2), fill = c("#ffbd00"), shape = c(21), size = 4)+
  # scale_y_continuous(name = "volumetric water content, g/cm3 (bar)",
  #                    sec.axis = sec_axis(~.*2, name = "bulk density, g/cm3 (gold point)"))+
  labs(fill = "", color = "", y = "volumetric water content, g/cm3")+
  ylim(0, 0.6)+
  scale_fill_manual(values = c("#6d6875", "#b5838d"))+
  scale_color_manual(values = c("grey35", "grey35"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(.~Area) 

ggsave("figures_finalized/vwc_1.png", plot = vwc_1, width = 9, height = 5)
ggsave("figures_finalized/vol_fig_2.png", plot = vol_fig_2, width = 9, height = 5)

#####comprehensive figure----------

spfig2 =
  spfig %>% 
  dplyr::select(c(Area, Site, soil_material, depth_forstack_avg, bd_avg, bd_se))


library(tibble)

gglabel = tribble(
  ~soil_material, ~Area, ~x, ~y, ~label,
  "mineral", 'acidic tundra', 1, 10, '0.73 ± 0.23',        
  "organic", 'acidic tundra', 1, 5, '0.15 ± 0.04',        
  "mineral", 'acidic tundra', 3, 10, '0.8',
  "organic", 'acidic tundra', 3, 5, '0.11 ± 0.02',
  "mineral", 'acidic tundra', 2, 10, '1.06 ± 0.06',        
  "organic", 'acidic tundra', 2, 5, '0.09 ± 0.02',
  "organic", 'non-acidic tundra', 1, 5, '0.13 ± 0.03',        
  "organic", 'non-acidic tundra', 2, 5, '0.11 ± 0.02',
  "organic", 'non-acidic tundra', 3, 5, '0.12 ± 0.01',        
  "mineral", 'non-acidic tundra', 2, 10, '0.91 ± 0.07',
  
)




depths_fig =
spfig %>% 
  mutate(soil_material = factor(soil_material, levels = c("mineral", "organic"))) %>% 
  ggplot(aes(x = Site))+
  geom_col(aes(y = depth_forstack_avg, fill = soil_material), position = "stack", width = 0.7)+
  geom_point(aes(y = thawavg), fill = c("#f07167"), shape = c(21), size = 3)+
  # geom_text(data = gglabel, aes(x = x, y = y, label = label), color = 'white', size = 2)+
  scale_y_reverse()+
  labs(fill = "",
       y = "depth, cm",
       caption = "red point = average thaw depth in August (cm)")+
  scale_fill_manual(values = c("#b5838d", "#6d6875"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(.~Area, scales="free") 

ggsave("figures_finalized/depths_fig.png", plot = depths_fig, width = 7, height = 5)

depths_fig_bd =
  spfig %>% 
  mutate(soil_material = factor(soil_material, levels = c("mineral", "organic"))) %>% 
  ggplot(aes(x = Site))+
  geom_col(aes(y = depth_forstack_avg, fill = soil_material), position = "stack", width = 0.7)+
  geom_point(aes(y = thawavg), fill = c("#f07167"), shape = c(21), size = 3)+
  geom_text(data = gglabel, aes(x = x, y = y, label = label), color = 'white', size = 2)+
  scale_y_reverse()+
  labs(fill = "",
       y = "depth, cm",
       caption = "red point = average thaw depth in August (cm)")+
  scale_fill_manual(values = c("#b5838d", "#6d6875"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(.~Area, scales="free") 

ggsave("figures_finalized/depths_fig_bd.png", plot = depths_fig_bd, width = 7, height = 5)
