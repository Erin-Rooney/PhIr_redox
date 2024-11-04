#6 1 2022
#Data processing/ggplots

#load all packages


source("code/0-packages.R")

###write file

rhizon_meta_combine = read.csv("processed/rhizon_2021.csv")
sipper_data = read.csv("processed/sipper_2021.csv")
rhizon_meta_combine_2022 = read.csv("processed/metadata_rhizon_withdate_2022.csv")


rhizon_meta_combine_notransect_prefix =
  rhizon_meta_combine %>% 
  filter(Site != 'Transect') %>% 
  mutate(ICP = recode(ICP, "Fe _ug/mL" = "Fe_ug/mL",
                      "K _ug/mL" = "K_ug/mL")) %>% 
  mutate(Site = recode(Site, "Hydric" = "Wet")) %>% 
  mutate(month = factor(month, levels = c("june", "july", "august")),
         Site = factor(Site, levels = c("Dry", "Mesic", "Wet"))) %>% 
  mutate(ICP = recode(ICP, "Al_ug/mL" = "aluminum",
                      "Ca_ug/mL" = "calcium",
                      "Fe_ug/mL" = "iron",
                      "K_ug/mL" = "potassium",
                      "Mg_ug/mL" = "magnesium",
                      "Mn_ug/mL" = "manganese",
                      "Na_ug/mL" = "sodium",
                      "P_ug/mL" = "phosphorus")) %>% 
  mutate(concentration = recode(concentration, "<0.05" = "0.03",
                                "<0.1" = "0.05")) %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  mutate(area_site = paste(Area, Site, sep = "-")) 


rhizon_meta_combine_notransect_2022 =
  rhizon_meta_combine_2022 %>% 
  filter(site != 'Transect') %>% 
  mutate(date2 = date) %>% 
  separate(date2, sep = "-", into = c("year", "month", "day")) %>% 
  mutate(site = recode(site, "hydric" = "wet")) %>% 
  mutate(site = factor(site, levels = c("dry", "mesic", "wet"))) %>% 
  mutate(ICP = recode(ICP, "Al_ug/mL" = "aluminum",
                      "Ca_ug/mL" = "calcium",
                      "Fe_ug/mL" = "iron",
                      "K_ug/mL" = "potassium",
                      "Mg_ug/mL" = "magnesium",
                      "Mn_ug/mL" = "manganese",
                      "Na_ug/mL" = "sodium",
                      "P_ug/mL" = "phosphorus")) %>% 
  # mutate(concentration = recode(concentration, "<0.05" = "0.03",
  #                               "<0.1" = "0.05")) %>% 
  #mutate(concentration = as.numeric(concentration)) %>% 
  mutate(area_site = paste(area, site, sep = "-")) 

aluminumfix_rhizon =
  rhizon_meta_combine_notransect_prefix %>%
  filter(ICP == "aluminum") %>%
  filter(concentration < 5)

###aluminum outliers are removed. MAY WANT TO ADD BACK AND DEAL WITH IN A BETTER WAY

rhizon_meta_combine_notransect =
  rhizon_meta_combine_notransect_prefix %>% 
  filter(ICP != "aluminum") %>%
  bind_rows(aluminumfix_rhizon)
  


write.csv(rhizon_meta_combine_notransect, "processed/rhizon_long_notransect.csv")


rhizon_meta_combine_notransect_forelements =
  rhizon_meta_combine_notransect %>% 
  group_by(Area, Site, month, Betterdate, ICP) %>% 
  dplyr::summarise(mean = mean(concentration),
                   n = n(), 
                   sd = sd(concentration)/sqrt(n)) %>% 
  na.omit() %>% 
  mutate(combo = paste(Area, "-", Site)) %>% 
  mutate(Area = as.factor(Area)) %>% 
  mutate(month = factor(month, levels = c("june", "july", "august")),
         Site = factor(Site, levels = c("Dry", "Mesic", "Wet"))) %>% 
  na.omit() %>% 
  mutate(area_site = paste(Area, Site, sep = "-"))
  # separate(Betterdate, sep = "-", into =c('year', 'month_num', 'day')) %>%
  # mutate(date2 = as.Date(paste0(month_num, "-", day, "-", year))) 
  # # mutate(date2 = factor(date2, levels = c("june-24", "june-28", "july-06", "july-07",
  # #                                         "july-14", "july-24", "july-30", "july-31", "august-07",
  # #                                         "august-10"))) 
  # 
  #        


rhizon_meta_combine_notransect_forelements_2022 =
  rhizon_meta_combine_notransect_2022 %>% 
  group_by(area, site, month, date, ICP) %>% 
  dplyr::summarise(mean = round(mean(concentration_undo_dilution),3),
                   n = n(), 
                   se = round(sd(concentration_undo_dilution)/sqrt(n),3)) %>% 
  na.omit() %>% 
  mutate(combo = paste(area, "-", site)) %>% 
  mutate(area = as.factor(area)) %>% 
  mutate(site = recode(site, "hydric" = "wet")) %>% 
  mutate(site = factor(site, levels = c("dry", "mesic", "wet"))) %>% 
  na.omit() %>% 
  mutate(area_site = paste(area, site, sep = "-")) %>% 
  mutate(ICP = recode(ICP, "Ca_ug_ml" = "calcium",
  "Fe_ug_ml" = "iron",
  "Al_ug_ml" = "aluminum",
  "P_ug_ml" = "phosphorus",
  "Na_ug_ml" = "sodium",
  "K_ug_ml" = "potassium",
  "Mg_ug_ml" = "magnesium",
  "Mn_ug_ml" = "manganese",)) %>%
  mutate(area = recode(area, "west" = "non-acidic tundra",
                       "east" = "acidic tundra")) %>% 
  mutate(area = factor(area, levels = c("non-acidic tundra", "acidic tundra")))
# separate(Betterdate, sep = "-", into =c('year', 'month_num', 'day')) %>%
# mutate(date2 = as.Date(paste0(month_num, "-", day, "-", year))) 
# # mutate(date2 = factor(date2, levels = c("june-24", "june-28", "july-06", "july-07",
# #                                         "july-14", "july-24", "july-30", "july-31", "august-07",
# #                                         "august-10"))) 
# 
#        


########## Manuscript figures

Caline_rhizon_fig =
  rhizon_meta_combine_notransect_forelements %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(ICP %in% c("calcium")) %>% 
  # mutate(ICP = recode(ICP, "calcium" = "calcium μg/mL",
  #                     "iron" = "iron μg/mL",
  #                     "aluminum" = "aluminum μg/mL",
  #                     "phosphorus" = "phosphorus μg/mL")) %>%   
  ggplot(aes(x = as.Date(Betterdate), y = mean, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.75, alpha = 0.3, orientation = "x")+
  geom_point(aes(group = area_site, shape = Area), size = 4, alpha = 0.75)+
  scale_color_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  scale_fill_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  ylim(0, 50)+
  labs(color = "Moisture",
       linetype = "Acidity",
       y = "Ca μg/mL",
       x = " ")+
  scale_x_date(date_labels = "%b-%d")+
  facet_grid(. ~ ICP, scales = "free_y") +
  theme_er1()+
  theme(
    #axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), 
    legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.x = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))


Feline_rhizon_fig =
  rhizon_meta_combine_notransect_forelements %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(ICP %in% c("iron")) %>% 
  ggplot(aes(x = as.Date(Betterdate), y = mean, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.75, alpha = 0.3,  orientation = "x")+
  geom_point(aes(group = area_site, shape = Area), size = 4, alpha = 0.75)+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_color_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  scale_fill_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  ylim(0, 60)+
  labs(color = "Moisture",
       linetype = "Acidity",
       y = "Fe μg/mL",
       x = " ")+
    scale_x_date(date_labels = "%b-%d")+
    facet_grid(. ~ ICP, scales = "free_y") +
    theme_er1()+
    theme(
      #axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), 
      legend.position = "none",
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.x = element_blank(),
      panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))
  


Alline_rhizon_fig =
  rhizon_meta_combine_notransect_forelements %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(ICP %in% c("aluminum")) %>% 
  # mutate(ICP = recode(ICP, "calcium" = "calcium μg/mL",
  #                     "iron" = "iron μg/mL",
  #                     "aluminum" = "aluminum μg/mL",
  #                     "phosphorus" = "phosphorus μg/mL")) %>%   
  ggplot(aes(x = as.Date(Betterdate), y = mean, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.75, alpha = 0.3, orientation = "x")+
  geom_point(aes(group = area_site, shape = Area), size = 4, alpha = 0.75)+
  ylim(0, 1)+
  scale_color_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  scale_fill_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  labs(color = "Moisture",
       linetype = "Acidity",
       y = "Al μg/mL",
       x = " ")+
  scale_x_date(date_labels = "%b-%d")+
  facet_grid(. ~ ICP, scales = "free_y") +
  theme_er1()+
  theme(
    #axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), 
    legend.position = "none",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.x = element_blank(),
    panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))


Pline_rhizon_fig =
  rhizon_meta_combine_notransect_forelements %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(ICP %in% c("phosphorus")) %>% 
  ggplot(aes(x = as.Date(Betterdate), y = mean, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.75, alpha = 0.3, orientation = "x")+
  geom_point(aes(group = area_site, shape = Area), size = 4, alpha = 0.75)+
  ylim(0.05, 0.1)+
  scale_color_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  scale_fill_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  labs(color = "Moisture",
       linetype = "Acidity",
       y = "P μg/mL",
       x = " ")+
  scale_x_date(date_labels = "%b-%d")+
  facet_grid(. ~ ICP, scales = "free_y") +
  theme_er1()+
  theme(
    #axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), 
    legend.position = "none",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.x = element_blank(),
    panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))



LEGEND =
  rhizon_meta_combine_notransect_forelements %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(ICP %in% c("phosphorus")) %>% 
  ggplot(aes(x = as.Date(Betterdate), y = mean, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.75, alpha = 0.3, orientation = "x")+
  geom_point(aes(group = area_site, shape = Area), size = 4, alpha = 0.75)+
  ylim(0.05, 0.1)+
  scale_color_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  scale_fill_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  labs(color = "Moisture",
       linetype = "Acidity",
       y = "P μg/mL",
       x = " ")+
  scale_x_date(date_labels = "%b-%d")+
  facet_grid(. ~ ICP, scales = "free_y") +
  theme_er1()+
  guides(color = guide_legend(nrow=3, byrow = TRUE),
         shape = guide_legend(nrow=2, byrow = TRUE),
         linetype = guide_legend(nrow=2, byrow = TRUE))+
  theme(
    #axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), 
    legend.position = "right",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))


library(patchwork)

all_line_rhizon_fig = Caline_rhizon_fig / Feline_rhizon_fig / Alline_rhizon_fig / Pline_rhizon_fig +  plot_layout(guides = "collect")

ggsave("output/all_line_rhizon_fig.png", plot = all_line_rhizon_fig, width = 5, height = 10)
ggsave("output/LEGEND_rhizons.png", plot = LEGEND, width = 9, height = 9)



##### 2022 !!!!!-----


Caline_rhizon_fig2022 =
  rhizon_meta_combine_notransect_forelements_2022 %>%
  filter(ICP %in% c("calcium")) %>% 
  ggplot(aes(x = as.Date(date), y = mean, color = site)) +
  geom_line(aes(group = area_site, linetype = area), size = 0.75, alpha = 0.3, orientation = "x")+
  geom_point(aes(group = area_site, shape = area), size = 4, alpha = 0.75)+
   scale_color_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  scale_fill_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  ylim(0,50)+
  labs(color = "Moisture",
       linetype = "Acidity",
       y = "Ca μg/mL",
       x = " ")+
  scale_x_date(date_labels = "%b-%d")+
  facet_grid(. ~ ICP, scales = "free_y") +
  theme_er1()+
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.x = element_blank(),
    panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))


Feline_rhizon_fig2022 =
  rhizon_meta_combine_notransect_forelements_2022 %>%
  filter(ICP %in% c("iron")) %>% 
  ggplot(aes(x = as.Date(date), y = mean, color = site)) +
  geom_line(aes(group = area_site, linetype = area), size = 0.75, alpha = 0.3, orientation = "x")+
  geom_point(aes(group = area_site, shape = area), size = 4, alpha = 0.75)+
   scale_color_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  scale_fill_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  ylim(0, 60)+
  labs(color = "Moisture",
       linetype = "Acidity",
       y = "Fe μg/mL",
       x = " ")+
  scale_x_date(date_labels = "%b-%d")+
  facet_grid(. ~ ICP, scales = "free_y") +
  theme_er1()+
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.x = element_blank(),
    panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))





Alline_rhizon_fig2022 =
  rhizon_meta_combine_notransect_forelements_2022 %>%
  filter(ICP %in% c("aluminum")) %>% 
  ggplot(aes(x = as.Date(date), y = mean, color = site)) +
  geom_line(aes(group = area_site, linetype = area), size = 0.75, alpha = 0.3, orientation = "x")+
  geom_point(aes(group = area_site, shape = area), size = 4, alpha = 0.75)+
   scale_color_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  scale_fill_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  ylim(0, 1)+
  labs(color = "Moisture",
       linetype = "Acidity",
       y = "Al μg/mL",
       x = " ")+
  scale_x_date(date_labels = "%b-%d")+
  facet_grid(. ~ ICP, scales = "free_y") +
  theme_er1()+
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.x = element_blank(),
    panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))



Pline_rhizon_fig2022 =
  rhizon_meta_combine_notransect_forelements_2022 %>%
   filter(ICP %in% c("phosphorus")) %>% 
  ggplot(aes(x = as.Date(date), y = mean, color = site)) +
  geom_line(aes(group = area_site, linetype = area), size = 0.75, alpha = 0.3, orientation = "x")+
  geom_point(aes(group = area_site, shape = area), size = 4, alpha = 0.75)+
   scale_color_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  scale_fill_manual(values = c("#bc4749", "#35a55f", "#0582ca", "#bc4749", "#35a55f", "#0582ca"))+
  ylim(0.05, 0.1)+
  labs(color = " ",
       linetype = " ",
       shape = " ",
       y = "P μg/mL",
       x = " ")+
  scale_x_date(date_labels = "%b-%d")+
  facet_grid(. ~ ICP, scales = "free_y") +
  theme_er1()+
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.x = element_blank(),
    panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))


library(patchwork)

all_line_rhizon_fig2022 = Caline_rhizon_fig2022 / Feline_rhizon_fig2022 / Alline_rhizon_fig2022 / Pline_rhizon_fig2022 +  plot_layout(guides = "collect")

ggsave("output/all_line_rhizon_fig2022.png", plot = all_line_rhizon_fig2022, width = 5, height = 10)


#############

# 
# 
# 
# Mnline_rhizon_fig =
#   rhizon_meta_combine_notransect_forelements %>%
#   mutate(Area = recode(Area, "West" = "non-acidic tundra",
#                        "East" = "acidic tundra")) %>% 
#   mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   filter(ICP %in% c("manganese")) %>% 
#   # mutate(ICP = recode(ICP, "calcium" = "calcium μg/mL",
#   #                     "iron" = "iron μg/mL",
#   #                     "aluminum" = "aluminum μg/mL",
#   #                     "phosphorus" = "phosphorus μg/mL")) %>%   
#   ggplot(aes(x = Betterdate, y = mean, color = Site)) +
#   geom_line(aes(group = area_site, linetype = Area), size = 0.7, orientation = "x")+
#   geom_point(aes(group = area_site, shape = Area), size = 3.5)+
#   # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
#   # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
#   scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
#   scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
#   labs(color = "Moisture",
#        linetype = "Acidity",
#        y = "Concentration μg/mL",
#        x = " ")+
#   facet_grid(. ~ ICP, scales = "free_y") +
#   theme_er1()+
#   theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "none",
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# 
# Kline_rhizon_fig =
#   rhizon_meta_combine_notransect_forelements %>%
#   mutate(Area = recode(Area, "West" = "non-acidic tundra",
#                        "East" = "acidic tundra")) %>% 
#   mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   filter(ICP %in% c("potassium")) %>% 
#   # mutate(ICP = recode(ICP, "calcium" = "calcium μg/mL",
#   #                     "iron" = "iron μg/mL",
#   #                     "aluminum" = "aluminum μg/mL",
#   #                     "phosphorus" = "phosphorus μg/mL")) %>%   
#   ggplot(aes(x = Betterdate, y = mean, color = Site)) +
#   geom_line(aes(group = area_site, linetype = Area), size = 0.7, orientation = "x")+
#   geom_point(aes(group = area_site, shape = Area), size = 3.5)+
#   # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
#   # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
#   scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
#   scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
#   labs(color = "Moisture",
#        linetype = "Acidity",
#        y = "Concentration μg/mL",
#        x = " ")+
#   facet_grid(. ~ ICP, scales = "free_y") +
#   theme_er1()+
#   theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "none",
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# 
# Mgline_rhizon_fig =
#   rhizon_meta_combine_notransect_forelements %>%
#   mutate(Area = recode(Area, "West" = "non-acidic tundra",
#                        "East" = "acidic tundra")) %>% 
#   mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   filter(ICP %in% c("magnesium")) %>% 
#   # mutate(ICP = recode(ICP, "calcium" = "calcium μg/mL",
#   #                     "iron" = "iron μg/mL",
#   #                     "aluminum" = "aluminum μg/mL",
#   #                     "phosphorus" = "phosphorus μg/mL")) %>%   
#   ggplot(aes(x = Betterdate, y = mean, color = Site)) +
#   geom_line(aes(group = area_site, linetype = Area), size = 0.7, orientation = "x")+
#   geom_point(aes(group = area_site, shape = Area), size = 3.5)+
#   # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
#   # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
#   scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
#   scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
#   labs(color = "Moisture",
#        linetype = "Acidity",
#        y = "Concentration μg/mL",
#        x = " ")+
#   facet_grid(. ~ ICP, scales = "free_y") +
#   theme_er1()+
#   theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "none",
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# 
# Naline_rhizon_fig =
#   rhizon_meta_combine_notransect_forelements %>%
#   mutate(Area = recode(Area, "West" = "non-acidic tundra",
#                        "East" = "acidic tundra")) %>% 
#   mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   filter(ICP %in% c("sodium")) %>% 
#   # mutate(ICP = recode(ICP, "calcium" = "calcium μg/mL",
#   #                     "iron" = "iron μg/mL",
#   #                     "aluminum" = "aluminum μg/mL",
#   #                     "phosphorus" = "phosphorus μg/mL")) %>%   
#   ggplot(aes(x = Betterdate, y = mean, color = Site)) +
#   geom_line(aes(group = area_site, linetype = Area), size = 0.7, orientation = "x")+
#   geom_point(aes(group = area_site, shape = Area), size = 3.5)+
#   # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
#   # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
#   scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
#   scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
#   labs(color = "Moisture",
#        linetype = "Acidity",
#        y = "Concentration μg/mL",
#        x = " ")+
#   facet_grid(. ~ ICP, scales = "free_y") +
#   theme_er1()+
#   theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "right",
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# library(patchwork)
# 
# all_line_rhizon_fig2 = Mnline_rhizon_fig + Kline_rhizon_fig + Mgline_rhizon_fig + Naline_rhizon_fig +  plot_layout(guides = "collect")
# 
# ggsave("output/all_line_rhizon_fig2.png", plot = all_line_rhizon_fig2, width = 10, height = 5)
# 













west_rhizon_fig = 
  rhizon_meta_combine_notransect_forelements %>%
  filter(ICP %in% c("iron", "phosphorus", "manganese") & Area == "West") %>% 
  mutate(ICP = recode(ICP, "phosphorus" = "phosphorus μg/mL",
                      "iron" = "iron μg/mL",
                      "manganese" = "manganese μg/mL")) %>% 
  ggplot(aes(x = Betterdate, y = mean, color = Site, fill = Site)) +
  #geom_point(size = 3, alpha = 0.7)+
  geom_col(position = 'dodge', width = 0.7, alpha = 0.5)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black")+
  labs(x = "date", y = "",
       subtitle = "Non-Acidic Tundra")+
  scale_color_manual(values = c("#9a031e", "#40916c", "#118ab2"))+
  scale_fill_manual(values = c("#9a031e", "#40916c", "#118ab2"))+
  # scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  # scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  theme_er1()+
  # theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(ICP ~ Site, switch = "y", scale = "free_y")+
  theme(legend.position = "NONE", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.placement = "outside")+
  NULL

ggsave("output/west_rhizon_fig.png", plot = west_rhizon_fig, width = 7, height = 9)
ggsave("output/east_rhizon_fig.png", plot = east_rhizon_fig, width = 7, height = 9)



east_rhizon_fig_cations = 
  rhizon_meta_combine_notransect_forelements %>%
  filter(ICP %in% c("calcium", "magnesium", "sodium", "potassium") & Area == "East") %>%   
  mutate(ICP = recode(ICP, "calcium" = "calcium ug/mL",
                      "magnesium" = "magnesium ug/mL",
                      "sodium" = "sodium ug/mL",
                      "potassium" = "potassium ug/mL")) %>% 
  ggplot(aes(x = Betterdate, y = mean, color = Site, fill = Site)) +
  #geom_point(size = 3, alpha = 0.7)+
  geom_col(position = 'dodge', width = 0.7, alpha = 0.5)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black")+
  labs(x = "date", y = "",
       subtitle = "Acidic Tundra")+
  #scale_x_date(date_labels = "%b-%d")+
  scale_color_manual(values = c("#9a031e", "#40916c", "#118ab2"))+
  scale_fill_manual(values = c("#9a031e", "#40916c", "#118ab2"))+
  # scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  # scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  theme_er1()+
  # theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(ICP ~ Site, switch = "y", scale = "free_y")+
  theme(legend.position = "NONE", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.placement = "outside")+
  NULL

west_rhizon_fig_cations = 
  rhizon_meta_combine_notransect_forelements %>%
  filter(ICP %in% c("calcium", "magnesium", "sodium", "potassium") & Area == "West") %>%   
  mutate(ICP = recode(ICP, "calcium" = "calcium ug/mL",
                      "magnesium" = "magnesium ug/mL",
                      "sodium" = "sodium ug/mL",
                      "potassium" = "potassium ug/mL")) %>% 
  ggplot(aes(x = Betterdate, y = mean, color = Site, fill = Site)) +
  #geom_point(size = 3, alpha = 0.7)+
  geom_col(position = 'dodge', width = 0.7, alpha = 0.5)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black")+
  labs(x = "date", y = "",
       subtitle = "Non-Acidic Tundra")+
  scale_color_manual(values = c("#9a031e", "#40916c", "#118ab2"))+
  scale_fill_manual(values = c("#9a031e", "#40916c", "#118ab2"))+
  # scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  # scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  theme_er1()+
  # theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(ICP ~ Site, switch = "y", scale = "free_y")+
  theme(legend.position = "NONE", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.placement = "outside")+
  NULL

