#6 1 2022
#Data processing/ggplots

#load all packages


source("code/0-packages.R")

###write file

rhizon_meta_combine = read.csv("processed/rhizon_2021.csv")
sipper_data = read.csv("processed/sipper_2021.csv")
rhizon_meta_combine_2022 = read.csv("processed/metadata_rhizon_withdate_2022.csv")

#there is weird double data
#all doubles look identical except for the blank
#grouping, will check with Beth/Sumant later
#delete and revise once checking is complete

rhizon_meta_combine_notransect_prefix =
  rhizon_meta_combine %>% 
  filter(Site != 'Transect') %>% 
  mutate(ICP = recode(ICP, "Fe _ug/mL" = "Fe_ug/mL",
                      "K _ug/mL" = "K_ug/mL")) %>% 
  mutate(month = factor(month, levels = c("june", "july", "august")),
         Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
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
  mutate(site = factor(site, levels = c("dry", "mesic", "hydric"))) %>% 
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


##metadata needed for sipper data
# 
# west_rhizon_month = 
#   rhizon_meta_combine_notransect %>% 
#   filter(Area == 'West') %>% 
#   ggplot(aes(x = Site, y = as.numeric(concentration), fill = month))+
#   #geom_point(size = 2.5, alpha = 0.8)+
#   geom_boxplot(alpha = 0.3)+
#   #geom_line(orientation = "x", group = 'Plot')+
#   labs(x = "",
#        y = 'concentration ug/mL')+
#   scale_fill_manual(values = natparks.pals(name = "SmokyMtns", 3))+
#   #scale_color_manual(values = natparks.pals(name = "SmokyMtns", 3))+
#   # scale_color_manual(values = rev(PNWColors::pnw_palette("Shuksan2", 2)))+
#   # scale_fill_manual(values = rev(PNWColors::pnw_palette("Shuksan2", 2)))+
#   facet_wrap(ICP~., scales = "free", ncol = 4)+
#   theme_er1()+
#   theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))
# 
# east_rhizon_month = 
#   rhizon_meta_combine_notransect %>% 
#   filter(Area == 'East') %>% 
#   ggplot(aes(x = Site, y = as.numeric(concentration), fill = month))+
#   #geom_point(size = 2.5, alpha = 0.8)+
#   geom_boxplot(alpha = 0.3, width = 0.6)+
#   #geom_line(orientation = "x", group = 'Plot')+
#   labs(x = "",
#        y = 'concentration ug/mL')+
#   scale_fill_manual(values = natparks.pals(name = "SmokyMtns", 3))+
#   #scale_color_manual(values = natparks.pals(name = "SmokyMtns", 3))+
#   # scale_color_manual(values = rev(PNWColors::pnw_palette("Shuksan2", 2)))+
#   # scale_fill_manual(values = rev(PNWColors::pnw_palette("Shuksan2", 2)))+
#   facet_wrap(ICP~., scales = "free", ncol = 4)+
#   theme_er1()+
#   theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))

# ggsave("output/east_2021_rhizonsmonth.tiff", plot = east_rhizon_month, height = 5.75, width = 10)
# ggsave("output/west_2021_rhizonsmonth.tiff", plot = west_rhizon_month, height = 5.75, width = 10)
# 


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
         Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
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
  mutate(site = factor(site, levels = c("dry", "mesic", "hydric"))) %>% 
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

# 
# aluminum_fig = 
#   rhizon_meta_combine_notransect_forelements %>%
#   filter(ICP == "aluminum") %>% 
#   ggplot(aes(x = Betterdate, y = mean, color = Site, fill = Site)) +
#   #geom_point(size = 3, alpha = 0.7)+
#   geom_col(position = 'dodge', width = 0.7)+
#   geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
#                 position=position_dodge(.9), color = "black")+
#   scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
#   scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
#   labs(y = "aluminum, ug/mL")+
#   theme_er1()+
#   theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
#   facet_grid(Area ~ .)
#   
# 
# 
# phosphorus_fig = 
#   rhizon_meta_combine_notransect_forelements %>%
#   filter(ICP == "phosphorus") %>% 
#   ggplot(aes(x = Betterdate, y = mean, color = Site, fill = Site)) +
#   #geom_point(size = 3, alpha = 0.7)+
#   geom_col(position = 'dodge', width = 0.7)+
#   geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
#                 position=position_dodge(.9), color = "black")+
#   labs(y = "phosphorus, ug/mL")+
#   scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
#   scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
#   theme_er1()+
#   theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
#   facet_grid(Area ~ Site)
# 
# redoxsensitive_fig =
#   rhizon_meta_combine_notransect_forelements %>%
#   mutate(Area = recode(Area, "West" = "non-acidic tundra",
#                        "East" = "acidic tundra")) %>% 
#     mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   filter(ICP %in% c("iron", "phosphorus", "manganese", "aluminum")) %>% 
#   mutate(ICP = recode(ICP, "phosphorus" = "phosphorus μg/mL",
#                       "iron" = "iron μg/mL",
#                       "manganese" = "manganese μg/mL",
#                       "aluminum" = "aluminum μg/mL")) %>% 
#   ggplot(aes(x = Betterdate, y = mean, color = Site, fill = Site)) +
#   #geom_point(size = 3, alpha = 0.7)+
#   geom_col(position = position_dodge2(width = 0.9, preserve = "single"), alpha = 0.8, color = "black")+
#   geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
#                 position=position_dodge(.9), color = "black")+
#   labs(x = " ", y = "", color = "", fill = "")+
#   # scale_x_date(date_labels = "%b-%d")+
#     # scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
#     # scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
#   scale_color_manual(values = c("#bc4749", "#35a55f", "#0582ca"))+
#   scale_fill_manual(values = c("#bc4749", "#35a55f", "#0582ca"))+
#   # scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
#   # scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
#   theme_er1()+
#   # theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
#   facet_grid(ICP ~ Area, switch = "y", scale = "free")+
#   theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, size = 11),
#         strip.placement = "outside", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank())+
#   NULL
# 
# 
# cations_fig =
#   rhizon_meta_combine_notransect_forelements %>%
#   mutate(Area = recode(Area, "West" = "non-acidic tundra",
#                        "East" = "acidic tundra")) %>% 
#   mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   filter(ICP %in% c("calcium", "sodium", "magnesium", "potassium")) %>% 
#   mutate(ICP = recode(ICP, "calcium" = "calcium μg/mL",
#                       "sodium" = "sodium μg/mL",
#                       "magnesium" = "magnesium μg/mL",
#                       "potassium" = "potassium μg/mL")) %>% 
#   ggplot(aes(x = Betterdate, y = mean, fill = Site)) +
#   #geom_point(size = 3, alpha = 0.7)+
#   geom_col(position = position_dodge2(width = 0.9, preserve = "single"), alpha = 0.8, color = "black")+
#   geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
#                 position=position_dodge(.9), color = "black")+
#   labs(x = " ", y = "", color = "", fill = "")+
#   # scale_x_date(date_labels = "%b-%d")+
#   scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
#   scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
#   # scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
#   # scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
#   theme_er1()+
#   # theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
#   facet_grid(ICP ~ Area, switch = "y", scale = "free")+
#   theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, size = 11),
#         strip.placement = "outside", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank())+
#   NULL
# 
# cations_fig_legend =
#   rhizon_meta_combine_notransect_forelements %>%
#   mutate(Area = recode(Area, "West" = "non-acidic tundra",
#                        "East" = "acidic tundra")) %>% 
#   mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   filter(ICP %in% c("calcium", "sodium", "magnesium", "potassium")) %>% 
#   mutate(ICP = recode(ICP, "calcium" = "calcium μg/mL",
#                       "sodium" = "sodium μg/mL",
#                       "magnesium" = "magnesium μg/mL",
#                       "potassium" = "potassium μg/mL")) %>% 
#   ggplot(aes(x = Betterdate, y = mean, fill = Site)) +
#   #geom_point(size = 3, alpha = 0.7)+
#   geom_col(position = position_dodge2(width = 0.9, preserve = "single"), alpha = 0.8, color = "black")+
#   geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
#                 position=position_dodge(0.9), color = "black")+
#   labs(x = " ", y = "", color = "", fill = "")+
#   # scale_x_date(date_labels = "%b-%d")+
#  # scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
#   scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
#   # scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
#   # scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
#   theme_er1()+
#   # theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
#   facet_grid(ICP ~ Area, switch = "y", scale = "free")+
#   theme(legend.position = "right", axis.text.x = element_text(angle = 45, vjust = 0.5, size = 9),
#         strip.placement = "outside", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank())+
#   NULL

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

Pline_rhizon_fig_non_acidic =
  rhizon_meta_combine_notransect_forelements %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(ICP %in% c("phosphorus") & Area %in% c("non-acidic tundra") & Site %in% c("Hydric")) %>% 
  ggplot(aes(x = as.Date(Betterdate), y = mean, color = Site)) +
  geom_line(aes(group = area_site), size = 0.75, alpha = 0.3, orientation = "x")+
  geom_point(aes(group = area_site), size = 4, alpha = 0.75)+
  ylim(0.05, 0.1)+
  scale_color_manual(values = c("#0582ca"))+
  scale_fill_manual(values = c("#0582ca"))+
  labs(color = "Moisture",
       y = "dissolved P μg/mL",
       x = " ")+
  scale_x_date(date_labels = "%b-%d", date_breaks = "1 week")+
  facet_grid(ICP ~ Area, scales = "free_y") +
  theme_er1()+
  theme(legend.position = c(.35, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",legend.title = element_text(face = "bold"),
        legend.margin = margin(6, 6, 6, 6), 
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 14), 
    axis.title = element_text(size = 16),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.y = element_blank(),
    strip.text.x = element_text(size = 16), panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))

ggsave("output/Pline_rhizon_fig_non_acidic.png", Pline_rhizon_fig_non_acidic, height = 4, width = 5)

Pline_rhizon_fig_acidic =
  rhizon_meta_combine_notransect_forelements %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(ICP %in% c("phosphorus") & Area %in% c("acidic tundra") & Site %in% c("Hydric")) %>% 
  ggplot(aes(x = as.Date(Betterdate), y = mean, color = Site)) +
  geom_line(aes(group = area_site), size = 0.75, alpha = 0.3, orientation = "x")+
  geom_point(aes(group = area_site), size = 4, alpha = 0.75)+
  ylim(0.05, 0.1)+
  scale_color_manual(values = c("#0582ca"))+
  scale_fill_manual(values = c("#0582ca"))+
  labs(color = "Moisture",
       y = "dissolved P μg/mL",
       x = " ")+
  scale_x_date(date_labels = "%b-%d", date_breaks = "1 week")+
  facet_grid(ICP ~ Area, scales = "free_y") +
  theme_er1()+
  theme(legend.position = c(.55, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",legend.title = element_text(face = "bold"),
        legend.margin = margin(6, 6, 6, 6), 
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.y = element_blank(),
        strip.text.x = element_text(size = 16), panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))

ggsave("output/Pline_rhizon_fig_acidic.png", Pline_rhizon_fig_acidic, height = 4, width = 5)

Feline_rhizon_fig_nonacidic =
  rhizon_meta_combine_notransect_forelements %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(ICP %in% c("iron") & Area %in% c("non-acidic tundra") & Site %in% c("Hydric")) %>%  
  ggplot(aes(x = as.Date(Betterdate), y = mean, color = Site)) +
  geom_line(aes(group = area_site), size = 0.75, alpha = 0.3,  orientation = "x")+
  geom_point(aes(group = area_site), size = 4, alpha = 0.75)+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_color_manual(values = c("#0582ca"))+
  scale_fill_manual(values = c("#0582ca"))+
  ylim(0, 30)+
  labs(color = "Moisture",
       y = "dissolved Fe μg/mL",
       x = " ")+
  scale_x_date(date_labels = "%b-%d", date_breaks = "1 week")+
  facet_grid(ICP ~ Area, scales = "free_y") +
  theme_er1()+
    theme(legend.position = c(.95, .75),
          legend.justification = c("right", "top"),
          legend.box.just = "right",legend.title = element_text(face = "bold"),
          legend.margin = margin(6, 6, 6, 6), 
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1), 
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 16),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.y = element_blank(),
          strip.text.x = element_text(size = 16), panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))

ggsave("output/Feline_rhizon_fig_nonacidic.png", Feline_rhizon_fig_nonacidic, height = 4, width = 5)

Feline_rhizon_fig_acidic =
  rhizon_meta_combine_notransect_forelements %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(ICP %in% c("iron") & Area %in% c("acidic tundra") & Site %in% c("Hydric")) %>% 
  ggplot(aes(x = as.Date(Betterdate), y = mean, color = Site)) +
  geom_line(aes(group = area_site), size = 0.75, alpha = 0.3,  orientation = "x")+
  geom_point(aes(group = area_site), size = 4, alpha = 0.75)+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_color_manual(values = c("#0582ca"))+
  scale_fill_manual(values = c("#0582ca"))+
  ylim(0, 30)+
  labs(color = "Moisture",
       y = "dissolved Fe μg/mL",
       x = " ")+
  scale_x_date(date_labels = "%b-%d", date_breaks = "1 week")+
  facet_grid(ICP ~ Area, scales = "free_y") +
  theme_er1()+
  theme(legend.position = c(.95, .65),
        legend.justification = c("right", "top"),
        legend.box.just = "right",legend.title = element_text(face = "bold"),
        legend.margin = margin(6, 6, 6, 6), 
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text.y = element_blank(),
        strip.text.x = element_text(size = 16), panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))

ggsave("output/Feline_rhizon_fig_acidic.png", Feline_rhizon_fig_acidic, height = 4, width = 5)




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

ggsave("figures_finalized/all_line_rhizon_fig.png", plot = all_line_rhizon_fig, width = 5, height = 10)
ggsave("figures_finalized/LEGEND_rhizons.png", plot = LEGEND, width = 9, height = 9)

ggsave("figures_finalized/redoxsensitive_fig.png", plot = redoxsensitive_fig, width = 9, height = 9)
ggsave("figures_finalized/cations_fig.png", plot = cations_fig, width = 9, height = 9)
ggsave("figures_finalized/cations_fig_legend.png", plot = cations_fig_legend, width = 8, height = 9)

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

ggsave("figures_finalized/all_line_rhizon_fig2022.png", plot = all_line_rhizon_fig2022, width = 5, height = 10)


#############




Mnline_rhizon_fig =
  rhizon_meta_combine_notransect_forelements %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(ICP %in% c("manganese")) %>% 
  # mutate(ICP = recode(ICP, "calcium" = "calcium μg/mL",
  #                     "iron" = "iron μg/mL",
  #                     "aluminum" = "aluminum μg/mL",
  #                     "phosphorus" = "phosphorus μg/mL")) %>%   
  ggplot(aes(x = Betterdate, y = mean, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.7, orientation = "x")+
  geom_point(aes(group = area_site, shape = Area), size = 3.5)+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  labs(color = "Moisture",
       linetype = "Acidity",
       y = "Concentration μg/mL",
       x = " ")+
  facet_grid(. ~ ICP, scales = "free_y") +
  theme_er1()+
  theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


Kline_rhizon_fig =
  rhizon_meta_combine_notransect_forelements %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(ICP %in% c("potassium")) %>% 
  # mutate(ICP = recode(ICP, "calcium" = "calcium μg/mL",
  #                     "iron" = "iron μg/mL",
  #                     "aluminum" = "aluminum μg/mL",
  #                     "phosphorus" = "phosphorus μg/mL")) %>%   
  ggplot(aes(x = Betterdate, y = mean, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.7, orientation = "x")+
  geom_point(aes(group = area_site, shape = Area), size = 3.5)+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  labs(color = "Moisture",
       linetype = "Acidity",
       y = "Concentration μg/mL",
       x = " ")+
  facet_grid(. ~ ICP, scales = "free_y") +
  theme_er1()+
  theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


Mgline_rhizon_fig =
  rhizon_meta_combine_notransect_forelements %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(ICP %in% c("magnesium")) %>% 
  # mutate(ICP = recode(ICP, "calcium" = "calcium μg/mL",
  #                     "iron" = "iron μg/mL",
  #                     "aluminum" = "aluminum μg/mL",
  #                     "phosphorus" = "phosphorus μg/mL")) %>%   
  ggplot(aes(x = Betterdate, y = mean, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.7, orientation = "x")+
  geom_point(aes(group = area_site, shape = Area), size = 3.5)+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  labs(color = "Moisture",
       linetype = "Acidity",
       y = "Concentration μg/mL",
       x = " ")+
  facet_grid(. ~ ICP, scales = "free_y") +
  theme_er1()+
  theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


Naline_rhizon_fig =
  rhizon_meta_combine_notransect_forelements %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(ICP %in% c("sodium")) %>% 
  # mutate(ICP = recode(ICP, "calcium" = "calcium μg/mL",
  #                     "iron" = "iron μg/mL",
  #                     "aluminum" = "aluminum μg/mL",
  #                     "phosphorus" = "phosphorus μg/mL")) %>%   
  ggplot(aes(x = Betterdate, y = mean, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.7, orientation = "x")+
  geom_point(aes(group = area_site, shape = Area), size = 3.5)+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  labs(color = "Moisture",
       linetype = "Acidity",
       y = "Concentration μg/mL",
       x = " ")+
  facet_grid(. ~ ICP, scales = "free_y") +
  theme_er1()+
  theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

library(patchwork)

all_line_rhizon_fig2 = Mnline_rhizon_fig + Kline_rhizon_fig + Mgline_rhizon_fig + Naline_rhizon_fig +  plot_layout(guides = "collect")

ggsave("figures_finalized/all_line_rhizon_fig2.png", plot = all_line_rhizon_fig2, width = 10, height = 5)














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

ggsave("figures_finalized/west_rhizon_fig.png", plot = west_rhizon_fig, width = 7, height = 9)
ggsave("figures_finalized/east_rhizon_fig.png", plot = east_rhizon_fig, width = 7, height = 9)



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

rhizon_meta_combine_notransect_forelements_simplified=
  rhizon_meta_combine_notransect_forelements %>%
  separate(Betterdate, sep = "-", into = c("year", "month", "day")) %>% 
  mutate(month = recode(month, "06" = "June", "07" = "July", "08" = "August")) %>% 
  mutate(date = (paste(month, day, sep = "-"))) %>% 
  mutate(date = factor(date, levels = c("June-24", "June-28", "July-06", "July-07", "July-14", "July-24", "July-30", "July-31",
                                        "August-07", "August-10")))
  
  

rhizon_simplified_P_2021_westhydric = 
  rhizon_meta_combine_notransect_forelements_simplified %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra")) %>% 
  filter(ICP %in% c("phosphorus") & Area == "non-acidic tundra" & Site == "Hydric") %>% 
  mutate(ICP = recode(ICP, "phosphorus" = "phosphorus μg/mL")) %>% 
  ggplot(aes(x = date, y = mean, color = Site, fill = Site)) +
  #geom_point(size = 3, alpha = 0.7)+
  geom_col(position = 'dodge', width = 0.7, alpha = 0.5)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black")+
  labs(x = "", y = "phosphorus μg/mL")+
  scale_color_manual(values = c("#118ab2"))+
  scale_fill_manual(values = c("#118ab2"))+
  # scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  # scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  theme_er1()+
  # theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(Site ~ Area)+
  theme(legend.position = "NONE", axis.text.x = element_text(size = 9, angle = 90))+
  NULL

ggsave("figures_finalized/rhizon_simplified_P_2021_westhydric.png", plot = rhizon_simplified_P_2021_westhydric, width = 4, height = 6)

rhizon_simplified_P_2021_easthydric = 
  rhizon_meta_combine_notransect_forelements_simplified %>%
  mutate(Area = recode(Area, "East" = "acidic tundra")) %>% 
  filter(ICP %in% c("phosphorus") & Area == "acidic tundra" & Site == "Hydric") %>% 
  mutate(ICP = recode(ICP, "phosphorus" = "phosphorus μg/mL")) %>% 
  ggplot(aes(x = date, y = mean, color = Site, fill = Site)) +
  #geom_point(size = 3, alpha = 0.7)+
  geom_col(position = 'dodge', width = 0.7, alpha = 0.5)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black")+
  labs(x = "", y = "phosphorus μg/mL")+
  scale_color_manual(values = c("#118ab2"))+
  scale_fill_manual(values = c("#118ab2"))+
  # scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  # scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  theme_er1()+
  # theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(Site ~ Area)+
  theme(legend.position = "NONE", axis.text.x = element_text(size = 9, angle = 90))+

  NULL

ggsave("figures_finalized/rhizon_simplified_P_2021_easthydric.png", plot = rhizon_simplified_P_2021_easthydric, width = 4, height = 6)


rhizon_meta_combine_notransect_forFe =
  rhizon_meta_combine_notransect_forelements_simplified %>% 
  mutate(Area = recode(Area, "West" = "non-acidic tundra")) %>% 
  filter(ICP %in% c("iron") & Area == "non-acidic tundra" & Site == "Hydric") %>% 
  mutate(ICP = recode(ICP, "iron" = "iron μg/mL")) 

rhizon_simplified_Fe_2021_westhydric = 
  rhizon_meta_combine_notransect_forFe %>%
  ggplot(aes(x = date, y = mean, color = Site, fill = Site)) +
  geom_col(position = 'dodge', width = 0.7, alpha = 0.5)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black") +
  labs(x = "", y = "iron μg/mL")+
  scale_color_manual(values = c("#118ab2"))+
  scale_fill_manual(values = c("#118ab2"))+
  facet_grid(Site ~ Area)+
  theme_er1()+
  theme(legend.position = "NONE", axis.text.x = element_text(size = 9, angle = 90))

ggsave("figures_finalized/rhizon_simplified_Fe_2021_westhydric.png", plot = rhizon_simplified_Fe_2021_westhydric, width = 4, height = 6)

rhizon_meta_combine_notransect_forFe_east =
  rhizon_meta_combine_notransect_forelements_simplified %>% 
  mutate(Area = recode(Area, "East" = "acidic tundra")) %>% 
  filter(ICP %in% c("iron") & Area == "acidic tundra" & Site == "Hydric") %>% 
  mutate(ICP = recode(ICP, "iron" = "iron μg/mL")) 

rhizon_simplified_Fe_2021_easthydric = 
  rhizon_meta_combine_notransect_forFe_east %>%
  ggplot(aes(x = date, y = mean, color = Site, fill = Site)) +
  geom_col(position = 'dodge', width = 0.7, alpha = 0.5)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black") +
  labs(x = "", y = "iron μg/mL")+
  scale_color_manual(values = c("#118ab2"))+
  scale_fill_manual(values = c("#118ab2"))+
  facet_grid(Site ~ Area)+
  theme_er1()+
  theme(legend.position = "NONE", axis.text.x = element_text(size = 9, angle = 90))

ggsave("figures_finalized/rhizon_simplified_Fe_2021_easthydric.png", plot = rhizon_simplified_Fe_2021_easthydric, width = 4, height = 6)



rhizon_simplified_Fe_2021 = 
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








calcium_fig = 
  rhizon_meta_combine_notransect_forelements %>%
  filter(ICP == "calcium") %>% 
  ggplot(aes(x = Betterdate, y = mean, color = Site, fill = Site)) +
  #geom_point(size = 3, alpha = 0.7)+
  geom_col(position = 'dodge', width = 0.7)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black")+
  labs(y = "calcium, ug/mL")+
  scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(Area ~ .)

magnesium_fig = 
  rhizon_meta_combine_notransect_forelements %>%
  filter(ICP == "magnesium") %>% 
  ggplot(aes(x = Betterdate, y = mean, color = Site, fill = Site)) +
  #geom_point(size = 3, alpha = 0.7)+
  geom_col(position = 'dodge', width = 0.7)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black")+
  labs(y = "magnesium, ug/mL")+
  scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(Area ~ .)

potassium_fig = 
  rhizon_meta_combine_notransect_forelements %>%
  filter(ICP == "potassium") %>% 
  ggplot(aes(x = Betterdate, y = mean, color = Site, fill = Site)) +
  #geom_point(size = 3, alpha = 0.7)+
  geom_col(position = 'dodge', width = 0.7)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black")+
  labs(y = "potassium, ug/mL")+
  scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(Area ~ .)

manganese_fig = 
  rhizon_meta_combine_notransect_forelements %>%
  filter(ICP == "manganese") %>% 
  ggplot(aes(x = Betterdate, y = mean, color = Site, fill = Site)) +
  #geom_point(size = 3, alpha = 0.7)+
  geom_col(position = 'dodge', width = 0.7)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black")+
  labs(y = "manganese, ug/mL")+
  scale_color_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  scale_fill_manual(values = rev(natparks.pals(name = "Banff", 3.5)))+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(Area ~ .)



ggsave("output/2021_rhizon_calcium.tiff", plot = calcium_fig, height = 6, width = 5)
ggsave("output/2021_rhizon_potassium.tiff", plot = potassium_fig, height = 6, width = 5)
ggsave("output/2021_rhizon_iron.tiff", plot = iron_fig, height = 6, width = 5)
ggsave("output/2021_rhizon_phosphorus.tiff", plot = phosphorus_fig, height = 6, width = 5)
ggsave("output/2021_rhizon_magnesium.tiff", plot = magnesium_fig, height = 6, width = 5)
ggsave("output/2021_rhizon_manganese.tiff", plot = manganese_fig, height = 6, width = 5)
ggsave("output/2021_rhizon_aluminum.tiff", plot = aluminum_fig, height = 6, width = 5)



write.csv(rhizon_meta_combine_notransect_forelements, "processed/rhizon_forelements.csv")

