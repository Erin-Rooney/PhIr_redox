#Raw data input
#sipper data
#E C Rooney
#1 21 2023


#load packages

source("code/0-packages.R")

#load document

raw_ICP = read.csv("raw/2023_LIME_ICPAES_002R_Herndon_0118.csv")
raw_ICP_diluted = read.csv("raw/2023_LIME_ICPAES_002R_Herndon_0118_diluted.csv")
combo_redox_withdepths2022 = read.csv("processed/allcombine_2022_frozen.csv")


processed_ICP_diluted = 
  raw_ICP_diluted %>% 
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  # dplyr::select(c(SampleID, Date, Time, Area, Site, Plot, Depth_cm, Al_ug_mL, Ca_ug_mL, Fe_ug_mL, 
  #                 K_ug_mL, Mg_ug_mL, Mn_ug_mL, Na_ug_mL, P_ug_mL)) %>% 
  mutate(P_ug_mL = recode(P_ug_mL, "<0.01" = "0.005",
                          "<0.1" = "0.05")) %>%  #this seems like an error...need to check
  separate(Date, sep = "-", into =c('day', 'month', 'year')) %>% 
  mutate(month = recode(month, "Jun" = "06", "Jul" = "07", "Aug" = "08", "Sep" = "09")) %>% 
  mutate(year = recode(year, "22" = "2022")) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(Al_ug_mL = as.numeric(Al_ug_mL)) %>% 
  mutate(Ca_ug_mL = as.numeric(Ca_ug_mL)) %>% 
  mutate(K_ug_mL = as.numeric(K_ug_mL)) %>% 
  mutate(Mn_ug_mL = as.numeric(Mn_ug_mL)) %>% 
  mutate(Mg_ug_mL = as.numeric(Mg_ug_mL)) %>% 
  mutate(Na_ug_mL = as.numeric(Na_ug_mL)) %>% 
  mutate(P_ug_mL = as.numeric(P_ug_mL)) %>%
  mutate(Fe_ug_mL = as.numeric(Fe_ug_mL)) %>% 
  pivot_longer(-c(SampleID, date, Time, Area, Site, Plot, day, month, year, Depth_cm), names_to = 'ICP', values_to = 'concentration') %>% 
  mutate(concentration = concentration * 10)


processed_ICP = 
  raw_ICP %>% 
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                       "East" = "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  na.omit() %>% 
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
  mutate(Ca_ug_mL = as.numeric(Ca_ug_mL)) %>% 
  mutate(K_ug_mL = as.numeric(K_ug_mL)) %>% 
  mutate(Mn_ug_mL = as.numeric(Mn_ug_mL)) %>% 
  mutate(Mg_ug_mL = as.numeric(Mg_ug_mL)) %>% 
  mutate(Na_ug_mL = as.numeric(Na_ug_mL)) %>% 
  mutate(P_ug_mL = as.numeric(P_ug_mL)) %>%
  mutate(Fe_ug_mL = as.numeric(Fe_ug_mL)) %>% 
  pivot_longer(-c(SampleID, date, Time, Area, Site, Plot, day, month, year, Depth_cm), names_to = 'ICP', values_to = 'concentration') %>% 
  vctrs::vec_c(processed_ICP_diluted) %>% 
  mutate(month = recode(month, "06" = "June", "07" = "July", "08" = "August", "09" = "September")) 
  
write.csv(processed_ICP, "processed/ICP_2022.csv")


processed_ICP_wider =
  processed_ICP %>% 
  pivot_wider(names_from = "ICP", values_from = "concentration") 


processed_ICP_grouped =
  processed_ICP %>% 
  pivot_wider(names_from = "ICP", values_from = "concentration") %>% 
  group_by(Area, Site, Depth_cm, date) %>%
  dplyr::summarise(mean_Al_ug_mL = round(mean(Al_ug_mL),3),
                   mean_Ca_ug_mL = round(mean(Ca_ug_mL),3),
                   mean_K_ug_mL = round(mean(K_ug_mL),3),
                   mean_Mn_ug_mL = round(mean(Mn_ug_mL),3),
                   mean_Mg_ug_mL = round(mean(Mg_ug_mL),3),
                   mean_Na_ug_mL = round(mean(Na_ug_mL),3),
                   mean_P_ug_mL = round(mean(P_ug_mL),3),
                   mean_Fe_ug_mL = round(mean(Fe_ug_mL),3),
                   sd_Al_ug_mL = round(sd(Al_ug_mL),3),
                   sd_Ca_ug_mL = round(sd(Ca_ug_mL),3),
                   sd_K_ug_mL = round(sd(K_ug_mL),3),
                   sd_Mn_ug_mL = round(sd(Mn_ug_mL),3),
                   sd_Mg_ug_mL = round(sd(Mg_ug_mL),3),
                   sd_Na_ug_mL = round(sd(Na_ug_mL),3),
                   sd_P_ug_mL = round(sd(P_ug_mL),3),
                   sd_Fe_ug_mL = round(sd(Fe_ug_mL),3)
  ) %>% 
  separate(date, sep = "-", into =c('year', 'month', 'day')) %>% 
  mutate(month2 = recode(month, "06" = "June", "07" = "July", "08" = "August", "09" = "September")) %>% 
  mutate(date_plot = paste(month2, day, year, sep = "-")) %>% 
  mutate(date_plot = factor(date_plot, levels = c("June-27-2022", "June-29-2022", "July-06-2022",
                                                  "July-07-2022", "July-11-2022", "July-12-2022",
                                                  "July-18-2022", "July-19-2022", "July-25-2022",
                                                  "July-26-2022", "August-07-2022", "August-08-2022",
                                                  "September-15-2022","September-17-2022", "September-23-2022"))) %>% 
  mutate(area_site = paste(Area, Site, sep = "-")) 

write.csv(processed_ICP, "output/processed_ICP_porewater2022Asamples.csv")
write.csv(processed_ICP_wider, "output/processed_ICP_porewater2022Asamples_wider.csv")


processed_ICP_grouped_longer =
  processed_ICP %>% 
  pivot_wider(names_from = "ICP", values_from = "concentration") %>% 
  group_by(Area, Site, Depth_cm, date) %>%
  dplyr::summarise(mean_Al_ug_mL = round(mean(Al_ug_mL),3),
                   mean_Ca_ug_mL = round(mean(Ca_ug_mL),3),
                   mean_K_ug_mL = round(mean(K_ug_mL),3),
                   mean_Mn_ug_mL = round(mean(Mn_ug_mL),3),
                   mean_Mg_ug_mL = round(mean(Mg_ug_mL),3),
                   mean_Na_ug_mL = round(mean(Na_ug_mL),3),
                   mean_P_ug_mL = round(mean(P_ug_mL),3),
                   mean_Fe_ug_mL = round(mean(Fe_ug_mL),3)
  ) %>% 
  separate(date, sep = "-", into =c('year', 'month', 'day')) %>% 
  mutate(month2 = recode(month, "06" = "June", "07" = "July", "08" = "August", "09" = "September")) %>% 
  mutate(date_plot = paste(month2, day, year, sep = "-")) %>% 
  mutate(date_plot = factor(date_plot, levels = c("June-27-2022", "June-29-2022", "July-06-2022",
                                                  "July-07-2022", "July-11-2022", "July-12-2022",
                                                  "July-18-2022", "July-19-2022", "July-25-2022",
                                                  "July-26-2022", "August-07-2022", "August-08-2022",
                                                  "September-15-2022","September-17-2022", "September-23-2022"))) %>% 
  mutate(area_site = paste(Area, Site, sep = "-")) %>% 
  pivot_longer(-c(Area, Site, day, month, year, Depth_cm, month2, date_plot, area_site, mean_P_ug_mL), names_to = 'ICP', values_to = 'concentration') 


# Fe_2022_fig =
#   processed_ICP_grouped %>% 
#   ggplot(aes(x = date_plot, y = Depth_cm, fill = mean_Fe_ug_mL, group = date_plot)) +
#   geom_point(size = 4, shape = c(21))+
#   #geom_line(orientation = "y")+
#   scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
#   labs(fill = "Fe ug/mL",
#        y = "Depth, cm")+
#   scale_y_reverse()+
#   facet_grid(Site ~ Area)+
#   theme_er1()


#Fe_2022_fig_hydric =
  # processed_ICP_grouped %>% 
  # filter(Area == "non-acidic tundra") %>% 
  # ggplot(aes(x = mean_Fe_ug_mL, y = Depth_cm, fill = Site, group = Site)) +
  # geom_point(size = 4, shape = c(21), alpha = 0.8)+
  # geom_line(aes(color = Site), orientation = "y", linetype = "longdash")+
  # #scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_fill_manual(values = (pnw_palette("Shuksan2")))+
  # scale_color_manual(values = (pnw_palette("Shuksan2")))+
  # labs(fill = "Fe ug/mL",
  #      y = "Depth, cm")+
  # scale_y_reverse()+
  # facet_grid(Area ~ date)+
  # theme_er1()


###


frozen_group2022 =
  combo_redox_withdepths2022 %>% 
  dplyr::mutate(keep = if_else(site == "non-acidic tundra" & position == "dry" & depth_cm > 21 & Betterdate >= '2022-07-30 00:00', paste0("frozen"),"unfrozen")) %>% 
  filter(keep == "unfrozen") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  separate(TIMESTAMP, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date2 = as.Date(paste(year, month, day, sep = "-"))) %>% 
  dplyr::select(site, position, probe, avg_values_fixed, depth_cm, date2) %>% 
  filter(date2 == c("2022-06-27", "2022-06-29", "2022-07-06", "2022-07-07",
                   "2022-07-11", "2022-07-12", "2022-07-18", "2022-07-19",
                   "2022-07-25", "2022-07-26", "2022-08-07", "2022-08-08",
                   "2022-09-15", "2022-9-17", "2022-09-23")) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  group_by(site, position, date2, depth_cm) %>% 
  dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
                   redox_sd = round(sd(avg_values_fixed),2)) %>% 
  mutate(depth_cm = case_when(depth_cm >=40 ~ "40",
                              depth_cm <= 10 ~ "10",
                              depth_cm == 0 ~ "0",
                                  TRUE ~ "20")) %>% 
  mutate(depth_cm = as.numeric(depth_cm))
  

processed_ICP_forredoxcombo =
  processed_ICP_grouped %>% 
  janitor::clean_names() %>% 
  dplyr::rename(position = site, site = area) %>% 
  mutate(date2 = as.Date(paste(year, month, day, sep = "-"))) %>% 
  dplyr::select(-year, -month, -day, -month2, -date_plot, -area_site) %>% 
  mutate(depth_cm = as.numeric(depth_cm)) %>% 
  mutate(position = recode(position, "Dry" = "dry",
                           "Mesic" = 'mesic',
                           "Hydric" = 'hydric')) 


redox_nutrients_leftjoin = 
  processed_ICP_forredoxcombo %>% 
  left_join(frozen_group2022) %>% 
  mutate(depth_cm = factor(depth_cm, levels = c("0", "10", "20", "40"))) 


redox_nutrients_leftjoin_longer = 
  processed_ICP_forredoxcombo %>% 
  left_join(frozen_group2022) %>% 
  mutate(depth_cm = factor(depth_cm, levels = c("0", "10", "20", "40"))) %>%
  dplyr::select(-sd_al_ug_m_l, -sd_ca_ug_m_l, -sd_k_ug_m_l, -sd_mn_ug_m_l, 
                -sd_mg_ug_m_l, -sd_na_ug_m_l, -sd_p_ug_m_l, -sd_fe_ug_m_l) %>% 
  pivot_longer(-c(site, position, depth_cm, date2, redox_avg_mV, redox_sd), names_to = 'ICP', values_to = 'concentration') %>% 
  mutate(ICP = recode(ICP, "mean_fe_ug_m_l" = "iron", "mean_p_ug_m_l" = "phosphorus",
                      "mean_ca_ug_m_l" = "calcium", "mean_al_ug_m_l" = "aluminum"))


######micromodel concentration prep


#####MANUSCRIPT FIGURES----------------
# 
# non_acidic_hydric_selectfig =
# processed_ICP %>% 
#    # filter(month %in% c("August", "September") & Site == "Hydric" & Area == "non-acidic tundra" & ICP %in% c("Fe_ug_mL", "Mn_ug_mL", "Ca_ug_mL", "Al_ug_mL")) %>% 
#   filter(Site == "Hydric" & Area == "non-acidic tundra" & ICP %in% c("Fe_ug_mL", "Mn_ug_mL", "Ca_ug_mL", "Al_ug_mL")) %>% 
#   mutate(grouping = paste0(Area, "-", Site, "-", date, "-", Plot)) %>% 
#   mutate(month = factor(month, levels = c("June", "July", "August", "September"))) %>% 
#   mutate(ICP = recode(ICP, "Fe_ug_mL" = "iron (μg/ml)", "Mn_ug_mL" = "manganese (μg/ml)", 
#                       "Ca_ug_mL" = "calcium (μg/ml)", "Al_ug_mL" = "aluminum (μg/ml)")) %>% 
#   ggplot()+
#   geom_point(aes(x = concentration, y = Depth_cm, color = month), size = 3, alpha = 0.75)+
#   geom_line(aes(x = concentration, y = Depth_cm, color = month, group = grouping), orientation = "y", size = 0.5, linetype = "longdash")+
#   labs(x = " ",
#        y = "Depth (cm)",
#        color = " "
#   )+
#   scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
#   scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
#   #scale_color_manual(values = c("#9e6374", "#de9b71"))+
#   scale_y_reverse()+
#   facet_wrap(.~ICP, scales = "free_x", switch = 'x')+
#   theme_er1()+
#   theme(legend.position = "bottom",
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         strip.placement = "outside",
#         strip.text.y = element_blank(),
#         axis.text.x = element_text(size = 13),
#         axis.text.y = element_text(size = 13), axis.title.y = element_text(size = 15),
#         panel.border = element_rect(color="gray",size=0.25, fill = NA))
# 
# ggsave("output/non_acidic_hydric_selectfig.png", plot = non_acidic_hydric_selectfig, height = 5, width = 4.5)










P_concentration_fig =
  processed_ICP %>% 
  mutate(grouping = paste0(Area, "-", Site, "-", date, "-", Plot)) %>% 
  mutate(month = factor(month, levels = c("June", "July", "August", "September"))) %>%
  filter(ICP == "P_ug_mL") %>% 
  ggplot() +
  geom_point(aes(x = concentration, y = Depth_cm, color = month), size = 3.5, alpha = 0.75)+
  geom_line(aes(x = concentration, y = Depth_cm, color = month, group = grouping), orientation = "y", size = 0.75, linetype = "dotted")+
  labs(x = "Dissolved P (μg/ml)",
       y = "depth, cm",
       color = "month, 2022"
  )+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  # scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_y_reverse()+
  facet_grid(Site~Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "right",
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      strip.placement = "outside",
      strip.text.y = element_blank())
      #axis.text.y=element_blank(),  #remove y axis labels
      #axis.ticks.y=element_blank(),
      #axis.text.x = element_text(size = 9, hjust=0.8,vjust=0.2,angle = 90)) 


ggsave("output/P_concentration_fig.png", plot = P_concentration_fig, height = 5.5, width = 6.5)

Fe_concentration_fig =
processed_ICP %>% 
  mutate(grouping = paste0(Area, "-", Site, "-", date, "-", Plot)) %>% 
  mutate(month = factor(month, levels = c("June", "July", "August", "September"))) %>%
  # filter(Area == "acidic tundra" & Site == "Hydric" & ICP == "P_ug_mL") %>%
  filter(ICP == "Fe_ug_mL") %>% 
  ggplot() +
  geom_point(aes(x = concentration, y = Depth_cm, color = month), size = 3.5, alpha = 0.75)+
  geom_line(aes(x = concentration, y = Depth_cm, color = month, group = grouping), orientation = "y", size = 0.75, linetype = "dotted")+
  labs(x = "Dissolved Fe (μg/ml)",
       y = " ",
       color = "month, 2022"
  )+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+  # scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_y_reverse()+
  facet_grid(Site~Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank())


ggsave("output/Fe_concentration_fig_plotonly.png", plot = Fe_concentration_fig, height = 5, width = 4)
# 
# Fe_concentration_fig2 =
#   processed_ICP %>% 
#   mutate(grouping = paste0(Area, "-", Site, "-", date, "-", Plot)) %>% 
#   mutate(month = factor(month, levels = c("June", "July", "August", "September"))) %>%
#   # filter(Area == "acidic tundra" & Site == "Hydric" & ICP == "P_ug_mL") %>%
#   filter(ICP == "Fe_ug_mL") %>% 
#   ggplot() +
#   geom_point(aes(x = concentration, y = Depth_cm, fill = month), size = 3, alpha = 0.75, shape = c(21))+
#   geom_line(aes(x = concentration, y = Depth_cm, color = month, group = grouping), orientation = "y", size = 0.75, alpha = 0.3, linetype = "dashed")+
#   labs(x = "Iron concentration, μg/ml",
#        y = "Depth, cm",
#        color = "month, 2022"
#   )+
#   scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
#   scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+  # scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
#   scale_y_reverse(breaks=c(0, 20, 40))+
#   guides(fill=guide_legend(nrow=2,byrow=TRUE), color = guide_legend(nrow=2, byrow=TRUE))+
#   facet_grid(Site~Area, scales = "free_x")+
#   theme_er1()+
#   theme(legend.position = "bottom",
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         strip.placement = "outside", panel.border = element_rect(color="gray",size=0.25, fill = NA))
# 
# 
# ggsave("output/Fe_concentration_fig.png", plot = Fe_concentration_fig2, height = 5, width = 4)
# 
# Fe_concentration_fig2LEGEND =
#   processed_ICP %>% 
#   mutate(grouping = paste0(Area, "-", Site, "-", date, "-", Plot)) %>% 
#   mutate(month = factor(month, levels = c("June", "July", "August", "September"))) %>%
#   # filter(Area == "acidic tundra" & Site == "Hydric" & ICP == "P_ug_mL") %>%
#   filter(ICP == "Fe_ug_mL") %>% 
#   ggplot() +
#   geom_point(aes(x = concentration, y = Depth_cm, fill = month), size = 3, alpha = 0.75, shape = c(21))+
#   labs(x = "Iron concentration, μg/ml",
#        y = "Depth, cm",
#        color = "month, 2022"
#   )+
#   scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
#   scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+  # scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
#   scale_y_reverse(breaks=c(0, 20, 40))+
#   guides(fill=guide_legend(nrow=2,byrow=TRUE))+
#   facet_grid(Site~Area, scales = "free_x")+
#   theme_er1()+
#   theme(legend.position = "bottom",
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         strip.placement = "outside", panel.border = element_rect(color="gray",size=0.25, fill = NA))
# 
# 
# ggsave("output/Fe_concentration_figLEGEND.png", plot = Fe_concentration_fig2LEGEND, height = 5, width = 4)


Mn_concentration_fig =
  processed_ICP %>% 
  mutate(grouping = paste0(Area, "-", Site, "-", date, "-", Plot)) %>% 
  mutate(month = factor(month, levels = c("June", "July", "August", "September"))) %>%
  # filter(Area == "acidic tundra" & Site == "Hydric" & ICP == "P_ug_mL") %>%
  filter(ICP == "Mn_ug_mL") %>% 
  ggplot() +
  geom_point(aes(x = concentration, y = Depth_cm, color = month), size = 3.5, alpha = 0.75)+
  geom_line(aes(x = concentration, y = Depth_cm, color = month, group = grouping), orientation = "y", size = 0.75, linetype = "dotted")+
  labs(x = "Dissolved Mn (μg/ml)",
       y = " ",
       color = "month, 2022"
  )+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_y_reverse()+
  facet_grid(Site~Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 9, hjust=0.8,vjust=0.2,angle = 90)) 

ggsave("output/Mn_concentration_fig.png", plot = Mn_concentration_fig, height = 5.5, width = 6.5)

Ca_concentration_fig =
  processed_ICP %>% 
  mutate(grouping = paste0(Area, "-", Site, "-", date, "-", Plot)) %>% 
  mutate(month = factor(month, levels = c("June", "July", "August", "September"))) %>%
  # filter(Area == "acidic tundra" & Site == "Hydric" & ICP == "P_ug_mL") %>%
  filter(ICP == "Ca_ug_mL") %>% 
  ggplot() +
  geom_point(aes(x = concentration, y = Depth_cm, color = month), size = 3.5, alpha = 0.75)+
  geom_line(aes(x = concentration, y = Depth_cm, color = month, group = grouping), orientation = "y", size = 0.75, linetype = "dotted")+
  labs(x = "Dissolved Ca (μg/ml)",
       y = "depth, cm",
       color = "month, 2022"
  )+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_y_reverse()+
  facet_grid(Site~Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank())
       # axis.text.y=element_blank(),  #remove y axis labels
       # axis.ticks.y=element_blank())

ggsave("output/Ca_concentration_fig.png", plot = Ca_concentration_fig, height = 5.5, width = 6.5)


library(patchwork)

nutrientsfig1 = Ca_concentration_fig + Mn_concentration_fig 

nutrientsfig2 = P_concentration_fig + Fe_concentration_fig 


nutrient_plot = nutrientsfig1 / nutrientsfig2 + plot_layout(guides = "collect")

# ggsave("output/2022nutrientsfigA.png", plot = nutrientsfigA, height = 9, width = 11)
# ggsave("output/2022nutrientsfigB.png", plot = nutrientsfigB, height = 10, width = 8)
ggsave("output/2022nutrient_plot.png", plot = nutrient_plot, height = 12, width = 9.5)



###########

#SUPPLEMENTAL MANUSCRIPT FIGURE

Al_concentration_fig =
  processed_ICP %>% 
  mutate(grouping = paste0(Area, "-", Site, "-", date, "-", Plot)) %>% 
  mutate(month = factor(month, levels = c("June", "July", "August", "September"))) %>%
  filter(ICP == "Al_ug_mL") %>% 
  ggplot() +
  geom_point(aes(x = concentration, y = Depth_cm, color = month), size = 3.5, alpha = 0.75)+
  geom_line(aes(x = concentration, y = Depth_cm, color = month, group = grouping), orientation = "y", size = 0.75, linetype = "dotted")+
  labs(x = "Dissolved Al (μg/ml)",
       y = "depth, cm",
       color = "month, 2022"
  )+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_y_reverse()+
  facet_grid(Site~Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        #axis.text.y=element_blank(),  #remove y axis labels
        #axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 9, hjust=0.8,vjust=0.2,angle = 90)) 


ggsave("output/Al_concentration_fig.png", plot = Al_concentration_fig, height = 5.5, width = 6.5)

Mg_concentration_fig =
  processed_ICP %>% 
  mutate(grouping = paste0(Area, "-", Site, "-", date, "-", Plot)) %>% 
  mutate(month = factor(month, levels = c("June", "July", "August", "September"))) %>%
  # filter(Area == "acidic tundra" & Site == "Hydric" & ICP == "P_ug_mL") %>%
  filter(ICP == "Mg_ug_mL") %>% 
  ggplot() +
  geom_point(aes(x = concentration, y = Depth_cm, color = month), size = 3.5, alpha = 0.75)+
  geom_line(aes(x = concentration, y = Depth_cm, color = month, group = grouping), orientation = "y", size = 0.75, linetype = "dotted")+
  labs(x = "Dissolved Mg (μg/ml)",
       y = " ",
       color = "month, 2022"
  )+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_y_reverse()+
  facet_grid(Site~Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 9, hjust=0.8,vjust=0.2,angle = 90)) 


ggsave("output/Mg_concentration_fig.png", plot = Mg_concentration_fig, height = 5.5, width = 6.5)


Na_concentration_fig =
  processed_ICP %>% 
  mutate(grouping = paste0(Area, "-", Site, "-", date, "-", Plot)) %>% 
  mutate(month = factor(month, levels = c("June", "July", "August", "September"))) %>%
  # filter(Area == "acidic tundra" & Site == "Hydric" & ICP == "P_ug_mL") %>%
  filter(ICP == "Na_ug_mL") %>% 
  ggplot() +
  geom_point(aes(x = concentration, y = Depth_cm, color = month), size = 3.5, alpha = 0.75)+
  geom_line(aes(x = concentration, y = Depth_cm, color = month, group = grouping), orientation = "y", size = 0.75, linetype = "dotted")+
  labs(x = "Dissolved Na (μg/ml)",
       y = " ",
       color = "month, 2022"
  )+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_y_reverse()+
  facet_grid(Site~Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 9, hjust=0.8,vjust=0.2,angle = 90)) 

ggsave("output/Na_concentration_fig.png", plot = Na_concentration_fig, height = 5.5, width = 6.5)

K_concentration_fig =
  processed_ICP %>% 
  mutate(grouping = paste0(Area, "-", Site, "-", date, "-", Plot)) %>% 
  mutate(month = factor(month, levels = c("June", "July", "August", "September"))) %>%
  # filter(Area == "acidic tundra" & Site == "Hydric" & ICP == "P_ug_mL") %>%
  filter(ICP == "K_ug_mL") %>% 
  ggplot() +
  geom_point(aes(x = concentration, y = Depth_cm, color = month), size = 3.5, alpha = 0.75)+
  geom_line(aes(x = concentration, y = Depth_cm, color = month, group = grouping), orientation = "y", size = 0.75, linetype = "dotted")+
  labs(x = "Dissolved K (μg/ml)",
       y = "depth, cm",
       color = "month, 2022"
  )+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_y_reverse()+
  facet_grid(Site~Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        # axis.text.y=element_blank(),  #remove y axis labels
        # axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 9, hjust=0.8,vjust=0.2,angle = 90)) 

ggsave("output/K_concentration_fig.png", plot = K_concentration_fig, height = 5.5, width = 6.5)


library(patchwork)

nutrientsfig1 = Al_concentration_fig + Na_concentration_fig 

nutrientsfig2 = K_concentration_fig + Mg_concentration_fig 


nutrient_plot = nutrientsfig1 / nutrientsfig2 + plot_layout(guides = "collect")

# ggsave("output/2022nutrientsfigA.png", plot = nutrientsfigA, height = 9, width = 11)
# ggsave("output/2022nutrientsfigB.png", plot = nutrientsfigB, height = 10, width = 8)
ggsave("output/2022nutrient_plot2.png", plot = nutrient_plot, height = 12, width = 9.5)

##################





redox_nutrients_log_fig =
redox_nutrients_leftjoin_longer %>% 
  filter(redox_avg_mV != "NA" & ICP %in% c("aluminum", "calcium", "iron", "phosphorus")) %>% 
  ggplot() +
  geom_point(aes(x = redox_avg_mV, y = concentration, color = position, shape = site), size = 3, alpha = 0.75)+
  labs(x = "redox potential, mV",
       y = "concentration, ug, ml",
       fill = "element",
       color = "element")+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  facet_grid(ICP~position, scales = "free_y")+
  theme_er1()+
  theme(legend.position = "right")

fe_redox_figfacet =
redox_nutrients_leftjoin %>% 
  filter(redox_avg_mV != "NA") %>% 
  ggplot() +
  geom_point(aes(x = redox_avg_mV, y = mean_fe_ug_m_l, color = position, shape = site), size = 3, alpha = 0.75)+
  labs(x = "redox potential, mV",
       y = "Dissolved Fe (μg/ml)"
       )+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  facet_grid(position~site, scales = "free_y")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside", panel.border = element_rect(color="gray",size=0.5, fill = NA))

mn_redox_figfacet =
redox_nutrients_leftjoin %>% 
  filter(redox_avg_mV != "NA") %>% 
  ggplot() +
  geom_point(aes(x = redox_avg_mV, y = mean_mn_ug_m_l, color = position, shape = site), size = 3, alpha = 0.75)+
  labs(x = "redox potential, mV",
       y = "manganese ug, ml"
  )+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  facet_wrap(position~site, scales = "free_y")+
  theme_er1()+
  theme(legend.position = "bottom")

p_redox_figfacet =
  redox_nutrients_leftjoin %>% 
  filter(redox_avg_mV != "NA") %>% 
  ggplot() +
  geom_point(aes(x = redox_avg_mV, y = mean_p_ug_m_l, color = position, shape = site), size = 3, alpha = 0.75)+
  labs(x = "redox potential, mV",
       y = "Dissolved P (μg/ml)"
  )+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  facet_grid(position~site, scales = "free_y")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside", panel.border = element_rect(color="gray",size=0.5, fill = NA))

p_date_figfacet =
  redox_nutrients_leftjoin %>% 
  filter(redox_avg_mV != "NA") %>% 
  ggplot() +
  geom_point(aes(x = date2, y = mean_p_ug_m_l, color = position, shape = site), size = 3, alpha = 0.75)+
  labs(x = "redox potential, mV",
       y = "phosphorus ug, ml"
  )+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  facet_wrap(position~site, scales = "free_y")+
  theme_er1()+
  theme(legend.position = "bottom")

ggsave("output/p_date_figfacet.png", plot = p_date_figfacet, height = 5.5, width = 8)
ggsave("output/p_redox_figfacet.png", plot = p_redox_figfacet, height = 5.5, width = 8)
ggsave("output/fe_redox_figfacet.png", plot = fe_redox_figfacet, height = 5.5, width = 8)
ggsave("output/mn_redox_figfacet.png", plot = mn_redox_figfacet, height = 5.5, width = 8)

redox_p_fe_fig = p_redox_figfacet + fe_redox_figfacet

ggsave("output/redox_p_fe_fig.png", plot = redox_p_fe_fig, height = 7, width = 10)


ggsave("output/redox_nutrients_log_fig.png", plot = redox_nutrients_log_fig, height = 7, width = 10)

#iron_redox_correlation_fig =
fe_p_correlation_log =
redox_nutrients_leftjoin %>% 
  filter(redox_avg_mV != "NA") %>% 
  ggplot() +
  geom_point(aes(x = log(mean_fe_ug_m_l), y = log(mean_p_ug_m_l), color = redox_avg_mV, group = redox_avg_mV), size = 3, alpha = 0.9)+
  labs(x = "log, iron, ug/ml",
       y = "log, phosphorus, ug/ml",
       fill = "redox potential, mV",
       color = "redox potential, mV")+
  scale_color_gradientn(colors=pnw_palette("Bay"))+
  scale_fill_gradientn(colors=pnw_palette("Bay"))+
  #facet_grid(position~site, scales = "free")+
  theme_er1()


ggsave("output/fe_p_correlation_log.png", plot = fe_p_correlation_log, height = 4.5, width = 5)

fe_p_correlation = 
redox_nutrients_leftjoin %>% 
  filter(redox_avg_mV != "NA") %>% 
  ggplot() +
  geom_point(aes(x = mean_fe_ug_m_l, y = mean_p_ug_m_l, color = redox_avg_mV, group = redox_avg_mV), size = 3, alpha = 0.9)+
  labs(x = "iron, ug/ml",
       y = "phosphorus, ug/ml",
       fill = "redox potential, mV",
       color = "redox potential, mV")+
  scale_color_gradientn(colors=pnw_palette("Bay"))+
  scale_fill_gradientn(colors=pnw_palette("Bay"))+
  facet_grid(position~site, scales = "free")+
  theme_er1()

ggsave("output/fe_p_correlation.png", plot = fe_p_correlation, height = 7, width = 4.5)


ca_p_correlation_log =
redox_nutrients_leftjoin %>% 
  filter(redox_avg_mV != "NA") %>% 
  ggplot() +
  geom_point(aes(x = log(mean_ca_ug_m_l), y = log(mean_p_ug_m_l), color = redox_avg_mV, group = redox_avg_mV), size = 3, alpha = 0.9)+
  labs(x = "log, calcium, ug/ml",
       y = "log, phosphorus, ug/ml",
       fill = "redox potential, mV",
       color = "redox potential, mV")+
  scale_color_gradientn(colors=pnw_palette("Bay"))+
  scale_fill_gradientn(colors=pnw_palette("Bay"))+
  #facet_grid(position~site, scales = "free")+
  theme_er1()

ggsave("output/ca_p_correlation_log.png", plot = ca_p_correlation_log, height = 4.5, width = 5)


ca_p_correlation =
redox_nutrients_leftjoin %>% 
  filter(redox_avg_mV != "NA") %>% 
  ggplot() +
  geom_point(aes(x = mean_ca_ug_m_l, y = mean_p_ug_m_l, color = redox_avg_mV, group = redox_avg_mV), size = 3, alpha = 0.9)+
  labs(x = "calcium, ug/ml",
       y = "phosphorus, ug/ml",
       fill = "redox potential, mV",
       color = "redox potential, mV")+
  scale_color_gradientn(colors=pnw_palette("Bay"))+
  scale_fill_gradientn(colors=pnw_palette("Bay"))+
  facet_grid(position~site, scales = "free")+
  theme_er1()

ggsave("output/ca_p_correlation.png", plot = ca_p_correlation, height = 7, width = 4.5)


logal_p_correlation =
redox_nutrients_leftjoin %>% 
  filter(redox_avg_mV != "NA") %>% 
  ggplot() +
  geom_point(aes(x = log(mean_al_ug_m_l), y = mean_p_ug_m_l, color = redox_avg_mV, group = redox_avg_mV), size = 3, alpha = 0.9)+
  labs(x = "log, aluminum, ug/ml",
       y = "phosphorus, ug/ml",
       fill = "redox potential, mV",
       color = "redox potential, mV")+
  scale_color_gradientn(colors=pnw_palette("Bay"))+
  scale_fill_gradientn(colors=pnw_palette("Bay"))+
  #facet_grid(position~site, scales = "free")+
  theme_er1()

ggsave("output/logal_p_correlation.png", plot = logal_p_correlation, height = 4.5, width = 5)


mn_fe_correlationlog =
redox_nutrients_leftjoin %>% 
  filter(redox_avg_mV != "NA") %>% 
  ggplot() +
  geom_point(aes(x = log(mean_mn_ug_m_l), y = log(mean_fe_ug_m_l), color = redox_avg_mV, group = redox_avg_mV), size = 3, alpha = 0.9)+
  labs(x = "log, manganese, ug/ml",
       y = "log, iron, ug/ml",
       fill = "redox potential, mV",
       color = "redox potential, mV")+
  scale_color_gradientn(colors=pnw_palette("Bay"))+
  scale_fill_gradientn(colors=pnw_palette("Bay"))+
  #facet_grid(position~site, scales = "free")+
  theme_er1()

ggsave("output/mn_fe_correlationlog.png", plot = mn_fe_correlationlog, height = 4.5, width = 5)


mn_fe_correlation =
redox_nutrients_leftjoin %>% 
  filter(redox_avg_mV != "NA") %>% 
  ggplot() +
  geom_point(aes(x = mean_mn_ug_m_l, y = mean_fe_ug_m_l, color = redox_avg_mV, group = redox_avg_mV), size = 3, alpha = 0.9)+
  labs(x = "manganese, ug/ml",
       y = "iron, ug/ml",
       fill = "redox potential, mV",
       color = "redox potential, mV")+
  scale_color_gradientn(colors=pnw_palette("Bay"))+
  scale_fill_gradientn(colors=pnw_palette("Bay"))+
  #facet_grid(position~site, scales = "free")+
  theme_er1()

ggsave("output/mn_fe_correlation.png", plot = mn_fe_correlation, height = 4.5, width = 5)




#ggsave("output/iron_redox_correlation_fig.png", plot = iron_redox_correlation_fig, height = 8, width = 4.5)
  
phos_redox_correlation_fig =
redox_nutrients_leftjoin %>% 
  filter(redox_avg_mV != "NA") %>% 
  ggplot() +
  geom_point(aes(x = redox_avg_mV, y = mean_p_ug_m_l, color = depth_cm, group = depth_cm), size = 3, alpha = 0.9)+
  scale_color_manual(values=pnw_palette("Lake", 3))+
  scale_fill_manual(values=pnw_palette("Lake", 3))+
  labs(x = "redox potential, mV",
       y = "Phosphorus, ug/ml",
       fill = "depth, cm",
       color = "depth, cm")+
  facet_grid(position~site)+
  theme_er1()

ggsave("output/phosphorus_redox_correlation_fig.png", plot = phos_redox_correlation_fig, height = 8, width = 4.5)

mn_redox_correlation_fig =
redox_nutrients_leftjoin %>% 
  filter(redox_avg_mV != "NA") %>% 
  ggplot() +
  geom_point(aes(x = redox_avg_mV, y = mean_mn_ug_m_l, color = depth_cm, group = depth_cm), size = 3, alpha = 0.9)+
  labs(x = "redox potential, mV",
       y = "Manganese, ug/ml",
       fill = "depth, cm",
       color = "depth, cm")+
  scale_color_manual(values=pnw_palette("Lake", 3))+
  scale_fill_manual(values=pnw_palette("Lake", 3))+
  facet_grid(position~site)+
  theme_er1()

ggsave("output/manganese_redox_correlation_fig.png", plot = mn_redox_correlation_fig, height = 8, width = 4.5)


Fe_fig =  
  processed_ICP %>% 
  filter(ICP == "Fe_ug_mL") %>% 
  ggplot(aes(x = concentration, y = Depth_cm, fill = month, group = month)) +
  #geom_errorbar(aes(xmin=(mean_Fe_ug_mL - sd_Fe_ug_mL), xmax=(mean_Fe_ug_mL + sd_Fe_ug_mL), color = date_plot))+
  geom_point(size = 3.5, shape = c(21), alpha = 0.6)+
  #geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset", 4)))+
  scale_color_manual(values = (pnw_palette("Sunset", 4)))+
  labs(fill = "Month",
       color = "Month",
       y = "",
       x = "Iron ug/mL")+
  scale_y_reverse()+
  scale_x_continuous(position = 'top') +
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 7.5, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks
#remove y axis ticks

Al_fig =  
  processed_ICP %>% 
  filter(ICP == "Al_ug_mL") %>% 
  ggplot(aes(x = concentration, y = Depth_cm, fill = month, group = month)) +
  #geom_errorbar(aes(xmin=(mean_Fe_ug_mL - sd_Fe_ug_mL), xmax=(mean_Fe_ug_mL + sd_Fe_ug_mL), color = date_plot))+
  geom_point(size = 3.5, shape = c(21), alpha = 0.6)+
  #geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset", 4)))+
  scale_color_manual(values = (pnw_palette("Sunset", 4)))+
  labs(fill = "Month",
       color = "Month",
       y = "",
       x = "Aluminum ug/mL")+
  scale_y_reverse()+
  scale_x_continuous(position = 'top') +
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 7.5, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks
#remove y axis ticks

P_fig =  
  processed_ICP %>% 
  filter(ICP == "P_ug_mL") %>% 
  ggplot(aes(x = concentration, y = Depth_cm, fill = month, group = month)) +
  #geom_errorbar(aes(xmin=(mean_Fe_ug_mL - sd_Fe_ug_mL), xmax=(mean_Fe_ug_mL + sd_Fe_ug_mL), color = date_plot))+
  geom_point(size = 3.5, shape = c(21), alpha = 0.6)+
  #geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset", 4)))+
  scale_color_manual(values = (pnw_palette("Sunset", 4)))+
  labs(fill = "Month",
       color = "Month",
       y = "",
       x = "Phosphorus ug/mL")+
  scale_y_reverse()+
  scale_x_continuous(position = 'top') +
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        axis.text.x = element_text(size = 7.5, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks

Ca_fig =  
  processed_ICP %>% 
  filter(ICP == "Ca_ug_mL") %>% 
  ggplot(aes(x = concentration, y = Depth_cm, fill = month, group = month)) +
  #geom_errorbar(aes(xmin=(mean_Fe_ug_mL - sd_Fe_ug_mL), xmax=(mean_Fe_ug_mL + sd_Fe_ug_mL), color = date_plot))+
  geom_point(size = 3.5, shape = c(21), alpha = 0.6)+
  #geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset", 4)))+
  scale_color_manual(values = (pnw_palette("Sunset", 4)))+
  labs(fill = "Month",
       color = "Month",
       y = "",
       x = "Calcium ug/mL")+
  scale_y_reverse()+
  scale_x_continuous(position = 'top') +
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        axis.text.x = element_text(size = 7.5, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks


library(patchwork)

nutrientsfig1 = Ca_fig + Al_fig 

nutrientsfig2 = P_fig + Fe_fig 


nutrient_plot = nutrientsfig1 / nutrientsfig2 + plot_layout(guides = "collect")

# ggsave("output/2022nutrientsfigA.png", plot = nutrientsfigA, height = 9, width = 11)
# ggsave("output/2022nutrientsfigB.png", plot = nutrientsfigB, height = 10, width = 8)
ggsave("output/2022nutrient_plot.png", plot = nutrient_plot, height = 8, width = 9.5)

#ggsave("formanuscript/Fe_fig.png", plot = Fe_fig, height = 5, width = 6)











########
  
Fe_grouped_fig =  
  processed_ICP_grouped %>% 
    #filter(Area == "non-acidic tundra") %>% 
    ggplot(aes(x = mean_Fe_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
    geom_errorbar(aes(xmin=(mean_Fe_ug_mL - sd_Fe_ug_mL), xmax=(mean_Fe_ug_mL + sd_Fe_ug_mL), color = date_plot))+
    geom_point(size = 3.5, shape = c(21), alpha = 0.6)+
    geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
    # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
    # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
    scale_fill_manual(values = (pnw_palette("Sunset2", 15)))+
    scale_color_manual(values = (pnw_palette("Sunset2", 15)))+
    labs(fill = "Date",
         color = "Date",
         y = "",
         x = "Iron ug/mL")+
    scale_y_reverse()+
    scale_x_continuous(position = 'top') +
    facet_grid(Site ~ Area, scales = "free_x")+
    theme_er1()+
    theme(legend.position = "none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
                strip.placement = "outside")
#

P_grouped_fig =  
  processed_ICP_grouped %>% 
    #filter(Area == "non-acidic tundra") %>% 
    ggplot(aes(x = mean_P_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
    geom_errorbar(aes(xmin=(mean_P_ug_mL - sd_P_ug_mL), xmax=(mean_P_ug_mL + sd_P_ug_mL), color = date_plot))+
    geom_point(size = 3.5, shape = c(21), alpha = 0.6)+
    geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
    # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
    # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
    scale_fill_manual(values = (pnw_palette("Sunset2", 15)))+
    scale_color_manual(values = (pnw_palette("Sunset2", 15)))+
    labs(fill = "Date",
         color = "Date",
         y = "Depth, cm",
         x = "Phosphorus ug/mL")+
  scale_y_reverse()+
  scale_x_continuous(position = 'top') +
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        axis.text.x = element_text(size = 7.5, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks



Mn_grouped_fig =  
  processed_ICP_grouped %>% 
  #filter(Area == "non-acidic tundra") %>% 
  ggplot(aes(x = mean_Mn_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
  geom_errorbar(aes(xmin=(mean_Mn_ug_mL - sd_Mn_ug_mL), xmax=(mean_Mn_ug_mL + sd_Mn_ug_mL), color = date_plot))+
  geom_point(size = 3.5, shape = c(21), alpha = 0.6)+
  geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset2", 15)))+
  scale_color_manual(values = (pnw_palette("Sunset2", 15)))+
  labs(fill = "Date",
       color = "Date",
       y = "",
       x = "Manganese ug/mL")+
  scale_y_reverse()+
  scale_x_continuous(position = 'top') +
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 7.5, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks



Legend_grouped_fig =  
  processed_ICP_grouped %>% 
  #filter(Area == "non-acidic tundra") %>% 
  ggplot(aes(x = mean_Mn_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
  geom_point(size = 3.5, shape = c(21), alpha = 0.6)+
  geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset2", 15)))+
  scale_color_manual(values = (pnw_palette("Sunset2", 15)))+
  labs(fill = "Date",
       color = "Date",
       y = "",
       x = "Manganese ug/mL")+
  scale_y_reverse()+
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


ggsave("output/2022_Mn_fig2.png", plot = Mn_grouped_fig, height = 6, width = 4)
ggsave("output/2022_Fe_fig2.png", plot = Fe_grouped_fig, height = 6, width = 4)
ggsave("output/2022_P_fig2.png", plot = P_grouped_fig, height = 6, width = 4)
ggsave("output/2022_fig2_legend.png", plot = Legend_grouped_fig, height = 10, width = 10)



Al_grouped_fig =  
  processed_ICP_grouped %>% 
  #filter(Area == "non-acidic tundra") %>% 
  ggplot(aes(x = mean_Al_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
  geom_errorbar(aes(xmin=(mean_Al_ug_mL - sd_Al_ug_mL), xmax=(mean_Al_ug_mL + sd_Al_ug_mL), color = date_plot))+
  geom_point(size = 3.5, shape = c(21), alpha = 0.6)+
  geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset2", 15)))+
  scale_color_manual(values = (pnw_palette("Sunset2", 15)))+
  labs(fill = "Date",
       color = "Date",
       y = "",
       x = "Aluminum ug/mL")+
  scale_y_reverse()+
  scale_x_continuous(position = 'top') +
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 7.5, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks

Ca_grouped_fig =  
  processed_ICP_grouped %>% 
  #filter(Area == "non-acidic tundra") %>% 
  ggplot(aes(x = mean_Ca_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
  geom_errorbar(aes(xmin=(mean_Ca_ug_mL - sd_Ca_ug_mL), xmax=(mean_Ca_ug_mL + sd_Ca_ug_mL), color = date_plot))+
  geom_point(size = 3.5, shape = c(21), alpha = 0.6)+
  geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset2", 15)))+
  scale_color_manual(values = (pnw_palette("Sunset2", 15)))+
  labs(fill = "Date",
       color = "Date",
       y = "Depth, cm",
       x = "Calcium ug/mL")+
  scale_y_reverse()+
  scale_x_continuous(position = 'top') +
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        axis.text.x = element_text(size = 7.5, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks



Na_grouped_fig =  
  processed_ICP_grouped %>% 
  #filter(Area == "non-acidic tundra") %>% 
  ggplot(aes(x = mean_Na_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
  geom_errorbar(aes(xmin=(mean_Na_ug_mL - sd_Na_ug_mL), xmax=(mean_Na_ug_mL + sd_Na_ug_mL), color = date_plot))+
  geom_point(size = 3, shape = c(21), alpha = 0.8)+
  geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset2", 15)))+
  scale_color_manual(values = (pnw_palette("Sunset2", 15)))+
  labs(fill = "Date",
       color = "Date",
       y = "Depth, cm",
       x = "Sodium ug/mL")+
  scale_y_reverse()+
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

Mg_grouped_fig =  
  processed_ICP_grouped %>% 
  #filter(Area == "non-acidic tundra") %>% 
  ggplot(aes(x = mean_Mg_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
  geom_errorbar(aes(xmin=(mean_Mg_ug_mL - sd_Mg_ug_mL), xmax=(mean_Mg_ug_mL + sd_Mg_ug_mL), color = date_plot))+
  geom_point(size = 3, shape = c(21), alpha = 0.8)+
  geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset2", 15)))+
  scale_color_manual(values = (pnw_palette("Sunset2", 15)))+
  labs(fill = "Date",
       color = "Date",
       y = "Depth, cm",
       x = "Magnesium ug/mL")+
  scale_y_reverse()+
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

K_grouped_fig =  
  processed_ICP_grouped %>% 
  #filter(Area == "non-acidic tundra") %>% 
  ggplot(aes(x = mean_K_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
  geom_errorbar(aes(xmin=(mean_K_ug_mL - sd_K_ug_mL), xmax=(mean_K_ug_mL + sd_K_ug_mL), color = date_plot))+
  geom_point(size = 3, shape = c(21), alpha = 0.8)+
  geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset2", 15)))+
  scale_color_manual(values = (pnw_palette("Sunset2", 15)))+
  labs(fill = "Date",
       color = "Date",
       y = "Depth, cm",
       x = "Potassium ug/mL")+
  scale_y_reverse()+
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        )


ggsave("output/2022_Mg_fig2.png", plot = Mg_grouped_fig, height = 6, width = 4)
ggsave("output/2022_K_fig2.png", plot = K_grouped_fig, height = 6, width = 4)
ggsave("output/2022_Na_fig2.png", plot = Na_grouped_fig, height = 6, width = 4)
ggsave("output/2022_Al_fig2.png", plot = Al_grouped_fig, height = 6, width = 4)
ggsave("output/2022_Ca_fig2.png", plot = Ca_grouped_fig, height = 6, width = 4)
ggsave("output/2022_fig2_legend.png", plot = Legend_grouped_fig, height = 10, width = 10)

library(patchwork)

nutrientsfigA = Ca_grouped_fig + Al_grouped_fig + Fe_grouped_fig 

nutrientsfigB = P_grouped_fig + Mn_grouped_fig 


nutrient_plot = nutrientsfigA / nutrientsfigB

ggsave("output/2022nutrientsfigA.png", plot = nutrientsfigA, height = 9, width = 11)
ggsave("output/2022nutrientsfigB.png", plot = nutrientsfigB, height = 10, width = 8)
ggsave("output/2022nutrient_plot.png", plot = nutrient_plot, height = 10, width = 10)


processed_ICP_grouped_longer %>% 
  mutate(Depth_cm = factor(Depth_cm)) %>% 
  filter(ICP == c("mean_Fe_ug_mL")) %>% 
  ggplot(aes(x = mean_P_ug_mL, y = concentration, color = Depth_cm, shape = Depth_cm)) +
  geom_point(size = 2.5, alpha = 0.8)+
  scale_fill_manual(values = rev((pnw_palette("Anemone", 4))))+
  scale_color_manual(values = rev((pnw_palette("Anemone", 4))))+
  labs(shape = "Depth, cm",
       color = "Depth, cm",
       y = "Fe ug/mL",
       x = "P ug/mL")+
  facet_grid(Site ~ Area, scales = "free")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(size = 8, hjust=0.8,vjust=0.2,angle = 90))



##lineplots

processed_ICP_grouped_longer %>% 
  mutate(Depth_cm = factor(Depth_cm)) %>% 
  filter(ICP == c("mean_Ca_ug_mL")) %>% 
  ggplot(aes(x = mean_P_ug_mL, y = concentration, color = Depth_cm, shape = Depth_cm)) +
  geom_point(size = 2.5, alpha = 0.8)+
  scale_fill_manual(values = rev((pnw_palette("Anemone", 4))))+
  scale_color_manual(values = rev((pnw_palette("Anemone", 4))))+
  labs(shape = "Depth, cm",
       color = "Depth, cm",
       y = "Ca ug/mL",
       x = "P ug/mL")+
  facet_grid(Site ~ Area, scales = "free")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(size = 8, hjust=0.8,vjust=0.2,angle = 90))

Ca_depth_site_area_lineplot =
processed_ICP_grouped %>% 
  mutate(Depth_cm = factor(Depth_cm)) %>% 
  mutate(depth_area = paste(Depth_cm, Area, "-")) %>% 
  ggplot(aes(x = date_plot, y = mean_Ca_ug_mL, color = Depth_cm)) +
  #geom_point(size = 2.5, alpha = 0.8)+
  geom_line(aes(linetype = Area, group = depth_area), size = 0.8)+
  scale_fill_manual(values = rev((pnw_palette("Anemone", 4))))+
  scale_color_manual(values = rev((pnw_palette("Anemone", 4))))+
  labs(linetype = "Acidity",
       color = "Depth, cm",
       y = "Ca ug/mL",
       x = " ")+
  facet_grid(Site ~ ., scales = "free")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(size = 8, hjust=0.8,vjust=0.2,angle = 90))

Fe_depth_site_area_lineplot =
processed_ICP_grouped %>% 
  mutate(Depth_cm = factor(Depth_cm)) %>% 
  mutate(depth_area = paste(Depth_cm, Area, "-")) %>% 
  ggplot(aes(x = date_plot, y = mean_Fe_ug_mL, color = Depth_cm)) +
  #geom_point(size = 2.5, alpha = 0.8)+
  geom_line(aes(linetype = Area, group = depth_area), size = 0.8)+
  scale_fill_manual(values = rev((pnw_palette("Anemone", 4))))+
  scale_color_manual(values = rev((pnw_palette("Anemone", 4))))+
  labs(linetype = "Acidity",
       color = "Depth, cm",
       y = "Fe ug/mL",
       x = " ")+
  facet_grid(Site ~ ., scales = "free")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(size = 8, hjust=0.8,vjust=0.2,angle = 90))

Al_depth_site_area_lineplot =
processed_ICP_grouped %>% 
  mutate(Depth_cm = factor(Depth_cm)) %>% 
  mutate(depth_area = paste(Depth_cm, Area, "-")) %>% 
  ggplot(aes(x = date_plot, y = mean_Al_ug_mL, color = Depth_cm)) +
  #geom_point(size = 2.5, alpha = 0.8)+
  geom_line(aes(linetype = Area, group = depth_area), size = 0.8)+
  scale_fill_manual(values = rev((pnw_palette("Anemone", 4))))+
  scale_color_manual(values = rev((pnw_palette("Anemone", 4))))+
  labs(linetype = "Acidity",
       color = "Depth, cm",
       y = "Al ug/mL",
       x = " ")+
  facet_grid(Site ~ ., scales = "free")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(size = 8, hjust=0.8,vjust=0.2,angle = 90))

P_depth_site_area_lineplot =
  processed_ICP_grouped %>% 
  mutate(Depth_cm = factor(Depth_cm)) %>% 
  mutate(depth_area = paste(Depth_cm, Area, "-")) %>% 
  ggplot(aes(x = date_plot, y = mean_P_ug_mL, color = Depth_cm)) +
  #geom_point(size = 2.5, alpha = 0.8)+
  geom_line(aes(linetype = Area, group = depth_area), size = 0.8)+
  scale_fill_manual(values = rev((pnw_palette("Anemone", 4))))+
  scale_color_manual(values = rev((pnw_palette("Anemone", 4))))+
  labs(linetype = "Acidity",
       color = "Depth, cm",
       y = "P ug/mL",
       x = " ")+
  facet_grid(Site ~ ., scales = "free")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(size = 8, hjust=0.8,vjust=0.2,angle = 90))

library(patchwork)

all_sitedeptharea_lineplot = Ca_depth_site_area_lineplot | Fe_depth_site_area_lineplot | Al_depth_site_area_lineplot | P_depth_site_area_lineplot | plot_layout(guides = "collect")

ggsave("figures_finalized/all_sitedeptharea_lineplot.png", plot = all_sitedeptharea_lineplot, width = 15, height = 6.5)



  

Ca_P_scatter_fig =
  processed_ICP_grouped_longer %>% 
  mutate(Depth_cm = factor(Depth_cm)) %>% 
  filter(ICP == c("mean_Ca_ug_mL")) %>% 
  ggplot(aes(x = mean_P_ug_mL, y = concentration, color = Depth_cm, shape = Depth_cm)) +
  geom_point(size = 2.5, alpha = 0.8)+
  scale_fill_manual(values = rev((pnw_palette("Anemone", 4))))+
  scale_color_manual(values = rev((pnw_palette("Anemone", 4))))+
  labs(shape = "Depth, cm",
       color = "Depth, cm",
       y = "Ca ug/mL",
       x = "P ug/mL")+
  facet_grid(. ~ Area, scales = "free")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(size = 8, hjust=0.8,vjust=0.2,angle = 90))

Fe_P_scatter_fig =
processed_ICP_grouped_longer %>% 
  mutate(Depth_cm = factor(Depth_cm)) %>% 
  filter(ICP == c("mean_Fe_ug_mL")) %>% 
  ggplot(aes(x = mean_P_ug_mL, y = concentration, color = Depth_cm, shape = Depth_cm)) +
  geom_point(size = 2.5, alpha = 0.8)+
  scale_fill_manual(values = rev((pnw_palette("Anemone", 4))))+
  scale_color_manual(values = rev((pnw_palette("Anemone", 4))))+
  labs(shape = "Depth, cm",
       color = "Depth, cm",
       y = "Fe ug/mL",
       x = "P ug/mL")+
  facet_grid(. ~ Area, scales = "free")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(size = 8, hjust=0.8,vjust=0.2,angle = 90))

Al_P_scatter_fig =
processed_ICP_grouped_longer %>% 
  mutate(Depth_cm = factor(Depth_cm)) %>% 
  filter(ICP == c("mean_Al_ug_mL")) %>% 
  ggplot(aes(x = mean_P_ug_mL, y = concentration, color = Depth_cm, shape = Depth_cm)) +
  geom_point(size = 2.5, alpha = 0.8)+
  scale_fill_manual(values = rev((pnw_palette("Anemone", 4))))+
  scale_color_manual(values = rev((pnw_palette("Anemone", 4))))+
  labs(shape = "Depth, cm",
       color = "Depth, cm",
       y = "Al ug/mL",
       x = "P ug/mL")+
  facet_grid(. ~ Area, scales = "free")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(size = 8, hjust=0.8,vjust=0.2,angle = 90))

Ca_Fe_Al_P_scatterfig = Ca_P_scatter_fig + Fe_P_scatter_fig + Al_P_scatter_fig + plot_layout(guides = "collect")

ggsave("output/Ca_Fe_Al_P_scatterfig.png", plot = Ca_Fe_Al_P_scatterfig, height = 3.5, width = 15)
  

Ca_P_scatter_fig =
  processed_ICP_grouped_longer %>% 
  mutate(Depth_cm = factor(Depth_cm)) %>% 
  filter(ICP == c("mean_Ca_ug_mL")) %>% 
  ggplot(aes(x = mean_P_ug_mL, y = concentration, color = Depth_cm, shape = Depth_cm)) +
  geom_point(size = 2.5, alpha = 0.8)+
  scale_fill_manual(values = rev((pnw_palette("Anemone", 4))))+
  scale_color_manual(values = rev((pnw_palette("Anemone", 4))))+
  labs(shape = "Depth, cm",
       color = "Depth, cm",
       y = "Ca ug/mL",
       x = "P ug/mL")+
  facet_grid(Site ~ ., scales = "free")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(size = 8, hjust=0.8,vjust=0.2,angle = 90))

Fe_P_scatter_fig =
  processed_ICP_grouped_longer %>% 
  mutate(Depth_cm = factor(Depth_cm)) %>% 
  filter(ICP == c("mean_Fe_ug_mL")) %>% 
  ggplot(aes(x = mean_P_ug_mL, y = concentration, color = Depth_cm, shape = Depth_cm)) +
  geom_point(size = 2.5, alpha = 0.8)+
  scale_fill_manual(values = rev((pnw_palette("Anemone", 4))))+
  scale_color_manual(values = rev((pnw_palette("Anemone", 4))))+
  labs(shape = "Depth, cm",
       color = "Depth, cm",
       y = "Fe ug/mL",
       x = "P ug/mL")+
  facet_grid(Site ~ ., scales = "free")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(size = 8, hjust=0.8,vjust=0.2,angle = 90))

Al_P_scatter_fig =
  processed_ICP_grouped_longer %>% 
  mutate(Depth_cm = factor(Depth_cm)) %>% 
  filter(ICP == c("mean_Al_ug_mL")) %>% 
  ggplot(aes(x = mean_P_ug_mL, y = concentration, color = Depth_cm, shape = Depth_cm)) +
  geom_point(size = 2.5, alpha = 0.8)+
  scale_fill_manual(values = rev((pnw_palette("Anemone", 4))))+
  scale_color_manual(values = rev((pnw_palette("Anemone", 4))))+
  labs(shape = "Depth, cm",
       color = "Depth, cm",
       y = "Al ug/mL",
       x = "P ug/mL")+
  facet_grid(Site ~ ., scales = "free")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(size = 8, hjust=0.8,vjust=0.2,angle = 90))

Ca_Fe_Al_P_scatterfig_bysite = Ca_P_scatter_fig + Fe_P_scatter_fig + Al_P_scatter_fig + plot_layout(guides = "collect")

ggsave("output/Ca_Fe_Al_P_scatterfig_bysite.png", plot = Ca_Fe_Al_P_scatterfig_bysite, height = 10, width = 15)


processed_ICP_grouped_longer %>% 
  mutate(Depth_cm = factor(Depth_cm)) %>% 
  filter(ICP == c("mean_Fe_ug_mL", "mean_Al_ug_mL", "mean_Ca_ug_mL")) %>% 
  ggplot(aes(x = mean_P_ug_mL, y = concentration, color = ICP, shape = Depth_cm)) +
  geom_point(size = 2.5, alpha = 0.8)+
  #geom_line(aes(color = ICP, group = ICP, fill = ICP), orientation = "X") +
  # scale_fill_gradientn(colors = (pnw_palette("Bay")))+
  # scale_color_gradientn(colors = (pnw_palette("Bay")))+
  scale_fill_manual(values = (pnw_palette("Bay", 4)))+
  scale_color_manual(values = (pnw_palette("Bay", 4)))+
  labs(fill = "Element",
       color = "Element",
       linetype = "Element",
       shape = "Depth, cm",
       y = "concentration, ug/mL",
       x = "date")+
  facet_grid(Site ~ Area, scales = "free")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text(size = 8, hjust=0.8,vjust=0.2,angle = 90))

















line_Fe_fig =
processed_ICP_grouped %>% 
  ggplot(aes(x = date_plot, y = mean_Fe_ug_mL, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.7, orientation = "x")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  labs(color = "Area",
       y = "Iron ug/mL",
       x = " ")+
  facet_grid(Depth_cm ~ .) +
  theme_er1()+
  theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

line_Mn_fig =
  processed_ICP_grouped %>% 
  ggplot(aes(x = date_plot, y = mean_Mn_ug_mL, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.7, orientation = "x")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  labs(color = "Area",
       y = "Manganese ug/mL",
       x = " ")+
  facet_grid(Depth_cm ~ .) +
  theme_er1()+
  theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


ggsave("output/line_Mn_fig.png", plot = line_Mn_fig, height = 10, width = 6.5)

line_Ca_fig =
  processed_ICP_grouped %>% 
  ggplot(aes(x = date_plot, y = mean_Ca_ug_mL, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.7, orientation = "x")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  labs(color = "Area",
       y = "Calcium ug/mL",
       x = " ")+
  facet_grid(Depth_cm ~ .) +
  theme_er1()+
  theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


ggsave("output/line_Ca_fig.png", plot = line_Ca_fig, height = 10, width = 6.5)

line_Al_fig =
  processed_ICP_grouped %>% 
  ggplot(aes(x = date_plot, y = mean_Al_ug_mL, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.7, orientation = "x")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  labs(color = "Area",
       y = "Aluminum ug/mL",
       x = " ")+
  facet_grid(Depth_cm ~ .) +
  theme_er1()+
  theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


ggsave("output/line_Al_fig.png", plot = line_Al_fig, height = 10, width = 6.5)

line_P_fig =
  processed_ICP_grouped %>% 
  ggplot(aes(x = date_plot, y = mean_P_ug_mL, color = Site)) +
  geom_line(aes(group = area_site, linetype = Area), size = 0.7, orientation = "x")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  labs(color = "Moisture",
       linetype = "Acidity",
       y = "Phosphorus ug/mL",
       x = " ")+
  facet_grid(Depth_cm ~ .) +
  theme_er1()+
  theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


ggsave("output/line_P_fig.png", plot = line_P_fig, height = 10, width = 6.5)

processed_ICP_grouped_longer %>% 
  filter(ICP == c("mean_Fe_ug_mL", "mean_Al_ug_mL", "mean_Ca_ug_mL")) %>% 
  ggplot(aes(x = date_plot, y = concentration, color = ICP)) +
  geom_line(aes(group = ICP, linetype = Area), size = 0.7, orientation = "x")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_fill_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  # scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  labs(
        color = "Moisture",
       linetype = "Acidity",
       y = "concentration ug/mL",
       x = " ")+
  facet_wrap(area_site ~ .) +
  theme_er1()+
  theme(axis.text.x = element_text(size = 8, hjust=0.25,vjust=0.2,angle = 45), legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


processed_ICP_grouped %>% 
  filter(Site == "Dry") %>% 
  ggplot(aes(x = date, y = Depth_cm, fill = mean_Fe_ug_mL, group = date)) +
  geom_point(size = 4, shape = c(21))+
  #geom_line(orientation = "y")+
  scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  labs(fill = "Fe ug/mL",
       y = "Depth, cm")+
  scale_y_reverse()+
  facet_grid(Site ~ Area)+
  theme_er1()


processed_ICP_grouped %>% 
  filter(Site == "Mesic") %>% 
  ggplot(aes(x = date, y = Depth_cm, fill = mean_Fe_ug_mL, group = date)) +
  geom_point(size = 4, shape = c(21))+
  #geom_line(orientation = "y")+
  scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
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
