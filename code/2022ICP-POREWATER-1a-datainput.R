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
  pivot_longer(-c(SampleID, date, Time, Area, Site, Plot, day, month, year, Depth_cm), names_to = 'ICP', values_to = 'concentration') 

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



Fe_2022_fig =
  processed_ICP_grouped %>% 
  ggplot(aes(x = date, y = Depth_cm, fill = mean_Fe_ug_mL, group = date)) +
  geom_point(size = 4, shape = c(21))+
  #geom_line(orientation = "y")+
  scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  labs(fill = "Fe ug/mL",
       y = "Depth, cm")+
  scale_y_reverse()+
  facet_grid(Site ~ Area)+
  theme_er1()


#Fe_2022_fig_hydric =
  processed_ICP_grouped %>% 
  filter(Area == "non-acidic tundra") %>% 
  ggplot(aes(x = mean_Fe_ug_mL, y = Depth_cm, fill = Site, group = Site)) +
  geom_point(size = 4, shape = c(21), alpha = 0.8)+
  geom_line(aes(color = Site), orientation = "y", linetype = "longdash")+
  #scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Shuksan2")))+
  scale_color_manual(values = (pnw_palette("Shuksan2")))+
  labs(fill = "Fe ug/mL",
       y = "Depth, cm")+
  scale_y_reverse()+
  facet_grid(Area ~ date)+
  theme_er1()
  
Fe_grouped_fig =  
  processed_ICP_grouped %>% 
    #filter(Area == "non-acidic tundra") %>% 
    ggplot(aes(x = mean_Fe_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
    geom_errorbar(aes(xmin=(mean_Fe_ug_mL - sd_Fe_ug_mL), xmax=(mean_Fe_ug_mL + sd_Fe_ug_mL), color = date_plot))+
    geom_point(size = 3, shape = c(21), alpha = 0.8)+
    geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
    # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
    # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
    scale_fill_manual(values = (pnw_palette("Sunset2", 15)))+
    scale_color_manual(values = (pnw_palette("Sunset2", 15)))+
    labs(fill = "Date",
         color = "Date",
         y = "Depth, cm",
         x = "Iron ug/mL")+
    scale_y_reverse()+
    facet_grid(Site ~ Area, scales = "free_x")+
    theme_er1()+
    theme(legend.position = "none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())

P_grouped_fig =  
  processed_ICP_grouped %>% 
    #filter(Area == "non-acidic tundra") %>% 
    ggplot(aes(x = mean_P_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
    geom_errorbar(aes(xmin=(mean_P_ug_mL - sd_P_ug_mL), xmax=(mean_P_ug_mL + sd_P_ug_mL), color = date_plot))+
    geom_point(size = 3, shape = c(21), alpha = 0.8)+
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
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
    theme(legend.position = "none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.text.x = element_text (size = 9))


Mn_grouped_fig =  
  processed_ICP_grouped %>% 
  #filter(Area == "non-acidic tundra") %>% 
  ggplot(aes(x = mean_Mn_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
  geom_errorbar(aes(xmin=(mean_Mn_ug_mL - sd_Mn_ug_mL), xmax=(mean_Mn_ug_mL + sd_Mn_ug_mL), color = date_plot))+
  geom_point(size = 3, shape = c(21), alpha = 0.8)+
  geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset2", 15)))+
  scale_color_manual(values = (pnw_palette("Sunset2", 15)))+
  labs(fill = "Date",
       color = "Date",
       y = "Depth, cm",
       x = "Manganese ug/mL")+
  scale_y_reverse()+
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


Legend_grouped_fig =  
  processed_ICP_grouped %>% 
  #filter(Area == "non-acidic tundra") %>% 
  ggplot(aes(x = mean_Mn_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
  geom_point(size = 3, shape = c(21), alpha = 0.8)+
  geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset2", 15)))+
  scale_color_manual(values = (pnw_palette("Sunset2", 15)))+
  labs(fill = "Date",
       color = "Date",
       y = "Depth, cm",
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
  geom_point(size = 3, shape = c(21), alpha = 0.8)+
  geom_line(aes(color = date_plot), orientation = "y", linetype = "longdash")+
  # scale_fill_gradientn(colors = (pnw_palette("Shuksan2")))+
  # scale_color_gradientn(colors = (pnw_palette("Shuksan2")))+
  scale_fill_manual(values = (pnw_palette("Sunset2", 15)))+
  scale_color_manual(values = (pnw_palette("Sunset2", 15)))+
  labs(fill = "Date",
       color = "Date",
       y = "Depth, cm",
       x = "Aluminum ug/mL")+
  scale_y_reverse()+
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

Ca_grouped_fig =  
  processed_ICP_grouped %>% 
  #filter(Area == "non-acidic tundra") %>% 
  ggplot(aes(x = mean_Ca_ug_mL, y = Depth_cm, fill = date_plot, group = date_plot)) +
  geom_errorbar(aes(xmin=(mean_Ca_ug_mL - sd_Ca_ug_mL), xmax=(mean_Ca_ug_mL + sd_Ca_ug_mL), color = date_plot))+
  geom_point(size = 3, shape = c(21), alpha = 0.8)+
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
  facet_grid(Site ~ Area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text (size = 9))


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
        panel.background = element_blank())


ggsave("output/2022_Mg_fig2.png", plot = Mg_grouped_fig, height = 6, width = 4)
ggsave("output/2022_K_fig2.png", plot = K_grouped_fig, height = 6, width = 4)
ggsave("output/2022_Na_fig2.png", plot = Na_grouped_fig, height = 6, width = 4)
ggsave("output/2022_Al_fig2.png", plot = Al_grouped_fig, height = 6, width = 4)
ggsave("output/2022_Ca_fig2.png", plot = Ca_grouped_fig, height = 6, width = 4)
ggsave("output/2022_fig2_legend.png", plot = Legend_grouped_fig, height = 10, width = 10)



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
