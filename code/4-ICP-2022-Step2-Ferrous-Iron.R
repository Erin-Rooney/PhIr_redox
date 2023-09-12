#Raw data input
#sipper data
#E C Rooney
#1 21 2023


#load packages

source("code/0-packages.R")

#load document

raw_Fe2 = read.csv("raw/PhIr_SOIL_GEOCHEM_results.csv")

Fe2_dat =
  raw_Fe2 %>% 
  separate(sample_date, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(year = recode(year, "22" = "2022")) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(month2 = recode(month, "6" = "June", "7" = "July", "8" = "August", "9" = "September")) %>%
  mutate(grouping = paste0(area, "-", site, "-", date, "-", plot)) %>% 
  mutate(month2 = factor(month2, levels = c("June", "July", "August", "September"))) %>% 
  mutate(area = factor(area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(site = factor(site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(Fe2totalratio = (FeII/Fe))

Fe2_grouped =
  Fe2_dat %>% 
  group_by(area, site, depth_cm) %>% 
  dplyr::summarize(mean_Fe = round(mean(Fe), 3),
                   sd_Fe = round(sd(Fe), 2),
                   mean_Al = round(mean(Al), 3),
                   sd_Al = round(sd(Al), 2),
                   mean_Ca = round(mean(Ca), 3),
                   sd_Ca = round(sd(Ca), 2),
                   mean_Mn = round(mean(Mn), 3),
                   sd_Mn = round(sd(Mn), 2),
                   mean_Mg = round(mean(Mg), 3),
                   sd_Mg = round(sd(Mg), 2),
                   mean_FeII = round(mean(FeII), 3),
                   sd_FeII = round(sd(FeII), 2)) %>% 
  mutate(area = factor(area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(site = factor(site, levels = c("Dry", "Mesic", "Hydric"))) 

Fe2_grouped2 =
  Fe2_dat %>% 
  group_by(area, site, depth_cm) %>%
  na.omit() %>% 
  dplyr::summarize(mean_Fe = round(mean(Fe), 3),
                   sd_Fe = round(sd(Fe), 2),
                   mean_FeII = round(mean(FeII), 3),
                   sd_FeII = round(sd(FeII), 2)) %>% 
  mutate(area = factor(area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(site = factor(site, levels = c("Dry", "Mesic", "Hydric"))) 
  
total_Fe_line_fig =
Fe2_grouped %>% 
  ggplot()+
  geom_point(aes(x = mean_Fe, y = depth_cm, color = area, shape = area), size = 2.5)+
  geom_line(aes(x = mean_Fe, y = depth_cm, color = area, group = area), orientation = "y", size = 1)+
  geom_ribbon(aes(xmin = mean_Fe-sd_Fe, xmax = mean_Fe+sd_Fe, 
                  y = depth_cm, fill = area, color = area, group = area), alpha = 0.4, size = 0.2)+
  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_y_reverse()+
 # xlim(0, 50)+
  labs(x = "total Fe (mg/L)",
       y = "Depth (cm)",
       color = " ",
       fill = " ")+
  facet_grid(site ~ .)+
  guides(color = guide_legend(nrow = 2))+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
        strip.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave("output/total_Fe_line_fig.png", plot = total_Fe_line_fig, height = 8, width = 1.5)

total_Mg_line_fig =
  Fe2_grouped %>% 
  ggplot()+
  geom_point(aes(x = mean_Mg, y = depth_cm, color = area, shape = area), size = 2.5)+
  geom_line(aes(x = mean_Mg, y = depth_cm, color = area, group = area), orientation = "y", size = 1)+
  geom_ribbon(aes(xmin = mean_Mg-sd_Mg, xmax = mean_Mg+sd_Mg, 
                  y = depth_cm, fill = area, color = area, group = area), alpha = 0.4, size = 0.2)+
  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_y_reverse()+
  # xlim(0, 50)+
  labs(x = "total Mg (mg/L)",
       y = "Depth (cm)",
       color = " ",
       fill = " ")+
  facet_grid(site ~ .)+
  guides(color = guide_legend(nrow = 2))+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
        strip.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave("output/total_Mg_line_fig.png", plot = total_Mg_line_fig, height = 8, width = 1.5)

total_Ca_line_fig =
  Fe2_grouped %>% 
  ggplot()+
  geom_point(aes(x = mean_Ca, y = depth_cm, color = area, shape = area), size = 2.5)+
  geom_line(aes(x = mean_Ca, y = depth_cm, color = area, group = area), orientation = "y", size = 1)+
  geom_ribbon(aes(xmin = mean_Ca-sd_Ca, xmax = mean_Ca+sd_Ca, 
                  y = depth_cm, fill = area, color = area, group = area), alpha = 0.4, size = 0.2)+
  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_y_reverse()+
  # xlim(0, 50)+
  labs(x = "total Ca (mg/L)",
       y = "Depth (cm)",
       color = " ",
       fill = " ")+
  facet_grid(site ~ .)+
  guides(color = guide_legend(nrow = 2))+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
        strip.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave("output/total_Ca_line_fig.png", plot = total_Ca_line_fig, height = 8, width = 1.5)

total_Mn_line_fig =
  Fe2_grouped %>% 
  ggplot()+
  geom_point(aes(x = mean_Mn, y = depth_cm, color = area, shape = area), size = 2.5)+
  geom_line(aes(x = mean_Mn, y = depth_cm, color = area, group = area), orientation = "y", size = 1)+
  geom_ribbon(aes(xmin = mean_Mn-sd_Mn, xmax = mean_Mn+sd_Mn, 
                  y = depth_cm, fill = area, color = area, group = area), alpha = 0.4, size = 0.2)+
  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_y_reverse()+
  # xlim(0, 50)+
  labs(x = "total Mn (mg/L)",
       y = "Depth (cm)",
       color = " ",
       fill = " ")+
  facet_grid(site ~ .)+
  guides(color = guide_legend(nrow = 2))+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
        strip.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave("output/total_Mn_line_fig.png", plot = total_Mn_line_fig, height = 8, width = 1.5)

total_Al_line_fig =
  Fe2_grouped %>% 
  ggplot()+
  geom_point(aes(x = mean_Al, y = depth_cm, color = area, shape = area), size = 2.5)+
  geom_line(aes(x = mean_Al, y = depth_cm, color = area, group = area), orientation = "y", size = 1)+
  geom_ribbon(aes(xmin = mean_Al-sd_Al, xmax = mean_Al+sd_Al, 
                  y = depth_cm, fill = area, color = area, group = area), alpha = 0.4, size = 0.2)+
  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_y_reverse()+
  # xlim(0, 50)+
  labs(x = "total Al (mg/L)",
       y = "Depth (cm)",
       color = " ",
       fill = " ")+
  facet_grid(site ~ .)+
  guides(color = guide_legend(nrow = 2))+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
        strip.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave("output/total_Al_line_fig.png", plot = total_Al_line_fig, height = 8, width = 1.5)



Ferrous_Fe_line_fig =
  Fe2_grouped2 %>% 
  ggplot()+
  geom_point(aes(x = mean_FeII, y = depth_cm, color = area, shape = area), size = 2.5)+
  geom_line(aes(x = mean_FeII, y = depth_cm, color = area, group = area), orientation = "y", size = 1)+
  geom_ribbon(aes(xmin = mean_FeII-sd_FeII, xmax = mean_FeII+sd_FeII, 
                  y = depth_cm, fill = area, color = area, group = area), alpha = 0.4, size = 0.2)+  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_y_reverse()+
  # xlim(0, 50)+
  labs(x = "Fe(II) (mg/L)",
       y = "Depth (cm)",
       color = " ",
       fill = " ")+
  facet_grid(site ~ .)+
  guides(color = guide_legend(nrow = 2))+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave("output/Ferrous_Fe_line_fig.png", plot = Ferrous_Fe_line_fig, height = 8, width = 1.65)


total_Fe_point_fig =
  Fe2_grouped %>% 
  ggplot()+
  geom_point(aes(x = mean_Fe, y = depth_cm, color = area, shape = area), size = 2)+
  geom_line(aes(x = mean_Fe, y = depth_cm, color = area, group = area), orientation = "y", size = 0.5)+
  geom_errorbar(aes(xmin=mean_Fe-sd_Fe, xmax = mean_Fe+sd_Fe, y = depth_cm, color = area), alpha = 0.5, width = 1, show.legend = FALSE)+
  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_y_reverse()+
  # xlim(0, 50)+
  labs(x = "total Fe (mg/L)",
       y = "Depth (cm)",
       color = " ",
       fill = " ")+
  facet_grid(site ~ .)+
  guides(color = guide_legend(nrow = 2))+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
        strip.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave("output/total_Fe_point_fig.png", plot = total_Fe_point_fig, height = 8, width = 1.5)


Ferrous_Fe_point_fig =
  Fe2_grouped2 %>% 
  ggplot()+
  geom_point(aes(x = mean_FeII, y = depth_cm, color = area, group = area, shape = area), size = 2)+
  geom_line(aes(x = mean_FeII, y = depth_cm, color = area, group = area), orientation = "y", size = 0.5)+
  geom_errorbar(aes(xmin=mean_FeII-sd_FeII, xmax = mean_FeII+sd_FeII, y = depth_cm, color = area), alpha = 0.5, width = 1, show.legend = FALSE)+
  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_y_reverse()+
  # xlim(0, 50)+
  labs(x = "Fe(II) (mg/L)",
       y = "Depth (cm)",
       color = " ",
       fill = " ")+
  facet_grid(site ~ .)+
  guides(color = guide_legend(nrow = 2))+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave("output/Ferrous_Fe_point_fig.png", plot = Ferrous_Fe_point_fig, height = 8, width = 1.65)




Fe2_dat %>% 
  ggplot() +
  geom_point(aes(x = FeII, y = depth_cm, color = month2), size = 2.5, alpha = 0.75, shape = c(21))+
  geom_line(aes(x = FeII, y = depth_cm, color = month2, group = grouping), orientation = "y", size = 0.75, linetype = "dotted")+
  labs(x = "Ferrous Iron, μg/ml",
       y = "depth, cm",
       color = "month, 2022"
  )+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  # scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_y_reverse()+
  facet_grid(site~area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        #axis.text.y=element_blank(),  #remove y axis labels
        #axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 9, hjust=0.8,vjust=0.2,angle = 90)) 

Fe2_dat %>% 
  ggplot() +
  geom_point(aes(x = Fe, y = depth_cm, color = month2), size = 2.5, alpha = 0.75)+
  geom_line(aes(x = Fe, y = depth_cm, color = month2, group = grouping), orientation = "y", size = 0.75, linetype = "dotted")+
  labs(x = "Total Iron, μg/ml",
       y = "depth, cm",
       color = "month, 2022"
  )+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  # scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_y_reverse()+
  facet_grid(site~area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        #strip.text.y = element_blank(),
        #axis.text.y=element_blank(),  #remove y axis labels
        #axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 9, hjust=0.8,vjust=0.2,angle = 90)) 

ratio_fig =
Fe2_dat %>% 
  filter(site == "Hydric") %>% 
  ggplot() +
  geom_point(aes(x = Fe2totalratio, y = depth_cm, fill = month2), shape = c(21), size = 3, alpha = 0.75)+
  geom_line(aes(x = Fe2totalratio, y = depth_cm, color = month2, group = grouping), orientation = "y", size = 0.75, alpha =0.3, linetype = "dashed")+
  labs(x = "Ferrous Iron to Total Iron Ratio",
       y = "Depth, cm",
       color = "month, 2022"
  )+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  # scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_y_reverse(breaks = c(0, 20, 40))+
  facet_grid(site~area)+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside", panel.border = element_rect(color="gray",size=0.25, fill = NA))
        #strip.text.y = element_blank()) 

ggsave("output/fe_ratio_plot.png", plot = ratio_fig, height = 3, width = 5)


fe2_fig =
  Fe2_dat %>% 
  ggplot() +
  geom_point(aes(x = FeII, y = depth_cm, color = month2), size = 2.5, alpha = 0.75)+
  geom_line(aes(x = FeII, y = depth_cm, color = month2, group = grouping), orientation = "y", size = 0.75, linetype = "dotted")+
  labs(x = "Ferrous Iron, μg/ml",
       y = "depth, cm",
       color = "month, 2022"
  )+
  scale_color_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  # scale_color_manual(values = c("#f07167", "#a7c957", "#1e96fc", "#f07167", "#a7c957", "#1e96fc"))+
  scale_y_reverse()+
  facet_grid(site~area, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        #axis.text.y=element_blank(),  #remove y axis labels
        #axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 9, hjust=0.8,vjust=0.2,angle = 90)) 

library(patchwork)


fe_plot = fe2_fig + ratio_fig + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

# ggsave("output/2022nutrientsfigA.png", plot = nutrientsfigA, height = 9, width = 11)
# ggsave("output/2022nutrientsfigB.png", plot = nutrientsfigB, height = 10, width = 8)
ggsave("output/fe_plot.png", plot = fe_plot, height = 9, width = 9.5)

