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

