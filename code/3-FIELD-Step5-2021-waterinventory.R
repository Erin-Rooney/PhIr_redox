#ECROONEY
#August 30 2023

#load all packages

source("code/0-packages.R")

#load data

water_dat = read.csv("raw/2021_waterinventory.csv") %>% 
  na.omit()

water_sum =
  water_dat %>% 
  group_by(Area, Site, Depth_cm) %>% 
  mutate(Solution_pH = as.numeric(Solution_pH)) %>% 
  dplyr::summarize(mean_pH = round(mean(Solution_pH), 3),
                   sd_pH = round(sd(Solution_pH), 2)) %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>%
  mutate(Area = recode(Area, "East" = "acidic tundra",
                       "West" = "non-acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(Depth_cm = as.numeric(Depth_cm))
  


water_sum %>% 
  ggplot()+
  geom_line(aes(x = mean_pH, y = Depth_cm, color = Site, group = Site), orientation = "y", size = 1)+
  geom_ribbon(aes(xmin = mean_pH-sd_pH, xmax = mean_pH+sd_pH, 
                  y = Depth_cm, fill = Site, color = Site, group = "Site"), alpha = 0.4)+
  scale_color_manual(values = c("#bc4749", "#35a55f", "#0582ca"))+
  scale_fill_manual(values = c("#bc4749", "#35a55f", "#0582ca"))+
  scale_y_reverse()+
  xlim(0,10)+
  labs(x = "porewater pH",
       y = "Depth (cm)")+
  facet_grid(Site ~ Area)+
  theme_er1()+
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))


pH_fig =
water_sum %>% 
  ggplot()+
  geom_line(aes(x = mean_pH, y = Depth_cm, color = Area, group = Area), orientation = "y", size = 1)+
  geom_ribbon(aes(xmin = mean_pH-sd_pH, xmax = mean_pH+sd_pH, 
                  y = Depth_cm, fill = Area, color = Area, group = Area), alpha = 0.4)+
  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  ylim(40,0)+
  xlim(0,10)+
  labs(x = "porewater pH",
       y = "Depth (cm)")+
  guides(color = guide_legend(nrow = 2))+
  facet_grid(Site ~ .)+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))

ggsave("output/pH_fig.png", plot = pH_fig, height = 6.5, width = 2)

