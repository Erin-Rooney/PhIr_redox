#Raw data input
#sipper data from Erin VanderJeugdt
#E C Rooney
#8 30 2023


#load packages

source("code/0-packages.R")

#load document

anions_dat = read.csv("raw/PhIr2022_porwater_IC anions_master_ev.csv") %>% 
  janitor::clean_names() %>% 
  mutate(area = recode(area, "East" = "acidic tundra",
                       "West" = "non-acidic tundra")) %>% 
  mutate(area = factor(area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(site = factor(site, levels = c("Dry", "Mesic", "Hydric"))) 

nitrate_summary =
  anions_dat %>% 
  dplyr::select(c(area, site, depth_cm, no3n_ppm)) %>% 
  na.omit() %>% 
  group_by(area, site, depth_cm) %>% 
  dplyr::summarize(mean_no3n_ppm = round(mean(no3n_ppm), 3),
                   sd_no3n_ppm = round(sd(no3n_ppm), 2))


phosphate_summary =
  anions_dat %>% 
  dplyr::select(c(area, site, depth_cm, po4_p_ppm)) %>% 
  na.omit() %>% 
  group_by(area, site, depth_cm) %>% 
  dplyr::summarize(mean_po4_p_ppm = round(mean(po4_p_ppm), 3),
                   sd_po4_p_ppm = round(sd(po4_p_ppm), 2))

nitrate_fig =
nitrate_summary %>% 
  ggplot()+
  geom_point(aes(x = mean_no3n_ppm, y = depth_cm, color = area, shape = area), size = 2.5)+
  geom_line(aes(x = mean_no3n_ppm, y = depth_cm, color = area, group = area), orientation = "y", size = 1)+
  geom_ribbon(aes(xmin = mean_no3n_ppm-sd_no3n_ppm, xmax = mean_no3n_ppm+sd_no3n_ppm, 
                  y = depth_cm, fill = area, color = area, group = area), alpha = 0.4, size = 0.2)+
  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_y_reverse()+
  scale_x_continuous(position="bottom", breaks = c(0, 0.5, 1.0, 1.5), n.breaks=4, limits = c(-0.2, 1.7))+
  labs(x = "Nitrate (ppm)",
       y = "Depth (cm)",
       color = " ",
       fill = " ")+
  facet_grid(site ~ .)+
  guides(color = guide_legend(nrow = 2))+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
        strip.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave("output/nitrate_line_fig.png", plot = nitrate_fig, height = 8, width = 1.5)


nitrate_fig2 =
  nitrate_summary %>% 
  ggplot()+
  geom_point(aes(x = mean_no3n_ppm, y = depth_cm, color = area, shape = area), size = 2)+
  geom_line(aes(x = mean_no3n_ppm, y = depth_cm, color = area, group = area), orientation = "y", size = 0.5)+
  geom_errorbar(aes(xmin = mean_no3n_ppm-sd_no3n_ppm, xmax = mean_no3n_ppm+sd_no3n_ppm, 
                  y = depth_cm, fill = area, color = area), alpha = 0.5, width = 1)+
  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_y_reverse()+
  scale_x_continuous(position="bottom", breaks = c(0, 0.5, 1.0, 1.5), n.breaks=4, limits = c(-0.2, 1.7))+
  labs(x = "Nitrate (ppm)",
       y = "Depth (cm)",
       color = " ",
       fill = " ")+
  facet_grid(site ~ .)+
  guides(color = guide_legend(nrow = 2))+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
        strip.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave("output/nitrate_line_fig2.png", plot = nitrate_fig2, height = 8, width = 1.5)


phosphate_fig =
  phosphate_summary %>% 
  ggplot()+
  # geom_line(aes(x = mean_po4_p_ppm, y = depth_cm, color = area, group = area), orientation = "y", size = 1)+
  # geom_ribbon(aes(xmin = mean_po4_p_ppm-sd_po4_p_ppm, xmax = mean_po4_p_ppm+sd_po4_p_ppm, 
  #                 y = depth_cm, fill = area, color = area, group = area), alpha = 0.4, size = 0.2)+
  # geom_point(aes(x = mean_po4_p_ppm, y = depth_cm, color = area, shape = area), size = 2)+
  geom_line(aes(x = mean_po4_p_ppm, y = depth_cm, color = area, group = area), orientation = "y", size = 0.5)+
  geom_errorbar(aes(xmin = mean_po4_p_ppm-sd_po4_p_ppm, xmax = mean_po4_p_ppm+sd_po4_p_ppm, 
                    y = depth_cm, fill = area, color = area), alpha = 0.5, width = 1)+
  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_y_reverse()+
  #scale_x_continuous(position="bottom", breaks = c(0, 0.5, 1.0, 1.5), n.breaks=4, limits = c(-0.2, 1.7))+
  labs(x = "Phosphate (ppm)",
       y = "Depth (cm)",
       color = " ",
       fill = " ")+
  facet_grid(site ~ .)+
  guides(color = guide_legend(nrow = 2))+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
        strip.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave("output/phosphate_line_fig.png", plot = phosphate_fig, height = 8, width = 1.5)
