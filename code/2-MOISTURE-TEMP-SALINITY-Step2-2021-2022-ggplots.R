#moisture and temp data
#2021 and 2022
#load all packages


#PhIr data
#redox contour plots
#redox line/dot plots

source("code/0-packages.R")


###temp----

final_temp_sal_moist = read.csv("processed/final_temp_salinity_avgs.csv")
final_temp_sal_moist2022 = read.csv("processed/2022final_temp_salinity_avgs.csv")


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
  mutate(month = recode(month,  "6" = "June", 
                        "7" = "July", 
                        "8" = "August",
                        "9" = "September")) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  dplyr::rename(depth_cm = depth) %>% 
  na.omit() %>% 
  mutate(frozen = case_when(temp < -1 ~ "frozen", TRUE ~ "unfrozen")) 
  

final_temp_sal_moist_forfig2022 =
  final_temp_sal_moist2022 %>% 
  mutate(forsep = TIMESTAMP) %>% 
  separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
  separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(datetime = ymd_hm(paste(date, time)),
         date = ymd(date)) %>% 
  separate(forsep, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "/", into = c("month", "day", "year")) %>% 
  mutate(month = recode(month,  "6" = "early summer", 
                        "7" = "mid summer", 
                        "8" = "late summer",
                        "9" = "early fall")) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  dplyr::rename(depth_cm = depth) %>% 
  na.omit() %>% 
  mutate(frozen = case_when(temp < -1 ~ "frozen", TRUE ~ "unfrozen")) 



grouped_moisture_forfigs_temporal = 
  final_temp_sal_moist_forfig %>% 
  filter(moisture > 1) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  group_by(site, position, depth_cm, month) %>% 
  dplyr::summarise(moisture_avg = round(mean(moisture),2),
                   moisture_sd = round(sd(moisture),2),
                   moisture_se = round(sd(moisture)/sqrt(n()),2))


grouped_moisture_forfigs_temporal2022 = 
  final_temp_sal_moist_forfig2022 %>% 
  filter(moisture > 1) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  group_by(site, position, depth_cm, month) %>% 
  dplyr::summarise(moisture_avg = round(mean(moisture),2),
                   moisture_sd = round(sd(moisture),2),
                   moisture_se = round(sd(moisture)/sqrt(n()),2))

grouped_moisture2022 =
  final_temp_sal_moist_forfig2022 %>% 
  filter(moisture > 10) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  group_by(site, position, depth_cm) %>% 
  dplyr::summarise(moisture_avg = round(mean(moisture),2),
                   moisture_sd = round(sd(moisture),2),
                   moisture_se = round(sd(moisture)/sqrt(n()),2))


nonacidic_moisture_fig =
grouped_moisture %>% 
  filter(site == "non-acidic tundra") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = moisture_avg, color = position, fill = position), group = 'position')+
  geom_point(size = 3.5, alpha = 0.8, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype="longdash", alpha = 0.3)+
  geom_errorbar(aes(xmin=moisture_avg-moisture_sd, xmax=moisture_avg+moisture_sd), show.legend = FALSE, width = 1)+
  scale_color_manual(values = c("#bc4749", "#35a55f", "#0582ca"))+
  scale_fill_manual(values = c("#bc4749", "#35a55f", "#0582ca"))+
  # scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  # scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  ylim(60, 0)+
  labs(
    x = ' 
       soil moisture, %
    ',
    y = "depth (cm)",
    color = "", fill = "")+
  scale_x_continuous(position="top", breaks = c(0, 20, 40, 60), n.breaks=4, limits = c(0, 60))+
  facet_grid(.~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))
# 

ggsave("formanuscript/moisturefig_depth_sd2021_nonacidic.png", plot = nonacidic_moisture_fig, height = 4.5, width = 2.25)


moist_simple_fig =
  grouped_moisture2022 %>% 
  ggplot()+
  geom_line(aes(x = moisture_avg, y = depth_cm, color = site, group = site), orientation = "y", size = 1)+
  geom_ribbon(aes(xmin = moisture_avg-moisture_sd, xmax = moisture_avg+moisture_sd, 
                  y = depth_cm, fill = site, color = site, group = site), alpha = 0.4)+ 
  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  ylim(40,0)+
  xlim(0,60)+
  labs(x = "soil moisture (%)",
       y = "Depth (cm)")+
  guides(color = guide_legend(nrow = 2))+
  facet_grid(position ~ .)+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
        strip.text.y = element_blank())

ggsave("output/moist_simple_fig.png", plot = moist_simple_fig, height = 8, width = 1.75)


moist_simplepoint_fig =
  grouped_moisture2022 %>% 
  ggplot()+
  geom_point(aes(x = moisture_avg, y = depth_cm, color = site, group = site, shape = site), size = 2) +
  geom_line(aes(x = moisture_avg, y = depth_cm, color = site, group = site), orientation = "y", size = 0.5)+
  geom_errorbar(aes(xmin=moisture_avg-moisture_sd, xmax=moisture_avg+moisture_sd, y = depth_cm, color = site), alpha = 0.5, show.legend = FALSE, width = 1)+
  scale_color_manual(values = c("#5aaa95", "#bb9f06"))+
  scale_fill_manual(values = c("#5aaa95", "#bb9f06"))+
  ylim(40,0)+
  xlim(0,60)+
  labs(x = "soil moisture (%)",
       y = "depth (cm)")+
  guides(color = guide_legend(nrow = 2))+
  facet_grid(position ~ .)+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
        strip.text.y = element_blank())

ggsave("output/moist_simplepoint_fig.png", plot = moist_simplepoint_fig, height = 8, width = 1.9)





moisturefig_temporal =
  grouped_moisture_forfigs_temporal %>% 
  mutate(month = factor(month, levels = c("June", "July", "August", "September")))   %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = moisture_avg, color = position, fill = position), group = 'position')+
  geom_point(size = 3, alpha = 0.4, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE)+
  geom_errorbar(aes(xmin=moisture_avg-moisture_sd, xmax=moisture_avg+moisture_sd), show.legend = FALSE)+
  scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  ylim(60, 0)+
  labs(x = 'soil moisture (%)',
       y = "depth (cm)",
       color = "", fill = "")+
  scale_x_continuous(position="top")+
  facet_grid(site~month, switch = "x")+
  theme_er1()

ggsave("formanuscript/moisture_temporal_fig.tiff", plot = moisturefig_temporal, height = 7, width = 7)

moisturefig_temporal =
  grouped_moisture_forfigs_temporal %>%
  mutate(month = factor(month, levels = c("June", "July", "August", "September")))   %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = depth_cm, x = moisture_avg, color = month, fill = month), group = 'month')+
  geom_point(size = 4, alpha = 0.8, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype = "longdash")+
  geom_errorbar(aes(xmin=moisture_avg-moisture_sd, xmax=moisture_avg+moisture_sd), show.legend = FALSE)+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  # scale_color_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  # scale_fill_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  ylim(60, 0)+
  labs(x = '2021
       soil moisture (%)',
       y = "depth (cm)",
       color = "", fill = "")+
  scale_x_continuous(position="top", breaks = c(10, 20, 30, 40, 50), n.breaks=5, limits = c(0, 60))+
  facet_grid(position~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        axis.text.x = element_text(size = 7.5, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks

moisturefig_temporal_nonacidic_hydric =
  grouped_moisture_forfigs_temporal %>%
  filter(site == "non-acidic tundra" & position == "hydric") %>%
  mutate(month = factor(month, levels = c("June", "July", "August", "September")))   %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = depth_cm, x = moisture_avg, color = month, fill = month), group = 'month')+
  geom_point(size = 3.75, alpha = 0.8, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype = "longdash", alpha = 0.3)+
  geom_errorbar(aes(xmin=moisture_avg-moisture_sd, xmax=moisture_avg+moisture_sd), show.legend = FALSE,
                width=1.5,
                size=0.5)+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  # scale_color_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  # scale_fill_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  ylim(60, 0)+
  labs(x = 'Hydric
       soil moisture (%)
       ',
       y = "depth (cm)",
       color = "", fill = "")+
  scale_x_continuous(position="top", breaks = c(0, 20, 40, 60), n.breaks=4, limits = c(0, 60))+
  facet_grid(position~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))
        #axis.text.x = element_text(size = 12, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks

  
ggsave("formanuscript/moisturefig_temporal_nonacidic_hydric.png", plot = moisturefig_temporal_nonacidic_hydric, height = 4.5, width = 2.1)

ggsave("formanuscript/moisture_temporal_fig_2021.png", plot = moisturefig_temporal, height = 7, width = 3.8)

moisturefig_temporal2022 =
  grouped_moisture_forfigs_temporal2022 %>% 
  filter(moisture_avg > 5) %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = moisture_avg, color = month, fill = month), group = 'month')+
  geom_point(size = 4, alpha = 0.8, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype = "longdash")+
  geom_errorbar(aes(xmin=moisture_avg-moisture_sd, xmax=moisture_avg+moisture_sd), show.legend = FALSE)+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  # scale_color_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  # scale_fill_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  ylim(60, 0)+
  labs(x = '2022
       soil moisture (%)',
       y = "depth (cm)",
       color = "", fill = "")+
  scale_x_continuous(position="top")+
  facet_grid(position~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_blank(),
        axis.text.x = element_text(size = 7.5, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks



ggsave("formanuscript/moisture_temporal_fig_2022.png", plot = moisturefig_temporal2022, height = 7, width = 3.8)

moisture_lines_dry =
  final_temp_sal_moist_forfig %>%
  filter(position == "dry" & datetime > "2021-07-06 00:00:00") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  #filter(site == "acidic tundra") %>% 
  #filter(position == "dry") %>% 
  mutate(depth_cm = recode(depth_cm, "5" = "0-10", "15" = "10-20", "25" = "20-30")) %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("0-10", "10-20", "20-30")))   %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  #geom_point(aes(y = temp, x = datetime, color = depth_2, group = depth_2), size = 0.25)+
  geom_line(aes(y = moisture, x = datetime, color = depth_2, group = depth_2), size = 0.5)+
  scale_x_datetime(date_breaks = "2 week", date_labels = "%b-%d")+
  #ylim(25, 5)+
  labs(x = " ", y = "soil moisture 
       (%)",
       color = "soil depth cm")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_color_manual(values=(pnw_palette("Lake", 3)))+
  guides(colour = guide_legend(override.aes = list(size=6)))+
  facet_grid(.~site, scales = "free_x")+
  theme_er1()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12),
        legend.position = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        strip.text.y = element_text(size = 13),
        strip.text.x = element_text(size = 13),
        panel.border = element_rect(color="gray",size=0.5, fill = NA))


ggsave("formanuscript/moisture_lines_dry.png", plot = moisture_lines_dry, width = 7.5, height = 2.5)


#####line plots for temp and moisture---------

temp_lines=
  final_temp_sal_moist_forfig %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  #filter(site == "acidic tundra") %>% 
  #filter(position == "dry") %>% 
  mutate(depth_cm = recode(depth_cm, "5" = "0-10", "15" = "10-20", "25" = "20-30")) %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("0-10", "10-20", "20-30")))   %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_point(aes(y = temp, x = datetime, color = depth_2, group = depth_2), size = 0.25)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_y_continuous(breaks = c(0, 10, 20, 30), n.breaks=4, limits = c(0, 35))+
  #ylim(25, 5)+
  labs(x = "2021", y = "soil temperature, celsius",
       color = "soil depth cm")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_color_manual(values=(pnw_palette("Lake", 3)))+
  guides(colour = guide_legend(override.aes = list(size=6)))+
  facet_grid(position~site, scales = "free_x")+
  theme_er1()+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, angle = 45),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text.y = element_blank(),

        strip.text.x = element_text(size = 16))

ggsave("formanuscript/temp_lines.tiff", plot = temp_lines, width = 6, height = 6)


temp_lines_dry =
  final_temp_sal_moist_forfig %>%
  filter(position == "dry" & datetime > "2021-07-06 00:00:00") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  mutate(depth_cm = recode(depth_cm, "5" = "0-10", "15" = "10-20", "25" = "20-30")) %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("0-10", "10-20", "20-30")))   %>% 
  ggplot()+
  #geom_point(aes(y = temp, x = datetime, color = depth_2, group = depth_2), size = 0.25)+
    geom_line(aes(y = temp, x = datetime, color = depth_2, group = depth_2), size = 0.5)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_y_continuous(breaks = c(0, 10, 20, 30), n.breaks=4, limits = c(0, 35))+
  #ylim(25, 5)+
  labs(x = " ", y = " soil temperature 
       (°C)",
       color = "soil depth cm")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_color_manual(values=(pnw_palette("Lake", 3)))+
  guides(colour = guide_legend(override.aes = list(size=6)))+
  facet_grid(.~site, scales = "free_x")+
  theme_er1()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        legend.position = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.ticks.x = element_blank(),
        strip.text.y = element_text(size = 13),
        strip.text.x = element_text(size = 13),
        panel.border = element_rect(color="gray",size=0.5, fill = NA))

ggsave("formanuscript/temp_lines_dry.png", plot = temp_lines_dry, width = 7.5, height = 2.5)



temp_lines2022=
  final_temp_sal_moist_forfig2022 %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  #filter(site == "acidic tundra") %>% 
  #filter(position == "dry") %>% 
  mutate(depth_cm = recode(depth_cm, "5" = "0-10", "15" = "10-20", "25" = "20-30")) %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("0-10", "10-20", "20-30")))   %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_point(aes(y = temp, x = datetime, color = depth_2, group = depth_2), size = 0.25)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_y_continuous(breaks = c(0, 10, 20, 30), n.breaks=4, limits = c(0, 35))+
  #ylim(25, 5)+
  labs(x = "2022", y = " ",
       color = "soil depth cm")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_color_manual(values=(pnw_palette("Lake", 3)))+
  guides(colour = guide_legend(override.aes = list(size=6)))+
  facet_grid(position~site, scales = "free_x")+
  theme_er1()+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, angle = 45),
        legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size = 16),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16))

ggsave("formanuscript/temp_lines2022.tiff", plot = temp_lines2022, width = 6, height = 6)

library(patchwork)

temp_linesAll = temp_lines + plot_spacer() + temp_lines2022 + plot_layout(guides = "collect", widths = c(6, 0.25 , 6)) & theme(legend.position = 'bottom')

ggsave("output/temp_linesAll.png", plot = temp_linesAll, height = 7, width = 13)



moisture_lines=
  final_temp_sal_moist_forfig %>%
  filter(moisture > 10) %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  #filter(site == "acidic tundra") %>% 
  mutate(depth_cm = recode(depth_cm, "5" = "0-10", "15" = "10-20", "25" = "20-30")) %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("0-10", "10-20", "20-30")))   %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  # geom_line(aes(y = moisture, x = datetime, color = depth_2, group = depth_2), size = 1)+
  geom_point(aes(y = moisture, x = datetime, color = depth_2, group = depth_2), size = 0.5)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  #ylim(25, 5)+
  labs(x = "2021", y = "soil moisture, %",
       color = "soil depth cm")+
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), n.breaks=6, limits = c(0, 60))+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_color_manual(values=(pnw_palette("Lake", 3)))+
  guides(colour = guide_legend(override.aes = list(size=6)))+
  facet_grid(position~site, scales = "free_x")+
  theme_er1()+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, angle = 45),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size = 16))

ggsave("formanuscript/moisture_lines.png", plot = moisture_lines, width = 6, height = 6)

moisture_lines2022=
  final_temp_sal_moist_forfig2022 %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  #filter(site == "acidic tundra") %>% 
  mutate(depth_cm = recode(depth_cm, "5" = "0-10", "15" = "10-20", "25" = "20-30")) %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("0-10", "10-20", "20-30")))   %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_line(aes(y = moisture, x = datetime, color = depth_2, group = depth_2), size = 1)+
  scale_x_datetime(date_breaks = "2 week", date_labels = "%b-%d")+
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), n.breaks=6, limits = c(0, 60))+
  #ylim(25, 5)+
  labs(x = "2022", y = " ",
       color = "soil depth cm")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_color_manual(values=(pnw_palette("Lake", 3)))+
  guides(colour = guide_legend(override.aes = list(size=6)))+
  facet_grid(position~site, scales = "free_x")+
  theme_er1()+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, angle = 45),
        legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size = 16),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16))

ggsave("formanuscript/moisture_lines2022.tiff", plot = moisture_lines2022, width = 6, height = 6)

moisture_linesAll = moisture_lines + plot_spacer() + moisture_lines2022 + plot_layout(guides = "collect", widths = c(6, 0.25 , 6)) & theme(legend.position = 'bottom')

ggsave("output/moisture_linesAll.png", plot = moisture_linesAll, height = 7, width = 13)



dry_acidic_temp_contour=
  final_temp_sal_moist_forfig %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(position == "dry" & site == "acidic tundra") %>% 
  mutate(depth_cm = recode(depth_cm, "5" = "0", "15" = "15", "25" = "30")) %>% 
  mutate(depth_cm = as.numeric(depth_cm)) %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = temp))+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(30, 0)+
  labs(x = "", y = "depth, cm",
       fill = "soil temperature, C")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=(pnw_palette("Sailboat", 14)))+
  #facet_grid(site~., scales = "free_x")+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "none")

ggsave("formanuscript/dry_acidic_temp_contour.tiff", plot = dry_acidic_temp_contour, width = 10, height = 2)

dry_nonacidic_temp_contour=
  final_temp_sal_moist_forfig %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  #filter(site == "acidic tundra") %>% 
  filter(position == "dry" & site == "non-acidic tundra") %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = temp))+
  #geom_line(orientation = "x", show.legend = FALSE)+
  # geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-07-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-07-01 00:00:00'), xmax= as_datetime('2021-08-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-08-01 00:00:00'), xmax= as_datetime('2021-09-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-09-01 00:00:00'), xmax= as_datetime('2021-09-20 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(25, 5)+
  labs(x = "", y = "depth, cm",
       fill = "soil temperature, C")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=(pnw_palette("Sailboat", 14)))+
  #facet_grid(site~., scales = "free_x")+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "none")

ggsave("formanuscript/dry_nonacidic_temp_contour.tiff", plot = dry_nonacidic_temp_contour, width = 10, height = 2)

dry_nonacidic_temp_contour_legend=
  final_temp_sal_moist_forfig %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  #filter(site == "acidic tundra") %>% 
  filter(position == "dry" & site == "non-acidic tundra") %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = temp))+
  #geom_line(orientation = "x", show.legend = FALSE)+
  # geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-07-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-07-01 00:00:00'), xmax= as_datetime('2021-08-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-08-01 00:00:00'), xmax= as_datetime('2021-09-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-09-01 00:00:00'), xmax= as_datetime('2021-09-20 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(25, 5)+
  labs(x = "", y = "depth, cm",
       fill = "soil temperature, C")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=(pnw_palette("Sailboat", 14)))+
  #facet_grid(site~., scales = "free_x")+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "right")

ggsave("formanuscript/dry_nonacidic_temp_contour_legend.tiff", plot = dry_nonacidic_temp_contour_legend, width = 10, height = 5)


dry_acidic_moisture_contour=
  final_temp_sal_moist_forfig %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  #filter(site == "acidic tundra") %>% 
  filter(position == "dry" & site == "acidic tundra") %>% 
  mutate(depth_cm = recode(depth_cm, "5" = "0", "15" = "15", "25" = "30")) %>% 
  mutate(depth_cm = as.numeric(depth_cm)) %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("0", "15", "30")))   %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = moisture))+
  #geom_line(orientation = "x", show.legend = FALSE)+
  # geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-07-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-07-01 00:00:00'), xmax= as_datetime('2021-08-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-08-01 00:00:00'), xmax= as_datetime('2021-09-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-09-01 00:00:00'), xmax= as_datetime('2021-09-20 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(30, 0)+
  labs(x = "", y = "depth, cm",
       fill = "soil moisture, %")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=rev(pnw_palette("Starfish", 14)))+
  facet_grid(site~., scales = "free_x")+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "none")

ggsave("formanuscript/dry_acidic_moisture_contour.tiff", plot = dry_acidic_moisture_contour, width = 10, height = 3)


dry_acidic_moisture_contour_legend=
  final_temp_sal_moist_forfig %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  #filter(site == "acidic tundra") %>% 
  filter(position == "dry" & site == "acidic tundra") %>% 
  mutate(depth_cm = recode(depth_cm, "5" = "0", "15" = "15", "25" = "30")) %>% 
  mutate(depth_cm = as.numeric(depth_cm)) %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("0", "15", "30")))   %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = moisture))+
  #geom_line(orientation = "x", show.legend = FALSE)+
  # geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-07-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-07-01 00:00:00'), xmax= as_datetime('2021-08-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-08-01 00:00:00'), xmax= as_datetime('2021-09-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-09-01 00:00:00'), xmax= as_datetime('2021-09-20 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(30, 0)+
  labs(x = "", y = "depth, cm",
       fill = "soil moisture, %")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=rev(pnw_palette("Starfish", 14)))+
  #facet_grid(site~., scales = "free_x")+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "right")

ggsave("formanuscript/dry_acidic_moisture_contour_legend.tiff", plot = dry_acidic_moisture_contour_legend, width = 10, height = 5)


dry_nonacidic_moisture_contour=
  final_temp_sal_moist_forfig %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  #filter(site == "acidic tundra") %>% 
  filter(position == "dry" & site == "non-acidic tundra") %>% 
  mutate(depth_cm = recode(depth_cm, "5" = "0", "15" = "15", "25" = "30")) %>% 
  mutate(depth_cm = as.numeric(depth_cm)) %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("0", "15", "30")))   %>%   #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = moisture))+
  #geom_line(orientation = "x", show.legend = FALSE)+
  # geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-07-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-07-01 00:00:00'), xmax= as_datetime('2021-08-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-08-01 00:00:00'), xmax= as_datetime('2021-09-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-09-01 00:00:00'), xmax= as_datetime('2021-09-20 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(30, 0)+
  labs(x = "", y = "depth, cm",
       fill = "soil moisture, %")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=rev(pnw_palette("Starfish", 14)))+
  #facet_grid(site~., scales = "free_x")+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "none")

ggsave("formanuscript/dry_nonacidic_moisture_contour.tiff", plot = dry_nonacidic_moisture_contour, width = 10, height = 3)


dry_nonacidic_moisture_contour_legend=
  final_temp_sal_moist_forfig %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  #filter(site == "acidic tundra") %>% 
  filter(position == "dry" & site == "non-acidic tundra") %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = moisture))+
  #geom_line(orientation = "x", show.legend = FALSE)+
  # geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-07-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-07-01 00:00:00'), xmax= as_datetime('2021-08-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-08-01 00:00:00'), xmax= as_datetime('2021-09-01 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  # geom_rect(aes(xmin=as_datetime('2021-09-01 00:00:00'), xmax= as_datetime('2021-09-20 00:00:00'), 
  #               ymin=5, ymax=25), color = c("#fbff12"), fill = NA, size = 1.5)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(25, 5)+
  labs(x = "", y = "depth, cm",
       fill = "soil moisture, %")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=rev(pnw_palette("Starfish", 14)))+
  facet_grid(site~., scales = "free_x")+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "right")

ggsave("formanuscript/dry_nonacidic_moisture_contour_legend.tiff", plot = dry_nonacidic_moisture_contour_legend, width = 10, height = 5)










nonacidic_moisture_contour=
  final_temp_sal_moist_forfig %>% 
  filter(site == "non-acidic tundra") %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  #mutate(site =factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = moisture))+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(25, 5)+
  labs(x = "", y = "depth, cm",
       fill = "redox potential, mV")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=rev(pnw_palette("Bay", 11)))+
  facet_grid(position~.)+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "bottom")


ggsave("formanuscript/acidic_moisture_contour.tiff", plot = acidic_moisture_contour, width = 7.5, height = 5.5)
ggsave("formanuscript/nonacidic_moisture_contour.tiff", plot = nonacidic_moisture_contour, width = 7.5, height = 5.5)


###scatter plots?

airtemp_EDC_forleftjoin =
  climate_1hr_data_airtemp_cleaned %>% 
  dplyr::select(date, air_temp_3m_avg) %>% 
  filter(date > "2021-06-14" & date < "2021-08-09") %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>% 
  mutate(date = paste0(year, "-", month, "-", day)) 



grouped_moisture_temp_salinity_forfigs_temporal =
  final_temp_sal_moist %>% 
  mutate(forsep = TIMESTAMP) %>% 
  separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
  separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(datetime = ymd_hm(paste(date, time)),
         date = ymd(date)) %>% 
  separate(forsep, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "/", into = c("month", "day", "year")) %>% 
  # mutate(month = recode(month,  "6" = "early summer", 
  #                       "7" = "mid summer", 
  #                       "8" = "late summer",
  #                       "9" = "early fall")) %>% 
  mutate(date = paste0(year, "-", month, "-", day)) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  dplyr::rename(depth_cm = depth) %>% 
  group_by(site, position, depth_cm, date) %>% 
  dplyr::summarise(moisture_avg = round(mean(moisture),2),
                   moisture_sd = round(sd(moisture),2),
                   temp_avg = round(mean(temp),2),
                   temp_sd = round(sd(temp),2),
                   sal_avg = round(mean(salinity),2),
                   sal_sd = round(sd(salinity)),2) 


grouped_moisture_temp_salinity_forfigs_temporal_dry =
  grouped_moisture_temp_salinity_forfigs_temporal %>%
  filter(site == "acidic tundra" & position == "dry" & depth_cm == 5) %>% 
  left_join(airtemp_EDC_forleftjoin)



final_temp_sal_moist_forfig %>% 
  filter(site == "acidic tundra") %>% 
  mutate(depth_cm = factor(depth_cm, levels = c("5", "15", "25")))   %>%
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  ggplot(aes(x = temp, y = moisture, color = depth_cm))+
  geom_point(alpha = 0.6, size = 1)+
  scale_color_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  facet_grid(month~position)+
  theme_minimal()

grouped_moisture_temp_salinity_forfigs_temporal %>% 
  mutate(depth_cm = factor(depth_cm, levels = c("5", "15", "25")))   %>%
  ggplot(aes(x = temp_avg, y = moisture_avg, color = position))+
  geom_point(alpha = 0.6, size = 1)+
  scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  facet_grid(depth_cm~.)+
  theme_minimal()

grouped_moisture_temp_salinity_forfigs_temporal %>% 
  mutate(depth_cm = factor(depth_cm, levels = c("5", "15", "25")))   %>%
  ggplot(aes(x = temp_avg, y = moisture_avg, color = position))+
  geom_point(alpha = 0.6, size = 1)+
  scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  facet_grid(depth_cm~site)+
  theme_minimal()




allsoiltemperature_fig =
  final_temp_sal_moist_forfig %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  #filter(position == "hydric" & site == "non-acidic tundra") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = temp, x = datetime, group = 'depth_cm'))+
  #geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 0.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  scale_fill_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  ylim(0, 25)+
  labs(x = "", y = "soil temperature (C)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "bottom", axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9))

ggsave("figures_finalized/allsoiltemp_fig.tiff", plot = allsoiltemperature_fig, width = 6, height = 5.5)

nonacidic_hydric_soiltemperature_fig =
  final_temp_sal_moist_forfig %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  filter(position == "hydric" & site == "non-acidic tundra") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = temp, x = datetime, group = 'depth_cm'))+
  #geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  scale_fill_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  ylim(0, 25)+
  labs(x = "", y = "soil temperature (C)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "bottom", axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9))

ggsave("figures_finalized/nonacidic_hydric_soiltemperature_fig.png", plot = nonacidic_hydric_soiltemperature_fig, width = 6, height = 4)

nonacidic_dry_soiltemperature_fig =
  final_temp_sal_moist_forfig %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  filter(position == "dry" & site == "non-acidic tundra") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = temp, x = datetime, group = 'depth_cm'))+
  #geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  scale_fill_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  ylim(0, 25)+
  labs(x = "", y = "soil temperature (C)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site, scales = "free_x")+
  theme_er1()+
  theme(legend.position = "bottom", axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9))

ggsave("figures_finalized/nonacidic_dry_soiltemperature_fig.png", plot = nonacidic_dry_soiltemperature_fig, width = 6, height = 4)



moisture_depth_lineplot_hydric =
  final_temp_sal_moist_forfig %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  filter(position == "hydric" & site == "non-acidic tundra") %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = moisture, x = datetime, group = 'depth_cm'))+
  #geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 1.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  scale_fill_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  ylim(0, 60)+
  labs(x = "", y = "soil moisture (%)",
       color = "depth (cm)", fill = "depth (cm)")+
  #facet_grid(position~site)+
  theme_er1()+
  theme(legend.position = "left", axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9))

moisture_depth_lineplot_dry =
  final_temp_sal_moist_forfig %>% 
  mutate(depth_2 = factor(depth_cm, levels = c("5", "15", "25")))   %>% 
  filter(position == "dry" & site == "non-acidic tundra") %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = moisture, x = datetime), group = 'depth_cm')+
  #geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 1.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  scale_fill_manual(values = rev(c("#ffd60a","#EDA24E", "#9b2226")))+
  ylim(0, 60)+
  labs(x = "", y = "soil moisture (%)",
       color = "depth (cm)", fill = "depth (cm)")+
  #facet_grid(position~site)+
  theme_er1()+
  theme(legend.position = "left", axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9))

ggsave("figures_finalized/moisture_depth_lineplothydric.png", plot = moisture_depth_lineplot_hydric, width = 6, height = 3)
ggsave("figures_finalized/moisture_depth_lineplotdry.png", plot = moisture_depth_lineplot_dry, width = 6, height = 3)


hydric_dual =
  final_temp_sal_moist_forfig %>% 
  filter(depth_cm == 25 & position == "hydric") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = moisture), color = c("#c8b6ff"), size = 1)+
  geom_line(aes(y = temp*10), size = 0.8)+
  labs(subtitle = "Hydric")+
  scale_x_datetime(date_breaks = "1 week" , date_labels = "%b-%d")+
  scale_y_continuous(name = "Soil Moisture, (purple line)",
                     sec.axis = sec_axis(~./10, name = "Soil Temperature"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(.~site, scales="free")  


mesic_dual =
  final_temp_sal_moist_forfig %>% 
  filter(depth_cm == 25 & position == "mesic") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = moisture), color = c("#c8b6ff"), size = 1)+
  geom_line(aes(y = temp*10), size = 0.8)+
  labs(subtitle = "Mesic")+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_y_continuous(name = "Soil Moisture (purple line)",
                     sec.axis = sec_axis(~./10, name = "Soil Temperature"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(.~site, scales="free")    

dry_dual =
  final_temp_sal_moist_forfig %>% 
  filter(depth_cm == 25 & position == "dry") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = moisture), color = c("#c8b6ff"), size = 1)+
  geom_line(aes(y = temp*10), size = 0.8)+
  labs(subtitle = "Dry")+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_y_continuous(name = "Soil Moisture (purple line)",
                     sec.axis = sec_axis(~./10, name = "Soil Temperature"))+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(.~site, scales="free") 

combo_dual = dry_dual + mesic_dual + hydric_dual


ggsave("output/combo_dual.png", plot = combo_dual, width = 15, height = 4)
ggsave("output/dry_dual.png", plot = dry_dual, width = 8, height = 5)
ggsave("output/mesic_dual.png", plot = mesic_dual, width = 8, height = 5)
ggsave("output/hydric_dual.png", plot = hydric_dual, width = 8, height = 5)
ggsave("figures_finalized/combo_dual.png", plot = combo_dual, width = 15, height = 4)
ggsave("figures_finalized/dry_dual.png", plot = dry_dual, width = 8, height = 5)
ggsave("figures_finalized/mesic_dual.png", plot = mesic_dual, width = 8, height = 5)
ggsave("figures_finalized/hydric_dual.png", plot = hydric_dual, width = 8, height = 5)



final_temp_sal_moist_forfig %>% 
  filter(depth_cm == 25) %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = moisture))+
  scale_x_datetime(date_breaks = "1 week")+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")+
  facet_grid(position~site)    


final_temp_sal_moist_forfig %>% 
  filter(depth_cm == 25 & position == "hydric") %>% 
  ggplot(aes(x = datetime))+
  geom_line(aes(y = temp, color = site))+
  scale_x_datetime(date_breaks = "1 day")+
  theme_er1()+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90, size = 9), legend.position = "bottom")
