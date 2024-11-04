#ECROONEY 
#January 3 2023

#load all packages


#PhIr data
#redox contour plots
#redox line/dot plots

source("code/0-packages.R")


combo_redox_withdepths2022 = read.csv("processed/allcombine_2022_frozen.csv")
combo_redox_withdepths2021 = read.csv("processed/allcombine_2021_frozen.csv")


###

#data output

all_redox_data_fordatabase =
  combo_redox_withdepths2021 %>% 
  dplyr::select(TIMESTAMP, site, position, avg_values_fixed, Plot, depth_cm, frozen) %>% 
  rbind(combo_redox_withdepths2022 %>% dplyr::select(TIMESTAMP, site, position, avg_values_fixed, Plot, depth_cm, frozen)) %>% 
  rename(redox_mV = avg_values_fixed,
         area = site,
         site = position,
         condition = frozen)

write.csv(all_redox_data_fordatabase, "processed/PhIr_redox_2021_2022.csv")

Bfrozen_group2021=
  combo_redox_withdepths2021 %>% 
  # mutate(keep = case_when(site == "non-acidic tundra" & position == "hydric" & probe == 1 ~ "keep",
  #                         site == "acidic tundra" & position == "hydric" & probe == 3 ~ "keep",
  #                         site == "non-acidic tundra" & position == "dry" & probe == 1 ~ "keep",
  #                         site == "acidic tundra" & position == "dry" & probe == 1 ~ "keep",
  #                         site == "acidic tundra" & position == "mesic" & probe == 1 ~ "keep",
  #                         site == "non-acidic tundra" & position == "mesic" & probe == 3 ~ "keep")) %>%
  dplyr::mutate(keep = if_else(site == "non-acidic tundra" & position == "hydric" & depth_cm > 40 & Betterdate <= '2021-08-09 00:00', paste0("frozen"),"unfrozen")) %>% 
  #filter(keep != "frozen") %>% 
  filter(keep == "unfrozen") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) 

Bfrozen_group2022 =
  combo_redox_withdepths2022 %>% 
  dplyr::mutate(keep = if_else(site == "non-acidic tundra" & position == "dry" & depth_cm > 21 & Betterdate >= '2022-07-30 00:00', paste0("frozen"),"unfrozen")) %>% 
  #filter(keep != "frozen") %>%
  
  filter(keep == "unfrozen") %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) 

all_grouped =
  frozen_group2022 %>% 
  vctrs::vec_c(frozen_group2021) %>% 
  mutate(position = recode(position, "hydric" = "wet")) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "wet"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  group_by(position, depth_cm) %>% 
  dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
                   redox_sd = round(sd(avg_values_fixed),2),
                   redox_se = round(sd(avg_values_fixed)/sqrt(n()),2),
                   redox_sd2 = (redox_sd)/2)


grouped_redox_forfigs2022 = 
  frozen_group2022 %>% 
 # filter(Betterdate >= '2022-07-01 00:00' & Betterdate <= "2022-09-01 00:00") %>% 
  mutate(position = recode(position, "hydric" = "wet")) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "wet"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  group_by(site, position, depth_cm) %>% 
  dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
                   redox_sd = round(sd(avg_values_fixed),2),
                   redox_se = round(sd(avg_values_fixed)/sqrt(n()),2),
                   redox_sd2 = (redox_sd)/2) %>% 
  mutate(year = 2022)

grouped_redox_forfigs2021 = 
  frozen_group2021 %>% 
#  filter(Betterdate >= '2021-07-01 00:00' & Betterdate <= "2021-09-01 00:00") %>% 
  mutate(position = recode(position, "hydric" = "wet")) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "wet"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  group_by(site, position, depth_cm) %>% 
  dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
                   redox_sd = round(sd(avg_values_fixed),2),
                   redox_se = round(sd(avg_values_fixed)/sqrt(n()),2),
                   redox_sd2 = (redox_sd)/2) %>% 
  mutate(year = 2021)


grouped_redox_forfigs_temporal2022 = 
  combo_redox_withdepths2022 %>% 
  #filter(Betterdate >= '2021-07-01 00:00') %>% 
  mutate(position = recode(position, "hydric" = "wet")) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "wet"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  mutate(forsep = TIMESTAMP) %>% 
  separate(forsep, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "/", into = c("month", "day", "year")) %>% 
  mutate(month = recode(month,  "6" = "June", 
                        "7" = "July", 
                        "8" = "August",
                        "9" = "September")) %>% 
  group_by(site, position, depth_cm, month) %>% 
  dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
                   redox_sd = round(sd(avg_values_fixed),2),
                   redox_se = round(sd(avg_values_fixed)/sqrt(n()),2),
                   redox_sd2 = (redox_sd)/2) %>% 
  mutate(year = 2022) 



grouped_redox_forfigs_temporal2021 = 
  frozen_group2021 %>% 
 # filter(Betterdate >= '2021-07-01 00:00') %>% 
  mutate(position = recode(position, "hydric" = "wet")) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "wet"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  mutate(forsep = TIMESTAMP) %>% 
  separate(forsep, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "/", into = c("month", "day", "year")) %>% 
  mutate(month = recode(month,  "6" = "June", 
                        "7" = "July", 
                        "8" = "August",
                        "9" = "September")) %>% 
  group_by(site, position, depth_cm, month) %>% 
  dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
                   redox_sd = round(sd(avg_values_fixed),2),
                   redox_se = round(sd(avg_values_fixed)/sqrt(n()),2),
                   redox_sd2 = (redox_sd)/2)  
  


ungrouped_redox_forfigs_nonhydric = 
  combo_redox_withdepths2021 %>% 
  
  mutate(position = recode(position, "hydric" = "wet")) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "wet"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  filter(position != "wet") %>% 
  separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
  separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(datetime = ymd_hm(paste(date, time)),
         date = ymd(date)) %>%
  mutate(depth_2 = factor(depth_cm, levels = c("1", "3.5", "5", "6", "8.5", "10", "15", "16", "18.5", "20", "25", "26", "28.5", "30"))) %>%
  #filter(probe == 1) %>%
  dplyr::rename(redox_avg_mV = avg_values_fixed) 
# group_by(site, position, depth_2, datetime) %>%
# dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
#                  redox_sd = round(sd(avg_values_fixed),2))

ungrouped_redox_forfigs2021 = 
  combo_redox_withdepths2021 %>% 
  mutate(position = recode(position, "hydric" = "wet")) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "wet"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
  separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(datetime = ymd_hm(paste(date, time)),
         date = ymd(date)) %>%
  #filter(probe == 1) %>%
  dplyr::rename(redox_avg_mV = avg_values_fixed) 

ungrouped_redox_forfigs2022 = 
  combo_redox_withdepths2022 %>% 
  mutate(position = recode(position, "hydric" = "wet")) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "wet"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
  separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(datetime = ymd_hm(paste(date, time)),
         date = ymd(date)) %>%
  #filter(probe == 1) %>%
  dplyr::rename(redox_avg_mV = avg_values_fixed) 
# 
#
redoxfig_depth_sd2021_nonacidic =
  grouped_redox_forfigs2021 %>%
  filter(site == "non-acidic tundra") %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = position, fill = position), group = 'position')+
  geom_rect(aes(xmin = 150, xmax = 250, ymin = 0, ymax = Inf), fill = "gray90", color = "gray90")+
  geom_point(size = 3.75, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype="longdash", alpha = 0.3)+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
   # scale_color_manual(values = c("#bc4749", "#87c38f", "#457b9d"))+
  # scale_fill_manual(values = c("#bc4749", "#87c38f", "#457b9d"))+
    scale_color_manual(values = rev(soil_palette("redox2",3)))+
    scale_fill_manual(values = rev(soil_palette("redox2",3)))+
  # scale_color_manual(values = rev(soil_palette("paleustalf",3)))+
  # scale_fill_manual(values = rev(soil_palette("paleustalf",3)))+
ylim(60, 0)+
  labs(
    x = '
redox potential (mV)
    ',
    y = "depth (cm)",
    color = "", fill = "")+
  scale_x_continuous(position="top", breaks = c(-300, 0, 300, 600), n.breaks=4, limits = c(-400, 850))+
  facet_grid(.~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "none", panel.grid.major = element_blank(),
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray50",size=0.25, fill = NA))
# 

ggsave("output/redoxfig_depth_sd2021_nonacidic.png", plot = redoxfig_depth_sd2021_nonacidic, height = 4.5, width = 2.25)
ggsave("output/redoxfig_depth_sd2021.png", plot = redoxfig_depth_sd2021, height = 4.85, width = 4)
ggsave("output/redoxfig_depth_sd2022.png", plot = redoxfig_depth_sd2022, height = 6, width = 4)

redoxfig_depth_sd2021_acidic =
  grouped_redox_forfigs2021 %>% 
  filter(site == "acidic tundra") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = position, fill = position), group = 'position')+
  geom_rect(aes(xmin = 250, xmax = 350, ymin = 0, ymax = Inf), fill = "gray90", color = "gray90")+
  geom_point(size = 3.75, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype="longdash", alpha = 0.3)+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  # scale_color_manual(values = c("#bc4749", "#35a55f", "#0582ca"))+
  # scale_fill_manual(values = c("#bc4749", "#35a55f", "#0582ca"))+
  scale_color_manual(values = rev(soil_palette("redox2",3)))+
  scale_fill_manual(values = rev(soil_palette("redox2",3)))+
  # scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  # scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  ylim(60, 0)+
  labs(
    x = ' 
redox potential (mV)
    ',
    y = "depth (cm)",
    color = "", fill = "")+
  scale_x_continuous(position="top", breaks = c(-300, 0, 300, 600), n.breaks=4, limits = c(-400, 850))+
  facet_grid(.~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "none", panel.grid.major = element_blank(), 
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray50",size=0.25, fill = NA))
# 
ggsave("output/redoxfig_depth_sd2021_acidic.png", plot = redoxfig_depth_sd2021_acidic, height = 4.5, width = 2.25)

redoxfig_depth_sd2021_legends =
  grouped_redox_forfigs2021 %>% 
  filter(site == "non-acidic tundra") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = position, fill = position), group = 'position')+
  geom_point(size = 5, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype="longdash", alpha = 0.3)+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  # scale_color_manual(values = c("#bc4749", "#87c38f", "#457b9d"))+
  # scale_fill_manual(values = c("#bc4749", "#87c38f", "#457b9d"))+
  scale_color_manual(values = rev(soil_palette("redox2",3)))+
  scale_fill_manual(values = rev(soil_palette("redox2",3)))+
  # scale_color_manual(values = rev(soil_palette("paleustalf",3)))+
  # scale_fill_manual(values = rev(soil_palette("paleustalf",3)))+
  ylim(60, 0)+
  labs(
    x = ' 
redox potential (mV)
    ',
y = "depth (cm)",
color = "", fill = "")+
  scale_x_continuous(position="top", breaks = c(-300, 0, 300, 600), n.breaks=4, limits = c(-400, 850))+
  facet_grid(.~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), 
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="gray50",size=0.25, fill = NA))
# 

ggsave("output/redoxfig_depth_sd2021_legends.png", plot = redoxfig_depth_sd2021_legends, height = 4.5, width = 4)


grouped_redox_all =
grouped_redox_forfigs2021 %>% 
  vctrs::vec_c(grouped_redox_forfigs2022) %>% 
  mutate(year = as.factor(year))



redox_temporal_fig_2021 =
  grouped_redox_forfigs_temporal2021 %>% 
  mutate(month = factor(month, levels = c("June", "July", "August", "September")))   %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = month, fill = month), group = 'position')+
  geom_rect(aes(xmin = 175, xmax = 325, ymin = 0, ymax = Inf), fill = "gray90", color = "gray90")+
  geom_point(size = 3, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype = "longdash")+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE, width = 2)+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  # scale_color_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  # scale_fill_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  ylim(60, 0)+
  labs(x = '2021
       redox potential (mV)',
       y = "",
       color = "", fill = "")+
  scale_x_continuous(position="top")+
  facet_grid(position~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 10, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks
#remove y axis ticks


ggsave("output/redox_temporal_fig_2021.png", plot = redox_temporal_fig_2021, height = 7, width = 5.5)

redox_temporal_fig_2021_non_hydric =
  grouped_redox_forfigs_temporal2021 %>%
  filter(site == "non-acidic tundra" & position == "wet") %>%
  mutate(month = factor(month, levels = c("June", "July", "August", "September")))   %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = month, fill = month), group = 'position')+
  geom_rect(aes(xmin = 150, xmax = 250, ymin = 0, ymax = Inf), fill = "gray90", color = "gray90")+
  geom_point(size = 4, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype = "longdash", alpha = 0.3)+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  # scale_color_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  # scale_fill_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  ylim(60, 0)+
  labs(x = 'Hydric
redox potential (mV)
       ',
       y = "",
       color = "", fill = "")+
  scale_x_continuous(position="top", breaks = c(-300, 0, 300, 600), n.breaks=4, limits = c(-400, 850))+
  facet_grid(position~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))
       # axis.text.x = element_text(hjust=0,angle = 90))  #remove y axis ticks
#remove y axis ticks

ggsave("output/redox_temporal_fig_2021_non_hydric.png", plot = redox_temporal_fig_2021_non_hydric, height = 4.5, width = 2.25)

redox_temporal_fig_2021_acidic_hydric =
  grouped_redox_forfigs_temporal2021 %>%
  filter(site == "acidic tundra" & position == "wet") %>%
  mutate(month = factor(month, levels = c("June", "July", "August", "September")))   %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = month, fill = month), group = 'position')+
  geom_rect(aes(xmin = 250, xmax = 350, ymin = 0, ymax = Inf), fill = "gray90", color = "gray90")+
  geom_point(size = 4, alpha = 0.8, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype = "longdash", alpha = 0.3)+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  # scale_color_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  # scale_fill_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  ylim(60, 0)+
  labs(x = 'Hydric
redox potential (mV)
       ',
       y = "",
       color = "", fill = "")+
  scale_x_continuous(position="top", breaks = c(-300, 0, 300, 600), n.breaks=4, limits = c(-400, 850))+
  facet_grid(position~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA))
# axis.text.x = element_text(hjust=0,angle = 90))  #remove y axis ticks
# #remove y axis ticks
# 
ggsave("output/redox_temporal_fig_2021_acidic_hydric.png", plot = redox_temporal_fig_2021_acidic_hydric, height = 4.5, width = 2.25)


redox_temporal_fig_2022 =
  grouped_redox_forfigs_temporal2022 %>% 
  mutate(month = factor(month, levels = c("June", "July", "August", "September")))   %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = month, fill = month), group = 'position')+
  geom_rect(aes(xmin = 175, xmax = 325, ymin = 0, ymax = Inf), fill = "gray90", color = "gray90")+
  geom_point(size = 3, alpha = 0.8, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype = "longdash")+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE, width = 2)+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  # scale_color_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  # scale_fill_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  ylim(60, 0)+
  labs(x = '2022
       redox potential (mV)',
       y = "",
       color = "", fill = "")+
  scale_x_continuous(position="top", breaks = c(-300, 0, 300, 600, 900), n.breaks=5, limits = c(-400, 900))+
  facet_grid(position~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 10, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks
#remove y axis ticks

ggsave("output/redox_temporal_fig_2022.png", plot = redox_temporal_fig_2022, height = 7, width = 5.5)

ggsave("output/2022redox_fig.png", plot = redoxfig_depth_sd, height = 7, width = 6)


ungrouped_redox_forfigs2021_probe =
  ungrouped_redox_forfigs2021 %>% 
  mutate(keep = case_when(site == "non-acidic tundra" & position == "wet" & probe == 1 ~ "keep",
                          site == "acidic tundra" & position == "wet" & probe == 3 ~ "keep",
                          site == "non-acidic tundra" & position == "dry" & probe == 1 ~ "keep",
                          site == "acidic tundra" & position == "dry" & probe == 1 ~ "keep",
                          site == "acidic tundra" & position == "mesic" & probe == 1 ~ "keep",
                          site == "non-acidic tundra" & position == "mesic" & probe == 3 ~ "keep")) %>% 
  filter(keep == "keep") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) 
  
ungrouped_redox_forfigs2022_probe =
  ungrouped_redox_forfigs2022 %>% 
  mutate(keep = case_when(site == "non-acidic tundra" & position == "wet" & probe == 3 ~ "keep",
                          site == "acidic tundra" & position == "wet" & probe == 3 ~ "keep",
                          site == "non-acidic tundra" & position == "dry" & probe == 2 ~ "keep",
                          site == "acidic tundra" & position == "dry" & probe == 1 ~ "keep",
                          site == "acidic tundra" & position == "mesic" & probe == 1 ~ "keep",
                          site == "non-acidic tundra" & position == "mesic" & probe == 3 ~ "keep")) %>% 
  filter(keep == "keep") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) 

                          
allredox_lines_2021_fig =                          
  ungrouped_redox_forfigs2021_probe %>% 
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  #geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_cm, fill = depth_cm), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_gradientn(colors = rev(natparks.pals(name = "Olympic")), limits=c(-5,60))+
  scale_fill_gradientn(colors = rev(natparks.pals(name = "Olympic")), limits=c(-5,60))+  
  ylim(-250, 700)+
  labs(x = "2021", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site)+
  theme_er1()+
  guides(color = guide_colorbar(reverse = TRUE), 
         fill = guide_colorbar(reverse = TRUE))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, angle = 45),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size = 16))

ggsave("output/allredox_lines_2021_fig.png", plot = allredox_lines_2021_fig, height = 6, width = 6)

allredox_lines_2022_fig =                          
  ungrouped_redox_forfigs2022_probe %>% 
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  #geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_cm, fill = depth_cm), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_gradientn(colors = rev(natparks.pals(name = "Olympic")), limits=c(-5,60))+
  scale_fill_gradientn(colors = rev(natparks.pals(name = "Olympic")), limits=c(-5,60))+
  ylim(-250, 700)+
  labs(x = "2022", y = " ",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site)+
  theme_er1()+
  guides(color = guide_colorbar(reverse = TRUE), 
         fill = guide_colorbar(reverse = TRUE))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, angle = 45),
        legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size = 16),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16))

ggsave("output/allredox_lines_2022_fig.png", plot = allredox_lines_2022_fig, height = 6, width = 6)

library(patchwork)


allredox_lines = allredox_lines_2021_fig + plot_spacer() + allredox_lines_2022_fig + plot_layout(guides = "collect", widths = c(6, 0.25 , 6)) & theme(legend.position = 'bottom')

ggsave("output/allredox_lines.png", plot = allredox_lines, height = 7, width = 13)



redox_lines_2021_fig_nonacidichydric =                          
  ungrouped_redox_forfigs2021_probe %>% 
  filter(site == "non-acidic tundra" & position == "wet") %>% 
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "#ffe5d9", alpha = 0.7)+
  geom_point(aes(color = depth_cm, fill = depth_cm), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-14 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_gradientn(colors = rev(natparks.pals(name = "Olympic")), limits=c(0,60))+
  scale_fill_gradientn(colors = rev(natparks.pals(name = "Olympic")), limits=c(0,60))+  
  ylim(-250, 700)+
  labs(title = " ", x = " ", y = "redox potential (mV)
       ",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site)+
  theme_er1()+
  guides(color = guide_colorbar(reverse = TRUE), 
         fill = guide_colorbar(reverse = TRUE))+
  theme(axis.text.x = element_text(size = 13, vjust = 0.5, angle = 45),
        axis.text.y = element_text(size = 13),
        legend.position = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color="gray",size=0.25, fill = NA),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text.y = element_blank(),
        strip.text.x = element_blank())

ggsave("output/redox_lines_2021_fig_nonacidichydric.png", plot = redox_lines_2021_fig_nonacidichydric, height = 3.5, width = 6)


redox_lines_2021_fig_acidichydric =                          
  ungrouped_redox_forfigs2021_probe %>% 
  filter(site == "acidic tundra" & position == "wet") %>% 
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "#ffe5d9", alpha = 0.7)+
  geom_point(aes(color = depth_cm, fill = depth_cm), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-14 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_gradientn(colors = rev(natparks.pals(name = "Olympic")), limits=c(0,60))+
  scale_fill_gradientn(colors = rev(natparks.pals(name = "Olympic")), limits=c(0,60))+  
  ylim(-250, 700)+
  labs(title = " ", x = " ", y = "redox potential (mV)
       ",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site)+
  theme_er1()+
  guides(color = guide_colorbar(reverse = TRUE), 
         fill = guide_colorbar(reverse = TRUE))+
  theme(axis.text.x = element_text(size = 13, vjust = 0.5, angle = 45),
        axis.text.y = element_text(size = 13),
        legend.position = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color="gray",size=0.25, fill = NA),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text.y = element_blank(),
        strip.text.x = element_blank())

ggsave("output/redox_lines_2021_fig_acidichydric.png", plot = redox_lines_2021_fig_acidichydric, height = 3.5, width = 6)


redox_lines_2022_fig_nonacidichydric  =                          
  ungrouped_redox_forfigs2022_probe %>% 
  filter(site == "non-acidic tundra" & position == "wet") %>% 
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  #geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_cm, fill = depth_cm), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_gradientn(colors = rev(natparks.pals(name = "Olympic")), limits=c(-5,55))+
  scale_fill_gradientn(colors = rev(natparks.pals(name = "Olympic")), limits=c(-5,55))+
  ylim(-250, 700)+
  labs(x = "2022", y = " ",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site)+
  theme_er1()+
  guides(color = guide_colorbar(reverse = TRUE), 
         fill = guide_colorbar(reverse = TRUE))+
  theme(axis.text.x = element_text(size = 13, vjust = 0.5, angle = 45),
        axis.text.y = element_text(size = 13),
        legend.position = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color="gray",size=0.25, fill = NA),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text.y = element_blank(),
        strip.text.x = element_blank())

ggsave("output/redox_lines_2022_fig_nonacidichydric.png", plot = redox_lines_2022_fig_nonacidichydric, height = 3.5, width = 6)

library(patchwork)


allredox_lines = allredox_lines_2021_fig + plot_spacer() + allredox_lines_2022_fig + plot_layout(guides = "collect", widths = c(6, 0.25 , 6)) & theme(legend.position = 'bottom')

ggsave("output/allredox_lines.png", plot = allredox_lines, height = 7, width = 13)

library(metR)

allredox_contour_2021_fig_filled =                          
  ungrouped_redox_forfigs2021_probe %>% 
  ggplot(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  geom_contour_fill(na.fill = TRUE)+
  #geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  # geom_point(aes(color = depth_cm, fill = depth_cm), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
 #scale_fill_distiller(super = metR::ScaleDiscretised, palette = "Spectral")+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_fill_gradientn(colors=pnw_palette("Bay"))+
  labs(x = "2021", y = "depth, cm",
       fill = "redox potential (mV)")+
  facet_grid(position~site, scales = "free_y")+
  scale_y_reverse()+
  theme_er1()+
  theme(axis.text.x = element_text(size = 9, vjust = 0.5, angle = 45),
        legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

allredox_contour_2021_fig =                          
  ungrouped_redox_forfigs2021_probe %>% 
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  # geom_point(aes(color = depth_cm, fill = depth_cm), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_fill_distiller(super = metR::ScaleDiscretised, palette = "PuOr")+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  #scale_fill_manual(values=pnw_palette("Bay", 11))+
  labs(x = "2021", y = "depth, cm",
       fill = "redox potential (mV)")+
  facet_grid(position~site, scales = "free_y")+
  scale_y_reverse()+
  theme_er1()+
  theme(axis.text.x = element_text(size = 9, vjust = 0.5, angle = 45),
        legend.position = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave("output/allredox_contour_2021_fig.png", plot = allredox_contour_2021_fig, height = 6, width = 6)

allredox_contour_2022_fig =                          
  ungrouped_redox_forfigs2022_probe %>% 
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV), stat = "contour_filled")+
  #geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  # geom_point(aes(color = depth_cm, fill = depth_cm), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  # scale_fill_distiller(super = metR::ScaleDiscretised, palette = "Spectral")+
  scale_fill_distiller(super = metR::ScaleDiscretised, palette = "PuOr")+
  labs(x = "2022", y = "depth, cm",
       fill = "redox potential (mV)")+
  facet_grid(position~site, scales = "free_y")+
  scale_y_reverse()+
  theme_er1()+
  theme(axis.text.x = element_text(size = 9, vjust = 0.5, angle = 45),
        legend.position = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())



ggsave("output/allredox_contour_2022_fig.png", plot = allredox_contour_2022_fig, height = 6, width = 6)


library(patchwork)

redox_contour_combo = allredox_contour_2021_fig + allredox_contour_2022_fig + plot_layout(guides = "collect") 

ggsave("output/redox_contour_combo.png", plot = redox_contour_combo, height = 7, width = 12)



allredox_contour_2022_fig_legend =                          
  ungrouped_redox_forfigs2022_probe %>% 
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  # geom_point(aes(color = depth_cm, fill = depth_cm), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_fill_manual(values=pnw_palette("Bay", 11))+
  labs(x = "2022", y = "depth, cm",
       fill = "redox potential (mV)")+
  facet_grid(position~site, scales = "free_y")+
  scale_y_reverse()+
  theme_er1()+
  theme(axis.text.x = element_text(size = 9, vjust = 0.5, angle = 45),
        legend.position = "bottom")

ggsave("output/allredox_contour_2022_fig.png", plot = allredox_contour_2022_fig, height = 6, width = 8)



nonacidic_hydric_redox =
  ungrouped_redox_forfigs_hydric %>% 
  filter(site == "non-acidic tundra" & probe == "3") %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  #geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 2.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(pnw_palette(name = "Bay", 4)))+
  scale_fill_manual(values = rev(pnw_palette(name = "Bay", 4)))+
  ylim(-250, 700)+
  labs(x = "", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(.~position)+
  theme_er1()+
  theme(axis.text.x = element_text(size = 9),
        legend.position = "bottom")

nonacidic_mesic_redox =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "mesic" & site == "non-acidic tundra" & probe == "3") %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  #geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 2.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(natparks.pals(name = "Olympic")))+
  scale_fill_manual(values = rev(natparks.pals(name = "Olympic")))+
  ylim(-250, 700)+
  labs(x = "", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(.~position)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 9),
        legend.position = "bottom")

nonacidic_dry_redox_legend =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "dry" & site == "non-acidic tundra" & probe == 1) %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  #geom_rect(aes(xmin=as_datetime('2021-06-21 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 2.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(natparks.pals(name = "Olympic")))+
  scale_fill_manual(values = rev(natparks.pals(name = "Olympic")))+
  ylim(-250, 700)+
  labs(x = "", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(.~position)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 9),
        legend.position = "right")




acidichydric =
  combo_redox_withdepths %>% 
  filter(site == "east" & position == "hydric") %>% 
  group_by(depth_cm) %>% 
  mutate(depth_cm = factor(depth_cm))

print(acidichydric$depth_cm)

acidicmesic =
  combo_redox_withdepths %>% 
  filter(site == "east" & position == "mesic") %>% 
  group_by(depth_cm) %>% 
  mutate(depth_cm = factor(depth_cm))

print(acidicmesic$depth_cm)


acidicdry =
  combo_redox_withdepths %>% 
  filter(site == "east" & position == "dry") %>% 
  group_by(depth_cm) %>% 
  mutate(depth_cm = factor(depth_cm))

print(acidicdry$depth_cm)


acidic_hydric_redox =
  ungrouped_redox_forfigs_hydric %>% 
  filter(site == "acidic tundra" & position == "hydric") %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  #geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 2.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(natparks.pals(name = "Olympic")))+
  scale_fill_manual(values = rev(natparks.pals(name = "Olympic")))+
  ylim(-250, 700)+
  labs(x = "", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(probe~position)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "bottom")

acidic_mesic_redox =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "mesic" & site == "acidic tundra") %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  #geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 2.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(natparks.pals(name = "Olympic")))+
  scale_fill_manual(values = rev(natparks.pals(name = "Olympic")))+
  ylim(-250, 700)+
  labs(x = "", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)",
       subtitle = "acidic mesic")+
  facet_grid(probe~site)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90),
        legend.position = "bottom")

acidic_dry_redox =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "dry" & site == "acidic tundra" & probe == "3") %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  #geom_rect(aes(xmin=as_datetime('2021-06-24 17:00:00'), xmax= as_datetime('2021-09-17 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_2, fill = depth_2), size = 2.5, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_manual(values = rev(natparks.pals(name = "Olympic")))+
  scale_fill_manual(values = rev(natparks.pals(name = "Olympic")))+
  ylim(-250, 700)+
  labs(x = "", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(.~position)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "right")

ggsave("output/redox_groupdepth.tiff", plot = redoxfig_depth_sd, height = 7, width = 5)
ggsave("output/redox_groupdepth.png", plot = redoxfig_depth_sd, height = 7, width = 5)
ggsave("output/nonacidic_hydric_redox2022.tiff", plot = nonacidic_hydric_redox, height = 6, width = 12)
ggsave("output/nonacidic_hydric_redox.png", plot = nonacidic_hydric_redox, height = 6.5, width = 4.5)
ggsave("output/nonacidic_mesic_redox.png", plot = nonacidic_mesic_redox, height = 6, width = 4.5)
ggsave("output/nonacidic_dry_redox.png", plot = nonacidic_dry_redox, height = 6, width = 4.5)
ggsave("output/acidic_hydric_redox2022.png", plot = acidic_hydric_redox, height = 9, width = 12)
ggsave("output/acidic_mesic_redox.png", plot = acidic_mesic_redox, height = 5.75, width = 9.5)
ggsave("output/acidic_dry_redox.png", plot = acidic_dry_redox, height = 6, width = 4.5)

ggsave("output/redox_groupdepth.tiff", plot = redoxfig_depth_sd, height = 7, width = 5)
ggsave("output/nonacidic_hydric_redox.tiff", plot = nonacidic_hydric_redox, height = 3, width = 7.5)
ggsave("output/nonacidic_mesic_redox.tiff", plot = nonacidic_mesic_redox, height = 3, width = 7.5)
ggsave("output/nonacidic_dry_redox.tiff", plot = nonacidic_dry_redox, height = 3, width = 9)
ggsave("output/acidic_hydric_redox.tiff", plot = acidic_hydric_redox, height = 3, width = 7.5)
ggsave("output/acidic_mesic_redox.tiff", plot = acidic_mesic_redox, height = 3, width = 7.5)
ggsave("output/acidic_dry_redox.tiff", plot = acidic_dry_redox, height = 3, width = 9)




####contour plots----



contour_nonacidic_hydric_legend =
  ungrouped_redox_forfigs_hydric %>%
  filter(site == "non-acidic tundra" & probe == 3) %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(55, 5)+
  labs(x = "", y = "depth, cm",
       fill = "redox potential, mV")+
  scale_fill_manual(values=pnw_palette("Bay", 11))+
  facet_grid(position~site)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90),
        legend.position = "right")

contour_nonacidic_hydric =
  ungrouped_redox_forfigs_nonacidichydric %>%
  filter(site == "non-acidic tundra" & probe == 1) %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(55, 5)+
  labs(x = "", y = "depth, cm",
       fill = "redox potential, mV")+
  scale_fill_manual(values=pnw_palette("Bay", 11))+
  facet_grid(position~site)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90),
        legend.position = "none")

contour_acidic_hydric =
  ungrouped_redox_forfigs_acidichydric %>% 
  filter(site == "acidic tundra" & position == "hydric" & probe == 3) %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(55, 5)+
  labs(x = "", y = "depth, cm",
       fill = "redox potential, mV")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=pnw_palette("Bay", 11))+
  facet_grid(position~site)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90),
        legend.position = "bottom")

contour_nonacidic_dry =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "dry" & probe == 1 & datetime > "2021-07-10 00:00:00") %>% 
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(30, 5)+
  labs(x = "", y = "depth, cm",
       fill = "redox potential, mV")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=pnw_palette("Bay", 11))+
  facet_grid(position~site)+
  theme_er1()+
  theme(
        legend.position = "bottom", axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.ticks.x = element_blank(),
        strip.text.y = element_text(size = 13),
        strip.text.x = element_text(size = 13),
        panel.border = element_rect(color="gray",size=0.5, fill = NA))

library(metR)


#contour_dry =
  ungrouped_redox_forfigs_nonhydric %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  filter(position == "dry" & probe == 1 & datetime > "2021-07-06 00:00:00") %>% 
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV),
                      linemitre = 0.05)+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "2 week", date_labels = "%b-%d")+
  ylim(30, 5)+
  labs(x = "", y = "depth (cm)",
       fill = "redox potential, mV")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  #scale_fill_manual(values=pnw_palette("Bay", 11))+
  facet_grid(.~site)+
  scale_fill_distiller(super = metR::ScaleDiscretised, palette = "PuOr")+
  theme_er1()+
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 12),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 12, vjust = 0.5, angle = 45),
    strip.text.y = element_text(size = 13),
    strip.text.x = element_text(size = 13),
    panel.border = element_rect(color="gray",size=0.5, fill = NA))


ggsave("output/contour_dry.png", plot = contour_dry, width = 7.5, height = 3)


contour_nonacidic_dry_legend =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "dry" & site == "non-acidic tundra" & probe == 1) %>% 
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(30, 5)+
  labs(x = "", y = "depth, cm",
       fill = "redox potential, mV")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=pnw_palette("Bay", 11))+
  facet_grid(position~site)+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "right")

contour_acidic_dry =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "dry" & site == "acidic tundra" & probe == 1 & datetime > "2021-07-10 00:00:00") %>% 
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(30, 5)+
  labs(x = "", y = "depth, cm",
       fill = "redox potential, mV")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=pnw_palette("Bay", 11))+
  #facet_grid(position~site)+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "none")


contour_acidic_dry_legend =
  ungrouped_redox_forfigs_nonhydric %>% 
  filter(position == "dry" & site == "acidic tundra" & probe == 1) %>% 
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  ylim(30, 5)+
  labs(x = "", y = "depth, cm",
       fill = "redox potential, mV")+
  #scale_fill_manual(values=natparks.pals("Arches", 11))+
  scale_fill_manual(values=pnw_palette("Bay", 11))+
  #facet_grid(position~site)+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "left")

ggsave("output/contour_acidic_dry.png", plot = contour_acidic_dry, height = 3, width = 10)
ggsave("output/contour_acidic_dry_legend.png", plot = contour_acidic_dry_legend, height = 5, width = 10)
ggsave("output/contour_nonacidic_dry.png", plot = contour_nonacidic_dry, height = 3, width = 10)
ggsave("output/contour_nonacidic_dry_legend.png", plot = contour_nonacidic_dry_legend, height = 5, width = 10)


ggsave("output/contour_nonacidic_dry.png", plot = contour_nonacidic_dry, height = 5.5, width = 8)
ggsave("output/contour_acidic_hydric.png", plot = contour_acidic_hydric, height = 5.5, width = 8)
ggsave("output/contour_nonacidic_hydric.png", plot = contour_nonacidic_hydric, height = 5.5, width = 8)

