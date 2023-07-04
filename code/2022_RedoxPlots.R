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



frozen_group2021=
  combo_redox_withdepths2021 %>% 
  # mutate(keep = case_when(site == "non-acidic tundra" & position == "hydric" & probe == 1 ~ "keep",
  #                         site == "acidic tundra" & position == "hydric" & probe == 3 ~ "keep",
  #                         site == "non-acidic tundra" & position == "dry" & probe == 1 ~ "keep",
  #                         site == "acidic tundra" & position == "dry" & probe == 1 ~ "keep",
  #                         site == "acidic tundra" & position == "mesic" & probe == 1 ~ "keep",
  #                         site == "non-acidic tundra" & position == "mesic" & probe == 3 ~ "keep")) %>%
  dplyr::mutate(keep = if_else(site == "non-acidic tundra" & position == "hydric" & depth_cm > 40 & Betterdate <= '2021-08-09 00:00', paste0("frozen"),"unfrozen")) %>% 
  filter(keep == "unfrozen") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) 

frozen_group2022 =
  combo_redox_withdepths2022 %>% 
  dplyr::mutate(keep = if_else(site == "non-acidic tundra" & position == "dry" & depth_cm > 21 & Betterdate >= '2022-07-30 00:00', paste0("frozen"),"unfrozen")) %>% 
  filter(keep == "unfrozen") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) 

all_grouped =
  frozen_group2022 %>% 
  vctrs::vec_c(frozen_group2021) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
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
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
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
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
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
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
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
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
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


# 
# 
# ungrouped_redox_forfigs_hydric = 
#   combo_redox_withdepths %>% 
#   mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
#   mutate(site = recode(site, "east" = "acidic tundra",
#                        "west" = "non-acidic tundra")) %>% 
#   filter(position == "hydric") %>% 
#   separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
#   separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
#   mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
#   mutate(datetime = ymd_hm(paste(date, time)),
#          date = ymd(date)) %>% 
#   mutate(depth_2 = factor(depth_cm, levels = c("0", "2", "4", "5", "6", "14", "15", "16", "24", "25", "26", "35", "44", "45", "46")))   %>% 
#   #filter(probe == 1) %>%
#   dplyr::rename(redox_avg_mV = avg_values_fixed) 

# group_by(site, position, depth_2, datetime) %>%
# dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
#                  redox_sd = round(sd(avg_values_fixed),2))

# ungrouped_redox_forfigs_acidichydric = 
#   combo_redox_withdepths %>% 
#   mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
#   mutate(site = recode(site, "east" = "acidic tundra",
#                        "west" = "non-acidic tundra")) %>% 
#   filter(position == "hydric") %>% 
#   separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
#   separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
#   mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
#   mutate(datetime = ymd_hm(paste(date, time)),
#          date = ymd(date)) %>% 
#   mutate(depth_2 = factor(depth_cm, levels = c("1", "3", "5", "7", "9", "11", "13", "15", "19", "23", "25", "29", "30", "33", "35", "43", "49", "53", "55")))   %>% 
#   #filter(probe == 1) %>%
#   dplyr::rename(redox_avg_mV = avg_values_fixed) 
# # group_by(site, position, depth_2, datetime) %>%
# # dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
# #                  redox_sd = round(sd(avg_values_fixed),2))

ungrouped_redox_forfigs_nonhydric = 
  combo_redox_withdepths2021 %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  filter(position != "hydric") %>% 
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
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
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
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
  separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(datetime = ymd_hm(paste(date, time)),
         date = ymd(date)) %>%
  #filter(probe == 1) %>%
  dplyr::rename(redox_avg_mV = avg_values_fixed) 

redoxfig_depth_sd =
  all_grouped %>% 
  #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = position, fill = position), group = 'position')+
  geom_point(size = 3, alpha = 0.4, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE)+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  ylim(60, 0)+
  labs(x = 'redox potential (mV)',
       y = "depth (cm)",
       color = "", fill = "")+
  scale_x_continuous(position="top")+
  #facet_grid(.~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "bottom")

ggsave("formanuscript/redoxfig_depth_sd.png", plot = redoxfig_depth_sd, height = 6.5, width = 3.5)



redoxfig_depth_sd2022 =
  grouped_redox_forfigs2022 %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = position, fill = position), group = 'position')+
  geom_point(size = 3, alpha = 0.8, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype="longdash")+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  ylim(60, 0)+
  labs(
      x = '2022 
      redox potential (mV)',
       y = "depth (cm)",
       color = "", fill = "")+
  scale_x_continuous(position="top")+
  facet_grid(.~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

redoxfig_depth_sd2021 =
  grouped_redox_forfigs2021 %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = position, fill = position), group = 'position')+
  geom_point(size = 3, alpha = 0.8, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype="longdash")+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  # scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  # scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  ylim(60, 0)+
  labs(
       x = '2021
       redox potential (mV)',
       y = "depth (cm)",
       color = "", fill = "")+
  scale_x_continuous(position="top")+
  facet_grid(.~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
# 
# redoxfig_depth_sd2021_hydric =
#   grouped_redox_forfigs2021 %>% 
#   mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   filter(position == "hydric") %>% 
#   ggplot(aes(y = depth_cm, x = redox_avg_mV, color = position, fill = position), group = 'position')+
#   geom_point(size = 3, alpha = 0.8, shape = c(21))+
#   geom_line(orientation = "y", show.legend = FALSE, linetype="longdash")+
#   geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
#   scale_color_manual(values = c("#118ab2"))+
#   scale_fill_manual(values = c("#118ab2"))+
#   ylim(60, 0)+
#   labs(
#     x = 'redox potential (mV)',
#     y = "depth (cm)",
#     color = "", fill = "")+
#   scale_x_continuous(position="top")+
#   facet_grid(.~site, switch = "x")+
#   theme_er1()+
#   theme(legend.position = "bottom")
# 
# 
# redoxfig_depth_sd2022_hydric =
#   grouped_redox_forfigs2022 %>% 
#   mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   filter(position == "hydric") %>% 
#   ggplot(aes(y = depth_cm, x = redox_avg_mV, color = position, fill = position), group = 'position')+
#   geom_point(size = 3, alpha = 0.8, shape = c(21))+
#   geom_line(orientation = "y", show.legend = FALSE)+
#   geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
#   scale_color_manual(values = c("#118ab2"))+
#   scale_fill_manual(values = c("#118ab2"))+
#   ylim(60, 0)+
#   labs(
#     x = 'redox potential (mV)',
#     y = "depth (cm)",
#     color = "", fill = "")+
#   scale_x_continuous(position="top")+
#   facet_grid(.~site, switch = "x")+
#   theme_er1()+
#   theme(legend.position = "bottom")
# 
# redoxfig_depth_sd2021_dry =
#   grouped_redox_forfigs2021 %>% 
#   mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   filter(position == "dry") %>% 
#   ggplot(aes(y = depth_cm, x = redox_avg_mV, color = site, fill = site), group = 'position')+
#   geom_point(size = 3, shape = c(21))+
#   geom_line(orientation = "y", show.legend = FALSE, linetype="longdash")+
#   geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
#   scale_color_manual(values = c("#d8572a", "#6a040f"))+
#   scale_fill_manual(values = c("#d8572a", "#6a040f"))+
#   ylim(60, 0)+
#   labs(
#     x = 'redox potential (mV)',
#     y = "depth (cm)",
#     color = "", fill = "")+
#   scale_x_continuous(position="top")+
#   facet_grid(.~position, switch = "x")+
#   theme_er1()+
#   theme(legend.position = "right")

ggsave("formanuscript/redoxfig_depth_sd2021.png", plot = redoxfig_depth_sd2021, height = 6, width = 4)
ggsave("formanuscript/redoxfig_depth_sd2022.png", plot = redoxfig_depth_sd2022, height = 6, width = 4)
# ggsave("formanuscript/redoxfig_depth_sd2021hydric.png", plot = redoxfig_depth_sd2021_hydric, height = 6, width = 4)
# ggsave("formanuscript/redoxfig_depth_sd2022hydric.png", plot = redoxfig_depth_sd2022_hydric, height = 6, width = 4)
# ggsave("formanuscript/redoxfig_depth_sd2021_dry.png", plot = redoxfig_depth_sd2021_dry, height = 6, width = 5)


grouped_redox_all =
grouped_redox_forfigs2021 %>% 
  vctrs::vec_c(grouped_redox_forfigs2022) %>% 
  mutate(year = as.factor(year))

redoxfig_depth_sdall =
  grouped_redox_all %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = year, fill = year), group = 'year')+
  geom_point(size = 3, alpha = 0.8, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype = "longdash")+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  scale_color_manual(values = c("#0496ff", "#f7717d"))+
  scale_fill_manual(values = c("#0496ff", "#f7717d"))+
  ylim(60, 0)+
  labs(x = 'redox potential (mV)',
       y = "depth (cm)",
       color = "", fill = "")+
  scale_x_continuous(position="top")+
  facet_grid(position~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave("formanuscript/redoxfig_depth_sdall.png", plot = redoxfig_depth_sdall, height = 9, width = 4)

# redoxfig_depth_sd_dry =
#   grouped_redox_all %>% 
#   filter(position == "dry" & site == "acidic tundra") %>% 
#   mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   ggplot(aes(y = depth_cm, x = redox_avg_mV, color = year, fill = year), group = 'year')+
#   geom_point(size = 3, alpha = 0.8, shape = c(21))+
#   geom_line(orientation = "y", show.legend = FALSE)+
#   geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
#   scale_color_manual(values = c("#0496ff", "#f7717d"))+
#   scale_fill_manual(values = c("#0496ff", "#f7717d"))+
#   #ylim(60, 0)+
#   labs(x = 'redox potential (mV)',
#        y = "depth (cm)",
#        color = "", fill = "")+
#   scale_x_continuous(position="top")+
#   scale_y_reverse()+
#   facet_grid(position~site, switch = "x")+
#   theme_er1()+
#   theme(legend.position = "bottom")
# 
# ggsave("formanuscript/redoxfig_depth_sd_dry.png", plot = redoxfig_depth_sd_dry, height = 5, width = 3.5)
# 
# 
# redoxfig_depth_sd_mesic =
#   grouped_redox_all %>% 
#   filter(position == "mesic") %>% 
#   mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   ggplot(aes(y = depth_cm, x = redox_avg_mV, color = year, fill = year), group = 'year')+
#   geom_point(size = 3, alpha = 0.8, shape = c(21))+
#   geom_line(orientation = "y", show.legend = FALSE)+
#   geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
#   scale_color_manual(values = c("#0496ff", "#f7717d"))+
#   scale_fill_manual(values = c("#0496ff", "#f7717d"))+
#   #ylim(60, 0)+
#   scale_y_reverse()+
#   labs(x = 'redox potential (mV)',
#        y = "depth (cm)",
#        color = "", fill = "")+
#   scale_x_continuous(position="top")+
#   facet_grid(position~site, switch = "x")+
#   theme_er1()+
#   theme(legend.position = "bottom")
# 
# ggsave("formanuscript/redoxfig_depth_sd_mesic.png", plot = redoxfig_depth_sd_mesic, height = 5, width = 5)
# 
# redoxfig_depth_sd_hydric =
#   grouped_redox_all %>% 
#   filter(position == "hydric" & site == "non-acidic tundra") %>% 
#   #mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   ggplot(aes(y = depth_cm, x = redox_avg_mV, color = year, fill = year), group = 'year')+
#   geom_point(size = 3, alpha = 0.8, shape = c(21))+
#   geom_line(orientation = "y", show.legend = FALSE)+
#   geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
#   scale_color_manual(values = c("#0496ff", "#f7717d"))+
#   scale_fill_manual(values = c("#0496ff", "#f7717d"))+
#   #ylim(60, 0)+
#   scale_y_reverse()+
#   labs(x = 'redox potential (mV)',
#        y = "depth (cm)",
#        color = "", fill = "")+
#   scale_x_continuous(position="top")+
#   facet_grid(position~site, switch = "x")+
#   theme_er1()+
#   theme(legend.position = "bottom")
# 
# ggsave("formanuscript/redoxfig_depth_sd_hydric.png", plot = redoxfig_depth_sd_hydric, height = 5, width = 3)
# 




redox_temporal_fig_2021 =
  grouped_redox_forfigs_temporal2021 %>% 
  mutate(month = factor(month, levels = c("June", "July", "August", "September")))   %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = month, fill = month), group = 'position')+
  geom_point(size = 3, alpha = 0.8, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype = "longdash")+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  # scale_color_manual(values = (pnw_palette('Sunset', 4)))+
  # scale_fill_manual(values = (pnw_palette('Sunset', 4)))+
  scale_color_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  scale_fill_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
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
        axis.text.x = element_text(size = 7.5, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks
#remove y axis ticks

ggsave("formanuscript/redox_temporal_fig_2021.png", plot = redox_temporal_fig_2021, height = 7, width = 5.5)

redox_temporal_fig_2022 =
  grouped_redox_forfigs_temporal2022 %>% 
  mutate(month = factor(month, levels = c("June", "July", "August", "September")))   %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = month, fill = month), group = 'position')+
  geom_point(size = 3, alpha = 0.8, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype = "longdash")+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  # scale_color_manual(values = (pnw_palette('Sunset', 4)))+
  # scale_fill_manual(values = (pnw_palette('Sunset', 4)))+
  scale_color_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  scale_fill_manual(values = rev(c("#f94144", "#f8961e", "#57cc99", "#4361ee")))+
  ylim(60, 0)+
  labs(x = '2022
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
        axis.text.x = element_text(size = 7.5, hjust=0.8,vjust=0.2,angle = 90))  #remove y axis ticks
#remove y axis ticks

ggsave("formanuscript/redox_temporal_fig_2022.png", plot = redox_temporal_fig_2022, height = 7, width = 5.5)

redox_temporal_fig_2021_legend =
  grouped_redox_forfigs_temporal2021 %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = month, fill = month), group = 'position')+
  geom_point(size = 3, alpha = 0.8, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype = "longdash")+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  scale_color_manual(values = (pnw_palette('Sunset', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset', 4)))+
  ylim(60, 0)+
  labs(x = '2021
       redox potential (mV)',
       y = "depth (cm)",
       color = "", fill = "")+
  scale_x_continuous(position="top")+
  facet_grid(position~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "right")

ggsave("formanuscript/redox_temporal_fig_2021_legend.png", plot = redox_temporal_fig_2021_legend, height = 8, width = 7)




redox_temporal_fig_dry =
  grouped_redox_forfigs_temporal2021 %>% 
  filter(position == "dry") %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = position, fill = position), group = 'position')+
  geom_point(size = 3, alpha = 0.4, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE)+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  ylim(60, 0)+
  labs(x = 'redox potential (mV)',
       y = "depth (cm)",
       color = "", fill = "")+
  scale_x_continuous(position="top")+
  facet_grid(site~month, switch = "x")+
  theme_er1()

ggsave("formanuscript/redox_temporal_fig_dry.png", plot = redox_temporal_fig_dry, height = 7, width = 7)

redox_temporal_fig_2 =
  grouped_redox_forfigs_temporal2022 %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
 # filter(position == "hydric" & site == "acidic tundra") %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
 # mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = month, fill = month), group = 'month')+
  geom_point(size = 3, alpha = 0.9, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE)+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  # scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  # scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = (pnw_palette('Sunset2', 3)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 3)))+
  ylim(60, 0)+
  labs(
       x = 'redox potential (mV)',
       y = "depth (cm)",
       color = "", fill = "")+
  scale_x_continuous(position="top")+
  facet_grid(position~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "right")

redox_temporal_fig_2021_nonacidichydric =
  grouped_redox_forfigs_temporal2021 %>% 
 filter(position == "hydric" & site == "non-acidic tundra") %>% 
  mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  ggplot(aes(y = depth_cm, x = redox_avg_mV, color = month, fill = month), group = 'month')+
  geom_point(size = 4, alpha = 0.9, shape = c(21))+
  geom_line(orientation = "y", show.legend = FALSE, linetype="longdash")+
  geom_errorbar(aes(xmin=redox_avg_mV-redox_sd, xmax=redox_avg_mV+redox_sd), show.legend = FALSE)+
  # scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  # scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  scale_color_manual(values = (pnw_palette('Sunset2', 4)))+
  scale_fill_manual(values = (pnw_palette('Sunset2', 4)))+
  ylim(60, 0)+
  labs(x = 'redox potential (mV)',
       y = "depth (cm)",
       color = "", fill = "")+
  scale_x_continuous(position="top")+
  facet_grid(position~site, switch = "x")+
  theme_er1()+
  theme(legend.position = "right",
        strip.text.y = element_blank())

redox_moisture_temporal = redox_temporal_fig_2 + moisturefig_temporal

ggsave("formanuscript/2022redox_temporal_all.png", plot = redox_temporal_fig_2, height = 8, width = 6.5)
ggsave("formanuscript/2021redox_temporal_fig_2.png", plot = redox_temporal_fig_2021_nonacidichydric, height = 5, width = 5)


#ggsave("formanuscript/redox_moisture_temporal.png", plot = redox_moisture_temporal, height = 10, width = 10)
ggsave("formanuscript/2022redox_fig.png", plot = redoxfig_depth_sd, height = 7, width = 6)


ungrouped_redox_forfigs2021_probe =
  ungrouped_redox_forfigs2021 %>% 
  mutate(keep = case_when(site == "non-acidic tundra" & position == "hydric" & probe == 1 ~ "keep",
                          site == "acidic tundra" & position == "hydric" & probe == 3 ~ "keep",
                          site == "non-acidic tundra" & position == "dry" & probe == 1 ~ "keep",
                          site == "acidic tundra" & position == "dry" & probe == 1 ~ "keep",
                          site == "acidic tundra" & position == "mesic" & probe == 1 ~ "keep",
                          site == "non-acidic tundra" & position == "mesic" & probe == 3 ~ "keep")) %>% 
  filter(keep == "keep") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) 
  
ungrouped_redox_forfigs2022_probe =
  ungrouped_redox_forfigs2022 %>% 
  mutate(keep = case_when(site == "non-acidic tundra" & position == "hydric" & probe == 3 ~ "keep",
                          site == "acidic tundra" & position == "hydric" & probe == 3 ~ "keep",
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
  scale_color_gradientn(colors = rev(natparks.pals(name = "Olympic")))+
  scale_fill_gradientn(colors = rev(natparks.pals(name = "Olympic")))+
  ylim(-250, 700)+
  labs(x = "2021", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site)+
  theme_er1()+
  guides(color = guide_colorbar(reverse = TRUE), 
         fill = guide_colorbar(reverse = TRUE))+
  theme(axis.text.x = element_text(size = 9, vjust = 0.5, angle = 45),
        legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave("formanuscript/allredox_lines_2021_fig.png", plot = allredox_lines_2021_fig, height = 6, width = 6)

allredox_lines_2022_fig =                          
  ungrouped_redox_forfigs2022_probe %>% 
  ggplot(aes(y = redox_avg_mV, x = datetime), group = 'depth_cm')+
  #geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  geom_point(aes(color = depth_cm, fill = depth_cm), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_gradientn(colors = rev(natparks.pals(name = "Olympic")))+
  scale_fill_gradientn(colors = rev(natparks.pals(name = "Olympic")))+
  ylim(-250, 700)+
  labs(x = "2022", y = "redox potential (mV)",
       color = "depth (cm)", fill = "depth (cm)")+
  facet_grid(position~site)+
  theme_er1()+
  guides(color = guide_colorbar(reverse = TRUE), 
         fill = guide_colorbar(reverse = TRUE))+
  theme(axis.text.x = element_text(size = 9, vjust = 0.5, angle = 45),
        legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave("formanuscript/allredox_lines_2022_fig.png", plot = allredox_lines_2022_fig, height = 6, width = 6)


allredox_contour_2021_fig =                          
  ungrouped_redox_forfigs2021_probe %>% 
  ggplot()+
  geom_contour_filled(aes(y = depth_cm, x = datetime, z = redox_avg_mV))+
  #geom_rect(aes(xmin=as_datetime('2021-06-14 17:00:00'), xmax= as_datetime('2021-09-20 10:15:00'), ymin=100, ymax=300), fill = "grey", alpha = 0.5)+
  # geom_point(aes(color = depth_cm, fill = depth_cm), size = 1, alpha = 0.6, shape = c(21))+
  #annotate(xmin='2021-06-21 00:15:00', xmax='2021-09-20 00:15:00', ymin=100, ymax=300, geom='rect', color='grey', alpha=0.5)+
  #geom_line(orientation = "x", show.legend = FALSE)+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_fill_manual(values=pnw_palette("Bay", 11))+
  labs(x = "2021", y = "depth, cm",
       fill = "redox potential (mV)")+
  facet_grid(position~site, scales = "free_y")+
  scale_y_reverse()+
  theme_er1()+
  theme(axis.text.x = element_text(size = 9, vjust = 0.5, angle = 45),
        legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave("formanuscript/allredox_contour_2021_fig.png", plot = allredox_contour_2021_fig, height = 6, width = 6)

allredox_contour_2022_fig =                          
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
        legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())



ggsave("formanuscript/allredox_contour_2022_fig.png", plot = allredox_contour_2022_fig, height = 6, width = 6)

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

ggsave("formanuscript/allredox_contour_2022_fig.png", plot = allredox_contour_2022_fig, height = 6, width = 8)



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

ggsave("figures_finalized/redox_groupdepth.tiff", plot = redoxfig_depth_sd, height = 7, width = 5)
ggsave("figures_finalized/redox_groupdepth.png", plot = redoxfig_depth_sd, height = 7, width = 5)
ggsave("figures_finalized/nonacidic_hydric_redox2022.tiff", plot = nonacidic_hydric_redox, height = 6, width = 12)
ggsave("figures_finalized/nonacidic_hydric_redox.png", plot = nonacidic_hydric_redox, height = 6.5, width = 4.5)
ggsave("figures_finalized/nonacidic_mesic_redox.png", plot = nonacidic_mesic_redox, height = 6, width = 4.5)
ggsave("figures_finalized/nonacidic_dry_redox.png", plot = nonacidic_dry_redox, height = 6, width = 4.5)
ggsave("figures_finalized/acidic_hydric_redox2022.png", plot = acidic_hydric_redox, height = 9, width = 12)
ggsave("figures_finalized/acidic_mesic_redox.png", plot = acidic_mesic_redox, height = 5.75, width = 9.5)
ggsave("figures_finalized/acidic_dry_redox.png", plot = acidic_dry_redox, height = 6, width = 4.5)

ggsave("figures_finalized/redox_groupdepth.tiff", plot = redoxfig_depth_sd, height = 7, width = 5)
ggsave("formanuscript/nonacidic_hydric_redox.tiff", plot = nonacidic_hydric_redox, height = 3, width = 7.5)
ggsave("formanuscript/nonacidic_mesic_redox.tiff", plot = nonacidic_mesic_redox, height = 3, width = 7.5)
ggsave("formanuscript/nonacidic_dry_redox.tiff", plot = nonacidic_dry_redox, height = 3, width = 9)
ggsave("formanuscript/acidic_hydric_redox.tiff", plot = acidic_hydric_redox, height = 3, width = 7.5)
ggsave("formanuscript/acidic_mesic_redox.tiff", plot = acidic_mesic_redox, height = 3, width = 7.5)
ggsave("formanuscript/acidic_dry_redox.tiff", plot = acidic_dry_redox, height = 3, width = 9)




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
  #facet_grid(position~site)+
  theme_minimal()+
  theme(axis.text.x = element_text (size = 9),
        legend.position = "none")

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

ggsave("formanuscript/contour_acidic_dry.png", plot = contour_acidic_dry, height = 3, width = 10)
ggsave("formanuscript/contour_acidic_dry_legend.png", plot = contour_acidic_dry_legend, height = 5, width = 10)
ggsave("formanuscript/contour_nonacidic_dry.png", plot = contour_nonacidic_dry, height = 3, width = 10)
ggsave("formanuscript/contour_nonacidic_dry_legend.png", plot = contour_nonacidic_dry_legend, height = 5, width = 10)


ggsave("figures_finalized/contour_nonacidic_dry.png", plot = contour_nonacidic_dry, height = 5.5, width = 8)
ggsave("figures_finalized/contour_acidic_hydric.png", plot = contour_acidic_hydric, height = 5.5, width = 8)
ggsave("figures_finalized/contour_nonacidic_hydric.png", plot = contour_nonacidic_hydric, height = 5.5, width = 8)

