#ECROONEY
#September 23 2022

#PCAS 

#load all packages

source("code/0-packages.R")

#load data

combo_redox_withdepths = read.csv("processed/all_combine.csv")

#redox_nosummary = read.csv('processed/all_combine_depthbins_nosummary.csv')

final_temp_sal_moist = read.csv("processed/final_temp_salinity_avgs.csv")

#add depth bins for figures

final_temp_sal_moist_bins =
  final_temp_sal_moist %>% 
  # create bins of 5 cm depth increments
  mutate(depth_bins = cut_width(depth, width = 10, center=5)) %>% 
  # mutate(depth_bins = case_when(depth_cm = 5 ~ cut_width(depth_cm, width = 1, center = 5))) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]","")) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[","")) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
  # now separate this into two different columns
  separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm")) %>% 
  mutate(depth_start_cm = as.integer(depth_start_cm),
         depth_stop_cm = as.integer(depth_stop_cm)) %>% 
  mutate(depth2 = depth_stop_cm - depth_start_cm)

final_temp_sal_moist_bins_forjoin =
  final_temp_sal_moist_bins %>% 
  dplyr::select(site, position, Betterdate, depth, moisture, temp, salinity)


combo_redox_threedepths_avg =
  combo_redox_withdepths %>% 
  rename(depth_redox = depth_cm) %>% 
  mutate(depth = case_when(depth_redox <= 10 ~ 5,
                           depth_redox <= 20 ~ 15,
                           depth_redox >= 21 ~ 25)) %>% 
  dplyr::select(site, position, Betterdate, Plot, avg_values_fixed, depth, depth_redox) %>%
  group_by(Betterdate, site, position, depth) %>% 
  dplyr::summarise(depth_avg = round(mean(avg_values_fixed),2),
                   depth_avg_se = round(sd(avg_values_fixed)/sqrt(n()),2),
                   n = n()) %>% 
  left_join(final_temp_sal_moist_bins_forjoin) %>% 
  mutate(Betterdate2 = Betterdate) %>% 
  separate(Betterdate, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>%
  dplyr::mutate(month_name = case_when(grepl("06", month)~"june",
                                       grepl("07", month)~"july",
                                       grepl("08", month)~"august",
                                       grepl("09", month)~"september")) %>% 
  mutate(month_name = factor(month_name, levels = c("june", "july", "august", "september"))) %>% 
  rename(redox_avg = depth_avg) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>% 
  ungroup()


final_temp_sal_moist_bins_forjoin_daily =
  final_temp_sal_moist_bins_forjoin %>% 
  separate(Betterdate, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>% 
  group_by(site, position, month, day, depth) %>% 
  dplyr::summarise(temp_avg = round(mean(temp),2),
                   temp_se = round(sd(temp)/sqrt(n()),2),
                   moisture_avg = round(mean(moisture),2),
                   moisture_se = round(sd(moisture)/sqrt(n()),2),
                   salinity_avg = round(mean(salinity),2),
                   salinity_se = round(sd(salinity)/sqrt(n()),2))


combo_redox_threedepths_avgdaily =
  combo_redox_withdepths %>% 
  rename(depth_redox = depth_cm) %>% 
  mutate(depth = case_when(depth_redox <= 10 ~ 5,
                           depth_redox <= 20 ~ 15,
                           depth_redox >= 21 ~ 25)) %>% 
  separate(Betterdate, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>%
  dplyr::select(site, position, Plot, avg_values_fixed, month, day, depth, depth_redox) %>%
  group_by(site, position, month, day, depth) %>% 
  dplyr::summarise(depth_avg = round(mean(avg_values_fixed),2),
                   depth_avg_se = round(sd(avg_values_fixed)/sqrt(n()),2),
                   n = n()) %>% 
  left_join(final_temp_sal_moist_bins_forjoin_daily) %>% 
  rename(redox_avg = depth_avg) %>% 
  dplyr::mutate(month_name = case_when(grepl("06", month)~"june",
                                       grepl("07", month)~"july",
                                       grepl("08", month)~"august",
                                       grepl("09", month)~"september")) %>%
  mutate(month_name = factor(month_name, levels = c("june", "july", "august", "september"))) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>% 
  ungroup()


#PCAS

library(ggbiplot)

## 3a. all samples ---------------------------------------------------------
## step i. make wider

#this code needs to be fixed up, it is currently silly and doesn't make sense.
# combo_redox_threedepths_avg_wider =
#   combo_redox_threedepths_avg %>% 
#   pivot_wider()

#already wider

## step ii. split into numeric/factor dataframes, and run PCA on those
num = 
  combo_redox_threedepths_avg %>% 
  dplyr::select(c(moisture, temp, salinity, redox_avg)) 

grp = 
  combo_redox_threedepths_avg %>% 
  dplyr::select(-c(moisture, temp, salinity, redox_avg),
                site,position, depth, month) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)


###


all_pca = 
  ggbiplot(pca, obs.scale = 1, var.scale = 1,
           groups = as.character(grp$position),
           alpha = 0,
           ellipse = FALSE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(fill = groups, color = groups, shape = interaction(grp$site, grp$depth)))+
  labs(shape = "", fill = "", color = "",
       caption = "solids = control")+
  scale_shape_manual(values = c(21, 22, 25, 1, 0, 6))+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  ylim(-4,4)+
  xlim(-4,4)+
  theme_er1()+
  guides(fill=guide_legend(override.aes=list(fill="black")))+
  theme(legend.position = "bottom", 
        panel.border = element_rect(color="white",size=0.5, fill = NA))+
  NULL

ggsave("output/all_pca.tiff", plot = all_pca, height = 4.5, width = 4.5)


all_pca = 
  ggbiplot(pca, obs.scale = 1, var.scale = 1,
           groups = as.character(grp$position),
           alpha = 0,
           ellipse = FALSE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(fill = groups, color = groups, shape = interaction(grp$site, grp$depth)))+
  labs(shape = "", fill = "", color = "",
       caption = "solids = control")+
  scale_shape_manual(values = c(21, 22, 25, 1, 0, 6))+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  ylim(-4,4)+
  xlim(-4,4)+
  theme_er1()+
  guides(fill=guide_legend(override.aes=list(fill="black")))+
  theme(legend.position = "bottom", 
        panel.border = element_rect(color="white",size=0.5, fill = NA))+
  NULL

ggsave("output/all_pca.tiff", plot = all_pca, height = 4.5, width = 4.5)


#5 cm

## step ii. split into numeric/factor dataframes, and run PCA on those

combo_redox_threedepths_avg5 = 
  combo_redox_threedepths_avg %>% 
  filter(depth == "5")


num = 
  combo_redox_threedepths_avg5 %>% 
  dplyr::select(c(moisture, temp, salinity, redox_avg)) 

grp = 
  combo_redox_threedepths_avg5 %>% 
  dplyr::select(-c(moisture, temp, salinity, redox_avg),
                site,position, depth, month) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)


###


all_pca_5cm = 
  ggbiplot(pca, obs.scale = 1, var.scale = 1,
           groups = as.character(grp$position),
           alpha = 0,
           ellipse = FALSE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(fill = groups, color = groups, shape = grp$site))+
  labs(shape = "", fill = "", color = "",
       caption = "solids = control")+
  scale_shape_manual(values = c(21, 22, 25, 1, 0, 6))+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  ylim(-4,4)+
  xlim(-4,4)+
  theme_er1()+
  guides(fill=guide_legend(override.aes=list(fill="black")))+
  theme(legend.position = "bottom", 
        panel.border = element_rect(color="white",size=0.5, fill = NA))+
  NULL

ggsave("output/all_pca.tiff", plot = all_pca, height = 4.5, width = 4.5)


all_pca = 
  ggbiplot(pca, obs.scale = 1, var.scale = 1,
           groups = as.character(grp$position),
           alpha = 0,
           ellipse = FALSE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(fill = groups, color = groups, shape = interaction(grp$site, grp$depth)))+
  labs(shape = "", fill = "", color = "",
       caption = "solids = control")+
  scale_shape_manual(values = c(21, 22, 25, 1, 0, 6))+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  ylim(-4,4)+
  xlim(-4,4)+
  theme_er1()+
  guides(fill=guide_legend(override.aes=list(fill="black")))+
  theme(legend.position = "bottom", 
        panel.border = element_rect(color="white",size=0.5, fill = NA))+
  NULL

ggsave("output/all_pca.tiff", plot = all_pca, height = 4.5, width = 4.5)






