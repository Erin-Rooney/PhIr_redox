#E Rooney
#Dataloggers 2021 and 2022
#frozen sensors


#January 3 2023

#load all packages

source("code/0-packages.R")

#load data

combo_redox_withdepths_2022 = read.csv("processed/all_combine_2022.csv")
combo_redox_withdepths_2021 = read.csv("processed/all_combine.csv")

#

frozen_2022 =
  combo_redox_withdepths_2022 %>% 
  dplyr::mutate(frozen1 = case_when(avg_values_fixed < 207 & avg_values_fixed > 187 ~ "frozen",
                                   avg_values_fixed >= 207 ~ "unfrozen",
                                   avg_values_fixed <= 187 ~ "unfrozen")) %>% 
  dplyr::mutate(frozen2 = case_when(Betterdate < "2022-07-11 00:00:00" ~ "frozen",
                                    Betterdate > "2022-07-11 00:00:00" ~ "unfrozen")) %>% 
  dplyr::mutate(frozen = if_else(frozen1 == "frozen" & frozen2 == "frozen",paste0("frozen"),"unfrozen")) %>% 
  #dplyr::mutate(frozen = case_when(frozen1 == "frozen" & frozen2 == "frozen" ~ "frozen"))
  mutate(site = recode(site, "west" = "non-acidic tundra", "east" = "acidic tundra")) %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>% 
  mutate(depth_cm = as.numeric(depth_cm)) 


frozen_2021 =
  combo_redox_withdepths_2021 %>% 
  dplyr::mutate(frozen1 = case_when(avg_values_fixed < 207 & avg_values_fixed > 187 ~ "frozen",
                                    avg_values_fixed >= 207 ~ "unfrozen",
                                    avg_values_fixed <= 187 ~ "unfrozen")) %>% 
  dplyr::mutate(frozen2 = case_when(Betterdate < "2021-07-11 00:00:00" ~ "frozen",
                                    Betterdate > "2021-07-11 00:00:00" ~ "unfrozen")) %>% 
  dplyr::mutate(frozen = if_else(frozen1 == "frozen" & frozen2 == "frozen",paste0("frozen"),"unfrozen")) %>% 
  mutate(site = recode(site, "west" = "non-acidic tundra", "east" = "acidic tundra")) %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>% 
  mutate(depth_cm = as.numeric(depth_cm)) 
  


  
  
frozen_2021 %>% 
  #mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  ggplot(aes(y = avg_values_fixed, x = Betterdate, color = depth_cm, fill = depth_cm), group = depth_cm)+
  geom_point(size = 3, alpha = 0.4, shape = c(21))+
  geom_line(orientation = "x", show.legend = FALSE)+
  # scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  # scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  labs(x = '',
       y = "redox potential (mV)",
       color = "", fill = "")+
  #scale_x_continuous(position="top")+
  facet_grid(position~site)+
  theme_er1()


frozen_2021_fig =
frozen_2021 %>% 
  filter(frozen == 'frozen') %>% 
  #mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  ggplot(aes(y = avg_values_fixed, x = as_datetime(Betterdate), color = depth_cm, fill = depth_cm), group = depth_cm)+
  geom_point(size = 3, alpha = 0.4, shape = c(21))+
  # scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  # scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  labs(y = "redox potential (mV)",
       x = "2021",
       color = "", fill = "")+
  #scale_x_continuous(position="top")+
  facet_grid(site~position)+
  theme_er1()

frozen_2022_fig =
  frozen_2022 %>% 
  filter(frozen == 'frozen') %>% 
  #mutate(month = factor(month, levels = c("early summer", "mid summer", "late summer", "early fall")))   %>%
  ggplot(aes(y = avg_values_fixed, x = as_datetime(Betterdate), color = depth_cm, fill = depth_cm), group = depth_cm)+
  geom_point(size = 3, alpha = 0.4, shape = c(21))+
  # scale_color_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  # scale_fill_manual(values = c("#9a031e", "#a7c957", "#1e96fc"))+
  labs(y = "redox potential (mV)",
       x = "2022",
       color = "", fill = "")+
  #scale_x_continuous(position="top")+
  facet_grid(site~position)+
  theme_er1()

ggsave("formanuscript/frozen_2021_fig.tiff", plot = frozen_2021_fig, height = 10, width = 20)
ggsave("formanuscript/frozen_2022_fig.tiff", plot = frozen_2022_fig, height = 10, width = 20)




df1 =
  frozen_2021 %>% 
  filter(frozen == "frozen" & probe == '1') 

df2 =
  frozen_2021 %>% 
  filter(frozen != "frozen" & probe == '1') 

fig_2021 =
  ggplot()+
  geom_point(data = df2, aes(y = depth_cm, x = as_datetime(Betterdate), color = avg_values_fixed, fill = avg_values_fixed), size = 2, alpha = 0.4, shape = c(21))+
  geom_point(data = df1, aes(y = depth_cm, x = as_datetime(Betterdate)), color = "#00f5d4", fill = "#00f5d4", size = 1.5, shape = c(21))+
  labs(y = "depth, cm",
       x = "2021",
       color = "redox potential, mV", fill = "redox potential, mV")+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_gradientn(colors = (natparks.pals(name = "Olympic")))+
  scale_fill_gradientn(colors = (natparks.pals(name = "Olympic")))+
  scale_y_reverse()+
  #scale_x_continuous(position="top")+
  facet_grid(position~site)+
  theme_er1()+
  theme(legend.position = "right")

ggsave("formanuscript/frozen_2021_fig.tiff", plot = fig_2021, height = 5, width = 15)


fig_2021_b =
  ggplot()+
  geom_point(data = df2, aes(y = avg_values_fixed, x = as_datetime(Betterdate), color = depth_cm, fill = depth_cm), size = 2, alpha = 0.4, shape = c(21))+
  geom_point(data = df1, aes(y = avg_values_fixed, x = as_datetime(Betterdate)), color = "#00f5d4", fill = "#00f5d4", size = 1.5, shape = c(21))+
  labs(y = "redox potential (mV)",
       x = "2021",
       color = "depth, cm", fill = "depth, cm")+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_gradientn(colors = rev(natparks.pals(name = "Olympic")))+
  scale_fill_gradientn(colors = rev(natparks.pals(name = "Olympic")))+
  #scale_y_reverse()+
  #scale_x_continuous(position="top")+
  facet_grid(position~site)+
  theme_er1()+
  theme(legend.position = "right")


fig_2021 =
  frozen_2021 %>% 
  filter(site == "acidic tundra" & position == "hydric" & probe == "3") %>% 
  ggplot()+
  geom_point(aes(y = avg_values_fixed, x = as_datetime(Betterdate), color = depth_cm, fill = depth_cm), size = 2, alpha = 0.4, shape = c(21))+
  labs(y = "redox potential (mV)",
       x = "2021",
       color = "depth, cm", fill = "depth, cm")+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_gradientn(colors = rev(natparks.pals(name = "Olympic")))+
  scale_fill_gradientn(colors = rev(natparks.pals(name = "Olympic")))+
  #scale_y_reverse()+
  #scale_x_continuous(position="top")+
  facet_grid(position~site)+
  theme_er1()+
  theme(legend.position = "right")



fig_2021_c =
  ggplot()+
  #geom_point(data = df2, aes(y = avg_values_fixed, x = as_datetime(Betterdate), color = depth_cm, fill = depth_cm), size = 2, alpha = 0.4, shape = c(21))+
  geom_point(data = df1, aes(y = avg_values_fixed, x = as_datetime(Betterdate), color = depth_cm, fill = depth_cm),  size = 1.5, shape = c(21))+
  labs(y = "redox potential (mV)",
       x = "2021",
       color = "depth, cm", fill = "depth, cm")+
  scale_x_datetime(date_breaks = "2 days", date_labels = "%b-%d")+
  scale_color_manual(values = rev(natparks.pals(name = "Olympic", 13)))+
  scale_fill_manual(values = rev(natparks.pals(name = "Olympic", 13)))+
  #scale_y_reverse()+
  #scale_x_continuous(position="top")+
  facet_grid(position~site)+
  theme_er1()+
  theme(legend.position = "right")

ggsave("formanuscript/frozen_2021_figb.png", plot = fig_2021_b, height = 9, width = 15)


df3 =
  frozen_2022 %>% 
  filter(frozen == "frozen") 

df4 =
  frozen_2022 %>% 
  filter(frozen != "frozen") 

fig_2022 =
  ggplot()+
  geom_point(data = df4, aes(y = depth_cm, x = as_datetime(Betterdate), color = avg_values_fixed, fill = avg_values_fixed), size = 2, alpha = 0.4, shape = c(21))+
  geom_point(data = df3, aes(y = depth_cm, x = as_datetime(Betterdate)), color = "#00f5d4", fill = "#00f5d4", size = 1.5, shape = c(21))+
  labs(y = "depth, cm",
       x = "2022",
       color = "redox potential, mV", fill = "redox potential, mV")+
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d")+
  scale_color_gradientn(colors = (natparks.pals(name = "Olympic")))+
  scale_fill_gradientn(colors = (natparks.pals(name = "Olympic")))+
  scale_y_reverse()+
  #scale_x_continuous(position="top")+
  facet_grid(position~site)+
  theme_er1()+
  theme(legend.position = "right")

ggsave("formanuscript/frozen_2022_fig.tiff", plot = fig_2022, height = 5, width = 15)

