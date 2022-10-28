#ECROONEY & IMTIAZ MIAH
#September 14 2022

#load all packages

source("code/0-packages.R")

#load data

enzymes2022 = read.csv("raw/Miah_enzymes_2022.csv")
enzymes2021 = read.csv("raw/Miah_enzymes2021.csv")

#processing data

#fix 2022 dates

enzymes2022_fixed =
  enzymes2022 %>% 
  separate(date, sep = "-", into = c("day", "month", "year")) %>%
  mutate(day = as.numeric(day)) %>% 
  mutate(day = (day + 1)) %>% 
  dplyr::select(-year) %>% 
  mutate(year = 2022) %>% 
  mutate(year = factor(year, levels = c("2021", "2022"))) %>% 
  mutate(soil_layer = factor(soil_layer, levels = c("Organic", "Mineral"))) %>% 
  separate(sample, sep = " ", into = c("area", "plot")) %>%
  dplyr::mutate(plotnum = case_when(grepl("1", plot)~"1",
                                  grepl("2", plot)~"2",
                                  grepl("3", plot)~"3")) %>% 
  mutate(site = factor(site, levels = c("Dry", "Mesic", "Hydric")))


enzymes2021_fixed =
  enzymes2021 %>% 
  separate(date, sep = "-", into = c("day", "month", "year")) %>%
  mutate(day = as.numeric(day)) %>% 
  mutate(day = (day + 1)) %>% 
  dplyr::select(-year) %>% 
  mutate(year = 2021) %>% 
  mutate(year = factor(year, levels = c("2021", "2022"))) %>% 
  mutate(soil_layer = factor(soil_layer, levels = c("Organic", "Mineral"))) %>% 
  separate(sample, sep = " ", into = c("area", "plot")) %>%
  dplyr::mutate(plotnum = case_when(grepl("1", plot)~"1",
                                    grepl("2", plot)~"2",
                                    grepl("3", plot)~"3")) %>% 
  mutate(site = factor(site, levels = c("Dry", "Mesic", "Hydric"))) 
  #filter(month == "Aug")


enzymes2022_fixed_longer =
  enzymes2022_fixed %>% 
  pivot_longer(-c(day, month, year, plot, plotnum, site, area, soil_layer, dry_wet), 
               names_to = "enzyme_type", values_to = "enzyme_activity")
  
enzymes2021_fixed_longer =
  enzymes2021_fixed %>% 
  pivot_longer(-c(day, month, year, plot, plotnum, site, area, soil_layer, dry_wet), 
               names_to = "enzyme_type", values_to = "enzyme_activity")

all_enzymes_longer = 
  enzymes2022_fixed_longer %>% 
  vctrs::vec_c(enzymes2021_fixed_longer) %>% 
  group_by(year, day, site, area, plot, soil_layer, enzyme_type) %>% 
  dplyr::mutate(n = n()) 


all_enzymes_wider = 
  enzymes2022_fixed_longer %>% 
  vctrs::vec_c(enzymes2021_fixed_longer) %>% 
  group_by(year, day, site, area, plot, soil_layer, enzyme_type) %>% 
  pivot_wider(names_from = 'enzyme_type', values_from = "enzyme_activity")
  
all_enzymes_wider_2021 = 
  enzymes2021_fixed_longer %>% 
  group_by(year, day, site, area, plot, soil_layer, enzyme_type) %>% 
  pivot_wider(names_from = 'enzyme_type', values_from = "enzyme_activity") %>% 
  mutate(month = recode(month, "Aug" = "August", "Jul" = "July")) %>% 
  #dplyr::mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(date = paste(month, day, sep = " ")) %>% 
  mutate(date = factor(date, levels = c("July 2", "July 5", "July 20", 
                                        "July 23", "August 3", "August 6"))) %>% 
  filter(soil_layer == "Organic")

all_enzymes_notwider_2021 = 
  enzymes2021_fixed_longer %>% 
  #group_by(year, day, site, area, plot, soil_layer, enzyme_type) %>% 
  #pivot_wider(names_from = 'enzyme_type', values_from = "enzyme_activity") %>% 
  mutate(month = recode(month, "Aug" = "August", "Jul" = "July")) %>% 
  #dplyr::mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(date = paste(month, day, sep = " ")) %>% 
  mutate(date = factor(date, levels = c("July 2", "July 5", "July 20", 
                                        "July 23", "August 3", "August 6"))) %>% 
  filter(soil_layer == "Organic")


#export csv file
write.csv(all_enzymes_longer, "output/all_enzymes_longer.csv")

write.csv(all_enzymes_wider, "output/all_enzymes_wider.csv")


###boxplots


boxplot_all_eastbyyear =
  all_enzymes_longer %>% 
  filter(area == "East") %>% 
  ggplot(aes(x = site, y = enzyme_activity, fill = year))+
  geom_boxplot(alpha = 0.6)+
  labs(y = "enzyme activity, nmol/h/g dry soil",
       x = "Site",
       subtitle = "East")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Shuksan", 2)))+
  facet_grid(enzyme_type~year, scales = "free_y")+
  theme_er1()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave("output/boxplot_all_eastbyyear.png",  plot = boxplot_all_eastbyyear, width = 4, height = 6)


boxplot_all_westbyyear =
all_enzymes_longer %>% 
  filter(area == "West") %>% 
  ggplot(aes(x = site, y = enzyme_activity, fill = year))+
  geom_boxplot(alpha = 0.6)+
  labs(y = "enzyme activity, nmol/h/g dry soil",
       x = "Site",
       subtitle = "West")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Shuksan", 2)))+
  facet_grid(enzyme_type~year, scales = "free_y")+
  theme_er1()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("output/boxplot_all_westbyyear.png",  plot = boxplot_all_westbyyear, width = 4, height = 6)

  
boxplot_all_yearbyarea =
  all_enzymes_longer %>% 
  ggplot(aes(x = site, y = enzyme_activity, fill = area))+
  geom_boxplot(alpha = 0.6)+
  labs(y = "enzyme activity, nmol/h/g dry soil")+
  facet_grid(enzyme_type~year, scales = "free_y")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  theme_er1()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("output/boxplot_all_yearbyarea.png",  plot = boxplot_all_yearbyarea, width = 4, height = 6)



all2022_data =
  all_enzymes_longer %>% 
  filter(year == 2022) %>% 
  ggplot(aes(x = site, y = enzyme_activity, fill = area))+
  geom_boxplot(alpha = 0.6)+
  labs(y = "enzyme activity, nmol/h/g dry soil",
       x = "site")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  facet_grid(enzyme_type~year, scales = "free_y")+
  theme_er1()

ggsave("output/boxplot_all2022_data.png",  plot = all2022_data, width = 4, height = 6)

all2021_data =
  all_enzymes_longer %>% 
  filter(year == 2021) %>% 
  ggplot(aes(x = site, y = enzyme_activity, fill = area))+
  geom_boxplot(alpha = 0.6)+
  labs(y = "enzyme activity, nmol/h/g dry soil",
       x = "site")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  facet_grid(enzyme_type~year, scales = "free_y")+
  theme_er1()

ggsave("output/boxplot_all2021_data.png",  plot = all2021_data, width = 4, height = 6)


#Mineral vs Organic ggplots


boxplot_organic =
  all_enzymes_longer %>% 
  filter(soil_layer == "Organic") %>% 
  ggplot(aes(x = site, y = enzyme_activity, fill = area))+
  geom_boxplot(alpha = 0.6)+
  labs(y = "enzyme activity, nmol/h/g dry organic soil",
       x = "site",
       subtitle = "organic layer")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  facet_grid(enzyme_type~year, scales = "free_y")+
  theme_er1()

ggsave("output/boxplot_organic.png",  plot = boxplot_organic, width = 6, height = 8)

boxplot_mineral =
  all_enzymes_longer %>% 
  filter(soil_layer == "Mineral") %>% 
  ggplot(aes(x = site, y = enzyme_activity, fill = area))+
  geom_boxplot(alpha = 0.6)+
  labs(y = "enzyme activity, nmol/h/g dry mineral soil",
       x = "site",
       subtitle = "mineral layer")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  facet_grid(enzyme_type~year, scales = "free_y")+
  theme_er1()

ggsave("output/boxplot_mineral.png",  plot = boxplot_mineral, width = 6, height = 8)


boxplot_2021bysoillayer =
  all_enzymes_longer %>% 
  filter(year == "2021") %>% 
  ggplot(aes(x = soil_layer, y = enzyme_activity, fill = area))+
  geom_boxplot(alpha = 0.6)+
  labs(y = "enzyme activity, nmol/h/g dry soil",
       x = "soil layer",
       subtitle = "2021")+
  facet_grid(enzyme_type~site, scales = "free_y")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  theme_er1()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("output/boxplot_2021_soillayers.png",  plot = boxplot_2021bysoillayer, width = 6, height = 10)


boxplot_2022bysoillayer =
all_enzymes_longer %>% 
  filter(year == "2022") %>% 
  ggplot(aes(x = soil_layer, y = enzyme_activity, fill = area))+
  geom_boxplot(alpha = 0.6)+
  labs(y = "enzyme activity, nmol/h/g dry soil",
       x = "soil layer",
       subtitle = "2022")+
  facet_grid(enzyme_type~site, scales = "free_y")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  theme_er1()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("output/boxplot_2022_soillayers.png",  plot = boxplot_2022bysoillayer, width = 6, height = 10)


##########

all_enzymes_longer %>% 
  filter(year == "2022") %>% 
  ggplot(aes(x = site, y = enzyme_activity, fill = plotnum, color = plotnum))+
  geom_point(size = 2.5, alpha = 0.5)+
  labs(y = "enzyme activity, nmol/h/g dry soil",
       x = "site",
       tag = "2022")+
  facet_grid(enzyme_type~plotnum, scales = "free_y")+
  theme_er1()


##### dotplots
dotplot2022west =
  all_enzymes_longer %>% 
  filter(year == "2022" & area == "West") %>% 
  ggplot(aes(x = site, y = enzyme_activity, fill = soil_layer, color = soil_layer))+
  geom_point(size = 2.5, alpha = 0.6)+
  labs(y = "enzyme activity, nmol/h/g dry soil",
       x = "site",
       subtitle = "West 2022")+
  facet_grid(enzyme_type~plotnum, scales = "free_y")+
  scale_color_manual(values = (PNWColors::pnw_palette("Starfish", 2)))+
  theme_er1()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("output/dotplot2022west.png",  plot = dotplot2022west, width = 6, height = 6)

dotplot2022east =
  all_enzymes_longer %>% 
  filter(year == "2022" & area == "East") %>% 
  ggplot(aes(x = site, y = enzyme_activity, fill = soil_layer, color = soil_layer))+
  geom_point(size = 2.5, alpha = 0.6)+
  labs(y = "enzyme activity, nmol/h/g dry soil",
       x = "site",
       subtitle = "East 2022")+
  facet_grid(enzyme_type~plotnum, scales = "free_y")+
  scale_color_manual(values = (PNWColors::pnw_palette("Starfish", 2)))+
  theme_er1()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("output/dotplot2022east.png",  plot = dotplot2022east, width = 6, height = 6)


dotplot2021west = 
  all_enzymes_longer %>% 
  filter(year == "2021" & area == "West") %>% 
  ggplot(aes(x = site, y = enzyme_activity, fill = soil_layer, color = soil_layer))+
  geom_point(size = 2.5, alpha = 0.6)+
  labs(y = "enzyme activity, nmol/h/g dry soil",
       x = "site",
       subtitle = "West 2021")+
  facet_grid(enzyme_type~plotnum, scales = "free_y")+
  scale_color_manual(values = (PNWColors::pnw_palette("Starfish", 2)))+
  theme_er1()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("output/dotplot2021west.png",  plot = dotplot2021west, width = 6, height = 6)

dotplot2021east = 
  all_enzymes_longer %>% 
  filter(year == "2021" & area == "East") %>% 
  ggplot(aes(x = site, y = enzyme_activity, fill = soil_layer, color = soil_layer))+
  geom_point(size = 2.5, alpha = 0.6)+
  labs(y = "enzyme activity, nmol/h/g dry soil",
       x = "site",
       subtitle = "East 2021")+
  facet_grid(enzyme_type~plotnum, scales = "free_y")+
  scale_color_manual(values = (PNWColors::pnw_palette("Starfish", 2)))+
  theme_er1()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("output/dotplot2021east.png",  plot = dotplot2021east, width = 6, height = 6)


#####new ggplots 10-28-2022

all_enzymes_wider_2021_fig_east_nag =
  all_enzymes_wider_2021 %>% 
  filter(area == "East") %>% 
    mutate(area = recode(area, "East" = "East Area, Acidic Tundra")) %>% 
  ggplot()+
  geom_boxplot(aes(x = date, y = NAG, fill = site, color = site), alpha = 0.6)+
  labs(y = "NAG activity, nmol/h/g dry soil",
       x = " ")+
  scale_fill_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  scale_color_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
 # scale_x_date(date_labels = "%b-%d")+
  facet_grid(.~area)+
  theme_er1()+
  theme(legend.position = "right")

ggsave("output/2021_fig_east_nag.png", plot =  all_enzymes_wider_2021_fig_east_nag, height = 3.5, width = 5)

all_enzymes_wider_2021_fig_east_bg =
  all_enzymes_wider_2021 %>% 
  filter(area == "East") %>% 
  mutate(area = recode(area, "East" = "East Area, Acidic Tundra")) %>% 
  ggplot()+
  geom_boxplot(aes(x = date, y = BG, fill = site, color = site), alpha = 0.6)+
  labs(y = "BG activity, nmol/h/g dry soil",
       x = " ")+
  scale_fill_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  scale_color_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  # scale_x_date(date_labels = "%b-%d")+
  facet_grid(.~area)+
  theme_er1()+
  theme(legend.position = "right")

ggsave("output/2021_fig_east_bg.png", plot =  all_enzymes_wider_2021_fig_east_bg, height = 3.5, width = 5)

all_enzymes_wider_2021_fig_east_phos =
  all_enzymes_wider_2021 %>% 
  filter(area == "East") %>% 
  mutate(area = recode(area, "East" = "East Area, Acidic Tundra")) %>% 
  ggplot()+
  geom_boxplot(aes(x = date, y = PHOS, fill = site, color = site), alpha = 0.6)+
  labs(y = "PHOS activity, nmol/h/g dry soil",
       x = " ")+
  scale_fill_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  scale_color_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  # scale_x_date(date_labels = "%b-%d")+
  facet_grid(.~area)+
  theme_er1()+
  theme(legend.position = "right")

ggsave("output/2021_fig_east_phos.png", plot =  all_enzymes_wider_2021_fig_east_phos, height = 3.5, width = 5)

all_enzymes_wider_2021_fig_east_lap =
  all_enzymes_wider_2021 %>% 
  filter(area == "East") %>% 
  mutate(area = recode(area, "East" = "East Area, Acidic Tundra")) %>% 
  ggplot()+
  geom_boxplot(aes(x = date, y = LAP, fill = site, color = site), alpha = 0.6)+
  labs(y = "LAP activity, nmol/h/g dry soil",
       x = " ")+
  scale_fill_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  scale_color_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  # scale_x_date(date_labels = "%b-%d")+
  facet_grid(.~area)+
  theme_er1()+
  theme(legend.position = "right")

ggsave("output/2021_fig_east_lap.png", plot =  all_enzymes_wider_2021_fig_east_lap, height = 3.5, width = 5)



allenzymes_east_fig =
all_enzymes_notwider_2021 %>% 
  filter(area == "East") %>% 
  mutate(area = recode(area, "East" = "East Area, Acidic Tundra")) %>% 
  ggplot()+
  geom_boxplot(aes(x = date, y = enzyme_activity, fill = site, color = site), alpha = 0.6)+
  labs(y = "nmol/h/g dry soil",
       x = " ",
       subtitle = "East Area, Acidic Tundra")+
  scale_fill_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  scale_color_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  # scale_x_date(date_labels = "%b-%d")+
  facet_wrap(enzyme_type~., scale = "free_y")+
  theme_er1()+
  theme(legend.position = "right")

ggsave("output/allenzymes_east_fig2021.png", plot =  allenzymes_east_fig, height = 5.5, width = 8)

allenzymes_west_fig =
  all_enzymes_notwider_2021 %>% 
  filter(area == "West") %>% 
  #mutate(area = recode(area, "East" = "East Area, Acidic Tundra")) %>% 
  ggplot()+
  geom_boxplot(aes(x = date, y = enzyme_activity, fill = site, color = site), alpha = 0.6)+
  labs(y = "nmol/h/g dry soil",
       x = " ",
       subtitle = "West Area, Non-Acidic Tundra")+
  scale_fill_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  scale_color_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  # scale_x_date(date_labels = "%b-%d")+
  facet_wrap(enzyme_type~., scale = "free_y")+
  theme_er1()+
  theme(legend.position = "right")

ggsave("output/allenzymes_west_fig2021.png", plot =  allenzymes_west_fig, height = 5.5, width = 8)






all_enzymes_wider_2021_fig_west_nag =
  all_enzymes_wider_2021 %>% 
  filter(area == "West") %>% 
  mutate(area = recode(area, "West" = "West Area, Non-Acidic Tundra")) %>% 
  ggplot()+
  geom_boxplot(aes(x = date, y = NAG, fill = site, color = site), alpha = 0.6)+
  labs(y = "NAG activity, nmol/h/g dry soil",
       x = " ")+
  scale_fill_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  scale_color_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  # scale_x_date(date_labels = "%b-%d")+
  facet_grid(.~area)+
  theme_er1()+
  theme(legend.position = "right")

ggsave("output/2021_fig_west_nag.png", plot =  all_enzymes_wider_2021_fig_west_nag, height = 3.5, width = 5)

all_enzymes_wider_2021_fig_west_bg =
  all_enzymes_wider_2021 %>% 
  filter(area == "West") %>% 
  mutate(area = recode(area, "West" = "West Area, Non-Acidic Tundra")) %>% 
  ggplot()+
  geom_boxplot(aes(x = date, y = BG, fill = site, color = site), alpha = 0.6)+
  labs(y = "BG activity, nmol/h/g dry soil",
       x = " ")+
  scale_fill_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  scale_color_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  # scale_x_date(date_labels = "%b-%d")+
  facet_grid(.~area)+
  theme_er1()+
  theme(legend.position = "right")


ggsave("output/2021_fig_west_bg.png", plot =  all_enzymes_wider_2021_fig_west_bg, height = 3.5, width = 5)

all_enzymes_wider_2021_fig_west_phos =
  all_enzymes_wider_2021 %>% 
  filter(area == "West") %>% 
  mutate(area = recode(area, "West" = "West Area, Non-Acidic Tundra")) %>% 
  ggplot()+
  geom_boxplot(aes(x = date, y = PHOS, fill = site, color = site), alpha = 0.6)+
  labs(y = "PHOS activity, nmol/h/g dry soil",
       x = " ")+
  scale_fill_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  scale_color_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  # scale_x_date(date_labels = "%b-%d")+
  facet_grid(.~area)+
  theme_er1()+
  theme(legend.position = "right")

ggsave("output/2021_fig_west_phos.png", plot =  all_enzymes_wider_2021_fig_west_phos, height = 3.5, width = 5)

all_enzymes_wider_2021_fig_west_lap =
  all_enzymes_wider_2021 %>% 
  filter(area == "West") %>% 
  mutate(area = recode(area, "West" = "West Area, Non-Acidic Tundra")) %>% 
  ggplot()+
  geom_boxplot(aes(x = date, y = LAP, fill = site, color = site), alpha = 0.6)+
  labs(y = "LAP activity, nmol/h/g dry soil",
       x = " ")+
  scale_fill_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  scale_color_manual(values = rev((PNWColors::pnw_palette("Bay", 3))))+
  # scale_x_date(date_labels = "%b-%d")+
  facet_grid(.~area)+
  theme_er1()+
  theme(legend.position = "right")

ggsave("output/2021_fig_west_lap.png", plot =  all_enzymes_wider_2021_fig_west_lap, height = 3.5, width = 5)
