#6 1 2022
#Data processing/ggplots

#load all packages

source("code/0-packages.R")

###write file

rhizon_meta_combine = read.csv("processed/rhizon_2021.csv")
sipper_data = read.csv("processed/sipper_2021.csv")

#there is weird double data
#all doubles look identical except for the blank
#grouping, will check with Beth/Sumant later
#delete and revise once checking is complete

rhizon_meta_combine_notransect =
  rhizon_meta_combine %>% 
  filter(Site != 'Transect') %>% 
  mutate(month = factor(month, levels = c("june", "july", "august")),
         Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(ICP = recode(ICP, "Al_ug/mL" = "aluminum",
                      "Ca_ug/mL" = "calcium",
                      "Fe _ug/mL" = "iron",
                      "K _ug/mL" = "potassium",
                      "Mg_ug/mL" = "magnesium",
                      "Mn_ug/mL" = "manganese",
                      "Na_ug/mL" = "sodium",
                      "P_ug/mL" = "phosphorus"))

write.csv(rhizon_meta_combine_notransect, "processed/rhizon_long_notransect.csv")


##metadata needed for sipper data

sipper_data_forplots = 
  sipper_data %>% 
  mutate(month = factor(month, levels = c("june", "july", "august")),
         #Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(elements = recode(elements, "Al_ug/mL" = "aluminum",
                      "Ca_mg/L" = "calcium",
                      "Fe_mg/L" = "iron",
                      "K_mg/L" = "potassium",
                      "Mg_mg/L" = "magnesium",
                      "Mn_mg/L" = "manganese",
                      "Na_mg/L" = "sodium",
                      "P_mg/L" = "phosphorus")))


East_rhizon_notransect_2021 = 
  rhizon_meta_combine_notransect %>% 
  filter(Area == 'East') %>% 
  ggplot(aes(x = Betterdate, y = as.numeric(concentration), color = Site, shape = Area))+
  geom_point(size = 2.5, alpha = 0.8)+
  #geom_line(orientation = "x", group = 'Plot')+
  labs(x = "Date",
       y = 'concentration ug/mL')+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  facet_wrap(ICP~., scales = "free")+
  theme_er1()

West_rhizon_notransect_2021 = 
  rhizon_meta_combine_notransect %>% 
  filter(Area == 'West') %>% 
  ggplot(aes(x = Betterdate, y = as.numeric(concentration), color = Site, shape = Area))+
  geom_point(size = 2.5, alpha = 0.8)+
  #geom_line(orientation = "x", group = 'Plot')+
  labs(x = "Date",
       y = 'concentration ug/mL')+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  facet_wrap(ICP~., scales = "free")+
  theme_er1()

ggsave("output/East_rhizon_notransect_2021.tiff", plot = East_rhizon_notransect_2021, height = 5.7, width = 6.7)
ggsave("output/West_rhizon_notransect_2021.tiff", plot = West_rhizon_notransect_2021, height = 5.7, width = 6.7)



west_rhizon_month = 
  rhizon_meta_combine_notransect %>% 
  filter(Area == 'West') %>% 
  ggplot(aes(x = Site, y = as.numeric(concentration), fill = month))+
  #geom_point(size = 2.5, alpha = 0.8)+
  geom_boxplot(alpha = 0.3)+
  #geom_line(orientation = "x", group = 'Plot')+
  labs(x = "Site",
       y = 'concentration ug/mL')+
  scale_fill_manual(values = natparks.pals(name = "SmokyMtns", 3))+
  #scale_color_manual(values = natparks.pals(name = "SmokyMtns", 3))+
  # scale_color_manual(values = rev(PNWColors::pnw_palette("Shuksan2", 2)))+
  # scale_fill_manual(values = rev(PNWColors::pnw_palette("Shuksan2", 2)))+
  facet_wrap(ICP~., scales = "free", ncol = 4)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))

east_rhizon_month = 
  rhizon_meta_combine_notransect %>% 
  filter(Area == 'East') %>% 
  ggplot(aes(x = Site, y = as.numeric(concentration), fill = month))+
  #geom_point(size = 2.5, alpha = 0.8)+
  geom_boxplot(alpha = 0.3, width = 0.6)+
  #geom_line(orientation = "x", group = 'Plot')+
  labs(x = "",
       y = 'concentration ug/mL')+
  scale_fill_manual(values = natparks.pals(name = "SmokyMtns", 3))+
  #scale_color_manual(values = natparks.pals(name = "SmokyMtns", 3))+
  # scale_color_manual(values = rev(PNWColors::pnw_palette("Shuksan2", 2)))+
  # scale_fill_manual(values = rev(PNWColors::pnw_palette("Shuksan2", 2)))+
  facet_wrap(ICP~., scales = "free", ncol = 4)+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))

ggsave("output/east_2021_rhizonsmonth.tiff", plot = east_rhizon_month, height = 5.75, width = 10)
ggsave("output/west_2021_rhizonsmonth.tiff", plot = west_rhizon_month, height = 5.75, width = 10)


rhizon_meta_combine_notransect_forelements =
  rhizon_meta_combine_notransect %>% 
  mutate(concentration = as.numeric(concentration)) %>% 
  group_by(Area, Site, date, ICP) %>% 
  dplyr::summarise(mean = mean(concentration),
                   n = n(), 
                   sd = sd(concentration)/sqrt(n)) %>% 
  na.omit() %>% 
  mutate(combo = paste(Area, "-", Site)) %>% 
  mutate(Area = as.factor(Area))
         

phosphorus_fig = 
  rhizon_meta_combine_notransect_forelements %>%
  filter(ICP == "phosphorus") %>% 
  ggplot(aes(x = date, y = mean, color = Site, fill = Site)) +
  #geom_point(size = 3, alpha = 0.7)+
  geom_col(position = 'dodge', width = 0.7)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black")+
  scale_color_manual(values = natparks.pals(name = "SmokyMtns", 3.5))+
  scale_fill_manual(values = natparks.pals(name = "SmokyMtns", 3.5))+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(Area ~ .)

iron_fig = 
  rhizon_meta_combine_notransect_forelements %>%
  filter(ICP == "iron") %>% 
  ggplot(aes(x = date, y = mean, color = Site, fill = Site)) +
  #geom_point(size = 3, alpha = 0.7)+
  geom_col(position = 'dodge', width = 0.7)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black")+
  scale_color_manual(values = natparks.pals(name = "SmokyMtns", 3.5))+
  scale_fill_manual(values = natparks.pals(name = "SmokyMtns", 3.5))+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(Area ~ .)

calcium_fig = 
  rhizon_meta_combine_notransect_forelements %>%
  filter(ICP == "calcium") %>% 
  ggplot(aes(x = date, y = mean, color = Site, fill = Site)) +
  #geom_point(size = 3, alpha = 0.7)+
  geom_col(position = 'dodge', width = 0.7)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9), color = "black")+
  scale_color_manual(values = natparks.pals(name = "SmokyMtns", 3.5))+
  scale_fill_manual(values = natparks.pals(name = "SmokyMtns", 3.5))+
  theme_er1()+
  theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))+
  facet_grid(Area ~ .)

ggsave("output/2021_rhizon_calcium.tiff", plot = calcium_fig, height = 6, width = 5)
ggsave("output/2021_rhizon_iron.tiff", plot = iron_fig, height = 6, width = 5)


write.csv(rhizon_meta_combine_notransect_forelements, "processed/rhizon_forelements.csv")

