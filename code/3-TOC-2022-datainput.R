#12 14 2022
#Data processing/ggplots

#load all packages


source("code/0-packages.R")

###write file

run1 = read.csv("raw/Rooney_PhIr_2022Fallrun1.csv")
run2= read.csv("raw/Rooney_PhIr_2022Fallrun2.csv")
metadata = read.csv("processed/PhIr2022_Water_Inventory.csv")

metadata_fordata =
  metadata %>% 
  dplyr::select(c(SampleID, Date, Time, Area, Site, Plot, Plot_ID, Depth_cm, 
                  Solution_pH, Solution_SC, Solution_temp
                  )) %>% 
  mutate(PhIr_SampleID = SampleID) %>% 
  separate(SampleID, sep = "-", into = c("projectinfo", "samplename"))


TOC_select =
  run1 %>% 
  vctrs::vec_c(run2) %>% 
  rename("samplename" = "Sample.Name",
         "result" = "Result",
         "exclude" = "Excluded") %>% 
  dplyr::select(c(samplename, result, exclude)) %>% 
  separate(result, sep = " ", into = c("NPOC_mgL", "TN_mgL")) %>%
  mutate(NPOC_mgL = str_remove(NPOC_mgL, "NPOC:")) %>% 
  mutate(NPOC_mgL = str_remove(NPOC_mgL, "mg/L")) %>% 
  mutate(TN_mgL = str_remove(TN_mgL, "TN:")) %>% 
  mutate(TN_mgL = str_remove(TN_mgL, "mg/L")) %>% 
  mutate(dilution = case_when(grepl("DI", samplename)~"blank",
                               grepl("MilliQ", samplename)~"blank",
                              grepl("Standard", samplename)~"stdcheck",
                              grepl("standard", samplename)~"stdcheck",
                              grepl("4x", samplename)~"4")) 


TOC_rerun =
  run1 %>% 
  rename("samplename" = "Sample.Name",
         "result" = "Result",
         "exclude" = "Excluded") %>% 
  dplyr::select(c(samplename, result, exclude)) %>% 
  separate(result, sep = " ", into = c("NPOC_mgL", "TN_mgL")) %>%
  mutate(NPOC_mgL = str_remove(NPOC_mgL, "NPOC:")) %>% 
  mutate(NPOC_mgL = str_remove(NPOC_mgL, "mg/L")) %>% 
  mutate(TN_mgL = str_remove(TN_mgL, "TN:")) %>% 
  mutate(TN_mgL = str_remove(TN_mgL, "mg/L")) %>% 
  mutate(rerun = case_when(grepl("4x", samplename)~"yes")) %>% 
  na.omit()


write.csv(TOC_rerun, 'processed/TOC_rerun.csv')

blanks =
  TOC_select %>% 
  filter(dilution == "blank")

stdchecks =
  TOC_select %>% 
  filter(dilution == "stdcheck")

dilution_4x =
  TOC_select %>% 
  filter(dilution == 4) %>% 
  dplyr::mutate(NPOC_mgL = as.numeric(NPOC_mgL),
                TN_mgL = as.numeric(TN_mgL)) %>% 
  dplyr::mutate(NPOC_fixed = (NPOC_mgL * 4)) %>% 
  dplyr::mutate(TN_fixed = (TN_mgL * 4)) %>%
  dplyr::select(-c(NPOC_mgL, TN_mgL)) %>% 
  rename("NPOC_mgL" = "NPOC_fixed", 
         "TN_mgL" = "TN_fixed") %>% 
  separate(samplename, sep = "_", into = c("samplename", "dilution")) %>% 
  dplyr::select(-c(dilution))
  

alldata_noblanks =
  TOC_select %>% 
  filter(is.na(dilution)) %>% 
  dplyr::mutate(NPOC_mgL = as.numeric(NPOC_mgL),
                TN_mgL = as.numeric(TN_mgL)) %>% 
  dplyr::select(-c(dilution)) %>% 
  vctrs::vec_c(dilution_4x) %>% 
  filter(exclude < 1) %>% 
  group_by(samplename) %>% 
  dplyr::summarise(NPOC_mean_mgL = round(mean(NPOC_mgL),3),
                   TN_mean_mgL = round(mean(TN_mgL),3)) %>% 
  left_join(metadata_fordata, by = "samplename") %>% 
  filter(Area != 'Blank' & Site != "Blank") %>% 
  group_by(PhIr_SampleID, Area, Site, Depth_cm, Date) %>% 
  dplyr::summarise(NPOC_sitemean_mgL = round(mean(NPOC_mean_mgL),3),
                   NPOC_se = sd(NPOC_mean_mgL)/sqrt(n()),
                   TN_sitemean_mgL = round(mean(TN_mean_mgL),3),
                   TN_se = sd(TN_mean_mgL)/sqrt(n())) %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>%
  mutate(Area = recode(Area, "West" = "non-acidic tundra",
                      "East" =  "acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) %>%
  separate(Date, sep = "-", into = c("Day", "Month", "Year")) 

all_data_fixdate =
  alldata_noblanks %>% 
  mutate(Month2 = recode(Month, "Jun" = "6", "Aug" = "8", "Jul" = "7", "Sep" = "9",
                         "Sept" = "9"
                        )) %>% 
  dplyr::mutate(Date2 = ymd(paste(Year,Month2,Day, sep = "-")))  
  #mutate(date = ymd(Date2)) %>% 

  
                
write.csv(all_data_fixdate, "output/NPOC-TN-PhIr2022.csv")
  
  


all_data_fixdate %>% 
  dplyr::mutate(Depth_cm = as.numeric(Depth_cm)) %>% 
  ggplot(aes(x = NPOC_sitemean_mgL, y = Depth_cm, color = Date2, group = Date2))+
  geom_point(size = 3, alpha = 0.6)+
  #geom_errorbar(aes(xmin=(NPOC_sitemean_mgL - NPOC_se), xmax=(NPOC_sitemean_mgL - NPOC_se)))+
  geom_line(orientation = "y")+
  labs(y = "Depth, cm",
       x = "NPOC, mg/L")+
  scale_y_reverse()+
  #scale_color_manual(values = (PNWColors::pnw_palette("Shuksan", 2)))+
  scale_color_gradientn(colors = (PNWColors::pnw_palette("Bay")))+
  facet_grid(Site~Area)+
  theme_er1()+
  theme(legend.position = "right")


all_data_grouped =
  all_data_fixdate %>% 
  group_by(Area, Site, Depth_cm) %>% 
  dplyr::summarise(NPOC_mgL = round(mean(NPOC_sitemean_mgL),3),
                   NPOC_se = sd(NPOC_sitemean_mgL)/sqrt(n()),
                   TN_mgL = round(mean(TN_sitemean_mgL),3),
                   TN_se = sd(TN_sitemean_mgL)/sqrt(n())) 

NPOC_grouped_fig =
all_data_grouped %>% 
  ggplot(aes(x = NPOC_mgL, y = Depth_cm, group = Site), color = "gray30")+
  geom_point(size = 3, alpha = 0.6)+
  geom_errorbar(aes(xmin=(NPOC_mgL + NPOC_se), xmax=(NPOC_mgL - NPOC_se)), color = "gray30")+
  geom_line(orientation = "y", color = "gray30", linetype = "dashed")+
  labs(y = "Depth, cm",
       x = "NPOC, mg/L")+
  scale_y_reverse()+
  scale_color_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
  #scale_color_gradientn(colors = (PNWColors::pnw_palette("Bay")))+
  facet_grid(Site~Area)+
  theme_er1()+
  theme(legend.position = "right")

TN_grouped_fig =
  all_data_grouped %>% 
  ggplot(aes(x = TN_mgL, y = Depth_cm, group = Site), color = "gray30")+
  geom_point(size = 3, alpha = 0.6)+
  geom_errorbar(aes(xmin=(TN_mgL + TN_se), xmax=(TN_mgL - TN_se)), color = "gray30")+
  geom_line(orientation = "y", color = "gray30", linetype = "dashed")+
  labs(y = "Depth, cm",
       x = "TN, mg/L")+
  scale_y_reverse()+
  scale_color_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
  #scale_color_gradientn(colors = (PNWColors::pnw_palette("Bay")))+
  facet_grid(Site~Area)+
  theme_er1()+
  theme(legend.position = "right")

ggsave("output/NPOC_2022_grouped.png", plot = NPOC_grouped_fig, height = 5, width = 4.5)
ggsave("output/TN_2022_grouped.png", plot = TN_grouped_fig, height = 5, width = 4.5)
  
##date plots

NPOC_fig =
all_data_fixdate %>% 
  ungroup() %>% 
  dplyr::mutate(Depth_cm = as.factor(Depth_cm)) %>% 
  mutate(Depth_cm = factor(Depth_cm, levels = c("0", "10", "20", "30","40"))) %>% 
  ggplot(aes(x = Date2, y = NPOC_sitemean_mgL, color = Depth_cm, group = Depth_cm))+
  geom_errorbar(aes(ymin=(NPOC_sitemean_mgL + NPOC_se), ymax=(NPOC_sitemean_mgL - (NPOC_se))), color = 'gray65')+
  geom_point(aes(group = Depth_cm), size = 3, alpha = 0.6)+
  geom_line(aes(group = Depth_cm), orientation = "x")+
  labs(y = "NPOC, mg/L",
       x = "Date",
       color = "depth, cm")+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_manual(values = c("#f94144", "#f8961e", "#43aa8b", "#577590"))+
  facet_grid(Site~Area)+
  theme_er1()+
  theme(legend.position = "right", axis.text.x = element_text(size = 10))

TN_fig =
all_data_fixdate %>% 
  ggplot(aes(x = Date2, y = TN_sitemean_mgL, color = Depth_cm, group = Depth_cm))+
  geom_point(aes(group = Depth_cm), size = 3, alpha = 0.6)+
  geom_errorbar(aes(ymin=(TN_sitemean_mgL + TN_se), ymax=(TN_sitemean_mgL - TN_se)), color = 'gray65')+
  geom_line(aes(group = Depth_cm), orientation = "x")+
  labs(y = "TN, mg/L",
       x = "Date",
       color = "depth, cm")+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_manual(values = c("#f94144", "#f8961e", "#43aa8b", "#577590"))+
  facet_grid(Site~Area)+
  theme_er1()+
  theme(legend.position = "right", axis.text.x = element_text(size = 10))

ggsave("output/NPOC_2022.png", plot = NPOC_fig, height = 5.5, width = 9.5)
ggsave("output/TN_2022.png", plot = TN_fig, height = 5.5, width = 9.5)


##for micromodel concentrations

micromodel_concentrationplanning_fig =
all_data_fixdate %>% 
  filter(Area == "acidic tundra" & Site == "Hydric") %>% 
  ggplot()+
  geom_point(aes(x = NPOC_sitemean_mgL, y = Depth_cm, color = Month2), size = 3, alpha = 0.6)+
  labs(y = "depth, cm",
       x = "NPOC, mg/L",
       color = "Month, 2022")+
  #scale_color_gradientn(colors = (PNWColors::pnw_palette("Bay")))+
  #scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d")+
  scale_color_manual(values = c("#f94144", "#f8961e", "#43aa8b", "#577590"))+
  scale_y_reverse()+
  facet_grid(Site~Area)+
  theme_er1()+
  theme(legend.position = "right", axis.text.x = element_text(size = 10))

ggsave("output/NPOC_2022_formicromodelplanning.png", plot = micromodel_concentrationplanning_fig, height = 5.5, width = 6.5)
