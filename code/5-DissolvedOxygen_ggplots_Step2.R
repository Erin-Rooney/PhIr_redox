
#Graphs
##Microelectrode DO Profiles
###DO vs. time, color coded by depth

source("code/0-packages.R")


###temp----

microDO = read.csv("processed/PhIr2023_DissolvedOxygenMeasurements.csv")

# 
# #all of the data except the first program
# microDO %>%
#   filter(program!= "28072023002440_TFS_PhIr_5cmdepth_2mm_365" ) %>%
#   ggplot( aes(x=RDateTimeAST, y=DO.mgL, color=Motor1.mm) ) +
#   geom_point()+
#   scale_color_gradient(low="blue", high="orange")
# 
# 
# 
# ###Graphs of DO vs. Depth--traditional profiles
# 
# 
# ggplot(microDO, aes(x=DO.mgL, y=Motor1.mm, color=as.POSIXct(RDateTimeAST), shape=DOsensor))+
#   geom_point()+
#   scale_y_reverse()
# 
# 
# 
# microDO%>%
#   filter(program=="28072023003700_TFS_PhIr_5cm_1hr_diurnal_368") %>%
#   ggplot(aes(x=DO.mgL, y=Motor1.mm, shape=DOsensor))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(~ProfileStart)
# 
# 
# ####Program 2, faceted by profile start time
# 
# microDO%>%
#   filter(program=="28072023142101_TFS_PhIr_5cm_1hr_diurnal_370") %>%
#   ggplot(aes(x=DO.mgL, y=Motor1.mm, shape=DOsensor))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(~ProfileStart)
# 
# 
# ####Program 3, faceted by profile start time
# 
# microDO%>%
#   filter(program=="29072023131024_TFS_PhIr_5cm_1hr_diurnal_371") %>%
#   ggplot(aes(x=DO.mgL, y=Motor1.mm, shape=DOsensor))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(~ProfileStart)
# 
# 
# ####Program 4, faceted by profile start time
# #this one has so many profiles, splitting it into sections by date
# 
# ##getting a list of all the profile start date times
# microDO4<-microDO%>%
#   filter(program== "29072023213519_TFS_PhIr_5cm_30min_2d_374")
# unique(microDO4$ProfileStart)
# length(unique(microDO4$ProfileStart))
# 
# #plotting profiles in groups of 16
# 
# microDO%>%
#   filter(program== "29072023213519_TFS_PhIr_5cm_30min_2d_374") %>%
#   filter(RDateTimeAST<as.POSIXct("2023-07-30 01:50")) %>%
#   ggplot(aes(x=DO.mgL, y=Motor1.mm, shape=DOsensor))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(~ProfileStart)
# 
# microDO%>%
#   filter(program== "29072023213519_TFS_PhIr_5cm_30min_2d_374") %>%
#   filter(RDateTimeAST>as.POSIXct("2023-07-30 01:50") & RDateTimeAST<as.POSIXct("2023-07-30 10:20" )) %>%
#   ggplot(aes(x=DO.mgL, y=Motor1.mm, shape=DOsensor))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(~ProfileStart)
# 
# microDO%>%
#   filter(program== "29072023213519_TFS_PhIr_5cm_30min_2d_374") %>%
#   filter(RDateTimeAST>as.POSIXct("2023-07-30 10:20") & RDateTimeAST<as.POSIXct("2023-07-30 18:40" )) %>%
#   ggplot(aes(x=DO.mgL, y=Motor1.mm, shape=DOsensor))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(~ProfileStart)
# 
# ##Looks like the motor goofed after the 2023-7-30 13:01 profiles 
# 
# microDO%>%
#   filter(program== "29072023213519_TFS_PhIr_5cm_30min_2d_374") %>%
#   filter(RDateTimeAST>as.POSIXct("2023-07-30 18:40") & RDateTimeAST<as.POSIXct("2023-07-31 3:10" )) %>%
#   ggplot(aes(x=DO.mgL, y=Motor1.mm, shape=DOsensor))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(~ProfileStart)
# 
# microDO%>%
#   filter(program== "29072023213519_TFS_PhIr_5cm_30min_2d_374") %>%
#   filter(RDateTimeAST>as.POSIXct("2023-07-31 3:10")) %>%
#   ggplot(aes(x=DO.mgL, y=Motor1.mm, shape=DOsensor))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(~ProfileStart)
# 
# 
# 
# 
# ###Graphs of DO vs. Depth, faceted by date, color coded by time
# 
# ggplot(microDO, aes(x=DO.mgL, y=Motor1.mm, color=as.numeric(RProfileStartTime), shape=DOsensor))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(~RDate)
# 
# 
# ###Individual Dates, faceted by profile, labeled with start time
# microDO %>%
#   filter(RDate==as.POSIXct("2023-07-29"))%>%
#   ggplot(aes(x=DO.mgL, y=Motor1.mm))+
#   geom_point()+
#   scale_y_reverse() +
#   facet_wrap(~RProfileStartTime)
# 
# ##Next step: make a column rounding to the start hour so I can make a grid of day by time. Later, chnage the label of each plot to the actual start time.
# 
# 
# ggplot(microDO, aes(x=DO.mgL, y=Motor1.mm))+
#   geom_point(size=0.1)+
#   scale_y_reverse() +
#   facet_grid(rows=vars(RDate), cols=vars(RHour))+
#   theme_light()+
#   theme(text=element_text(size=8))+
#   labs(x="DO (mg/L)", y= "Depth (mm)")
# ggsave("Figures/AllDOProfiles.png", width=10, height=3, units="in")
# 
# 
# ###Graphing vertical profiles connected by lines
# 
# ##Loooking at all the profiles from July 29, 2023
# microDO %>%
#   filter(RDate==as.POSIXct("2023-07-29"))%>%
#   ggplot(aes(x=DO.mgL, y=Motor1.mm))+
#   geom_point()+
#   scale_y_reverse() +
#   facet_wrap(~ProfIndex)
# ##Proflie number 44 looks cool
# ##Let's try to figure out how to draw teh graph we want with just that one
# 
# microDO_Prof44 <- microDO %>%
#   filter(ProfIndex==44)
# summary(microDO_Prof44)
# 
# ggplot(microDO_Prof44, aes(x=DO.mgL, y=Motor1.mm))+
#   geom_point()+
#   scale_y_reverse()+
#   geom_line()
# 
# #Geom_line() connects by order of x axis
# 
# ggplot(microDO_Prof44, aes(x=DO.mgL, y=Motor1.mm))+
#   geom_point()+
#   scale_y_reverse()+
#   geom_path()
# #geom_path connects by order of the data
# 
# #OK geom_path is what I want
# 
# 
# ###Graphs of DO vs. Depthcolor coded by time
# 
# ###Micro DO vs. Depth, color coded by time of day
# #this is a helpful resource for the color I chose and other ggplot things: https://ggplot2-book.org/scales-colour#sec-colour-continuous
# #another helpful resource: https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/#change-legend-title
# 
# ggplot(microDO, aes(x=DO.mgL, y=Motor1.mm, color=as.numeric(RProfileStartTime),group=ProfIndex))+
#   theme_light()+
#   geom_point(size=0.1)+
#   scale_color_viridis_c(limits=c(0,86400),
#                         breaks=c(0,21600, 43200,64800,86400),
#                         labels=c("Midnight","06:00","Noon","18:00","Midnight"))+
#   #making sure the color is full 24 hours, there are 86400 seconds in 24 hours
#   geom_path()+
#   scale_y_reverse()+
#   theme(text=element_text(size=10),
#         legend.position = c(0.9,0.1),
#         legend.justification = c(1,0))+
#   labs(x="DO (mg/L)", y= "Depth (mm)", color="Time")+
#   theme(text=element_text(size=15))
# ggsave("Figures/MicroDOByTime.png", width=5, height=6)
# 
# 
# 
# 
# ###Micro DO vs. Depth, color coded by time of day and facetted by date
# ggplot(microDO, aes(x=DO.mgL, y=Motor1.mm, color=as.numeric(RProfileStartTime),group=ProfIndex))+
#   theme_light()+
#   geom_point(size=0.1)+
#   scale_color_viridis_c(limits=c(0,86400),
#                         breaks=c(0,21600, 43200,64800,86400),
#                         labels=c("Midnight","06:00","Noon","18:00","Midnight"))+
#   #making sure the color is full 24 hours, there are 86400 seconds in 24 hours
#   geom_path()+
#   scale_y_reverse()+
#   theme(text=element_text(size=10),
#         legend.position = c(1,0),
#         legend.justification = c(1,0))+
#   labs(x="DO (mg/L)", y= "Depth (mm)", color="Time")+
#   theme(text=element_text(size=15))+
#   facet_grid(~Date)
# ggsave("Figures/MicroDOByTimeDateFacet.png", width=9, height=6)
# 
# 
# 
# ##Presens Fibox DO profiles
# 
# ###All Presens data, DO vs. Depth, color coded by Date Time, shape coded by Site
# ggplot(FiboxDO, aes(x=DO.mgL, y=Depth.cm, color=RDateTime, shape=AreaSite))+
#   geom_point()+
#   theme_light()+
#   scale_y_reverse()+
#   theme(text=element_text(size=15))+
#   scale_shape_discrete(name="Non-Acidic Site", labels = c("Hydric", "Mesic"))+
#   labs(x="Dissolved Oxygen (mg/L)", y= "Depth (cm)", color="Date Time")
# ggsave("Figures/PresenseDO.Date.Site.png", width=6, height = 4)
# 
# ###All Presens data, DO vs. Depth,  shape coded by Site
# ggplot(FiboxDO, aes(x=DO.mgL, y=Depth.cm, shape=AreaSite))+
#   geom_point()+
#   theme_light()+
#   scale_y_reverse()+
#   theme(text=element_text(size=15))+
#   scale_shape_discrete(name="Non-Acidic Site", labels = c("Hydric", "Mesic"))+
#   labs(x="Dissolved Oxygen (mg/L)", y= "Depth (cm)")
# ggsave("Figures/PresenseDO.Site.png", width=6, height = 4)
# 
# 
# ###All Presens data, DO vs. Depth
# ggplot(FiboxDO, aes(x=DO.mgL, y=Depth.cm))+
#   geom_point()+
#   theme_light()+
#   scale_y_reverse()+
#   theme(text=element_text(size=15))+
#   labs(x="Dissolved Oxygen (mg/L)", y= "Depth (cm)")
# ggsave("Figures/PresenseDO.png", width=5, height = 4)


###Three versions of Presens data graphed with Rooney et al. theme

theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA),
      axis.title = element_text(size = 16), axis.text = element_text(size = 16))

#export via ggsave (height = 4.5, width = 2.25).

####All Presens data, Rooney et al. theme, DO vs. Depth
ggplot(FiboxDO, aes(x=DO.mgL, y=Depth.cm))+
  geom_point()+
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA), axis.title = element_text(size = 16), axis.text = element_text(size = 16))+
  scale_y_reverse()+
  labs(x="Dissolved Oxygen (mg/L)", y= "Depth (cm)")
ggsave("Figures/RooneyeaPresenseDO.png", width=3.25, height = 4.5)

####All Presens data, Rooney et al. theme, DO vs. Depth, shape coded by site
ggplot(FiboxDO, aes(x=DO.mgL, y=Depth.cm, shape=AreaSite))+
  geom_point()+
  scale_y_reverse()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA), axis.title = element_text(size = 16), axis.text = element_text(size = 16),
        legend.position = c(0.9,0.1),
        legend.justification = c(1,0)
  )+
  scale_shape_discrete(name="Non-Acidic Site", labels = c("Hydric", "Mesic"))+
  labs(x="Dissolved Oxygen (mg/L)", y= "Depth (cm)")
ggsave("Figures/RooneyeaPresenseDO.Site.png", width=3.25, height = 4.5)


####All Presens data, Rooney et al. theme, DO vs. Depth, shape coded by site, color coded by Date Time
ggplot(FiboxDO, aes(x=DO.mgL, y=Depth.cm, color=RDateTime, shape=AreaSite))+
  geom_point()+
  theme_light()+
  scale_y_reverse()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA), axis.title = element_text(size = 16), axis.text = element_text(size = 16),
        legend.position = c(0.9,0.1),
        legend.justification = c(1,0)
  )+
  scale_shape_discrete(name="Non-Acidic Site", labels = c("Hydric", "Mesic"))+
  labs(x="Dissolved Oxygen (mg/L)", y= "Depth (cm)", color="Date Time")
ggsave("Figures/RoonyeaPresenseDO.Date.Site.png", width=3.25, height = 4.5)

####All Presens data, Rooney et al. theme, DO vs. Depth,  color coded by Date Time
ggplot(FiboxDO, aes(x=DO.mgL, y=Depth.cm, color=RDateTime))+
  geom_point()+
  theme_light()+
  scale_y_reverse()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_rect(color="gray",size=0.25, fill = NA), axis.title = element_text(size = 16), axis.text = element_text(size = 16),
        legend.position = c(0.9,0.1),
        legend.justification = c(1,0)
  )+
  labs(x="Dissolved Oxygen (mg/L)", y= "Depth (cm)", color="Date Time")
ggsave("Figures/RoonyeaPresenseDO.Date.png", width=3.25, height = 4.5)

###All Presens data, DO vs. Depth, data color coded by Time
ggplot(FiboxDO, aes(x=DO.mgL, y=Depth.cm, color=as.POSIXct(Time), shape=AreaSite))+
  geom_point()+
  scale_y_reverse()+
  theme_light()+
  scale_colour_gradient2(low="black", mid="yellow", high = "blue" )
### All Presens data, DO vs. Depth, Faceted by site
ggplot(FiboxDO, aes(x=DO.mgL, y=Depth.cm, color=as.POSIXct(Time), shape=AreaSite))+
  geom_point()+
  scale_y_reverse()+
  theme_light()+
  scale_colour_gradient2(low="black", mid="yellow", high = "blue" )+
  facet_wrap(~O2.Location.Code)

microDO_forfig =
microDO %>% 
  filter(Depth.cm != "multi (?)" & Site %in% c("Hydric", "Mesic")) %>% 
  mutate(depth = as.numeric(Depth.cm)) %>% 
  mutate(Site = recode(Site, "Hydric" = "Wet")) %>% 
  mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Wet"))) 

manuscript_figure =
  microDO_forfig %>% 
  ggplot()+
  geom_point(aes(x=DO.mgL, y = depth, fill = Site, shape = Site), size = 3.5, alpha = 0.8)+
    scale_fill_manual(values = c("#FFB206", "#2D5895"))+
    scale_shape_manual(values = c(22, 21))+
    scale_y_reverse()+
  labs(y = "depth, cm",
       x = "dissolved oxygen, mg/L")+
  theme_er1()+
  theme(legend.position = c(.80, .45),
        legend.justification = c("right", "top"),
        legend.box.just = "right",legend.title = element_text(face = "bold"),
        legend.margin = margin(6, 6, 6, 6), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(color="black",size=0.25, fill = NA),
        legend.background = element_rect(color = NA))

ggsave("output/DOfig.png", manuscript_figure, height = 6, width = 4)
