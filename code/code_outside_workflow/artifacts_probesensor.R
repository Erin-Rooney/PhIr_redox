westhydric_depths_outliers_1_1 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "1") 

westhydric_depths_outliers_1_2 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "2") 


westhydric_depths_outliers_1_3 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "3") 


westhydric_depths_outliers_1_4 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "4") 

westhydric_depths_outliers_1_5 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "5") 

westhydric_depths_outliers_1_6 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "6") 


westhydric_depths_outliers_1_7 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "7") 


westhydric_depths_outliers_1_8 = 
  westhydric_depths %>% 
  filter(probe == "1" & sensor == "8") 


westhydric_1_1_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_1)   

westhydric_1_2_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_2)   

westhydric_1_3_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_3)   

westhydric_1_4_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_4)   

westhydric_1_5_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_5)   

westhydric_1_6_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_6)  


westhydric_1_7_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_7)   

westhydric_1_8_forcombine =
  datacleaning_function(westhydric_depths_outliers_1_8) 

westhydric_probe1_cleaned =
  westhydric_1_1_forcombine %>% 
  vctrs::vec_c(westhydric_1_2_forcombine, westhydric_1_3_forcombine,
               westhydric_1_4_forcombine, westhydric_1_5_forcombine,
               westhydric_1_6_forcombine, westhydric_1_7_forcombine,
               westhydric_1_8_forcombine)


#####
#test for Beth, delete later

# westhydric_probe1_notcleaned =
#   westhydric_depths %>% 
#   filter(probe == "1")
# 
# temporary_fig =
#   westhydric_probe1_cleaned %>% 
#   #filter(type == "temp_avg" & depth > 20) %>% 
#   ggplot(aes(x = Betterdate, y = avg_values_fixed))+
#   geom_point(aes(color = depth_cm), alpha = 0.5)+
#   geom_line(aes(group = depth_cm, color = depth_cm, orientation = "x"))+
#   scale_color_gradientn(colors = natparks.pals(name = "KingsCanyon", 8))+
#   #scale_x_date(date_breaks = "1 day" , date_labels = "%Y-%m-%d")+
#   #scale_x_discrete(breaks = seq(-1,31,2))+
#   labs(y = "redox potential, mV")+
#   #facet_wrap(month_name~.)+
#   theme_er1()+
#   theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))
# 
# ggsave("output/temporary_fig_cleaned7.png", plot = temporary_fig, width = 20, height = 8)
# 
# temporary_fig_notcleaned =
#   westhydric_probe1_notcleaned %>% 
#   #filter(type == "temp_avg" & depth > 20) %>% 
#   ggplot(aes(x = Betterdate, y = avg_values_fixed))+
#   geom_point(aes(color = depth_cm), alpha = 0.5)+
#   geom_line(aes(group = depth_cm, color = depth_cm, orientation = "x"))+
#   scale_color_gradientn(colors = natparks.pals(name = "KingsCanyon", 8))+
#   #scale_x_date(date_breaks = "1 day" , date_labels = "%Y-%m-%d")+
#   #scale_x_discrete(breaks = seq(-1,31,2))+
#   labs(y = "redox potential, mV")+
#   #facet_wrap(month_name~.)+
#   theme_er1()+
#   theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))
# 
# ggsave("output/temporary_fig_notcleaned.png", plot = temporary_fig_notcleaned, width = 20, height = 8)


################
#WEST HYDRIC PROBE 2 SENSORS 1-8 

westhydric_depths_outliers_2_1 = 
  westhydric_depths %>% 
  filter(probe == "2" & sensor == "1") 

westhydric_depths_outliers_2_2 = 
  westhydric_depths %>% 
  filter(probe == "2" & sensor == "2") 


westhydric_depths_outliers_2_3 = 
  westhydric_depths %>% 
  filter(probe == "2" & sensor == "3") 


westhydric_depths_outliers_2_4 = 
  westhydric_depths %>% 
  filter(probe == "2" & sensor == "4") 

westhydric_depths_outliers_2_5 = 
  westhydric_depths %>% 
  filter(probe == "2" & sensor == "5") 

westhydric_depths_outliers_2_6 = 
  westhydric_depths %>% 
  filter(probe == "2" & sensor == "6") 


westhydric_depths_outliers_2_7 = 
  westhydric_depths %>% 
  filter(probe == "2" & sensor == "7") 


westhydric_depths_outliers_2_8 = 
  westhydric_depths %>% 
  filter(probe == "2" & sensor == "8") 


westhydric_2_1_forcombine =
  datacleaning_function(westhydric_depths_outliers_2_1)   

westhydric_2_2_forcombine =
  datacleaning_function(westhydric_depths_outliers_2_2)   

westhydric_2_3_forcombine =
  datacleaning_function(westhydric_depths_outliers_2_3)   

westhydric_2_4_forcombine =
  datacleaning_function(westhydric_depths_outliers_2_4)   

westhydric_2_5_forcombine =
  datacleaning_function(westhydric_depths_outliers_2_5)   

westhydric_2_6_forcombine =
  datacleaning_function(westhydric_depths_outliers_2_6)  

westhydric_2_7_forcombine =
  datacleaning_function(westhydric_depths_outliers_2_7)   

westhydric_2_8_forcombine =
  datacleaning_function(westhydric_depths_outliers_2_8) 

westhydric_probe2_cleaned =
  westhydric_2_1_forcombine %>% 
  vctrs::vec_c(westhydric_2_2_forcombine, westhydric_2_3_forcombine,
               westhydric_2_4_forcombine, westhydric_2_5_forcombine,
               westhydric_2_6_forcombine, westhydric_2_7_forcombine,
               westhydric_2_8_forcombine)

#test for Beth, delete later

# westhydric_probe2_notcleaned =
#   westhydric_depths %>% 
#   filter(probe == "2")

# temporary_fig =
#   westhydric_probe2_cleaned %>% 
#   #filter(type == "temp_avg" & depth > 20) %>% 
#   ggplot(aes(x = Betterdate, y = avg_values_fixed))+
#   geom_point(aes(color = depth_cm), alpha = 0.5)+
#   geom_line(aes(group = depth_cm, color = depth_cm, orientation = "x"))+
#   scale_color_gradientn(colors = natparks.pals(name = "KingsCanyon", 8))+
#   #scale_x_date(date_breaks = "1 day" , date_labels = "%Y-%m-%d")+
#   #scale_x_discrete(breaks = seq(-1,31,2))+
#   labs(y = "redox potential, mV")+
#   #facet_wrap(month_name~.)+
#   theme_er1()+
#   theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))
# 
# ggsave("output/temporary_fig_cleaned8.png", plot = temporary_fig, width = 20, height = 8)
# 
# temporary_fig_notcleaned =
#   westhydric_probe2_notcleaned %>% 
#   #filter(type == "temp_avg" & depth > 20) %>% 
#   ggplot(aes(x = Betterdate, y = avg_values_fixed))+
#   geom_point(aes(color = depth_cm), alpha = 0.5)+
#   geom_line(aes(group = depth_cm, color = depth_cm, orientation = "x"))+
#   scale_color_gradientn(colors = natparks.pals(name = "KingsCanyon", 8))+
#   #scale_x_date(date_breaks = "1 day" , date_labels = "%Y-%m-%d")+
#   #scale_x_discrete(breaks = seq(-1,31,2))+
#   labs(y = "redox potential, mV")+
#   #facet_wrap(month_name~.)+
#   theme_er1()+
#   theme(axis.text.x = element_text (size = 10 , vjust = 0.5, hjust=1, angle = 90))
# 
# ggsave("output/temporary_fig_notcleaned2.png", plot = temporary_fig_notcleaned, width = 20, height = 8)

#I need to find a way to group_by so that this cleaning is happening within plots/sensors/depths/dataloggers

#####

#WEST HYDRIC PROBE 3 SENSORS 1-8 

westhydric_depths_outliers_3_1 = 
  westhydric_depths %>% 
  filter(probe == "3" & sensor == "1") 

westhydric_depths_outliers_3_2 = 
  westhydric_depths %>% 
  filter(probe == "3" & sensor == "2") 


westhydric_depths_outliers_3_3 = 
  westhydric_depths %>% 
  filter(probe == "3" & sensor == "3") 


westhydric_depths_outliers_3_4 = 
  westhydric_depths %>% 
  filter(probe == "3" & sensor == "4") 

westhydric_depths_outliers_3_5 = 
  westhydric_depths %>% 
  filter(probe == "3" & sensor == "5") 

westhydric_depths_outliers_3_6 = 
  westhydric_depths %>% 
  filter(probe == "3" & sensor == "6") 


westhydric_depths_outliers_3_7 = 
  westhydric_depths %>% 
  filter(probe == "3" & sensor == "7") 


westhydric_depths_outliers_3_8 = 
  westhydric_depths %>% 
  filter(probe == "3" & sensor == "8") 


westhydric_3_1_forcombine =
  datacleaning_function(westhydric_depths_outliers_3_1)   

westhydric_3_2_forcombine =
  datacleaning_function(westhydric_depths_outliers_3_2)   

westhydric_3_3_forcombine =
  datacleaning_function(westhydric_depths_outliers_3_3)   

westhydric_3_4_forcombine =
  datacleaning_function(westhydric_depths_outliers_3_4)   

westhydric_3_5_forcombine =
  datacleaning_function(westhydric_depths_outliers_3_5)   

westhydric_3_6_forcombine =
  datacleaning_function(westhydric_depths_outliers_3_6)  

westhydric_3_7_forcombine =
  datacleaning_function(westhydric_depths_outliers_3_7)   

westhydric_3_8_forcombine =
  datacleaning_function(westhydric_depths_outliers_3_8) 

westhydric_probe3_cleaned =
  westhydric_3_1_forcombine %>% 
  vctrs::vec_c(westhydric_3_2_forcombine, westhydric_3_3_forcombine,
               westhydric_3_4_forcombine, westhydric_3_5_forcombine,
               westhydric_3_6_forcombine, westhydric_3_7_forcombine,
               westhydric_3_8_forcombine)

westhydric_combine = 
  westhydric_probe3_cleaned %>% 
  vctrs::vec_c(westhydric_probe2_cleaned, westhydric_probe1_cleaned)

#####

#EAST HYDRIC PROBE 1 SENSORS 1-8 

easthydric_depths_outliers_1_1 = 
  easthydric_depths %>% 
  filter(probe == "1" & sensor == "1") 

easthydric_depths_outliers_1_2 = 
  easthydric_depths %>% 
  filter(probe == "1" & sensor == "2") 


easthydric_depths_outliers_1_3 = 
  easthydric_depths %>% 
  filter(probe == "1" & sensor == "3") 


easthydric_depths_outliers_1_4 = 
  easthydric_depths %>% 
  filter(probe == "1" & sensor == "4") 

easthydric_depths_outliers_1_5 = 
  easthydric_depths %>% 
  filter(probe == "1" & sensor == "5") 

easthydric_depths_outliers_1_6 = 
  easthydric_depths %>% 
  filter(probe == "1" & sensor == "6") 


easthydric_depths_outliers_1_7 = 
  easthydric_depths %>% 
  filter(probe == "1" & sensor == "7") 


easthydric_depths_outliers_1_8 = 
  easthydric_depths %>% 
  filter(probe == "1" & sensor == "8") 


easthydric_1_1_forcombine =
  datacleaning_function(easthydric_depths_outliers_1_1)   

easthydric_1_2_forcombine =
  datacleaning_function(easthydric_depths_outliers_1_2)   

easthydric_1_3_forcombine =
  datacleaning_function(easthydric_depths_outliers_1_3)   

easthydric_1_4_forcombine =
  datacleaning_function(easthydric_depths_outliers_1_4)   

easthydric_1_5_forcombine =
  datacleaning_function(easthydric_depths_outliers_1_5)   

easthydric_1_6_forcombine =
  datacleaning_function(easthydric_depths_outliers_1_6)  

easthydric_1_7_forcombine =
  datacleaning_function(easthydric_depths_outliers_1_7)   

easthydric_1_8_forcombine =
  datacleaning_function(easthydric_depths_outliers_1_8) 

easthydric_probe1_cleaned =
  easthydric_1_1_forcombine %>% 
  vctrs::vec_c(easthydric_1_2_forcombine, easthydric_1_3_forcombine,
               easthydric_1_4_forcombine, easthydric_1_5_forcombine,
               easthydric_1_6_forcombine, easthydric_1_7_forcombine,
               easthydric_1_8_forcombine)

#####

#EAST HYDRIC PROBE 2 SENSORS 1-8 

easthydric_depths_outliers_2_1 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "1") 

easthydric_depths_outliers_2_2 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "2") 


easthydric_depths_outliers_2_3 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "3") 


easthydric_depths_outliers_2_4 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "4") 

easthydric_depths_outliers_2_5 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "5") 

easthydric_depths_outliers_2_6 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "6") 


easthydric_depths_outliers_2_7 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "7") 


easthydric_depths_outliers_2_8 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "8") 


easthydric_2_1_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_1)   

easthydric_2_2_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_2)   

easthydric_2_3_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_3)   

easthydric_2_4_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_4)   

easthydric_2_5_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_5)   

easthydric_2_6_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_6)  

easthydric_2_7_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_7)   

easthydric_2_8_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_8) 

easthydric_probe2_cleaned =
  easthydric_2_1_forcombine %>% 
  vctrs::vec_c(easthydric_2_2_forcombine, easthydric_2_3_forcombine,
               easthydric_2_4_forcombine, easthydric_2_5_forcombine,
               easthydric_2_6_forcombine, easthydric_2_7_forcombine,
               easthydric_2_8_forcombine)


#####

#EAST HYDRIC PROBE 3 SENSORS 1-8 

easthydric_depths_outliers_3_1 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "1") 

easthydric_depths_outliers_3_2 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "2") 


easthydric_depths_outliers_3_3 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "3") 


easthydric_depths_outliers_3_4 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "4") 

easthydric_depths_outliers_3_5 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "5") 

easthydric_depths_outliers_3_6 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "6") 


easthydric_depths_outliers_3_7 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "7") 


easthydric_depths_outliers_3_8 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "8") 


easthydric_3_1_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_1)   

easthydric_3_2_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_2)   

easthydric_3_3_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_3)   

easthydric_3_4_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_4)   

easthydric_3_5_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_5)   

easthydric_3_6_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_6)  

easthydric_3_7_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_7)   

easthydric_3_8_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_8) 

easthydric_probe3_cleaned =
  easthydric_3_1_forcombine %>% 
  vctrs::vec_c(easthydric_3_2_forcombine, easthydric_3_3_forcombine,
               easthydric_3_4_forcombine, easthydric_3_5_forcombine,
               easthydric_3_6_forcombine, easthydric_3_7_forcombine,
               easthydric_3_8_forcombine)



#EAST MESIC PROBE 3 SENSORS 1-8 

eastmesic_depths_outliers_3_1 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "1") 

eastmesic_depths_outliers_3_2 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "2") 


eastmesic_depths_outliers_3_3 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "3") 


eastmesic_depths_outliers_3_4 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "4") 

eastmesic_depths_outliers_3_5 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "5") 

eastmesic_depths_outliers_3_6 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "6") 


eastmesic_depths_outliers_3_7 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "7") 


eastmesic_depths_outliers_3_8 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "8") 


eastmesic_3_1_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_1)   

eastmesic_3_2_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_2)   

eastmesic_3_3_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_3)   

eastmesic_3_4_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_4)   

eastmesic_3_5_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_5)   

eastmesic_3_6_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_6)  

eastmesic_3_7_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_7)   

eastmesic_3_8_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_8) 

eastmesic_probe3_cleaned =
  eastmesic_3_1_forcombine %>% 
  vctrs::vec_c(eastmesic_3_2_forcombine, eastmesic_3_3_forcombine,
               eastmesic_3_4_forcombine, eastmesic_3_5_forcombine,
               eastmesic_3_6_forcombine, eastmesic_3_7_forcombine,
               eastmesic_3_8_forcombine)
#####

#####
#EAST MESIC PROBE 1 SENSORS 1-8 

eastmesic_depths_outliers_1_1 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "1") 

eastmesic_depths_outliers_1_2 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "2") 


eastmesic_depths_outliers_1_3 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "3") 


eastmesic_depths_outliers_1_4 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "4") 

eastmesic_depths_outliers_1_5 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "5") 

eastmesic_depths_outliers_1_6 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "6") 


eastmesic_depths_outliers_1_7 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "7") 


eastmesic_depths_outliers_1_8 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "8") 


eastmesic_1_1_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_1)   

eastmesic_1_2_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_2)   

eastmesic_1_3_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_3)   

eastmesic_1_4_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_4)   

eastmesic_1_5_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_5)   

eastmesic_1_6_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_6)  

eastmesic_1_7_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_7)   

eastmesic_1_8_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_8) 

eastmesic_probe1_cleaned =
  eastmesic_1_1_forcombine %>% 
  vctrs::vec_c(eastmesic_1_2_forcombine, eastmesic_1_3_forcombine,
               eastmesic_1_4_forcombine, eastmesic_1_5_forcombine,
               eastmesic_1_6_forcombine, eastmesic_1_7_forcombine,
               eastmesic_1_8_forcombine)

#####
#EAST MESIC PROBE 2 SENSORS 1-8 

eastmesic_depths_outliers_2_1 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "1") 

eastmesic_depths_outliers_2_2 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "2") 


eastmesic_depths_outliers_2_3 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "3") 


eastmesic_depths_outliers_2_4 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "4") 

eastmesic_depths_outliers_2_5 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "5") 

eastmesic_depths_outliers_2_6 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "6") 


eastmesic_depths_outliers_2_7 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "7") 


eastmesic_depths_outliers_2_8 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "8") 


eastmesic_2_1_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_1)   

eastmesic_2_2_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_2)   

eastmesic_2_3_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_3)   

eastmesic_2_4_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_4)   

eastmesic_2_5_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_5)   

eastmesic_2_6_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_6)  

eastmesic_2_7_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_7)   

eastmesic_2_8_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_8) 

eastmesic_probe2_cleaned =
  eastmesic_2_1_forcombine %>% 
  vctrs::vec_c(eastmesic_2_2_forcombine, eastmesic_2_3_forcombine,
               eastmesic_2_4_forcombine, eastmesic_2_5_forcombine,
               eastmesic_2_6_forcombine, eastmesic_2_7_forcombine,
               eastmesic_2_8_forcombine)
#####
#EAST MESIC PROBE 3 SENSORS 1-8 

eastmesic_depths_outliers_3_1 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "1") 

eastmesic_depths_outliers_3_2 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "2") 


eastmesic_depths_outliers_3_3 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "3") 


eastmesic_depths_outliers_3_4 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "4") 

eastmesic_depths_outliers_3_5 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "5") 

eastmesic_depths_outliers_3_6 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "6") 


eastmesic_depths_outliers_3_7 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "7") 


eastmesic_depths_outliers_3_8 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "8") 


eastmesic_3_1_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_1)   

eastmesic_3_2_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_2)   

eastmesic_3_3_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_3)   

eastmesic_3_4_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_4)   

eastmesic_3_5_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_5)   

eastmesic_3_6_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_6)  

eastmesic_3_7_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_7)   

eastmesic_3_8_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_8) 

eastmesic_probe3_cleaned =
  eastmesic_3_1_forcombine %>% 
  vctrs::vec_c(eastmesic_3_2_forcombine, eastmesic_3_3_forcombine,
               eastmesic_3_4_forcombine, eastmesic_3_5_forcombine,
               eastmesic_3_6_forcombine, eastmesic_3_7_forcombine,
               eastmesic_3_8_forcombine)
#####

#WEST MESIC PROBE 1 SENSORS 1-8 

westmesic_depths_outliers_1_1 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "1") 

westmesic_depths_outliers_1_2 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "2") 


westmesic_depths_outliers_1_3 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "3") 


westmesic_depths_outliers_1_4 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "4") 

westmesic_depths_outliers_1_5 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "5") 

westmesic_depths_outliers_1_6 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "6") 


westmesic_depths_outliers_1_7 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "7") 


westmesic_depths_outliers_1_8 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "8") 


westmesic_1_1_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_1)   

westmesic_1_2_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_2)   

westmesic_1_3_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_3)   

westmesic_1_4_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_4)   

westmesic_1_5_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_5)   

westmesic_1_6_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_6)  

westmesic_1_7_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_7)   

westmesic_1_8_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_8) 

westmesic_probe1_cleaned =
  westmesic_1_1_forcombine %>% 
  vctrs::vec_c(westmesic_1_2_forcombine, westmesic_1_3_forcombine,
               westmesic_1_4_forcombine, westmesic_1_5_forcombine,
               westmesic_1_6_forcombine, westmesic_1_7_forcombine,
               westmesic_1_8_forcombine)
#####

#WEST MESIC PROBE 2 SENSORS 1-8 

westmesic_depths_outliers_2_1 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "1") 

westmesic_depths_outliers_2_2 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "2") 


westmesic_depths_outliers_2_3 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "3") 


westmesic_depths_outliers_2_4 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "4") 

westmesic_depths_outliers_2_5 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "5") 

westmesic_depths_outliers_2_6 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "6") 


westmesic_depths_outliers_2_7 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "7") 


westmesic_depths_outliers_2_8 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "8") 


westmesic_2_1_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_1)   

westmesic_2_2_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_2)   

westmesic_2_3_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_3)   

westmesic_2_4_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_4)   

westmesic_2_5_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_5)   

westmesic_2_6_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_6)  

westmesic_2_7_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_7)   

westmesic_2_8_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_8) 

westmesic_probe2_cleaned =
  westmesic_2_1_forcombine %>% 
  vctrs::vec_c(westmesic_2_2_forcombine, westmesic_2_3_forcombine,
               westmesic_2_4_forcombine, westmesic_2_5_forcombine,
               westmesic_2_6_forcombine, westmesic_2_7_forcombine,
               westmesic_2_8_forcombine)

#####

#WEST MESIC PROBE 3 SENSORS 1-8 

westmesic_depths_outliers_3_1 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "1") 

westmesic_depths_outliers_3_2 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "2") 


westmesic_depths_outliers_3_3 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "3") 


westmesic_depths_outliers_3_4 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "4") 

westmesic_depths_outliers_3_5 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "5") 

westmesic_depths_outliers_3_6 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "6") 


westmesic_depths_outliers_3_7 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "7") 


westmesic_depths_outliers_3_8 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "8") 


westmesic_3_1_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_1)   

westmesic_3_2_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_2)   

westmesic_3_3_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_3)   

westmesic_3_4_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_4)   

westmesic_3_5_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_5)   

westmesic_3_6_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_6)  

westmesic_3_7_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_7)   

westmesic_3_8_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_8) 

westmesic_probe3_cleaned =
  westmesic_3_1_forcombine %>% 
  vctrs::vec_c(westmesic_3_2_forcombine, westmesic_3_3_forcombine,
               westmesic_3_4_forcombine, westmesic_3_5_forcombine,
               westmesic_3_6_forcombine, westmesic_3_7_forcombine,
               westmesic_3_8_forcombine)



#####
#WEST DRY PROBE 1 SENSORS 1-8 

westdry_depths_outliers_1_1 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "1") 

westdry_depths_outliers_1_2 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "2") 


westdry_depths_outliers_1_3 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "3") 


westdry_depths_outliers_1_4 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "4") 

westdry_depths_outliers_1_5 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "5") 

westdry_depths_outliers_1_6 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "6") 


westdry_depths_outliers_1_7 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "7") 


westdry_depths_outliers_1_8 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "8") 


westdry_1_1_forcombine =
  datacleaning_function(westdry_depths_outliers_1_1)   

westdry_1_2_forcombine =
  datacleaning_function(westdry_depths_outliers_1_2)   

westdry_1_3_forcombine =
  datacleaning_function(westdry_depths_outliers_1_3)   

westdry_1_4_forcombine =
  datacleaning_function(westdry_depths_outliers_1_4)   

westdry_1_5_forcombine =
  datacleaning_function(westdry_depths_outliers_1_5)   

westdry_1_6_forcombine =
  datacleaning_function(westdry_depths_outliers_1_6)  

westdry_1_7_forcombine =
  datacleaning_function(westdry_depths_outliers_1_7)   

westdry_1_8_forcombine =
  datacleaning_function(westdry_depths_outliers_1_8) 

westdry_probe1_cleaned =
  westdry_1_1_forcombine %>% 
  vctrs::vec_c(westdry_1_2_forcombine, westdry_1_3_forcombine,
               westdry_1_4_forcombine, westdry_1_5_forcombine,
               westdry_1_6_forcombine, westdry_1_7_forcombine,
               westdry_1_8_forcombine)

#####

#WEST DRY PROBE 2 SENSORS 1-8 

westdry_depths_outliers_2_1 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "1") 

westdry_depths_outliers_2_2 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "2") 


westdry_depths_outliers_2_3 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "3") 


westdry_depths_outliers_2_4 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "4") 

westdry_depths_outliers_2_5 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "5") 

westdry_depths_outliers_2_6 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "6") 


westdry_depths_outliers_2_7 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "7") 


westdry_depths_outliers_2_8 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "8") 


westdry_2_1_forcombine =
  datacleaning_function(westdry_depths_outliers_2_1)   

westdry_2_2_forcombine =
  datacleaning_function(westdry_depths_outliers_2_2)   

westdry_2_3_forcombine =
  datacleaning_function(westdry_depths_outliers_2_3)   

westdry_2_4_forcombine =
  datacleaning_function(westdry_depths_outliers_2_4)   

westdry_2_5_forcombine =
  datacleaning_function(westdry_depths_outliers_2_5)   

westdry_2_6_forcombine =
  datacleaning_function(westdry_depths_outliers_2_6)  

westdry_2_7_forcombine =
  datacleaning_function(westdry_depths_outliers_2_7)   

westdry_2_8_forcombine =
  datacleaning_function(westdry_depths_outliers_2_8) 

westdry_probe2_cleaned =
  westdry_2_1_forcombine %>% 
  vctrs::vec_c(westdry_2_2_forcombine, westdry_2_3_forcombine,
               westdry_2_4_forcombine, westdry_2_5_forcombine,
               westdry_2_6_forcombine, westdry_2_7_forcombine,
               westdry_2_8_forcombine)
#####

#WEST DRY PROBE 3 SENSORS 1-8 

westdry_depths_outliers_3_1 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "1") 

westdry_depths_outliers_3_2 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "2") 


westdry_depths_outliers_3_3 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "3") 


westdry_depths_outliers_3_4 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "4") 

westdry_depths_outliers_3_5 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "5") 

westdry_depths_outliers_3_6 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "6") 


westdry_depths_outliers_3_7 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "7") 


westdry_depths_outliers_3_8 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "8") 


westdry_3_1_forcombine =
  datacleaning_function(westdry_depths_outliers_3_1)   

westdry_3_2_forcombine =
  datacleaning_function(westdry_depths_outliers_3_2)   

westdry_3_3_forcombine =
  datacleaning_function(westdry_depths_outliers_3_3)   

westdry_3_4_forcombine =
  datacleaning_function(westdry_depths_outliers_3_4)   

westdry_3_5_forcombine =
  datacleaning_function(westdry_depths_outliers_3_5)   

westdry_3_6_forcombine =
  datacleaning_function(westdry_depths_outliers_3_6)  

westdry_3_7_forcombine =
  datacleaning_function(westdry_depths_outliers_3_7)   

westdry_3_8_forcombine =
  datacleaning_function(westdry_depths_outliers_3_8) 

westdry_probe3_cleaned =
  westdry_3_1_forcombine %>% 
  vctrs::vec_c(westdry_3_2_forcombine, westdry_3_3_forcombine,
               westdry_3_4_forcombine, westdry_3_5_forcombine,
               westdry_3_6_forcombine, westdry_3_7_forcombine,
               westdry_3_8_forcombine)



#####

#EAST DRY PROBE 1 SENSORS 1-8 

eastdry_depths_outliers_1_1 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "1") 

eastdry_depths_outliers_1_2 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "2") 


eastdry_depths_outliers_1_3 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "3") 


eastdry_depths_outliers_1_4 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "4") 

eastdry_depths_outliers_1_5 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "5") 

eastdry_depths_outliers_1_6 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "6") 


eastdry_depths_outliers_1_7 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "7") 


eastdry_depths_outliers_1_8 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "8") 


eastdry_1_1_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_1)   

eastdry_1_2_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_2)   

eastdry_1_3_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_3)   

eastdry_1_4_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_4)   

eastdry_1_5_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_5)   

eastdry_1_6_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_6)  

eastdry_1_7_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_7)   

eastdry_1_8_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_8) 

eastdry_probe1_cleaned =
  eastdry_1_1_forcombine %>% 
  vctrs::vec_c(eastdry_1_2_forcombine, eastdry_1_3_forcombine,
               eastdry_1_4_forcombine, eastdry_1_5_forcombine,
               eastdry_1_6_forcombine, eastdry_1_7_forcombine,
               eastdry_1_8_forcombine)

#####
#EAST DRY PROBE 2 SENSORS 1-8 

eastdry_depths_outliers_2_1 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "1") 

eastdry_depths_outliers_2_2 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "2") 


eastdry_depths_outliers_2_3 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "3") 


eastdry_depths_outliers_2_4 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "4") 

eastdry_depths_outliers_2_5 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "5") 

eastdry_depths_outliers_2_6 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "6") 


eastdry_depths_outliers_2_7 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "7") 


eastdry_depths_outliers_2_8 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "8") 


eastdry_2_1_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_1)   

eastdry_2_2_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_2)   

eastdry_2_3_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_3)   

eastdry_2_4_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_4)   

eastdry_2_5_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_5)   

eastdry_2_6_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_6)  

eastdry_2_7_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_7)   

eastdry_2_8_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_8) 

eastdry_probe2_cleaned =
  eastdry_2_1_forcombine %>% 
  vctrs::vec_c(eastdry_2_2_forcombine, eastdry_2_3_forcombine,
               eastdry_2_4_forcombine, eastdry_2_5_forcombine,
               eastdry_2_6_forcombine, eastdry_2_7_forcombine,
               eastdry_2_8_forcombine)

#####

#EAST DRY PROBE 3 SENSORS 1-8 

eastdry_depths_outliers_3_1 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "1") 

eastdry_depths_outliers_3_2 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "2") 


eastdry_depths_outliers_3_3 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "3") 


eastdry_depths_outliers_3_4 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "4") 

eastdry_depths_outliers_3_5 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "5") 

eastdry_depths_outliers_3_6 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "6") 


eastdry_depths_outliers_3_7 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "7") 


eastdry_depths_outliers_3_8 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "8") 


eastdry_3_1_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_1)   

eastdry_3_2_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_2)   

eastdry_3_3_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_3)   

eastdry_3_4_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_4)   

eastdry_3_5_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_5)   

eastdry_3_6_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_6)  

eastdry_3_7_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_7)   

eastdry_3_8_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_8) 

eastdry_probe3_cleaned =
  eastdry_3_1_forcombine %>% 
  vctrs::vec_c(eastdry_3_2_forcombine, eastdry_3_3_forcombine,
               eastdry_3_4_forcombine, eastdry_3_5_forcombine,
               eastdry_3_6_forcombine, eastdry_3_7_forcombine,
               eastdry_3_8_forcombine)



#####

#EAST HYDRIC PROBE 2 SENSORS 1-8 

easthydric_depths_outliers_2_1 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "1") 

easthydric_depths_outliers_2_2 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "2") 


easthydric_depths_outliers_2_3 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "3") 


easthydric_depths_outliers_2_4 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "4") 

easthydric_depths_outliers_2_5 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "5") 

easthydric_depths_outliers_2_6 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "6") 


easthydric_depths_outliers_2_7 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "7") 


easthydric_depths_outliers_2_8 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "8") 


easthydric_2_1_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_1)   

easthydric_2_2_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_2)   

easthydric_2_3_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_3)   

easthydric_2_4_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_4)   

easthydric_2_5_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_5)   

easthydric_2_6_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_6)  

easthydric_2_7_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_7)   

easthydric_2_8_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_8) 

easthydric_probe2_cleaned =
  easthydric_2_1_forcombine %>% 
  vctrs::vec_c(easthydric_2_2_forcombine, easthydric_2_3_forcombine,
               easthydric_2_4_forcombine, easthydric_2_5_forcombine,
               easthydric_2_6_forcombine, easthydric_2_7_forcombine,
               easthydric_2_8_forcombine)


#####

#EAST HYDRIC PROBE 3 SENSORS 1-8 

easthydric_depths_outliers_3_1 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "1") 

easthydric_depths_outliers_3_2 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "2") 


easthydric_depths_outliers_3_3 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "3") 


easthydric_depths_outliers_3_4 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "4") 

easthydric_depths_outliers_3_5 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "5") 

easthydric_depths_outliers_3_6 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "6") 


easthydric_depths_outliers_3_7 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "7") 


easthydric_depths_outliers_3_8 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "8") 


easthydric_3_1_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_1)   

easthydric_3_2_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_2)   

easthydric_3_3_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_3)   

easthydric_3_4_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_4)   

easthydric_3_5_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_5)   

easthydric_3_6_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_6)  

easthydric_3_7_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_7)   

easthydric_3_8_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_8) 

easthydric_probe3_cleaned =
  easthydric_3_1_forcombine %>% 
  vctrs::vec_c(easthydric_3_2_forcombine, easthydric_3_3_forcombine,
               easthydric_3_4_forcombine, easthydric_3_5_forcombine,
               easthydric_3_6_forcombine, easthydric_3_7_forcombine,
               easthydric_3_8_forcombine)



#EAST MESIC PROBE 3 SENSORS 1-8 

eastmesic_depths_outliers_3_1 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "1") 

eastmesic_depths_outliers_3_2 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "2") 


eastmesic_depths_outliers_3_3 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "3") 


eastmesic_depths_outliers_3_4 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "4") 

eastmesic_depths_outliers_3_5 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "5") 

eastmesic_depths_outliers_3_6 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "6") 


eastmesic_depths_outliers_3_7 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "7") 


eastmesic_depths_outliers_3_8 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "8") 


eastmesic_3_1_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_1)   

eastmesic_3_2_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_2)   

eastmesic_3_3_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_3)   

eastmesic_3_4_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_4)   

eastmesic_3_5_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_5)   

eastmesic_3_6_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_6)  

eastmesic_3_7_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_7)   

eastmesic_3_8_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_8) 

eastmesic_probe3_cleaned =
  eastmesic_3_1_forcombine %>% 
  vctrs::vec_c(eastmesic_3_2_forcombine, eastmesic_3_3_forcombine,
               eastmesic_3_4_forcombine, eastmesic_3_5_forcombine,
               eastmesic_3_6_forcombine, eastmesic_3_7_forcombine,
               eastmesic_3_8_forcombine)
#####

#####
#EAST MESIC PROBE 1 SENSORS 1-8 

eastmesic_depths_outliers_1_1 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "1") 

eastmesic_depths_outliers_1_2 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "2") 


eastmesic_depths_outliers_1_3 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "3") 


eastmesic_depths_outliers_1_4 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "4") 

eastmesic_depths_outliers_1_5 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "5") 

eastmesic_depths_outliers_1_6 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "6") 


eastmesic_depths_outliers_1_7 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "7") 


eastmesic_depths_outliers_1_8 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "8") 


eastmesic_1_1_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_1)   

eastmesic_1_2_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_2)   

eastmesic_1_3_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_3)   

eastmesic_1_4_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_4)   

eastmesic_1_5_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_5)   

eastmesic_1_6_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_6)  

eastmesic_1_7_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_7)   

eastmesic_1_8_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_8) 

eastmesic_probe1_cleaned =
  eastmesic_1_1_forcombine %>% 
  vctrs::vec_c(eastmesic_1_2_forcombine, eastmesic_1_3_forcombine,
               eastmesic_1_4_forcombine, eastmesic_1_5_forcombine,
               eastmesic_1_6_forcombine, eastmesic_1_7_forcombine,
               eastmesic_1_8_forcombine)

#####
#EAST MESIC PROBE 2 SENSORS 1-8 

eastmesic_depths_outliers_2_1 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "1") 

eastmesic_depths_outliers_2_2 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "2") 


eastmesic_depths_outliers_2_3 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "3") 


eastmesic_depths_outliers_2_4 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "4") 

eastmesic_depths_outliers_2_5 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "5") 

eastmesic_depths_outliers_2_6 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "6") 


eastmesic_depths_outliers_2_7 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "7") 


eastmesic_depths_outliers_2_8 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "8") 


eastmesic_2_1_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_1)   

eastmesic_2_2_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_2)   

eastmesic_2_3_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_3)   

eastmesic_2_4_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_4)   

eastmesic_2_5_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_5)   

eastmesic_2_6_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_6)  

eastmesic_2_7_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_7)   

eastmesic_2_8_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_8) 

eastmesic_probe2_cleaned =
  eastmesic_2_1_forcombine %>% 
  vctrs::vec_c(eastmesic_2_2_forcombine, eastmesic_2_3_forcombine,
               eastmesic_2_4_forcombine, eastmesic_2_5_forcombine,
               eastmesic_2_6_forcombine, eastmesic_2_7_forcombine,
               eastmesic_2_8_forcombine)
#####
#EAST MESIC PROBE 3 SENSORS 1-8 

eastmesic_depths_outliers_3_1 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "1") 

eastmesic_depths_outliers_3_2 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "2") 


eastmesic_depths_outliers_3_3 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "3") 


eastmesic_depths_outliers_3_4 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "4") 

eastmesic_depths_outliers_3_5 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "5") 

eastmesic_depths_outliers_3_6 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "6") 


eastmesic_depths_outliers_3_7 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "7") 


eastmesic_depths_outliers_3_8 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "8") 


eastmesic_3_1_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_1)   

eastmesic_3_2_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_2)   

eastmesic_3_3_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_3)   

eastmesic_3_4_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_4)   

eastmesic_3_5_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_5)   

eastmesic_3_6_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_6)  

eastmesic_3_7_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_7)   

eastmesic_3_8_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_8) 

eastmesic_probe3_cleaned =
  eastmesic_3_1_forcombine %>% 
  vctrs::vec_c(eastmesic_3_2_forcombine, eastmesic_3_3_forcombine,
               eastmesic_3_4_forcombine, eastmesic_3_5_forcombine,
               eastmesic_3_6_forcombine, eastmesic_3_7_forcombine,
               eastmesic_3_8_forcombine)
#####

#WEST MESIC PROBE 1 SENSORS 1-8 

westmesic_depths_outliers_1_1 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "1") 

westmesic_depths_outliers_1_2 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "2") 


westmesic_depths_outliers_1_3 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "3") 


westmesic_depths_outliers_1_4 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "4") 

westmesic_depths_outliers_1_5 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "5") 

westmesic_depths_outliers_1_6 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "6") 


westmesic_depths_outliers_1_7 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "7") 


westmesic_depths_outliers_1_8 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "8") 


westmesic_1_1_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_1)   

westmesic_1_2_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_2)   

westmesic_1_3_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_3)   

westmesic_1_4_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_4)   

westmesic_1_5_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_5)   

westmesic_1_6_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_6)  

westmesic_1_7_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_7)   

westmesic_1_8_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_8) 

westmesic_probe1_cleaned =
  westmesic_1_1_forcombine %>% 
  vctrs::vec_c(westmesic_1_2_forcombine, westmesic_1_3_forcombine,
               westmesic_1_4_forcombine, westmesic_1_5_forcombine,
               westmesic_1_6_forcombine, westmesic_1_7_forcombine,
               westmesic_1_8_forcombine)
#####

#WEST MESIC PROBE 2 SENSORS 1-8 

westmesic_depths_outliers_2_1 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "1") 

westmesic_depths_outliers_2_2 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "2") 


westmesic_depths_outliers_2_3 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "3") 


westmesic_depths_outliers_2_4 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "4") 

westmesic_depths_outliers_2_5 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "5") 

westmesic_depths_outliers_2_6 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "6") 


westmesic_depths_outliers_2_7 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "7") 


westmesic_depths_outliers_2_8 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "8") 


westmesic_2_1_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_1)   

westmesic_2_2_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_2)   

westmesic_2_3_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_3)   

westmesic_2_4_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_4)   

westmesic_2_5_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_5)   

westmesic_2_6_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_6)  

westmesic_2_7_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_7)   

westmesic_2_8_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_8) 

westmesic_probe2_cleaned =
  westmesic_2_1_forcombine %>% 
  vctrs::vec_c(westmesic_2_2_forcombine, westmesic_2_3_forcombine,
               westmesic_2_4_forcombine, westmesic_2_5_forcombine,
               westmesic_2_6_forcombine, westmesic_2_7_forcombine,
               westmesic_2_8_forcombine)

#####

#WEST MESIC PROBE 3 SENSORS 1-8 

westmesic_depths_outliers_3_1 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "1") 

westmesic_depths_outliers_3_2 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "2") 


westmesic_depths_outliers_3_3 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "3") 


westmesic_depths_outliers_3_4 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "4") 

westmesic_depths_outliers_3_5 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "5") 

westmesic_depths_outliers_3_6 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "6") 


westmesic_depths_outliers_3_7 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "7") 


westmesic_depths_outliers_3_8 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "8") 


westmesic_3_1_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_1)   

westmesic_3_2_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_2)   

westmesic_3_3_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_3)   

westmesic_3_4_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_4)   

westmesic_3_5_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_5)   

westmesic_3_6_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_6)  

westmesic_3_7_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_7)   

westmesic_3_8_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_8) 

westmesic_probe3_cleaned =
  westmesic_3_1_forcombine %>% 
  vctrs::vec_c(westmesic_3_2_forcombine, westmesic_3_3_forcombine,
               westmesic_3_4_forcombine, westmesic_3_5_forcombine,
               westmesic_3_6_forcombine, westmesic_3_7_forcombine,
               westmesic_3_8_forcombine)



#####
#WEST DRY PROBE 1 SENSORS 1-8 

westdry_depths_outliers_1_1 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "1") 

westdry_depths_outliers_1_2 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "2") 


westdry_depths_outliers_1_3 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "3") 


westdry_depths_outliers_1_4 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "4") 

westdry_depths_outliers_1_5 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "5") 

westdry_depths_outliers_1_6 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "6") 


westdry_depths_outliers_1_7 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "7") 


westdry_depths_outliers_1_8 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "8") 


westdry_1_1_forcombine =
  datacleaning_function(westdry_depths_outliers_1_1)   

westdry_1_2_forcombine =
  datacleaning_function(westdry_depths_outliers_1_2)   

westdry_1_3_forcombine =
  datacleaning_function(westdry_depths_outliers_1_3)   

westdry_1_4_forcombine =
  datacleaning_function(westdry_depths_outliers_1_4)   

westdry_1_5_forcombine =
  datacleaning_function(westdry_depths_outliers_1_5)   

westdry_1_6_forcombine =
  datacleaning_function(westdry_depths_outliers_1_6)  

westdry_1_7_forcombine =
  datacleaning_function(westdry_depths_outliers_1_7)   

westdry_1_8_forcombine =
  datacleaning_function(westdry_depths_outliers_1_8) 

westdry_probe1_cleaned =
  westdry_1_1_forcombine %>% 
  vctrs::vec_c(westdry_1_2_forcombine, westdry_1_3_forcombine,
               westdry_1_4_forcombine, westdry_1_5_forcombine,
               westdry_1_6_forcombine, westdry_1_7_forcombine,
               westdry_1_8_forcombine)

#####

#WEST DRY PROBE 2 SENSORS 1-8 

westdry_depths_outliers_2_1 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "1") 

westdry_depths_outliers_2_2 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "2") 


westdry_depths_outliers_2_3 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "3") 


westdry_depths_outliers_2_4 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "4") 

westdry_depths_outliers_2_5 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "5") 

westdry_depths_outliers_2_6 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "6") 


westdry_depths_outliers_2_7 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "7") 


westdry_depths_outliers_2_8 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "8") 


westdry_2_1_forcombine =
  datacleaning_function(westdry_depths_outliers_2_1)   

westdry_2_2_forcombine =
  datacleaning_function(westdry_depths_outliers_2_2)   

westdry_2_3_forcombine =
  datacleaning_function(westdry_depths_outliers_2_3)   

westdry_2_4_forcombine =
  datacleaning_function(westdry_depths_outliers_2_4)   

westdry_2_5_forcombine =
  datacleaning_function(westdry_depths_outliers_2_5)   

westdry_2_6_forcombine =
  datacleaning_function(westdry_depths_outliers_2_6)  

westdry_2_7_forcombine =
  datacleaning_function(westdry_depths_outliers_2_7)   

westdry_2_8_forcombine =
  datacleaning_function(westdry_depths_outliers_2_8) 

westdry_probe2_cleaned =
  westdry_2_1_forcombine %>% 
  vctrs::vec_c(westdry_2_2_forcombine, westdry_2_3_forcombine,
               westdry_2_4_forcombine, westdry_2_5_forcombine,
               westdry_2_6_forcombine, westdry_2_7_forcombine,
               westdry_2_8_forcombine)
#####

#WEST DRY PROBE 3 SENSORS 1-8 

westdry_depths_outliers_3_1 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "1") 

westdry_depths_outliers_3_2 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "2") 


westdry_depths_outliers_3_3 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "3") 


westdry_depths_outliers_3_4 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "4") 

westdry_depths_outliers_3_5 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "5") 

westdry_depths_outliers_3_6 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "6") 


westdry_depths_outliers_3_7 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "7") 


westdry_depths_outliers_3_8 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "8") 


westdry_3_1_forcombine =
  datacleaning_function(westdry_depths_outliers_3_1)   

westdry_3_2_forcombine =
  datacleaning_function(westdry_depths_outliers_3_2)   

westdry_3_3_forcombine =
  datacleaning_function(westdry_depths_outliers_3_3)   

westdry_3_4_forcombine =
  datacleaning_function(westdry_depths_outliers_3_4)   

westdry_3_5_forcombine =
  datacleaning_function(westdry_depths_outliers_3_5)   

westdry_3_6_forcombine =
  datacleaning_function(westdry_depths_outliers_3_6)  

westdry_3_7_forcombine =
  datacleaning_function(westdry_depths_outliers_3_7)   

westdry_3_8_forcombine =
  datacleaning_function(westdry_depths_outliers_3_8) 

westdry_probe3_cleaned =
  westdry_3_1_forcombine %>% 
  vctrs::vec_c(westdry_3_2_forcombine, westdry_3_3_forcombine,
               westdry_3_4_forcombine, westdry_3_5_forcombine,
               westdry_3_6_forcombine, westdry_3_7_forcombine,
               westdry_3_8_forcombine)



#####

#EAST DRY PROBE 1 SENSORS 1-8 

eastdry_depths_outliers_1_1 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "1") 

eastdry_depths_outliers_1_2 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "2") 


eastdry_depths_outliers_1_3 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "3") 


eastdry_depths_outliers_1_4 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "4") 

eastdry_depths_outliers_1_5 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "5") 

eastdry_depths_outliers_1_6 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "6") 


eastdry_depths_outliers_1_7 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "7") 


eastdry_depths_outliers_1_8 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "8") 


eastdry_1_1_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_1)   

eastdry_1_2_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_2)   

eastdry_1_3_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_3)   

eastdry_1_4_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_4)   

eastdry_1_5_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_5)   

eastdry_1_6_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_6)  

eastdry_1_7_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_7)   

eastdry_1_8_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_8) 

eastdry_probe1_cleaned =
  eastdry_1_1_forcombine %>% 
  vctrs::vec_c(eastdry_1_2_forcombine, eastdry_1_3_forcombine,
               eastdry_1_4_forcombine, eastdry_1_5_forcombine,
               eastdry_1_6_forcombine, eastdry_1_7_forcombine,
               eastdry_1_8_forcombine)

#####
#EAST DRY PROBE 2 SENSORS 1-8 

eastdry_depths_outliers_2_1 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "1") 

eastdry_depths_outliers_2_2 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "2") 


eastdry_depths_outliers_2_3 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "3") 


eastdry_depths_outliers_2_4 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "4") 

eastdry_depths_outliers_2_5 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "5") 

eastdry_depths_outliers_2_6 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "6") 


eastdry_depths_outliers_2_7 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "7") 


eastdry_depths_outliers_2_8 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "8") 


eastdry_2_1_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_1)   

eastdry_2_2_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_2)   

eastdry_2_3_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_3)   

eastdry_2_4_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_4)   

eastdry_2_5_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_5)   

eastdry_2_6_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_6)  

eastdry_2_7_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_7)   

eastdry_2_8_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_8) 

eastdry_probe2_cleaned =
  eastdry_2_1_forcombine %>% 
  vctrs::vec_c(eastdry_2_2_forcombine, eastdry_2_3_forcombine,
               eastdry_2_4_forcombine, eastdry_2_5_forcombine,
               eastdry_2_6_forcombine, eastdry_2_7_forcombine,
               eastdry_2_8_forcombine)

#####

#EAST DRY PROBE 3 SENSORS 1-8 

eastdry_depths_outliers_3_1 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "1") 

eastdry_depths_outliers_3_2 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "2") 


eastdry_depths_outliers_3_3 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "3") 


eastdry_depths_outliers_3_4 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "4") 

eastdry_depths_outliers_3_5 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "5") 

eastdry_depths_outliers_3_6 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "6") 


eastdry_depths_outliers_3_7 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "7") 


eastdry_depths_outliers_3_8 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "8") 


eastdry_3_1_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_1)   

eastdry_3_2_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_2)   

eastdry_3_3_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_3)   

eastdry_3_4_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_4)   

eastdry_3_5_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_5)   

eastdry_3_6_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_6)  

eastdry_3_7_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_7)   

eastdry_3_8_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_8) 

eastdry_probe3_cleaned =
  eastdry_3_1_forcombine %>% 
  vctrs::vec_c(eastdry_3_2_forcombine, eastdry_3_3_forcombine,
               eastdry_3_4_forcombine, eastdry_3_5_forcombine,
               eastdry_3_6_forcombine, eastdry_3_7_forcombine,
               eastdry_3_8_forcombine)



#####

#EAST HYDRIC PROBE 2 SENSORS 1-8 

easthydric_depths_outliers_2_1 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "1") 

easthydric_depths_outliers_2_2 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "2") 


easthydric_depths_outliers_2_3 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "3") 


easthydric_depths_outliers_2_4 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "4") 

easthydric_depths_outliers_2_5 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "5") 

easthydric_depths_outliers_2_6 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "6") 


easthydric_depths_outliers_2_7 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "7") 


easthydric_depths_outliers_2_8 = 
  easthydric_depths %>% 
  filter(probe == "2" & sensor == "8") 


easthydric_2_1_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_1)   

easthydric_2_2_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_2)   

easthydric_2_3_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_3)   

easthydric_2_4_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_4)   

easthydric_2_5_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_5)   

easthydric_2_6_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_6)  

easthydric_2_7_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_7)   

easthydric_2_8_forcombine =
  datacleaning_function(easthydric_depths_outliers_2_8) 

easthydric_probe2_cleaned =
  easthydric_2_1_forcombine %>% 
  vctrs::vec_c(easthydric_2_2_forcombine, easthydric_2_3_forcombine,
               easthydric_2_4_forcombine, easthydric_2_5_forcombine,
               easthydric_2_6_forcombine, easthydric_2_7_forcombine,
               easthydric_2_8_forcombine)
#EAST HYDRIC PROBE 3 SENSORS 1-8 

easthydric_depths_outliers_3_1 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "1") 

easthydric_depths_outliers_3_2 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "2") 


easthydric_depths_outliers_3_3 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "3") 


easthydric_depths_outliers_3_4 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "4") 

easthydric_depths_outliers_3_5 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "5") 

easthydric_depths_outliers_3_6 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "6") 


easthydric_depths_outliers_3_7 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "7") 


easthydric_depths_outliers_3_8 = 
  easthydric_depths %>% 
  filter(probe == "3" & sensor == "8") 


easthydric_3_1_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_1)   

easthydric_3_2_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_2)   

easthydric_3_3_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_3)   

easthydric_3_4_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_4)   

easthydric_3_5_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_5)   

easthydric_3_6_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_6)  

easthydric_3_7_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_7)   

easthydric_3_8_forcombine =
  datacleaning_function(easthydric_depths_outliers_3_8) 

easthydric_probe3_cleaned =
  easthydric_3_1_forcombine %>% 
  vctrs::vec_c(easthydric_3_2_forcombine, easthydric_3_3_forcombine,
               easthydric_3_4_forcombine, easthydric_3_5_forcombine,
               easthydric_3_6_forcombine, easthydric_3_7_forcombine,
               easthydric_3_8_forcombine)



#EAST MESIC PROBE 3 SENSORS 1-8 

eastmesic_depths_outliers_3_1 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "1") 

eastmesic_depths_outliers_3_2 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "2") 


eastmesic_depths_outliers_3_3 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "3") 


eastmesic_depths_outliers_3_4 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "4") 

eastmesic_depths_outliers_3_5 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "5") 

eastmesic_depths_outliers_3_6 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "6") 


eastmesic_depths_outliers_3_7 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "7") 


eastmesic_depths_outliers_3_8 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "8") 


eastmesic_3_1_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_1)   

eastmesic_3_2_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_2)   

eastmesic_3_3_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_3)   

eastmesic_3_4_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_4)   

eastmesic_3_5_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_5)   

eastmesic_3_6_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_6)  

eastmesic_3_7_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_7)   

eastmesic_3_8_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_8) 

eastmesic_probe3_cleaned =
  eastmesic_3_1_forcombine %>% 
  vctrs::vec_c(eastmesic_3_2_forcombine, eastmesic_3_3_forcombine,
               eastmesic_3_4_forcombine, eastmesic_3_5_forcombine,
               eastmesic_3_6_forcombine, eastmesic_3_7_forcombine,
               eastmesic_3_8_forcombine)

#EAST MESIC PROBE 1 SENSORS 1-8 

eastmesic_depths_outliers_1_1 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "1") 

eastmesic_depths_outliers_1_2 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "2") 


eastmesic_depths_outliers_1_3 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "3") 


eastmesic_depths_outliers_1_4 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "4") 

eastmesic_depths_outliers_1_5 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "5") 

eastmesic_depths_outliers_1_6 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "6") 


eastmesic_depths_outliers_1_7 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "7") 


eastmesic_depths_outliers_1_8 = 
  eastmesic_depths %>% 
  filter(probe == "1" & sensor == "8") 


eastmesic_1_1_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_1)   

eastmesic_1_2_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_2)   

eastmesic_1_3_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_3)   

eastmesic_1_4_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_4)   

eastmesic_1_5_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_5)   

eastmesic_1_6_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_6)  

eastmesic_1_7_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_7)   

eastmesic_1_8_forcombine =
  datacleaning_function(eastmesic_depths_outliers_1_8) 

eastmesic_probe1_cleaned =
  eastmesic_1_1_forcombine %>% 
  vctrs::vec_c(eastmesic_1_2_forcombine, eastmesic_1_3_forcombine,
               eastmesic_1_4_forcombine, eastmesic_1_5_forcombine,
               eastmesic_1_6_forcombine, eastmesic_1_7_forcombine,
               eastmesic_1_8_forcombine)

#EAST MESIC PROBE 2 SENSORS 1-8 

eastmesic_depths_outliers_2_1 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "1") 

eastmesic_depths_outliers_2_2 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "2") 


eastmesic_depths_outliers_2_3 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "3") 


eastmesic_depths_outliers_2_4 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "4") 

eastmesic_depths_outliers_2_5 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "5") 

eastmesic_depths_outliers_2_6 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "6") 


eastmesic_depths_outliers_2_7 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "7") 


eastmesic_depths_outliers_2_8 = 
  eastmesic_depths %>% 
  filter(probe == "2" & sensor == "8") 


eastmesic_2_1_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_1)   

eastmesic_2_2_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_2)   

eastmesic_2_3_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_3)   

eastmesic_2_4_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_4)   

eastmesic_2_5_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_5)   

eastmesic_2_6_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_6)  

eastmesic_2_7_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_7)   

eastmesic_2_8_forcombine =
  datacleaning_function(eastmesic_depths_outliers_2_8) 

eastmesic_probe2_cleaned =
  eastmesic_2_1_forcombine %>% 
  vctrs::vec_c(eastmesic_2_2_forcombine, eastmesic_2_3_forcombine,
               eastmesic_2_4_forcombine, eastmesic_2_5_forcombine,
               eastmesic_2_6_forcombine, eastmesic_2_7_forcombine,
               eastmesic_2_8_forcombine)

#EAST MESIC PROBE 3 SENSORS 1-8 

eastmesic_depths_outliers_3_1 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "1") 

eastmesic_depths_outliers_3_2 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "2") 


eastmesic_depths_outliers_3_3 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "3") 


eastmesic_depths_outliers_3_4 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "4") 

eastmesic_depths_outliers_3_5 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "5") 

eastmesic_depths_outliers_3_6 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "6") 


eastmesic_depths_outliers_3_7 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "7") 


eastmesic_depths_outliers_3_8 = 
  eastmesic_depths %>% 
  filter(probe == "3" & sensor == "8") 


eastmesic_3_1_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_1)   

eastmesic_3_2_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_2)   

eastmesic_3_3_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_3)   

eastmesic_3_4_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_4)   

eastmesic_3_5_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_5)   

eastmesic_3_6_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_6)  

eastmesic_3_7_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_7)   

eastmesic_3_8_forcombine =
  datacleaning_function(eastmesic_depths_outliers_3_8) 

eastmesic_probe3_cleaned =
  eastmesic_3_1_forcombine %>% 
  vctrs::vec_c(eastmesic_3_2_forcombine, eastmesic_3_3_forcombine,
               eastmesic_3_4_forcombine, eastmesic_3_5_forcombine,
               eastmesic_3_6_forcombine, eastmesic_3_7_forcombine,
               eastmesic_3_8_forcombine)


#WEST MESIC PROBE 1 SENSORS 1-8 

westmesic_depths_outliers_1_1 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "1") 

westmesic_depths_outliers_1_2 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "2") 


westmesic_depths_outliers_1_3 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "3") 


westmesic_depths_outliers_1_4 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "4") 

westmesic_depths_outliers_1_5 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "5") 

westmesic_depths_outliers_1_6 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "6") 


westmesic_depths_outliers_1_7 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "7") 


westmesic_depths_outliers_1_8 = 
  westmesic_depths %>% 
  filter(probe == "1" & sensor == "8") 


westmesic_1_1_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_1)   

westmesic_1_2_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_2)   

westmesic_1_3_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_3)   

westmesic_1_4_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_4)   

westmesic_1_5_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_5)   

westmesic_1_6_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_6)  

westmesic_1_7_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_7)   

westmesic_1_8_forcombine =
  datacleaning_function(westmesic_depths_outliers_1_8) 

westmesic_probe1_cleaned =
  westmesic_1_1_forcombine %>% 
  vctrs::vec_c(westmesic_1_2_forcombine, westmesic_1_3_forcombine,
               westmesic_1_4_forcombine, westmesic_1_5_forcombine,
               westmesic_1_6_forcombine, westmesic_1_7_forcombine,
               westmesic_1_8_forcombine)


#WEST MESIC PROBE 2 SENSORS 1-8 

westmesic_depths_outliers_2_1 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "1") 

westmesic_depths_outliers_2_2 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "2") 


westmesic_depths_outliers_2_3 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "3") 


westmesic_depths_outliers_2_4 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "4") 

westmesic_depths_outliers_2_5 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "5") 

westmesic_depths_outliers_2_6 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "6") 


westmesic_depths_outliers_2_7 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "7") 


westmesic_depths_outliers_2_8 = 
  westmesic_depths %>% 
  filter(probe == "2" & sensor == "8") 


westmesic_2_1_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_1)   

westmesic_2_2_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_2)   

westmesic_2_3_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_3)   

westmesic_2_4_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_4)   

westmesic_2_5_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_5)   

westmesic_2_6_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_6)  

westmesic_2_7_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_7)   

westmesic_2_8_forcombine =
  datacleaning_function(westmesic_depths_outliers_2_8) 

westmesic_probe2_cleaned =
  westmesic_2_1_forcombine %>% 
  vctrs::vec_c(westmesic_2_2_forcombine, westmesic_2_3_forcombine,
               westmesic_2_4_forcombine, westmesic_2_5_forcombine,
               westmesic_2_6_forcombine, westmesic_2_7_forcombine,
               westmesic_2_8_forcombine)

#WEST MESIC PROBE 3 SENSORS 1-8 

westmesic_depths_outliers_3_1 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "1") 

westmesic_depths_outliers_3_2 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "2") 


westmesic_depths_outliers_3_3 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "3") 


westmesic_depths_outliers_3_4 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "4") 

westmesic_depths_outliers_3_5 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "5") 

westmesic_depths_outliers_3_6 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "6") 


westmesic_depths_outliers_3_7 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "7") 


westmesic_depths_outliers_3_8 = 
  westmesic_depths %>% 
  filter(probe == "3" & sensor == "8") 


westmesic_3_1_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_1)   

westmesic_3_2_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_2)   

westmesic_3_3_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_3)   

westmesic_3_4_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_4)   

westmesic_3_5_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_5)   

westmesic_3_6_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_6)  

westmesic_3_7_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_7)   

westmesic_3_8_forcombine =
  datacleaning_function(westmesic_depths_outliers_3_8) 

westmesic_probe3_cleaned =
  westmesic_3_1_forcombine %>% 
  vctrs::vec_c(westmesic_3_2_forcombine, westmesic_3_3_forcombine,
               westmesic_3_4_forcombine, westmesic_3_5_forcombine,
               westmesic_3_6_forcombine, westmesic_3_7_forcombine,
               westmesic_3_8_forcombine)


#WEST DRY PROBE 1 SENSORS 1-8 

westdry_depths_outliers_1_1 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "1") 

westdry_depths_outliers_1_2 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "2") 


westdry_depths_outliers_1_3 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "3") 


westdry_depths_outliers_1_4 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "4") 

westdry_depths_outliers_1_5 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "5") 

westdry_depths_outliers_1_6 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "6") 


westdry_depths_outliers_1_7 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "7") 


westdry_depths_outliers_1_8 = 
  westdry_depths %>% 
  filter(probe == "1" & sensor == "8") 


westdry_1_1_forcombine =
  datacleaning_function(westdry_depths_outliers_1_1)   

westdry_1_2_forcombine =
  datacleaning_function(westdry_depths_outliers_1_2)   

westdry_1_3_forcombine =
  datacleaning_function(westdry_depths_outliers_1_3)   

westdry_1_4_forcombine =
  datacleaning_function(westdry_depths_outliers_1_4)   

westdry_1_5_forcombine =
  datacleaning_function(westdry_depths_outliers_1_5)   

westdry_1_6_forcombine =
  datacleaning_function(westdry_depths_outliers_1_6)  

westdry_1_7_forcombine =
  datacleaning_function(westdry_depths_outliers_1_7)   

westdry_1_8_forcombine =
  datacleaning_function(westdry_depths_outliers_1_8) 

westdry_probe1_cleaned =
  westdry_1_1_forcombine %>% 
  vctrs::vec_c(westdry_1_2_forcombine, westdry_1_3_forcombine,
               westdry_1_4_forcombine, westdry_1_5_forcombine,
               westdry_1_6_forcombine, westdry_1_7_forcombine,
               westdry_1_8_forcombine)

#WEST DRY PROBE 2 SENSORS 1-8 

westdry_depths_outliers_2_1 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "1") 

westdry_depths_outliers_2_2 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "2") 


westdry_depths_outliers_2_3 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "3") 


westdry_depths_outliers_2_4 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "4") 

westdry_depths_outliers_2_5 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "5") 

westdry_depths_outliers_2_6 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "6") 


westdry_depths_outliers_2_7 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "7") 


westdry_depths_outliers_2_8 = 
  westdry_depths %>% 
  filter(probe == "2" & sensor == "8") 


westdry_2_1_forcombine =
  datacleaning_function(westdry_depths_outliers_2_1)   

westdry_2_2_forcombine =
  datacleaning_function(westdry_depths_outliers_2_2)   

westdry_2_3_forcombine =
  datacleaning_function(westdry_depths_outliers_2_3)   

westdry_2_4_forcombine =
  datacleaning_function(westdry_depths_outliers_2_4)   

westdry_2_5_forcombine =
  datacleaning_function(westdry_depths_outliers_2_5)   

westdry_2_6_forcombine =
  datacleaning_function(westdry_depths_outliers_2_6)  

westdry_2_7_forcombine =
  datacleaning_function(westdry_depths_outliers_2_7)   

westdry_2_8_forcombine =
  datacleaning_function(westdry_depths_outliers_2_8) 

westdry_probe2_cleaned =
  westdry_2_1_forcombine %>% 
  vctrs::vec_c(westdry_2_2_forcombine, westdry_2_3_forcombine,
               westdry_2_4_forcombine, westdry_2_5_forcombine,
               westdry_2_6_forcombine, westdry_2_7_forcombine,
               westdry_2_8_forcombine)

#WEST DRY PROBE 3 SENSORS 1-8 

westdry_depths_outliers_3_1 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "1") 

westdry_depths_outliers_3_2 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "2") 


westdry_depths_outliers_3_3 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "3") 


westdry_depths_outliers_3_4 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "4") 

westdry_depths_outliers_3_5 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "5") 

westdry_depths_outliers_3_6 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "6") 


westdry_depths_outliers_3_7 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "7") 


westdry_depths_outliers_3_8 = 
  westdry_depths %>% 
  filter(probe == "3" & sensor == "8") 


westdry_3_1_forcombine =
  datacleaning_function(westdry_depths_outliers_3_1)   

westdry_3_2_forcombine =
  datacleaning_function(westdry_depths_outliers_3_2)   

westdry_3_3_forcombine =
  datacleaning_function(westdry_depths_outliers_3_3)   

westdry_3_4_forcombine =
  datacleaning_function(westdry_depths_outliers_3_4)   

westdry_3_5_forcombine =
  datacleaning_function(westdry_depths_outliers_3_5)   

westdry_3_6_forcombine =
  datacleaning_function(westdry_depths_outliers_3_6)  

westdry_3_7_forcombine =
  datacleaning_function(westdry_depths_outliers_3_7)   

westdry_3_8_forcombine =
  datacleaning_function(westdry_depths_outliers_3_8) 

westdry_probe3_cleaned =
  westdry_3_1_forcombine %>% 
  vctrs::vec_c(westdry_3_2_forcombine, westdry_3_3_forcombine,
               westdry_3_4_forcombine, westdry_3_5_forcombine,
               westdry_3_6_forcombine, westdry_3_7_forcombine,
               westdry_3_8_forcombine)
#EAST DRY PROBE 1 SENSORS 1-8 

eastdry_depths_outliers_1_1 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "1") 

eastdry_depths_outliers_1_2 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "2") 


eastdry_depths_outliers_1_3 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "3") 


eastdry_depths_outliers_1_4 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "4") 

eastdry_depths_outliers_1_5 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "5") 

eastdry_depths_outliers_1_6 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "6") 


eastdry_depths_outliers_1_7 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "7") 


eastdry_depths_outliers_1_8 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "8") 


eastdry_1_1_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_1)   

eastdry_1_2_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_2)   

eastdry_1_3_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_3)   

eastdry_1_4_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_4)   

eastdry_1_5_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_5)   

eastdry_1_6_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_6)  

eastdry_1_7_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_7)   

eastdry_1_8_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_8) 

eastdry_probe1_cleaned =
  eastdry_1_1_forcombine %>% 
  vctrs::vec_c(eastdry_1_2_forcombine, eastdry_1_3_forcombine,
               eastdry_1_4_forcombine, eastdry_1_5_forcombine,
               eastdry_1_6_forcombine, eastdry_1_7_forcombine,
               eastdry_1_8_forcombine)
#EAST DRY PROBE 1 SENSORS 1-8 

eastdry_depths_outliers_1_1 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "1") 

eastdry_depths_outliers_1_2 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "2") 


eastdry_depths_outliers_1_3 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "3") 


eastdry_depths_outliers_1_4 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "4") 

eastdry_depths_outliers_1_5 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "5") 

eastdry_depths_outliers_1_6 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "6") 


eastdry_depths_outliers_1_7 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "7") 


eastdry_depths_outliers_1_8 = 
  eastdry_depths %>% 
  filter(probe == "1" & sensor == "8") 


eastdry_1_1_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_1)   

eastdry_1_2_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_2)   

eastdry_1_3_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_3)   

eastdry_1_4_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_4)   

eastdry_1_5_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_5)   

eastdry_1_6_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_6)  

eastdry_1_7_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_7)   

eastdry_1_8_forcombine =
  datacleaning_function(eastdry_depths_outliers_1_8) 

eastdry_probe1_cleaned =
  eastdry_1_1_forcombine %>% 
  vctrs::vec_c(eastdry_1_2_forcombine, eastdry_1_3_forcombine,
               eastdry_1_4_forcombine, eastdry_1_5_forcombine,
               eastdry_1_6_forcombine, eastdry_1_7_forcombine,
               eastdry_1_8_forcombine)

#EAST DRY PROBE 2 SENSORS 1-8 

eastdry_depths_outliers_2_1 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "1") 

eastdry_depths_outliers_2_2 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "2") 


eastdry_depths_outliers_2_3 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "3") 


eastdry_depths_outliers_2_4 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "4") 

eastdry_depths_outliers_2_5 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "5") 

eastdry_depths_outliers_2_6 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "6") 


eastdry_depths_outliers_2_7 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "7") 


eastdry_depths_outliers_2_8 = 
  eastdry_depths %>% 
  filter(probe == "2" & sensor == "8") 


eastdry_2_1_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_1)   

eastdry_2_2_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_2)   

eastdry_2_3_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_3)   

eastdry_2_4_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_4)   

eastdry_2_5_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_5)   

eastdry_2_6_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_6)  

eastdry_2_7_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_7)   

eastdry_2_8_forcombine =
  datacleaning_function(eastdry_depths_outliers_2_8) 

eastdry_probe2_cleaned =
  eastdry_2_1_forcombine %>% 
  vctrs::vec_c(eastdry_2_2_forcombine, eastdry_2_3_forcombine,
               eastdry_2_4_forcombine, eastdry_2_5_forcombine,
               eastdry_2_6_forcombine, eastdry_2_7_forcombine,
               eastdry_2_8_forcombine)

#EAST DRY PROBE 3 SENSORS 1-8 

eastdry_depths_outliers_3_1 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "1") 

eastdry_depths_outliers_3_2 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "2") 


eastdry_depths_outliers_3_3 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "3") 


eastdry_depths_outliers_3_4 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "4") 

eastdry_depths_outliers_3_5 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "5") 

eastdry_depths_outliers_3_6 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "6") 


eastdry_depths_outliers_3_7 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "7") 


eastdry_depths_outliers_3_8 = 
  eastdry_depths %>% 
  filter(probe == "3" & sensor == "8") 


eastdry_3_1_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_1)   

eastdry_3_2_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_2)   

eastdry_3_3_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_3)   

eastdry_3_4_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_4)   

eastdry_3_5_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_5)   

eastdry_3_6_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_6)  

eastdry_3_7_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_7)   

eastdry_3_8_forcombine =
  datacleaning_function(eastdry_depths_outliers_3_8) 

eastdry_probe3_cleaned =
  eastdry_3_1_forcombine %>% 
  vctrs::vec_c(eastdry_3_2_forcombine, eastdry_3_3_forcombine,
               eastdry_3_4_forcombine, eastdry_3_5_forcombine,
               eastdry_3_6_forcombine, eastdry_3_7_forcombine,
               eastdry_3_8_forcombine)

