#####
#  eastdry_depths_probe1 =
#   eastdry_depths %>%
#   mutate(sensor = recode(sensor, "1" = "S1", "2" = "S2", "3" = "S3", "4" = "S4")) %>% 
#   filter(probe == 1) %>%
#   dplyr::select(-c("depth_cm", 'RECORD', 'avg_values')) %>% 
#   pivot_wider(names_from = 'sensor', values_from = 'avg_values_fixed') %>%
#   mutate(lagS1 = lag(S1)-S1,
#           leadS1 = lead(S1)-S1,
#          lagS2 = lag(S2)-S2,
#          leadS2 = lead(S2)-S2,
#          lagS3 = lag(S3)-S3,
#          leadS3 = lead(S3)-S3,
#          lagS4 = lag(S4)-S4,
#          leadS4 = lead(S4)-S4,
#          ) %>% 
#    mutate(laglead = case_when(lagS1 > 200 & leadS1 < -200 ~ 'artifact',
#                               lagS1 < -200 & leadS1 > 200 ~ 'artifact',
#                               lagS2 > 200 & leadS2 < -200 ~ 'artifact',
#                               lagS2 < -200 & leadS2 > 200 ~ 'artifact',
#                               lagS3 > 200 & leadS3 < -200 ~ 'artifact',
#                               lagS3 < -200 & leadS3 > 200 ~ 'artifact',
#                               lagS4 > 200 & leadS4 < -200 ~ 'artifact',
#                               lagS4 < -200 & leadS4 > 200 ~ 'artifact'))
#  
#   mutate(checknum = row_number())
# 
# eastdry_depths_probe1_dataonly =
#   eastdry_depths_probe1 %>% 
#   dplyr::select(-c("lagS1", 'leadS1', 'lagS2', "leadS2",
#                    "lagS3", "leadS3", 'lagS4', "leadS4")) %>% 
#   pivot_longer(-c("TIMESTAMP", "site", "position", "Betterdate",
#                   "datalogger", "probe", "Plot"),
#                names_to = "sensor", values_to = "avg_values_fixed") %>% 
#   mutate(sensor = recode(sensor, "S1" = 1, "S2" = 2, "S3" = 3, "S4" = 4)) %>% 
#   na.omit() %>% 
#   left_join(eastdry_depths)


###currently lag/lead are being impossible. 

# eastdry_depths_probe1_artifacts =
#   eastdry_depths_probe1 %>% 
#   dplyr::select(-c("S1", 'S2', 'S3', "S4")) %>% 
#   # pivot_longer(-c("TIMESTAMP", "site", "position", "Betterdate",
#   #                 "datalogger", "probe", "Plot"),
#   #              names_to = "Lags", values_to = "") %>% 
#      mutate(laglead = case_when(lagS1 > 200 & leadS1 < -200 ~ 'artifact',
#                                 lagS1 < -200 & leadS1 > 200 ~ 'artifact',
#                                 lagS2 > 200 & leadS2 < -200 ~ 'artifact',
#                                 lagS2 < -200 & leadS2 > 200 ~ 'artifact',
#                                 lagS3 > 200 & leadS3 < -200 ~ 'artifact',
#                                 lagS3 < -200 & leadS3 > 200 ~ 'artifact',
#                                 lagS4 > 200 & leadS4 < -200 ~ 'artifact',
#                                 lagS4 < -200 & leadS4 > 200 ~ 'artifact'))



#summarise so that all plots are combined


alldata_cleaned %>% 
  group_by(site, position, depth_cm, Betterdate) %>%
  dplyr::summarize(avg_values_summarised = mean(avg_values_fixed),
                   std_values_summarised = sd(avg_values_fixed)/sqrt(n())) %>%
  mutate(depth_cm = as.numeric(depth_cm)) %>% 
  
  
  #without summarising
  
  depths_function_nosummary <- function(dat){
    dat %>% 
      mutate(redox_avg = recode(redox_avg, 'redox_5.2.3_fromtip_Avg' = "redox_5_2_3_fromtip_Avg")) %>% 
      mutate(redox_avg = str_remove(redox_avg, "Redox_")) %>% 
      mutate(redox_avg = str_remove(redox_avg, "redox_")) %>% 
      mutate(redox_avg = str_remove(redox_avg, "_fromtip_Avg")) %>% 
      separate(redox_avg, sep = "_", into = c("datalogger", "probe_sensor")) %>%
      filter(datalogger != "NUM") %>% 
      dplyr::select(-redox_NUM_Avg) %>% 
      mutate(probe_sensor = recode(probe_sensor, "1.8" = "1_8", "1.7" = "1_7", "1.6" = "1_6", "1.5" = "1_5", "1.4" = "1_4", "1.3" = "1_3", "1.2" = "1_2", "1.1" = "1_1",
                                   "2.8" = "2_8", "2.7" = "2_7", "2.6" = "2_6", "2.5" = "2_5", "2.4" = "2_4", "2.3" = "2_3", "2.2" = "2_2", "2.1" = "2_1",
                                   "3.8" = "3_8", "3.7" = "3_7", "3.6" = "3_6", "3.5" = "3_5", "3.4" = "3_4", "3.3" = "3_3", "3.2" = "3_2", "3.1" = "3_1")) %>% 
      separate(probe_sensor, sep = "_", into = c("probe", "sensor")) %>% 
      mutate(sensor = as.numeric(sensor)) %>%
      left_join(sensor_depths) %>% 
      #add 197 to all redox potentials to report data relative to the standard hydrogen electrode
      dplyr::mutate(avg_values_fixed = avg_values + 197) %>% 
      mutate(depth_cm = as.numeric(depth_cm)) %>% 
      force()
    
    
  }
  #run on all six dataframes to get site, position, depth, date, avg, std
  
  
  ##no summarise
  
  westhydric_depths_nosummary = 
    depths_function_nosummary(westhydric_avg)%>% 
    na.omit()
  
  easthydric_depths_nosummary = 
    depths_function_nosummary(easthydric_avg) %>% 
    na.omit()
  
  westmesic_depths_nosummary = 
    depths_function_nosummary(westmesic_avg)%>% 
    na.omit()
  
  eastmesic_depths_nosummary = 
    depths_function_nosummary(eastmesic_avg)%>% 
    na.omit()
  
  westdry_depths_nosummary = 
    depths_function_nosummary(westdry_avg)%>% 
    na.omit()
  
  eastdry_depths_nosummary = 
    depths_function_nosummary(eastdry_avg) %>% 
    na.omit()
  
  
  ##summarise
  
  #change names since I copy and pasted these to go above for the data cleaning.
  
  westhydric_depths = 
    depths_function(westhydric_avg)%>% 
    na.omit()
  
  easthydric_depths = 
    depths_function(easthydric_avg) %>% 
    na.omit()
  
  westmesic_depths = 
    depths_function(westmesic_avg)%>% 
    na.omit()
  
  eastmesic_depths = 
    depths_function(eastmesic_avg)%>% 
    na.omit()
  
  westdry_depths = 
    depths_function(westdry_avg)%>% 
    na.omit()
  
  eastdry_depths = 
    depths_function(eastdry_avg) %>% 
    na.omit()
  
  
  #####
  
  all_combine_depthbins = 
    westhydric_depths %>% 
    bind_rows(easthydric_depths, westmesic_depths, eastmesic_depths, westdry_depths, eastdry_depths) %>% 
    #mutate(depth_bins = case_when(depth_cm <= 100 ~ cut_width(depth_cm, width = 1, center = 1))) %>% 
    mutate(depth_bins = cut_width(depth_cm, width = 5, center=2.5)) %>% 
    mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]","")) %>% 
    mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[","")) %>% 
    mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
    # now separate this into two different columns
    separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm")) %>% 
    mutate(depth_start_cm = as.integer(depth_start_cm),
           depth_stop_cm = as.integer(depth_stop_cm)) %>% 
    mutate(depth2 = depth_stop_cm - depth_start_cm)
  
  
  all_combine = 
    westhydric_depths %>% 
    bind_rows(easthydric_depths, westmesic_depths, eastmesic_depths, westdry_depths, eastdry_depths)  
  
  #write csv
  
  write.csv(all_combine, "processed/all_combine.csv")
  
  write.csv(all_combine_depthbins, "processed/all_combine_depthbins.csv")
  
  
  #######no summarise
  
  all_combine_depthbins_nosummmary = 
    westhydric_depths_nosummary %>% 
    bind_rows(easthydric_depths_nosummary, westmesic_depths_nosummary, eastmesic_depths_nosummary, 
              westdry_depths_nosummary, eastdry_depths_nosummary) %>% 
    #mutate(depth_bins = case_when(depth_cm <= 100 ~ cut_width(depth_cm, width = 1, center = 1))) %>% 
    mutate(depth_bins = cut_width(depth_cm, width = 5, center=2.5)) %>% 
    mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]","")) %>% 
    mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[","")) %>% 
    mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
    # now separate this into two different columns
    separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm")) %>% 
    mutate(depth_start_cm = as.integer(depth_start_cm),
           depth_stop_cm = as.integer(depth_stop_cm)) %>% 
    mutate(depth2 = depth_stop_cm - depth_start_cm)
  
  
  all_combine_nosummary = 
    westhydric_depths_nosummary %>% 
    bind_rows(easthydric_depths_nosummary, westmesic_depths_nosummary, 
              eastmesic_depths_nosummary, westdry_depths_nosummary, eastdry_depths_nosummary)  
  
  
  write.csv(all_combine_nosummary, "processed/all_combine_nosummary.csv")
  
  write.csv(all_combine_depthbins_nosummmary, "processed/all_combine_depthbins_nosummary.csv")
  