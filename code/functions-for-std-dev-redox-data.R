#EC Rooney

#Functions for std-dev-redox-data


###

#REDOX---------------

std_separate = function(dat){
  dat %>% 
    #create the "label" column. It needs to start with redox and end with avg in order to be
    #selected in the dplyr::select arguments below. If your data has a different labeling pattern
    #adjust the starts_with and ends_with arguments and the name of the X column
    mutate(redox_NUM_Std = X) %>%
    #we made a new column, so X is still present. The below is a little unnecessary since 
    #X will be deselected after dplyr::select, but I don't want it floating around
    dplyr::select(-X) %>% 
    dplyr::select(starts_with('redox')) %>% 
    dplyr::select(ends_with('Std')) %>%
    #if needed, get rid of NA rows then columns. Not needed.
    
    force()
}

avg_separate = function(dat){
  dat %>% 
    #create the "label" column. It needs to start with redox and end with avg in order to be
    #selected in the dplyr::select arguments below. If your data has a different labeling pattern
    #adjust the starts_with and ends_with arguments and the name of the X column
    mutate(redox_NUM_Avg = X) %>%
    #we made a new column, so X is still present. The below is a little unnecessary since 
    #X will be deselected after dplyr::select, but I don't want it floating around
    dplyr::select(-X) %>% 
    dplyr::select(starts_with('redox')) %>% 
    dplyr::select(ends_with('Avg')) %>%
    #if needed, get rid of NA rows then columns. Not needed.
    
    force()
}

depths_function = function(dat){
  dat  %>% 
    mutate(redox_depth = recode(redox_depth, 'redox_5.2.3_fromtip' = "redox_5_2_3_fromtip")) %>% 
    mutate(redox_depth = str_remove(redox_depth, "Redox_")) %>% 
    mutate(redox_depth = str_remove(redox_depth, "redox_")) %>% 
    mutate(redox_depth = str_remove(redox_depth, "_fromtip")) %>% 
    separate(redox_depth, sep = "_", into = c("datalogger", "probe_sensor")) %>%
    filter(datalogger != "NUM") %>% 
    mutate(probe_sensor = recode(probe_sensor, "1.8" = "1_8", "1.7" = "1_7", "1.6" = "1_6", "1.5" = "1_5", "1.4" = "1_4", "1.3" = "1_3", "1.2" = "1_2", "1.1" = "1_1",
                                 "2.8" = "2_8", "2.7" = "2_7", "2.6" = "2_6", "2.5" = "2_5", "2.4" = "2_4", "2.3" = "2_3", "2.2" = "2_2", "2.1" = "2_1",
                                 "3.8" = "3_8", "3.7" = "3_7", "3.6" = "3_6", "3.5" = "3_5", "3.4" = "3_4", "3.3" = "3_3", "3.2" = "3_2", "3.1" = "3_1")) %>% 
    separate(probe_sensor, sep = "_", into = c("probe", "sensor")) %>% 
    mutate(sensor = as.numeric(sensor)) %>%
    left_join(sensor_depths) %>% 
    
    
    force()
  
  
}


datacleaning_function = function(dat){
  dat %>% 
    group_by(probe, sensor) %>% 
    mutate(lag = lag(avg_values_fixed)-avg_values_fixed,
           lead = lead(avg_values_fixed)-avg_values_fixed) %>% 
    mutate(laglead = case_when(lag > 25 & lead < -25 ~ 'artifact',
                               lag < -25 & lead > 25 ~ 'artifact',
                               lag > 25 & lead > 25 ~ 'artifact',
                               lag < -25 & lead < -25 ~ 'artifact'),
           laglead = if_else(is.na(laglead), "keep", laglead)) %>% 
    filter(laglead == "keep") %>% 
    filter(avg_values_fixed < 800) %>% 
    filter(avg_values_fixed > -250) %>% 
    
    force()
  
}



