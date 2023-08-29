#EC Rooney

#Functions for data explainer


###

#REDOX---------------

redox2021_remove_frozenpoints_function = function(dat){
  
dat %>% 
    dplyr::mutate(keep = if_else(site == "non-acidic tundra" & position == "hydric" & depth_cm > 40 & Betterdate <= '2021-08-09 00:00', paste0("frozen"),"unfrozen")) %>% 
    filter(keep == "unfrozen") %>% 
    mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
    dplyr::select(TIMESTAMP, site, position, datalogger, probe, sensor, avg_values_fixed, Plot, depth_cm)
  
}
 
redox2022_remove_frozenpoints_function = function(dat){
  
dat %>% 
  dplyr::mutate(keep = if_else(site == "non-acidic tundra" & position == "dry" & depth_cm > 21 & Betterdate >= '2022-07-30 00:00', paste0("frozen"),"unfrozen")) %>% 
  filter(keep == "unfrozen") %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
    dplyr::select(TIMESTAMP, site, position, datalogger, probe, sensor, avg_values_fixed, Plot, depth_cm)
  
}


redox_2022_groupbytemporal_function = function(dat){
  
dat %>% 
    #filter(Betterdate >= '2021-07-01 00:00') %>% 
    mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
    mutate(site = recode(site, "east" = "acidic tundra",
                         "west" = "non-acidic tundra")) %>% 
    mutate(forsep = TIMESTAMP) %>% 
    separate(forsep, sep = " ", into = c("date", "time")) %>% 
    separate(date, sep = "/", into = c("month", "day", "year")) %>% 
    mutate(month = recode(month,  "6" = "early summer", 
                          "7" = "mid summer", 
                          "8" = "late summer",
                          "9" = "early fall")) %>% 
    group_by(site, position, depth_cm, month) %>% 
    dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
                     redox_sd = round(sd(avg_values_fixed),2)) %>% 
    mutate(year = 2022)
  
}


redox_2021_groupbytemporal_function = function(dat){
  
dat %>% 
  # filter(Betterdate >= '2021-07-01 00:00') %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  mutate(forsep = TIMESTAMP) %>% 
  separate(forsep, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "/", into = c("month", "day", "year")) %>% 
  mutate(month = recode(month,  "6" = "early summer", 
                        "7" = "mid summer", 
                        "8" = "late summer",
                        "9" = "early fall")) %>% 
  group_by(site, position, depth_cm, month) %>% 
  dplyr::summarise(redox_avg_mV = round(mean(avg_values_fixed),2),
                   redox_sd = round(sd(avg_values_fixed),2)) %>% 
    mutate(year = 2021)
  
}


#MOISTURE, TEMP, SALINITY---------

moisturetempsal_2021_cleaningfunction = function(dat){
  
dat %>% 
    mutate(forsep = TIMESTAMP) %>% 
    separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
    separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
    mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
    mutate(datetime = ymd_hm(paste(date, time)),
           date = ymd(date)) %>% 
    separate(forsep, sep = " ", into = c("date", "time")) %>% 
    separate(date, sep = "/", into = c("month", "day", "year")) %>% 
    mutate(month = recode(month,  "6" = "early summer", 
                          "7" = "mid summer", 
                          "8" = "late summer",
                          "9" = "early fall")) %>% 
    mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
    mutate(site = recode(site, "east" = "acidic tundra",
                         "west" = "non-acidic tundra")) %>% 
    dplyr::rename(depth_cm = depth) %>% 
    na.omit() %>% 
    mutate(frozen = case_when(temp < -1 ~ "frozen", TRUE ~ "unfrozen")) 
  
  
} 

moisturetempsal_2022_cleaningfunction = function(dat){
dat %>% 
  mutate(forsep = TIMESTAMP) %>% 
  separate(TIMESTAMP, sep = " ", into = c("date2", "time")) %>% 
  separate(date2, sep = "/", into =c('month', 'day', 'year')) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  mutate(datetime = ymd_hm(paste(date, time)),
         date = ymd(date)) %>% 
  separate(forsep, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "/", into = c("month", "day", "year")) %>% 
  mutate(month = recode(month,  "6" = "early summer", 
                        "7" = "mid summer", 
                        "8" = "late summer",
                        "9" = "early fall")) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  dplyr::rename(depth_cm = depth) %>% 
  na.omit() %>% 
  mutate(frozen = case_when(temp < -1 ~ "frozen", TRUE ~ "unfrozen")) 
}


moisturetempsal_2021_groupfunction = function(dat){
  dat %>% 
  filter(moisture > 1) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  group_by(site, position, depth_cm, month) %>% 
  dplyr::summarise(moisture_avg = round(mean(moisture),2),
                   moisture_sd = round(sd(moisture),2),
                   temp_avg = round(mean(temp),2),
                   temp_sd = round(sd(temp),2),
                   salinity_avg = round(mean(salinity),2),
                   salinity_sd = round(sd(salinity),2)
                   )
}

moisturetempsal_2022_groupfunction = function(dat){
  dat %>% 
  filter(moisture > 1) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>%
  mutate(site = recode(site, "east" = "acidic tundra",
                       "west" = "non-acidic tundra")) %>% 
  group_by(site, position, depth_cm, month) %>% 
  dplyr::summarise(moisture_avg = round(mean(moisture),2),
                   moisture_sd = round(sd(moisture),2),
                   temp_avg = round(mean(temp),2),
                   temp_sd = round(sd(temp),2),
                   salinity_avg = round(mean(salinity),2),
                   salinity_sd = round(sd(salinity),2))
}

#SOIL PROPERTIES-------------

thawdepths_2021_cleaningfunction = function(dat){
  
  dat %>% 
  dplyr::select(Date, Area, Site, Plot, Plot_ID, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10,
                X11, X12, X13, X14, X15) %>% 
  na.omit() %>% 
  pivot_longer(-c(Date, Area, Site, Plot, Plot_ID), names_to = "rep", values_to = "thaw_depth_cm") %>% 
  separate(Date, sep = "-", into = c("Day", "Month_num", "Year")) %>% 
  mutate(Month = recode(Month_num, "Jun" = "06", "Jul" = "07", "Aug" = "08")) %>% 
  mutate(date2 = as.Date(paste(Year,Month,Day, sep = "-"))) %>% 
  mutate(Area = recode(Area, "East" = "acidic tundra",
                       "West" = "non-acidic tundra")) %>% 
  mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) 
}

thawdepths_2022_cleaningfunction = function(dat){
  
  dat %>% 
    dplyr::select(Date, Area, Site, Plot, Plot_ID, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10,
                  X11, X12, X13, X14, X15) %>% 
    na.omit() %>% 
    pivot_longer(-c(Date, Area, Site, Plot, Plot_ID), names_to = "rep", values_to = "thaw_depth_cm") %>% 
    separate(Date, sep = "-", into = c("Day", "Month_num", "Year")) %>% 
    mutate(Year = recode(Year, "22" = "2022")) %>% 
    mutate(Month = recode(Month_num, "Jun" = "06", "Jul" = "07", "Aug" = "08")) %>% 
    mutate(date2 = as.Date(paste(Year,Month,Day, sep = "-"))) %>% 
    mutate(Area = recode(Area, "East" = "acidic tundra",
                         "West" = "non-acidic tundra")) %>% 
    mutate(Area = factor(Area, levels = c("non-acidic tundra", "acidic tundra"))) 
  
}




soil_properties_cleaningfunction = function(dat){

  dat %>% 
    mutate(Area = recode(Area, "East" = "acidic tundra",
                         "West" = "non-acidic tundra")) %>% 
    dplyr::mutate(soil_material = case_when(grepl("O",Horizon)~"organic",
                                            grepl("M",Horizon)~"mineral")) %>% 
    mutate(volumetric_water_content_cm3_cm3 = soil_bulk_density_g_cm3 * grav_water_gh20_per_gdrysoil)   %>% 
    dplyr::select(Sample_ID, Core_ID, Date_collected, Area, Site, Plot_num, Plot_ID, Horizon, soil_material, Average_Depth_cm, real_depth_cm, soil_bulk_density_g_cm3,
           grav_moist_perc, grav_water_gh20_per_gdrysoil, volumetric_water_content_cm3_cm3) %>% 
    rename(Average_thickness_cm = Average_Depth_cm)
  
}



soil_properties_groupingfunction = function(dat){

    dat %>% 
    mutate(label = Horizon) %>% 
    mutate(label = factor(label, levels = c("O", "O1", "O2", "O3", "M", "M1", "M2"))) %>% 
    mutate(date_simple = recode(Date_collected, "6-Jul-21" = "Plot-1", "7-Jul-21" = "Plot-1",
                                "13-Jul-21" = "Plot-2", "14-Jul-21" = "Plot-2",
                                "24-Jul-21" = "Plot-3",
                                "30-Jul-21" = "Plot-4", "31-Jul-21" = "Plot-4",
                                "7-Aug-21" = "Plot-5")) %>% 
    mutate(label = paste0(date_simple, "-", Plot_num)) %>% 
    filter(Site != "Transect") %>% 
    mutate(Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
    mutate(Horizon = factor(Horizon, levels = c("O", "O1", "O2", "O3", "M", "M1", "M2"))) %>% 
    dplyr::select(c(Area, Site, Horizon, soil_bulk_density_g_cm3, volumetric_water_content_cm3_cm3)) %>% 
    na.omit() %>% 
    group_by(Area, Site, Horizon) %>% 
    dplyr::summarise(mean_bd = round(mean(soil_bulk_density_g_cm3),2),
                     sd_bd = round(sd(soil_bulk_density_g_cm3),2),
                     mean_vwc = round(mean(volumetric_water_content_cm3_cm3),2),
                     sd_vwc = round(sd(volumetric_water_content_cm3_cm3),2)) %>% 
    dplyr::select(c(Area, Site, Horizon, mean_bd, sd_bd, mean_vwc, sd_vwc)) 
  
}
 

#Porewater data processing-------------

rhizon_cleaningfunction = function(dat){
dat %>% 
  filter(Site != 'Transect') %>% 
  mutate(ICP = recode(ICP, "Fe _ug/mL" = "Fe_ug/mL",
                      "K _ug/mL" = "K_ug/mL")) %>% 
  mutate(month = factor(month, levels = c("june", "july", "august")),
         Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
  mutate(concentration = recode(concentration, "<0.05" = "0.03",
                                "<0.1" = "0.05")) %>% 
  mutate(concentration = as.numeric(concentration)) 
}


rhizon_groupingfunction = function(dat){
  dat %>% 
    group_by(Area, Site, month, Betterdate, ICP) %>% 
    dplyr::summarise(mean = mean(concentration),
                     n = n(), 
                     sd = sd(concentration)/sqrt(n)) %>% 
    na.omit() %>% 
    mutate(combo = paste(Area, "-", Site)) %>% 
    mutate(Area = as.factor(Area)) %>% 
    mutate(month = factor(month, levels = c("june", "july", "august")),
           Site = factor(Site, levels = c("Dry", "Mesic", "Hydric"))) %>% 
    na.omit() %>% 
    mutate(area_site = paste(Area, Site, sep = "-")) 
  
  
  
}

sipper_groupingfunction = function(dat){
  
 dat %>% 
    group_by(Area, Site, Depth_cm, date, ICP) %>%
   dplyr::summarise(mean = round(mean(concentration), 3),
                    sd = round(sd(concentration),3)) %>% 
    separate(date, sep = "-", into =c('year', 'month', 'day')) %>% 
    mutate(month2 = recode(month, "06" = "June", "07" = "July", "08" = "August", "09" = "September")) %>% 
    mutate(date_plot = paste(month2, day, year, sep = "-")) %>% 
    mutate(date_plot = factor(date_plot, levels = c("June-27-2022", "June-29-2022", "July-06-2022",
                                                    "July-07-2022", "July-11-2022", "July-12-2022",
                                                    "July-18-2022", "July-19-2022", "July-25-2022",
                                                    "July-26-2022", "August-07-2022", "August-08-2022",
                                                    "September-15-2022","September-17-2022", "September-23-2022"))) %>% 
    mutate(area_site = paste(Area, Site, sep = "-")) 
}
