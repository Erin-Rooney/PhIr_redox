#Input PhIr soil inventory 2021

##I need to relearn how to do this :(

source("code/0-packages.R")

#https://docs.google.com/spreadsheets/d/1pLe7wouMIRtZQqerVsaJj_y6XlZuj1sX/edit#gid=1590584967

soil_inventory = read.csv("raw/PhIr2021_Soil_InventoryUTKCores.csv") %>% 
  janitor::clean_names() %>% 
  rename(Sample_ID = sample_id)

realdepths = read.csv("raw/PhIr2021_Soil_Inventory_bd.csv") 

realdepths_forcombining =
  realdepths %>% 
  mutate(Area = recode(Area, "East" = "acidic tundra",
                       "West" = "non-acidic tundra")) %>% 
  mutate(volumetric_water_content_cm3_cm3 = soil_bulk_density_g_cm3 * grav_water_gh20_per_gdrysoil) %>% 
  dplyr::select(Sample_ID, real_depth_manualcalcs, real_depth_cm, volumetric_water_content_cm3_cm3) %>% 
  left_join(soil_inventory) 

write.csv(realdepths_forcombining, "processed/PhIr2021_Soil_InventoryUTKCores_foranalysis.csv")
