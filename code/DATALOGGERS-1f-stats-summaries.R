#ECROONEY
#September 1 2022

#combining redox data with sensor depths

#load all packages

source("code/0-packages.R")

#load data

combo_redox_withdepths = read.csv("processed/all_combine.csv")


#add depth bins

combo_redox_withdepths_bins =
  combo_redox_withdepths %>% 
  mutate(depth_bins = cut_width(depth_cm, width = 4, center=2)) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]","")) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[","")) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
  # now separate this into two different columns
  separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm")) %>% 
  mutate(depth_start_cm = as.integer(depth_start_cm),
         depth_stop_cm = as.integer(depth_stop_cm)) %>% 
  mutate(depth2 = depth_stop_cm - depth_start_cm)