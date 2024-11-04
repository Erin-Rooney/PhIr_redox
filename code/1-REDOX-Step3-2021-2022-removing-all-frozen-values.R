source("code/0-packages.R")

#load data

combo_redox_withdepths_2022 = read.csv("processed/2022all_combine_temporary.csv")
combo_redox_withdepths_2021 = read.csv("processed/all_combine_temporary.csv")

#

frozen_2022 =
  combo_redox_withdepths_2022 %>% 
  dplyr::mutate(frozen1 = case_when(avg_values_fixed < 207 & avg_values_fixed > 187 ~ "frozen",
                                    avg_values_fixed >= 207 ~ "unfrozen",
                                    avg_values_fixed <= 187 ~ "unfrozen")) %>% 
  dplyr::mutate(frozen2 = case_when(Betterdate < "2022-07-01 00:00:00" ~ "frozen",
                                    Betterdate > "2022-09-15 00:00:00" ~ "frozen")) %>% 
  dplyr::mutate(frozen = if_else(frozen1 == "frozen" & frozen2 == "frozen",paste0("frozen"),"unfrozen")) %>%
  # dplyr::mutate(frozen = case_when(frozen1 == "frozen" & frozen2 == "frozen" ~ "Frozen",
  #                                  frozen1 == "frozen" & frozen2 == "unfrozen" ~ "FrozenZero",
  #                                  frozen1 == "unfrozen" & frozen2 == "frozen" ~ "FrozenTemp",
  #                                  frozen2 == "unfrozen" & frozen2 == "unfrozen" ~ "Unfrozen")) %>% 
  mutate(site = recode(site, "west" = "non-acidic tundra", "east" = "acidic tundra")) %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>% 
  mutate(depth_cm = as.numeric(depth_cm)) 


frozen_2021 =
  combo_redox_withdepths_2021 %>% 
  dplyr::mutate(frozen1 = case_when(avg_values_fixed < 207 & avg_values_fixed > 187 ~ "frozen",
                                    avg_values_fixed >= 207 ~ "unfrozen",
                                    avg_values_fixed <= 187 ~ "unfrozen")) %>% 
  dplyr::mutate(frozen2 = case_when(Betterdate < "2021-07-01 00:00:00" ~ "frozen",
                                    Betterdate > "2021-09-15 00:00:00" ~ "frozen")) %>% 
  # dplyr::mutate(frozen = case_when(frozen1 == "frozen" & frozen2 == "frozen" ~ "Frozen",
  #                                  frozen1 == "frozen" & frozen2 == "unfrozen" ~ "FrozenZero",
  #                                  frozen1 == "unfrozen" & frozen2 == "frozen" ~ "FrozenTemp",
  #                                  frozen2 == "unfrozen" & frozen2 == "unfrozen" ~ "Unfrozen")) %>% 
  dplyr::mutate(frozen = if_else(frozen1 == "frozen" & frozen2 == "frozen",paste0("frozen"),"unfrozen")) %>%
  mutate(site = recode(site, "west" = "non-acidic tundra", "east" = "acidic tundra")) %>% 
  mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>% 
  mutate(depth_cm = as.numeric(depth_cm)) 


write.csv(frozen_2021, "processed/allcombine_2021_frozen.csv")
write.csv(frozen_2022, "processed/allcombine_2022_frozen.csv")

test = read.csv("processed/allcombine_2022_frozen.csv")

# all_redox_data_fordatabase =
#   frozen_2021 %>% 
#   dplyr::select(TIMESTAMP, site, position, avg_values_fixed, Plot, depth_cm, frozen) %>% 
#   rbind(frozen_2021 %>% dplyr::select(TIMESTAMP, site, position, avg_values_fixed, Plot, depth_cm, frozen)) %>% 
#   rename(redox_mV = avg_values_fixed,
#          area = site,
#          site = position,
#          condition = frozen)
# 
# write.csv(all_redox_data_fordatabase, "processed/PhIr_redox_2021_2022.csv")

###

# frozen_2022 =
#   combo_redox_withdepths_2022 %>% 
#   dplyr::mutate(frozen1 = case_when(avg_values_fixed < 207 & avg_values_fixed > 187 ~ "frozenTemp",
#                                     avg_values_fixed >= 207 ~ "unfrozen",
#                                     avg_values_fixed <= 187 ~ "unfrozen", 
#                                     Betterdate < "2022-07-01 00:00:00" ~ "frozenZero",
#                                     Betterdate > "2022-09-15 00:00:00" ~ "frozenZero")) %>% 
#   #dplyr::mutate(frozen = if_else(frozen1 == "frozen" & frozen2 == "frozen",paste0("frozen"),"unfrozen")) %>% 
#   #dplyr::mutate(frozen = case_when(frozen1 == "frozen" & frozen2 == "frozen" ~ "frozen"))
#   mutate(site = recode(site, "west" = "non-acidic tundra", "east" = "acidic tundra")) %>% 
#   mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>% 
#   mutate(depth_cm = as.numeric(depth_cm)) 
#   
#   
#   
#   frozen_2021 =
#   combo_redox_withdepths_2021 %>% 
#     dplyr::mutate(frozen1 = case_when(avg_values_fixed < 207 & avg_values_fixed > 187 ~ "frozenTemp",
#                                       avg_values_fixed >= 207 ~ "unfrozen",
#                                       avg_values_fixed <= 187 ~ "unfrozen", 
#                                       Betterdate < "2022-07-01 00:00:00" ~ "frozenZero",
#                                       Betterdate > "2022-09-15 00:00:00" ~ "frozenZero")) %>% 
#   #dplyr::mutate(frozen = if_else(frozen1 == "frozen" & frozen2 == "frozen",paste0("frozen"),"unfrozen")) %>% 
#   mutate(site = recode(site, "west" = "non-acidic tundra", "east" = "acidic tundra")) %>% 
#   mutate(site = factor(site, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
#   mutate(position = factor(position, levels = c("dry", "mesic", "hydric"))) %>% 
#   mutate(depth_cm = as.numeric(depth_cm)) 
# 
# 
# write.csv(frozen_2021, "processed/allcombine_2021_frozen.csv")
# write.csv(frozen_2022, "processed/allcombine_2022_frozen.csv")
# 
# test = read.csv("processed/allcombine_2022_frozen.csv")
