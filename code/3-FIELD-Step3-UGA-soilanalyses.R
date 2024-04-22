#Soil Analyses from UGA


source("code/0-packages.R")

#load data

soil_dat = read.csv("raw/uga_soil_analyses_detectionlimit.csv") %>% janitor::clean_names()

soil_dat_sep =
  soil_dat %>% 
  separate(sample, sep = "-", into = c("date", "area", "site")) %>% 
  separate(site, sep = "_", into = c("siterep", "soilhorizon")) %>% 
  mutate(soilhorizon = recode(soilhorizon, "0" = "O",
                              "01" = "O1",
                              "02" = "O2")) %>% 
  dplyr::mutate(plotnum = case_when(grepl("1", siterep)~"1",
                                    grepl("2", siterep)~"2",
                                    grepl("3", siterep)~"3")) %>% 
  mutate(siterep = recode(siterep, "02" = "D2")) %>% 
  dplyr::mutate(site = case_when(grepl("D", siterep)~"dry",
                                 grepl("M", siterep)~"mesic",
                                  grepl("H", siterep)~"hydric")) %>% 
  dplyr::mutate(area = case_when(grepl("E", area)~"acidic tundra",
                                 grepl("W", area)~"non-acidic tundra")) %>% 
  mutate(area = factor(area, levels = c("non-acidic tundra", "acidic tundra"))) %>% 
  mutate(site = factor(site, levels = c("dry", "mesic", "hydric"))) 
  
soil_dat_individual = 
  soil_dat_sep
                                    
soil_dat_individual %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(soil_dat_individual, "output/soil_dat_individual.csv", row.names = FALSE)

  
soil_dat_summary =
  soil_dat_sep %>% 
  group_by(area, site, soilhorizon) %>% 
  dplyr::summarise(pH2_mean = round(mean(p_h_2),2),
                   pH2_se = round(sd(p_h_2)/sqrt(n()),2),
                   ca_mgkg_mean = round(mean(ca_mgkg),2),
                   ca_mgkg_se = round(sd(ca_mgkg)/sqrt(n()),2),
                   k_mgkg_mean = round(mean(k_mgkg),2),
                   k_mgkg_se = round(sd(k_mgkg)/sqrt(n()),2),
                   mg_mgkg_mean = round(mean(mg_mgkg),2),
                   mg_mgkg_se = round(sd(mg_mgkg)/sqrt(n()),2),
                   mn_mgkg_mean = round(mean(mn_mgkg),2),
                   mn_mgkg_se = round(sd(mn_mgkg)/sqrt(n()),2),
                   p_mgkg_mean = round(mean(p_mgkg),2),
                   p_mgkg_se = round(sd(p_mgkg)/sqrt(n()),2),
                   zn_mgkg_mean = round(mean(zn_mgkg),2),
                   zn_mgkg_se = round(sd(zn_mgkg)/sqrt(n()),2),
                   nh4_mgkg_mean = round(mean(nh4_n_mgkg),2),
                   nh4_mgkg_se = round(sd(nh4_n_mgkg)/sqrt(n()),2),
                   no3_mgkg_mean = round(mean(no3_n_mgkg),2),
                   no3_mgkg_se= round(sd(no3_n_mgkg)/sqrt(n()),2),
                   cpercmean = round(mean(cperc),2),
                   cpercse = round(sd(cperc)/sqrt(n()),2),
                   npercmean = round(mean(nperc),2),
                   npercse = round(sd(nperc)/sqrt(n()),2),
                   tocpercmean = round(mean(to_cperc),2),
                   tocpercse = round(sd(to_cperc)/sqrt(n()),2)) %>% 
  mutate(pH = paste(pH2_mean, "\u00b1", pH2_se),
        ca_mgkg = paste(ca_mgkg_mean, "\u00b1", ca_mgkg_se),
         k_mgkg = paste(k_mgkg_mean, "\u00b1", k_mgkg_se),
         mg_mgkg = paste(mg_mgkg_mean, "\u00b1", mg_mgkg_se),
         mn_mgkg = paste(mn_mgkg_mean, "\u00b1", mn_mgkg_se),
         p_mgkg = paste(p_mgkg_mean, "\u00b1", p_mgkg_se),
         zn_mgkg = paste(zn_mgkg_mean, "\u00b1", zn_mgkg_se),
         nh4_n_mgkg = paste(nh4_mgkg_mean, "\u00b1", nh4_mgkg_se),
         no3_n_mgkg = paste(no3_mgkg_mean, "\u00b1", no3_mgkg_se),
         cperc = paste(cpercmean, "\u00b1", cpercse),
         nperc = paste(npercmean, "\u00b1", npercse),
         to_cperc = paste(tocpercmean, "\u00b1", tocpercse)
         ) %>% 
  dplyr::select(-c(pH2_mean, pH2_se, ca_mgkg_mean, ca_mgkg_se, k_mgkg_mean, k_mgkg_se, mg_mgkg_mean,
                   mg_mgkg_se, mn_mgkg_mean, mn_mgkg_se, p_mgkg_mean, p_mgkg_se,
                   zn_mgkg_mean, zn_mgkg_se, nh4_mgkg_mean, nh4_mgkg_se, 
                   no3_mgkg_mean, no3_mgkg_se,
                   cpercmean, cpercse, npercmean, npercse, tocpercmean, tocpercse))
  
soil_dat_summary %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(soil_dat_summary, "output/soil_dat_summary.csv", row.names = FALSE)

library(ggpmisc)

nonacidic_P_Ca =
  soil_dat_sep %>% 
  filter(area %in% "non-acidic tundra") %>% 
  ggplot(aes(x = ca_mgkg, y = p_mgkg))+
  geom_point()+
  labs(title = "Non-Acidic")+
 # geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)+
  stat_poly_line() +
  stat_poly_eq() 

acidic_P_Ca =
soil_dat_sep %>% 
  filter(area %in% "acidic tundra") %>% 
  ggplot(aes(x = ca_mgkg, y = p_mgkg))+
  geom_point()+
  labs(title = "Acidic")+
  # geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)+
  stat_poly_line() +
  stat_poly_eq() 

ggsave("nonacidic_P_Ca.png", nonacidic_P_Ca, height = 5, width = 5)
ggsave("acidic_P_Ca.png", acidic_P_Ca, height = 5, width = 5)


P_summary =
  soil_dat_sep %>% 
  group_by(area, site) %>% 
  dplyr::summarise(p_mgkg_mean = round(mean(p_mgkg),2),
                   p_mgkg_sd = round(sd(p_mgkg)))

                   