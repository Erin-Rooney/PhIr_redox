#A sample ID list

#load all packages

source("code/0-packages.R")

#load data

A_samples = read.csv("raw/PhIr_2022_Asamples_IDlist.csv")

A_samples_merged = 
  A_samples %>% 
  mutate(year_id = paste0(year, "_", id, sep = "")) %>% 
  mutate(sampleID = paste0(proj, year_id, analysis, sep=""))

write.csv(A_samples_merged, "processed/A_samplelist_2022.csv")
