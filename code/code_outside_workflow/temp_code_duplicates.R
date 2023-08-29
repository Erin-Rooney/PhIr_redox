#get rid of duplicates

# calculate n to see which timestamps were duplicates 

duplicatedata <- function(dat){
  dat %>% 
    group_by(TIMESTAMP) %>% 
    dplyr::mutate(n = n()) %>% 
    filter(n == 2) %>% 
    
    force()
  
  
}




easthydric_dlname = 
  easthydric_dlraw %>%
  group_by(TIMESTAMP) %>% 
  dplyr::mutate(n = n()) %>% 
  filter(n == 2)


westmesic_dlname = 
  westmesic_dlraw %>% 
  group_by(TIMESTAMP) %>% 
  dplyr::mutate(n = n()) %>% 
  filter(n == 2) %>% 
  group_by(TIMESTAMP)


dplyr::summarise(sd = sd(soilmoisture5cm_Avg)/sqrt(n)) 

westdry_dlname = 
  westdry_dlraw %>% 
  group_by(TIMESTAMP) %>% 
  dplyr::mutate(n = n()) %>% 
  filter(n == 2) %>% 
  dplyr::summarise(sd = sd(soilmoisture5cm_Avg)/sqrt(n))