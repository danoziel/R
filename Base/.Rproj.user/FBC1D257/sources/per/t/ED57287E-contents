#total_land_cultivated----
cultivated_size<-land_TC_18 %>% filter(!is.na(total_land_cultivated),
                             season!="ANNAUL 2074-75") 

cultivated_size %>%
  summarise(mean = mean(total_land_cultivated),
            median=median(total_land_cultivated))

#size_land - group_by: season
tbl_cultivated_size18<-cultivated_size%>% group_by(season) %>%
  summarise(
    mean = mean(total_land_cultivated))
rm(cultivated_size)



#Irrigation ratio----
irrigation_ratio18<-land_TC_18 %>% filter(Ir_Retio<=1,season!="ANNAUL 2074-75")

irrigation_ratio18 %>% 
  summarise(count = n(),mean = mean(Ir_Retio))

#Irrigation ratio/season
irrigation_ratio18%>% group_by(season) %>%
  summarise(count = n(),mean = mean(Ir_Retio))

# cult_area_under_crop- main crops----
caucS<-Agriculture_Baseline_2018_ %>% filter(!is.na(cult_area_under_crop))%>%
  group_by(name_of_crop) %>% 
  summarise(count = n(),mean = mean(cult_area_under_crop))

caucS[c(31,26,37,35,17),]