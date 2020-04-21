
filter(!is.na(irri_for_season),name_of_crop %in% c("PADDY","WHEAT"))

rowwise() %>% # ignor NA in dplyr

drop_na(irri_for_season,season_of_crop)%>% library(tidyverse)
  
  
mutate(x= sub(NaN, NA, x)) %>% #replace NaN to Na, x=column
  
Data$TC[Data$TC == 2] <- 1 #replace 2 to 1

land_Treats <- subset(R.Lands_Baseline_2018_,  TC == 1)
colMeans(land_Treats)

Control_and_treatment_4_districts$TC[Control_and_treatment_4_districts$TC == 1] <- 0
Control_and_treatment_4_districts$TC[Control_and_treatment_4_districts$TC == 2] <- 1

mutate(avm_self = rowMeans(.[names(.)[7:8]], na.rm = T),#mean per row - by defined columns
       
#-----  frequency in percentage   ----    
R_Aquaculture_Baseline_2018_ %>% filter(!is.na(practice_aquaculture)) %>%
  group_by(practice_aquaculture,TreatmentControl) %>% mutate(freq = n / sum(n)) #Percentage per sub group

R_Aquaculture_Baseline_2018_ %>% filter(!is.na(practice_aquaculture)) %>%
  count(practice_aquaculture,TreatmentControl) %>%mutate(freq = n / sum(n))#Percentage per group

#   R_Aquaculture_Baseline_2018_ %>%filter(!is.na(practice_aquaculture)) %>%
#     group_by(practice_aquaculture)%>%summarise(n=n()) %>%mutate(freq = n / sum(n))
#                                 _||_
#                                 \  /
#                                  \/
R_Aquaculture_Baseline_2018_ %>%filter(!is.na(practice_aquaculture)) %>%
  count(practice_aquaculture) %>%mutate(freq = n / sum(n))

