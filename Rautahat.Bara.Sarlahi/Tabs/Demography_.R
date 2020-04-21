# SECTION 3 - HOUSING AND ASSETS											

# family members----
Demography_Baseline_2018_$household_questionnaire_id
hh<-table(Demography_Baseline_2018_$household_questionnaire_id)
mean(hh)

# caste----
Demography_2_Baseline_2018_$which_is_the_caste_of_you_househ
table(Demography_2_Baseline_2018_$which_is_the_caste_of_you_househ)
# 2 Tharu # 5 Yadavs  #


# [3.5] Apart from this main house, how many houses do you own?----
aft<-table(Demography_2_Baseline_2018_$apart_from_this_main_house__h)
prop.table(aft)

#[3.6] Apart from this main house, how many houses do you own?
wit<-table(Demography_2_Baseline_2018_$what_is_the_main_source_of_wa)
prop.table(wit)
# 1 Tap/Piped # 2 Tubewell/hand pump #

# source of energy for lighting----
sol<-table(Demography_2_Baseline_2018_$hh_source_of_lighting)
prop.table(sol)
# 3 Electricity #

#How many of the following items are owned by the household members?----

table(Demography_2_Baseline_2018_$plough) #3.9b Iron Plough
table(Demography_2_Baseline_2018_$power_tiller_tractor)#3.9b Power tiller/tractor
table(Demography_2_Baseline_2018_$threshing_machine)
table(Demography_2_Baseline_2018_$rambha) #Threshing & Husking Machine
table(Demography_2_Baseline_2018_$bullock)
table(Demography_2_Baseline_2018_$motorcycle)
# [3.9a] How many of the following items are owned by the household for irrigation?
# [7]
R_Demography_2_Baseline_2018_%>%select(1,37,35,38,39,41,40,63)%>%filter(borewell!=0)

# [2]         
R_Demography_2_Baseline_2018_%>%select(1,37,35,38,39,41,40,63)%>%filter(electric_pump!=0)


# FUEL - diesel_pump + cd_pump_vuplator
aa <- R_Demography_2_Baseline_2018_ %>% select(1,37,35,38,39,41,40,63) %>%
  filter(diesel_pump!=0 | cd_pump_vuplator!=0)


#[Ram] how many of those who have or do not have borewell irrigate their crops (by season)?----

bore_wells_ow <- R_Demography_2_Baseline_2018_%>%select(1,35,63)
bore_wells_ow$borewell[bore_wells_ow$borewell>0] <- 1 #replace 2 to 1

irrigated_land_cult <- R.Lands_Baseline_2018_ %>% select(1,2,8) 

library(tidyverse)
borewell_and_irrigated_land <- inner_join(irrigated_land_cult,bore_wells_ow)%>%
  drop_na(irrigated_out_of_tot_land_cult)

borewell_and_irrigated_land %>% group_by(borewell,season) %>%filter(irrigated_out_of_tot_land_cult>0) %>%
  summarise(n(),mean(irrigated_out_of_tot_land_cult),n()/137)

table(bore_wells_ow$borewell)

borewell_and_irrigated_land %>% group_by(season) %>%
  filter(borewell==1,irrigated_out_of_tot_land_cult>0) %>%
  summarise(n(),mean(irrigated_out_of_tot_land_cult),n()/94)

borewell_and_irrigated_land %>% group_by(season) %>%
  filter(borewell==0,irrigated_out_of_tot_land_cult>0) %>%
  summarise(n(),mean(irrigated_out_of_tot_land_cult),n()/43)

# [Ram]how many own some kind of pump (electric or diesel)?----
# what percent of each group irrigate any crop by season?

R_Demography_2_Baseline_2018_ %>% select(1,37,35,38,39,41,40,63) %>%
  filter(diesel_pump!=0 | cd_pump_vuplator!=0 |electric_pump!=0) %>%
  tally() #103HH : Pump owners - of any kind

R_Demography_2_Baseline_2018_ %>% select(1,38,39,41,63) %>%
  filter(diesel_pump!=0 | cd_pump_vuplator!=0) #98HH : diesel/cd pump

R_Demography_2_Baseline_2018_ %>% select(1,38,39,41,63) %>%
  filter(electric_pump!=0) #21HH : electric pump

bore_wells_ow <- R_Demography_2_Baseline_2018_%>%select(1,35,63)
bore_wells_ow$borewell[bore_wells_ow$borewell>0] <- 1 #replace 2 to 1

irrigated_land_cult <- R.Lands_Baseline_2018_ %>% select(1,2,8) 





