###-------------------------------------------------------###
###     befor and after - treatment group B&A_T           ###
###-------------------------------------------------------###
Treats_Lands <- subset(R.Lands_Endline_EPC_2019_,  TC == 1)
w <- Treats_Lands %>% group_by(household_questionnaire_id) %>% tally()

# R_Lands_Baseline_2018_ \/ R_Lands_Endline_EPC_2019_    ----
#----------------------------------------------------    

# total_ownland_cultivated
# total_land_cultivated
# irrigated_out_of_tot_land_cult
# crop_intensity
# irrigate_intensity

x <- R_Lands_Baseline_2018_ %>% 
  filter(total_land_cultivated_year>0,!is.na(irrigated_out_of_tot_land_cult)) %>%
  mutate(ci=total_land_cultivated/nca,ii=irrigated_out_of_tot_land_cult/nca) %>%
  group_by(TreatmentControl, household_questionnaire_id) %>%
  summarise(own=sum(total_ownland_cultivated,na.rm = T)*0.0338,
            cult=sum(total_land_cultivated)*0.0338,
            irrigated=sum(irrigated_out_of_tot_land_cult)*0.0338,
            crop_intensity=sum(ci),irrigate_intensity=sum(ii)) %>% 
  group_by(TreatmentControl) %>%
  summarise(n(), mean(own),mean(cult),mean(irrigated),
            mean(crop_intensity),mean(irrigate_intensity)) 

x <- R_Lands_Endline_EPC_2019_ %>% 
  filter(total_land_cultivated_year>0,!is.na(irrigated_out_of_tot_land_cult)) %>%
  mutate(ci=total_land_cultivated/nca,ii=irrigated_out_of_tot_land_cult/nca) %>%
  group_by(TreatmentControl, household_questionnaire_id) %>%
  summarise(own=sum(total_ownland_cultivated,na.rm = T)*0.0338,
            cult=sum(total_land_cultivated)*0.0338,
            irrigated=sum(irrigated_out_of_tot_land_cult)*0.0338,
            crop_intensity=sum(ci),irrigate_intensity=sum(ii)) %>% 
  group_by(TreatmentControl) %>%
  summarise(n(), mean(own),mean(cult),mean(irrigated),
            mean(crop_intensity),mean(irrigate_intensity)) 


# R.Agriculture_Baseline_2018_ vs R.Agriculture_Endline_EPC_2019_----
#--------------------------------------------------------------------
#irrigation- IN HOURS: n=HH 
#time taken to irrigate the cultivated area.A single crop season IN HOURS
Treats <- subset(R.Agriculture_Endline_EPC_2019_,  TC == 1)

# HH level in a year
x <- R_Agriculture_Baseline_2018_ %>%
  group_by(TreatmentControl, household_questionnaire_id) %>% 
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season)) %>% 
  summarise(n(),mean(hr_per_ha,na.rm = T),mean(irrigate_hr,na.rm = T))

x <- R_Agriculture_Endline_EPC_2019_ %>%
  group_by(TreatmentControl, household_questionnaire_id) %>% 
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season)) %>% 
  summarise(n(),mean(hr_per_ha,na.rm = T),mean(irrigate_hr,na.rm = T))

# season
cc <- R_Agriculture_Baseline_2018_ %>% 
  group_by(household_questionnaire_id,season_of_crop) %>% 
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season)) %>% 
  group_by(season_of_crop) %>%
  summarise(n=n(), mean(irrigate_hr),mean(hr_per_ha,na.rm = T),
            p_HH=n/26)

tc <- R.Agriculture_Endline_EPC_2019_ %>% filter(TC==1) %>%   
  drop_na(irri_for_season,season_of_crop)%>% 
  group_by(household_questionnaire_id,season_of_crop) %>% 
  summarise(sum_ir=sum(irri_for_season),mean_hr=mean(hrs_irr_1katha)) %>% 
  group_by(season_of_crop) %>%
  summarise(n.HH=n(), mean(sum_ir),mean(mean_hr),mints=mean(mean_hr)*60,p_HH=n.HH/21)


# most_imp_source ----
R.Agriculture_Baseline_2018_ %>%filter(TC==1) %>%  
  select(1,8,17) %>% 
  filter(most_imp_source>0) %>% 
  group_by(household_questionnaire_id,most_imp_source) %>% 
  summarise(n()) %>% 
  group_by(most_imp_source) %>% 
  summarise(n() ,p_HH=n()/26)

R.Agriculture_Endline_EPC_2019_ %>% filter(TC==1) %>% 
  select(1,8,17) %>% 
  filter(most_imp_source>0) %>% 
  group_by(household_questionnaire_id,most_imp_source) %>% 
  summarise(n()) %>% 
  group_by(most_imp_source) %>% 
  summarise(n() ,p_HH=n()/21)


R.Lands_Endline_EPC_2019_ %>% filter(TC==1) %>% group_by(household_questionnaire_id) %>% tally()

# frequency HH grow type of crop----

# year
R.Agriculture_Baseline_2018_ %>% filter( TC==1) %>% group_by(household_questionnaire_id,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>% group_by(name_of_crop) %>%
  summarise(no.HH=n(),no.HH/26)

R.Agriculture_Endline_EPC_2019_ %>% filter(!is.na(cult_area_under_crop),TC==1) %>%
  group_by(household_questionnaire_id,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>% group_by(name_of_crop) %>%
  summarise(no.HH=n(),no.HH/21)

# crop
R.Agriculture_Baseline_2018_ %>%  filter(TC==1) %>%  
  group_by(household_questionnaire_id,season_of_crop,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(season_of_crop ,name_of_crop) %>% 
  summarise(no.HH=n(),no.HH/26)

R.Agriculture_Endline_EPC_2019_ %>% filter(!is.na(cult_area_under_crop),TC==1) %>%
  group_by(household_questionnaire_id,season_of_crop,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(season_of_crop ,name_of_crop) %>% 
  summarise(no.HH=n(),no.HH/21)


# How much of the cultivated area was under this crop: mean & %

#Monsoon  Winter  Summer  Annual 
#   1       2       3       4 

#season
R.Agriculture_Baseline_2018_ %>% filter(TC==1) %>% 
  group_by(household_questionnaire_id,season_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(season_of_crop) %>% 
  summarise(mean = mean(sum_cult_area),
            per=sum(sum_cult_area)/
              sum(TreatsB_agri$cult_area_under_crop,na.rm = T))

R.Agriculture_Endline_EPC_2019_ %>% filter(TC==1) %>% 
  group_by(household_questionnaire_id,season_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(season_of_crop) %>% 
  summarise(mean = mean(sum_cult_area),
            per=sum(sum_cult_area)/
              sum(TreatsE_agri$cult_area_under_crop,na.rm = T),n())

# crop
R.Agriculture_Baseline_2018_ %>% filter(TC==1) %>% 
  group_by(household_questionnaire_id,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(name_of_crop) %>% 
  summarise(mean = mean(sum_cult_area),
            per= sum(sum_cult_area)/
              sum(TreatsB_agri$cult_area_under_crop,na.rm = T))

R.Agriculture_Endline_EPC_2019_ %>% filter(TC==1) %>% 
  group_by(household_questionnaire_id,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(name_of_crop) %>% 
  summarise(mean = mean(sum_cult_area),
            per= sum(sum_cult_area)/
              sum(TreatsE_agri$cult_area_under_crop,na.rm = T))

# SECTION 7 - PROCUREMENT: ELECTRICITY, FUEL----

# [7.12] Did you buy fuel for the pump?
R_Procurement_Baseline_2018_ %>% filter(TC==1) %>%
  group_by(do_you_buy_fuel_for_the_pump) %>% tally()

R_Procurement_Endline_EPC_2019_ %>% filter(TC==1) %>%
  group_by(do_you_buy_fuel_for_the_pump) %>% tally()

# [7.13] If yes, which fuel is predominantly used for your pump?
table(R_Procurement_Baseline_2018_$if_yes__which_fuel_do_you_use)
#     1 Diesel  # 2 Kerosene #
R_Procurement_Baseline_2018_ %>% filter(TC==1) %>%
  group_by(if_yes__which_fuel_do_you_use) %>% tally()

R_Procurement_Endline_EPC_2019_ %>% filter(TC==1) %>%
  group_by(if_yes__which_fuel_do_you_use) %>% tally()



# [7.16] Total litres of diesel/kerosene consumed for agriculture pumps
aa <- R_Procurement_Baseline_2018_ %>% filter(!is.na(total_litres_consumed_dieselkero),TC==1) %>% 
  summarise(mean(total_litres_consumed_dieselkero),n())

aa <- R_Procurement_Endline_EPC_2019_ %>% filter(!is.na(total_litres_consumed_dieselkero),TC==1) %>% 
  summarise(mean(total_litres_consumed_dieselkero),n())


aa <- R_Demography_2_Baseline_2018_%>%select(1,37,35,38,39,41,40,63)


# Aquaculture----
R_Aquaculture_Endline_EPC_2019_ <- inner_join(Aquaculture_Endline_EPC_2019_,Control_and_treatment_4_districts)

# [11.1] Do you practice aquaculture/fish farming? # 1 Yes # 2 No #
a <- R_Aquaculture_Baseline_2018_ %>% filter(!is.na(practice_aquaculture),TC==1) %>% 
  count(practice_aquaculture) %>% mutate(freq = n / sum(n))

a <- R_Aquaculture_Endline_EPC_2019_ %>% filter(!is.na(practice_aquaculture),TC==1) %>% 
  count(practice_aquaculture) %>% mutate(freq = n / sum(n))

# [11.3]What is the total area of the pond? (in kathas)
a <- R_Aquaculture_Baseline_2018_ %>% filter(!is.na(total_area_of_pond),total_area_of_pond!=400,TC==1) %>% 
  summarise(n(),mean(total_area_of_pond))

a <- R_Aquaculture_Endline_EPC_2019_ %>% filter(!is.na(total_area_of_pond),total_area_of_pond!=300,TC==1) %>% 
  summarise(n(),mean(total_area_of_pond))

#[11.4] Do all ponds have sufficient water for aquaculture throughout the year?
# 1 Yes # 2 No #

a <- R_Aquaculture_Baseline_2018_ %>%
  filter(!is.na(suff_water_aquaculture),total_area_of_pond!=400,TC==1) %>%
  group_by(suff_water_aquaculture)%>% 
  summarise(n=n(),mean(total_area_of_pond))%>% mutate(freq = n / sum(n)) 

a <- R_Aquaculture_Endline_EPC_2019_ %>%
  filter(!is.na(suff_water_aquaculture),total_area_of_pond!=300,TC==1) %>%
  group_by(suff_water_aquaculture)%>% 
  summarise(n=n(),mean(total_area_of_pond))%>% mutate(freq = n / sum(n)) 

#[11.5] Is a WEM already in use for filling and draining the ponds?

a <- R_Aquaculture_Baseline_2018_ %>%
  filter(!is.na(wem_used_forpond),total_area_of_pond!=400,TC==1) %>% 
  group_by(wem_used_forpond)%>% 
  summarise(n=n(),mean(total_area_of_pond))%>% mutate(freq = n / sum(n)) 

a <- R_Aquaculture_Endline_EPC_2019_ %>%
  filter(!is.na(wem_used_forpond),total_area_of_pond!=300,TC==1) %>% 
  group_by(wem_used_forpond)%>% 
  summarise(n=n(),mean(total_area_of_pond))%>% mutate(freq = n / sum(n)) 


#Are you planning to use the SPIP for aquaculture?
a <- R_Aquaculture_Baseline_2018_ %>%
  filter(practice_aquaculture==1,!is.na(planning_spip_aqua),total_area_of_pond!=400,TC==1) %>%
  count(planning_spip_aqua)%>% mutate(freq = n / sum(n)) 

a <- R_Aquaculture_Endline_EPC_2019_ %>%
  filter(practice_aquaculture==1,!is.na(planning_spip_aqua),total_area_of_pond!=300,TC==1) %>%
  count(planning_spip_aqua)%>% mutate(freq = n / sum(n)) 


# [11.6]If yes, what type of WEM is used for filling or draining pond for aquaculture? 
a <- R_Aquaculture_Baseline_2018_ %>% filter(!is.na(type_wem_aquaculture),TC==1) %>% 
  count(type_wem_aquaculture)%>% mutate(freq = n / sum(n))
# no HH on this one
R_Aquaculture_Endline_EPC_2019_ %>% filter(!is.na(type_wem_aquaculture),TC==1) %>% 
  count(type_wem_aquaculture)%>% mutate(freq = n / sum(n))






# [3.9a] How many of the following items are owned by the household for irrigation?----
R_Demography_2_Endline_EPC_2019_ <- inner_join(Demography_2_Endline_EPC_2019_,Control_and_treatment_4_districts)



# [7]
R_Demography_2_Baseline_2018_%>%select(1,37,35,38,39,41,40,63)%>%filter(borewell>0,TC==1)
R_Demography_2_Endline_EPC_2019_%>%select(1,37,35,38,39,41,40,63)%>%filter(borewell>0,TC==1)

# total pump in sample
R_Demography_2_Baseline_2018_ %>% select(1,37,35,38,39,41,40,63) %>%
  filter(diesel_pump>0 | cd_pump_vuplator>0 |electric_pump>0,TC==1)

R_Demography_2_Endline_EPC_2019_ %>% select(1,37,35,38,39,41,40,63) %>%
  filter(diesel_pump>0 | cd_pump_vuplator>0 |electric_pump>0,TC==1)
# [2]         
R_Demography_2_Baseline_2018_%>%select(1,37,35,38,39,41,40,63)%>%filter(electric_pump!=0,TC==1) 
R_Demography_2_Endline_EPC_2019_%>%select(1,37,35,38,39,41,40,63)%>%filter(electric_pump!=0,TC==1) 


# FUEL - diesel_pump + cd_pump_vuplator
a <- R_Demography_2_Baseline_2018_ %>% select(1,37,35,38,39,41,40,63) %>%
  filter(diesel_pump!=0 | cd_pump_vuplator!=0,TC==1)
  
a <- R_Demography_2_Endline_EPC_2019_ %>% select(1,37,35,38,39,41,40,63) %>%
  filter(diesel_pump!=0 | cd_pump_vuplator!=0,TC==1)

# intensity----
a <- R_intensity_Baseline%>% group_by(TC) %>% 
  summarise(mean(cropping_intensity))
