###-------------------------------------------------------###
###     befor and after - treatment group B&A_T           ###
###-------------------------------------------------------###

# R_Lands_Baseline_2018_ \/ R_Lands_Endline_EPC_2019_    ----
----------------------------------------------------    
Land_18_19 <-bind_rows( R_Lands_Baseline_2018_,R_Lands_Endline_EPC_2019_)

mutate(across(is.numeric, round, 2))


# In the following variables: total_land_cultivated_year>0  to omit HH 
# butttt Including the value 0 in the variables themselves 

# total_ownland_cultivated----
df2 <- Land_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(own=sum(total_ownland_cultivated)*0.0338) %>% 
  group_by(year, TreatmentControl) %>%
  summarise(N=n(), own=mean(own)) %>% 
  mutate(across(is.numeric, round, 2))

# total_land_cultivated-----
Land_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(cult=sum(total_land_cultivated)*0.0338) %>% 
  group_by(year, TreatmentControl) %>%
  summarise(N=n(), cult=mean(cult))

# irrigated_out_of_tot_land_cult----
Land_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(irrigated=sum(irrigated_out_of_tot_land_cult)*0.0338) %>% 
  group_by(year, TreatmentControl) %>%
  summarise(N=n(), irrigated=mean(irrigated))


# TreatmentControl 2018 2019 
spread(df2, HH, own)

# crop_intensity----
x <- Land_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(cult=sum(total_land_cultivated),net=mean(nca))%>%mutate(ci=cult/net*100) %>% 
  group_by(year, TreatmentControl) %>%
  summarise(N=n(),crop_intensity=mean(ci)) %>% 
  mutate(across(is.numeric, round, 2))

# irrigate_intensity----
year <- Land_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  mutate(ii=irrigated_out_of_tot_land_cult/nca)%>%
  group_by(year,TreatmentControl,household_questionnaire_id) %>%
  summarise(sumii=sum(ii)) %>% summarise(irrigate_intensity=mean(sumii)*100)
              
seasons <- Land_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  mutate(ii=irrigated_out_of_tot_land_cult/nca)%>%
  group_by(year,TreatmentControl,season,household_questionnaire_id) %>%
  summarise(sumii=sum(ii)) %>% summarise(irrigate_intensity=mean(sumii)*100)

# chaking changes in number of HH----

# in "Monsoon" and "Winter" -2018 2019 same same
# in "Annual" wired numbers
------------------------------------------------------------------
# total_land_cultivated
Land_18_19 %>% filter(season=="Summer") %>% 
  filter(total_land_cultivated_year>0,total_land_cultivated>0) %>%
  group_by(year,TreatmentControl) %>%
  summarise(n = n())

Land_18_19 %>% filter(season=="Summer") %>% 
  filter(total_land_cultivated_year>0,total_land_cultivated>0) %>%
  group_by(year,TreatmentControl) %>%
  summarise(n = n())

-------------------------------------------------------------------
# irrigated_out_of_tot_land_cult
Land_18_19%>%  filter(season=="Annual") %>% 
  filter(total_land_cultivated_year>0,irrigated_out_of_tot_land_cult>0) %>%
  group_by(year,TreatmentControl) %>%
  summarise(n = n()) 

-------------------------------------------------------------------
# total_ownland_cultivated
Land_18_19 %>% filter(season=="Summer") %>% 
  filter(total_land_cultivated_year>0,total_ownland_cultivated>0) %>%
  group_by(year,TreatmentControl) %>%
  summarise(n = n())

Land_18_19 %>% filter(season=="Annual") %>% 
  filter(total_land_cultivated_year>0,total_ownland_cultivated>0) %>%
  group_by(year,TreatmentControl) %>%
  summarise(n = n())

---------------------------------------------------------------------
  
# R.Agriculture_Baseline_2018_ vs R.Agriculture_Endline_EPC_2019_----

---------------------------------------------------------------------
  
df2 <- Agriculture_18_19 %>% group_by(year,TreatmentControl,household_questionnaire_id) %>%
  summarise(mean(cult_area_under_crop)) %>% 
  count()

  
R_Agriculture_Baseline_2018_ <- R_Agriculture_Baseline_2018_%>%select(16,everything())
R_Agriculture_Endline_EPC_2019_ <- R_Agriculture_Endline_EPC_2019_%>%select(15,everything())

Agriculture_18_19 <- R_Agriculture_Baseline_2018_%>%bind_rows(R_Agriculture_Endline_EPC_2019_)
write.csv(Agriculture_18_19,"C:/Users/Dan/Documents/R/Rautahat.Bara.Sarlahi/Agriculture_18_19.csv", row.names = FALSE)

#irrigation- IN HOURS: n=HH ---Agriculture_18_19----
#time taken to irrigate the cultivated area.A single crop season IN HOURS
Treats <- subset(R.Agriculture_Endline_EPC_2019_,  TC == 1)

summary(Agriculture_18_19$hrs_irr_1katha/0.0339)

## year ##
# baseline 2018
Agriculture_18_19%>% filter(year==2018) %>% 
  group_by(household_questionnaire_id) %>% 
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season)) %>% 
  summarise(N=n(),average_hr_per_ha=mean(hr_per_ha,na.rm = T),
            total_irrigate_hr=mean(irrigate_hr,na.rm = T))

# # TreatmentControl 2018 2019
df3 <- Agriculture_18_19%>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season)) %>% 
  summarise(N=n(),average_hr_per_ha=mean(hr_per_ha,na.rm = T),
            total_irrigate_hr=mean(irrigate_hr,na.rm = T)) %>% 
  mutate(across(is.numeric, round, 2))

## season  ##
#baseline 2018
Agriculture_18_19 %>%filter(year==2018) %>% 
  group_by(household_questionnaire_id,season_of_crop) %>%
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season)) %>% 
  group_by(season_of_crop) %>%
  summarise(N=n(),average_hr_per_ha=mean(hr_per_ha,na.rm = T),
            total_irrigate_hr=mean(irrigate_hr,na.rm = T)) 

# # TreatmentControl 2018 2019
cc <- Agriculture_18_19 %>% filter(season_of_crop=="Summer") %>% 
  group_by(year,TreatmentControl,household_questionnaire_id) %>%
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season))%>% 
  group_by(year, TreatmentControl) %>%
  summarise(n=n(),average_hr_per_ha=mean(hr_per_ha,na.rm = T),
            total_irrigate_hr=mean(irrigate_hr,na.rm = T))

HH_summer <- data.frame(year=c(2018," ",2019," "),tc=c("Control","Treatment"),n = c(33,7,38,11),
           N=c(107,26,95,21),freq=n/N,Percentage_increase=c(" "," ",0.1515,0.5714))

# Water_extraction_mechanism_Baseline_2018_$time_to_irrigate_1_katha__p_----

Wem_18_19_time_to_irrigate_1_ha <- bind_rows(Water_extraction_mechanism_Baseline_2018_[,c(140,1,73)],
      Water_extraction_mechanism_Endline_EPC_2019_[,c(136,1,70)] ) %>%
  inner_join(Control_and_treatment_4_districts) %>% 
  filter(!is.na(time_to_irrigate_1_katha__p_1)) %>%
  group_by(year,TreatmentControl) %>% 
  summarise(mean(time_to_irrigate_1_katha__p_1)/60/0.0339,n())
        


# crop pattern----

# frequency HH grow type of crop
# How much of the cultivated area was under this crop: mean & %
#Monsoon  Winter  Summer  Annual 
#   1       2       3       4 


# year
Agriculture_18_19 %>% group_by(year,TreatmentControl, household_questionnaire_id,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>% group_by(year,TreatmentControl, name_of_crop) %>%
  summarise(no.HH=n(),mean(sum_cult_area))

# crop/season
x <- Agriculture_18_19 %>% group_by(year,TreatmentControl, household_questionnaire_id,season_of_crop,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(year,TreatmentControl,season_of_crop ,name_of_crop) %>% 
  summarise(no.HH=n(),mean(sum_cult_area))

# SECTION 7 - PROCUREMENT: ELECTRICITY, FUEL----

Procurement_18_19 <- Procurement_Baseline_2018_ %>%bind_rows(Procurement_Endline_EPC_2019_) %>% 
  inner_join(Control_and_treatment_4_districts) %>% select(28,everything()) 

# [7.16] Total litres of diesel/kerosene consumed for agriculture pumps in a YEAR----

Procurement_18_19 %>%  # omit 2 faemrs with 15,000 and 10,000 liter
  filter(year==2018,!is.na(total_litres_consumed_dieselkero),total_litres_consumed_dieselkero<10000) %>%
  summarise(N=n(), liters_yearly=mean(total_litres_consumed_dieselkero))

Procurement_18_19 %>%  
  filter(!is.na(total_litres_consumed_dieselkero),total_litres_consumed_dieselkero<10000) %>%
  group_by(year,TreatmentControl) %>%
  summarise(N=n(), liters_yearly=mean(total_litres_consumed_dieselkero))

# buy fuel----
#[7.12] Did you buy fuel for the pump?
Procurement_18_19 %>% filter(year==2018) %>% 
  group_by(do_you_buy_fuel_for_the_pump) %>% tally()

Procurement_18_19 %>% filter(!is.na(do_you_buy_fuel_for_the_pump),do_you_buy_fuel_for_the_pump==1) %>% 
  group_by(year,do_you_buy_fuel_for_the_pump,TreatmentControl) %>% tally()
  
# [7.13] If yes, which fuel is predominantly used for your pump?
table(R_Procurement_Baseline_2018_$if_yes__which_fuel_do_you_use)
#     1 Diesel  # 2 Kerosene #

Procurement_18_19 %>% filter(year==2018) %>% 
  group_by(if_yes__which_fuel_do_you_use) %>% tally()

Procurement_18_19 %>% filter(!is.na(if_yes__which_fuel_do_you_use)) %>% 
  group_by(year,if_yes__which_fuel_do_you_use,TreatmentControl) %>%tally()

# fuel use by the 'Water_extraction_mechanism'files----
x <- R_WEM_liter_hr_season %>% select(1,32:35) %>%
  left_join(Water_extraction_mechanism_Baseline_2018_[,c(1,139:140)]) %>% 
  inner_join(Control_and_treatment_4_districts) %>% select(7,1:5,8:9,6)
y <- R_WEM_liter_hr_season_Endline %>% select(1,32:35) %>% 
  inner_join(Control_and_treatment_4_districts) %>% 
  left_join(Water_extraction_mechanism_Endline_EPC_2019_ [,c(1,136:137)]) %>% 
  select(8,everything())
WEM_fueliter_18_19 <- rbind(x,y)

WEM_fueliter_18_19 %>% filter(year==2018) %>%
  summarise(summer=mean(p123_s),monsoon=mean(p123_m),winter=mean(p123_w),yearly=mean(p123_year))
            
WEM_fueliter_18_19 %>% group_by(year,TreatmentControl) %>% 
  summarise(summer=mean(p123_s),monsoon=mean(p123_m),winter=mean(p123_w),yearly=mean(p123_year),n())


            






# Aquaculture----
R_Aquaculture_Endline_EPC_2019_ <- inner_join(Aquaculture_Endline_EPC_2019_,Control_and_treatment_4_districts)

# [11.1] Do you practice aquaculture/fish farming? # 1 Yes # 2 No #


a <- R_Aquaculture_Baseline_2018_ %>% filter(!is.na(practice_aquaculture)) %>% 
  group_by(TreatmentControl) %>% 
  count(practice_aquaculture) %>% mutate(freq = n / sum(n))

a <- R_Aquaculture_Endline_EPC_2019_ %>% filter(!is.na(practice_aquaculture)) %>%
  group_by(TreatmentControl) %>% 
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






# R_Demography_2_Endline_EPC_2019_----
#[3.9a] How many of the following items are owned by the household for irrigation?
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


