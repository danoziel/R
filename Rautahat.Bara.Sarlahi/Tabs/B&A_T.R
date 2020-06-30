
# In the following variables: total_land_cultivated_year>0  to omit HH 
# butttt Including the value 0 in the variables themselves 

# total_ownland_cultivated   [4.4]  ----   
df2 <- Land_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(own=sum(total_ownland_cultivated)*0.0338) %>% 
  group_by(year, TreatmentControl) %>%
  summarise(N=n(), own=mean(own)) %>% 
  mutate(across(is.numeric, round, 2))

# total_land_cultivated     [4.8]   ----   
Land_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(cult=sum(total_land_cultivated)*0.0338) %>% 
  group_by(year, TreatmentControl) %>%
  summarise(N=n(), cult=mean(cult))

# crop_intensity           [4.0]   ----    
x <- Land_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(cult=sum(total_land_cultivated),net=mean(nca))%>%mutate(ci=cult/net*100) %>% 
  group_by(year, TreatmentControl) %>%
  summarise(N=n(),crop_intensity=mean(ci)) %>% 
  mutate(across(is.numeric, round, 2))



# irrigated_out_of_tot_land_cult (In ha) [4.9]----
Land_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(irrigated=sum(irrigated_out_of_tot_land_cult)*0.0338) %>% 
  group_by(year, TreatmentControl) %>%
  summarise(N=n(), irrigated=mean(irrigated))



# Time to irrigate 1 ha [6.21] ----
# How long does it take to irrigate 1 katha of land with this pump, in min? [6.21]
Wem_18_19_time_to_irrigate_1_ha <- bind_rows(Water_extraction_mechanism_Baseline_2018_[,c(140,1,73)],
                                             Water_extraction_mechanism_Endline_EPC_2019_[,c(136,1,70)] ) %>%
  inner_join(Control_and_treatment_4_districts) %>% 
  filter(!is.na(time_to_irrigate_1_katha__p_1)) %>%
  group_by(year,TreatmentControl) %>% 
  summarise(N=n(),mean(time_to_irrigate_1_katha__p_1)/60/0.0339)












# irrigation- IN HOURS By year [5.0]----
df3 <- Agriculture_18_19%>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season)) %>% 
  summarise(N=n(),average_hr_per_ha=mean(hr_per_ha,na.rm = T),
            total_irrigate_hr=mean(irrigate_hr,na.rm = T)) %>% 
  mutate(across(is.numeric, round, 2))

# irrigation- IN HOURS By season [5.0]-----
c <- Agriculture_18_19 %>% filter(season_of_crop!="Annual") %>% 
  group_by(year,season_of_crop,TreatmentControl,household_questionnaire_id) %>%
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season))%>% 
  group_by(TreatmentControl,season_of_crop,year) %>%
  summarise(n=n(),average_hr_per_ha=mean(hr_per_ha,na.rm = T),
            total_irrigate_hr=mean(irrigate_hr,na.rm = T)) %>% 
  mutate(across(is.numeric, round, 2))


  # irrigate_intensity [4.0] year----
  irri_inten_y <- Land_18_19 %>% filter(season!="Annual") %>% 
  filter(total_land_cultivated_year>0) %>%
  mutate(ii=irrigated_out_of_tot_land_cult/nca) %>%
  group_by(TreatmentControl, season,year,household_questionnaire_id) %>%
  summarise(irrigate_intensity=sum(ii)) %>% 
  group_by(TreatmentControl,year,household_questionnaire_id) %>%
  summarise(ir_in=sum(irrigate_intensity)) %>% 
  summarise(N=n(),`Irrigation Intensity`=mean(ir_in)*100) %>% 
  mutate(across(is.numeric, round, 2))

iyc <- irri_inten_y[1:2,]
iyt <- irri_inten_y[3:4,3:4]
iyc_iyt <- cbind(iyc,iyt)[,-1]

kable(iyc_iyt, "latex",col.names = c("Year","N","Intensity","N","Intensity"), booktabs = T,align = "lcccc",linesep = "") %>%
  column_spec(1, bold = T) %>%
  kable_styling(latex_options = "striped", position = "left") %>% 
  column_spec(1:5, width = "1.5cm",border_left = F) %>%
  column_spec(3,border_right = T ,width = "1.5cm") %>%
  row_spec(0, font_size= 9) %>% 
  add_header_above(c(" " = 1, "Control" = 2, "Treatment" = 2), bold = F, align = "c")

  # irrigate_intensity [4.0] seasons----
  irri_inten_s <- Land_18_19 %>% filter(season!="Annual") %>% 
  filter(total_land_cultivated_year>0) %>%
  mutate(ii=irrigated_out_of_tot_land_cult/nca) %>%
  group_by(TreatmentControl, season,year,household_questionnaire_id) %>%
  summarise(irrigate_intensity=sum(ii)) %>% 
  summarise(N=n(),`Irrigation Intensity`=mean(irrigate_intensity)*100) %>% 
  mutate(across(is.numeric, round, 2))

iic <-irri_inten[1:6,]
iit <- irri_inten[7:12,4:5]
iic_iit <- cbind(iic,iit)[,-1]

kable(iic_iit,col.names = c("Season","Year","N","mean","N","mean"), booktabs = T,align = "llcccc",linesep = "") %>%
  column_spec(1, bold = T) %>%
  kable_styling(latex_options = "striped", position = "left") %>% 
  column_spec(1:6, width = "1.5cm") %>%
  column_spec(4,border_right = T ,width = "1.5cm") %>% 
  collapse_rows(columns = 1, valign = "top") %>% 
  row_spec(0, font_size= 9) %>% 
  add_header_above(c(" " = 2, "Control" = 2, "Treatment" = 2), bold = F, align = "c")

# fuel [7.16]----
#  Total litres of diesel/kerosene consumed for agriculture pumps in a YEAR
fuel_Proc <- Procurement_18_19 %>%  
  filter(!is.na(total_litres_consumed_dieselkero),total_litres_consumed_dieselkero<10000) %>%
  group_by(TreatmentControl,year) %>%
  summarise(N=n(), liters_yearly=mean(total_litres_consumed_dieselkero)) %>% 
  mutate(across(is.numeric, round, 2))



# fuel [6.2]----
# fuel use by the 'Water_extraction_mechanism'files

wem_liter_fuel_18_19%>%
  inner_join(Control_and_treatment_4_districts) %>%
  group_by(TreatmentControl,year) %>% 
  summarise(Monsoon=mean(p123_m,na.rm = T),
            Summer=mean(p123_s,na.rm = T),Winter=mean(p123_w,na.rm = T),
            Year=mean(p123_year,na.rm = T))














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









# ___________________to be continued___________________--------
# chaking changes in number of HH ----

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

# crop pattern----

# frequency HH grow type of crop
# How much of the cultivated area was under this crop: mean & %
#Monsoon  Winter  Summer  Annual 
#   1       2       3       4 


# year
crop_pattern <- Agriculture_18_19 %>% filter(!name_of_crop %in%  c("Oilseeds","others") )%>% 
  group_by(year,TreatmentControl, household_questionnaire_id,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>% group_by(year,TreatmentControl, name_of_crop) %>%
  summarise(no.HH=n(),mean(sum_cult_area))


# crop/season
x <- Agriculture_18_19 %>% group_by(year,TreatmentControl, household_questionnaire_id,season_of_crop,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(year,TreatmentControl,season_of_crop ,name_of_crop) %>% 
  summarise(no.HH=n(),mean(sum_cult_area))

# buy fuel----
מספר החקלאים שרוכשים דלקים הוא מספר בעלי המשאבות דלק- כל השאר משתמשים במשאבות חשמל
#[7.12] Did you buy fuel for the pump?
Procurement_18_19 %>%filter(do_you_buy_fuel_for_the_pump==1) %>% 
  group_by(TreatmentControl,year,do_you_buy_fuel_for_the_pump) %>% tally()

# [7.13] If yes, which fuel is predominantly used for your pump?
table(R_Procurement_Baseline_2018_$if_yes__which_fuel_do_you_use)
#     1 Diesel  # 2 Kerosene #

Procurement_18_19 %>% filter(year==2018) %>% 
  group_by(if_yes__which_fuel_do_you_use) %>% tally()

Procurement_18_19 %>% filter(!is.na(if_yes__which_fuel_do_you_use)) %>% 
  group_by(year,if_yes__which_fuel_do_you_use,TreatmentControl) %>%tally()

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


