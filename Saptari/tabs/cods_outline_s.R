# Organize data----
library(haven)
# Lands_ -combined
Lands_Baseline_2017_ <- read_dta("~/Nepal Data/Saptari/Baseline 73-74 (Saptari)/Lands_Baseline(2017).dta")
Lands_Midline_2018_ <- read_dta("~/Nepal Data/Saptari/Midline 2074-75 (Saptari)/Lands_Midline(2018).dta")
Lands_Endline_2019_Saptari <- read_dta("~/Nepal Data/Saptari/Saptari-Endline(2019)/Lands_Endline(2019)-Saptari.dta")

land_17_18_19 <- rbind(Lands_Baseline_2017_,Lands_Midline_2018_,Lands_Endline_2019_Saptari)
land_17_18_19 <- inner_join(land_17_18_19,Control_and_treatment_4_districts)

# Lands_I_ -combined
Lands_I_Baseline_2017_ <- read_dta("~/Nepal Data/Saptari/Baseline 73-74 (Saptari)/Lands_I_Baseline(2017).dta")
Lands_I_Midline_2018_ <- read_dta("~/Nepal Data/Saptari/Midline 2074-75 (Saptari)/Lands_I_Midline(2018).dta")
Lands_I_Endline_2019_Saptari <- read_dta("~/Nepal Data/Saptari/Saptari-Endline(2019)/Lands_I_Endline(2019)-Saptari.dta")

lands_I_17_18_19 <- rbind(Lands_I_Baseline_2017_,Lands_I_Midline_2018_,Lands_I_Endline_2019_Saptari)
lands_I_17_18_19 <- inner_join(lands_I_17_18_19,Control_and_treatment_4_districts)

#add "land_for_cultivitation" (nca) TO land_17_18_19
land_17_18_19 <- left_join(land_17_18_19,lands_I_17_18_19[,c(1,37,3)])

# Objects omitted with NAs in all variables
land_17_18_19 <- land_17_18_19 %>% filter(!is.na(total_land_cultivated))

#creat "total_land_cultivated_year"
land_17_18_19 <- land_17_18_19 %>% group_by(year,household_questionnaire_id) %>% 
  mutate(total_land_cultivated_year=sum(total_land_cultivated))

write.csv(land_17_18_19,"C:/Users/Dan/Documents/R/Saptari/land_17_18_19.csv", row.names = FALSE)

# total_ownland_cultivated        ----
lso <- land_17_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(own=sum(total_ownland_cultivated)*0.0338) %>% 
  group_by(TreatmentControl,year) %>%
  summarise(N=n(), Mean=mean(own)) %>% 
  mutate(across(is.numeric, round, 2))

# total_land_cultivated           -----
lsc <- land_17_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(cult=sum(total_land_cultivated)*0.0338) %>% 
  group_by(TreatmentControl,year) %>%
  summarise(N=n(), Mean=mean(cult))

# crop_intensity                  ----
land_17_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  mutate(nca= case_when(TreatmentControl=="Control" & land_for_cultivation < total_land_cultivated ~ NA_real_, TRUE ~ land_for_cultivation)) %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(cult=sum(total_land_cultivated),net=mean(nca,na.rm = T))%>%mutate(ci=cult/net*100) %>% 
  group_by(TreatmentControl,year) %>%
  summarise(N=n(),Mean=mean(ci)) %>% 
  mutate(across(is.numeric, round))















# irrigated_out_of_tot_land_cult  ----
lsi <- land_17_18_19%>% 
  filter(total_land_cultivated_year>0) %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>%
  summarise(irrigated=sum(irrigated_out_of_tot_land_cult,na.rm = T)*0.0338) %>% 
  group_by(TreatmentControl,year) %>%
  summarise(N=n(), Mean=mean(irrigated))

# Time to irrigate 1 ha [6.21] ----
# How long does it take to irrigate 1 katha of land with this pump, in min? [6.21]

wem_liter_fuel_17_18_19 %>% group_by(TreatmentControl,year) %>% 
  summarise(N=n(),Mean=mean(time_to_irrigate_1_katha__p_1,na.rm = T))

# irrigation- IN HOURS By year [5.0]----
lsay<- Agriculture_17_18_19%>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season)) %>% 
  summarise(N=n(),average_hr_per_ha=mean(hr_per_ha,na.rm = T),
            total_irrigate_hr=mean(irrigate_hr,na.rm = T)) %>% 
  mutate(across(is.numeric, round, 2))

# irrigation- IN HOURS By season [5.0]-----
lsas <- Agriculture_17_18_19 %>% filter(season_of_crop!="Annual") %>% 
  group_by(year,season_of_crop,TreatmentControl,household_questionnaire_id) %>%
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season))%>% 
  group_by(TreatmentControl,season_of_crop,year) %>%
  summarise(n=n(),average_hr_per_ha=mean(hr_per_ha,na.rm = T),
            total_irrigate_hr=mean(irrigate_hr,na.rm = T)) %>% 
  mutate(across(is.numeric, round, 2))

# irrigate_intensity [4.0] seasons----
irri_inten_s <- land_17_18_19 %>% filter(season!="Annual") %>% 
  filter(total_land_cultivated_year>0) %>%
  mutate(ii=irrigated_out_of_tot_land_cult/nca) %>%
  group_by(TreatmentControl, season,year,household_questionnaire_id) %>%
  summarise(irrigate_intensity=sum(ii)) %>% 
  summarise(N=n(),`Irrigation Intensity`=mean(irrigate_intensity)*100) %>% 
  mutate(across(is.numeric, round, 2))


# irrigate_intensity [4.0] year----
irri_inten_sy <- land_17_18_19 %>% filter(season!="Annual") %>% 
  filter(total_land_cultivated_year>0) %>%
  mutate(ii=irrigated_out_of_tot_land_cult/land_for_cultivation) %>%
  group_by(TreatmentControl, season,year,household_questionnaire_id) %>%
  summarise(irrigate_intensity=sum(ii)) %>% 
  group_by(TreatmentControl,year,household_questionnaire_id) %>%
  summarise(ir_in=sum(irrigate_intensity)) %>% 
  summarise(N=n(),`Irrigation Intensity`=mean(ir_in)*100) %>% 
  mutate(across(is.numeric, round, 2))

# fuel [6.2]----
# fuel use by the 'Water_extraction_mechanism'files
wem_liter_fuel_17_18_19%>%
  group_by(TreatmentControl,year) %>% 
  summarise(Monsoon=mean(p123_m,na.rm = T),
            Summer=mean(p123_s,na.rm = T),Winter=mean(p123_w,na.rm = T),
            Year=mean(p123_year,na.rm = T))

