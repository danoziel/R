S_Agriculture_Baseline_2018_ %>%
  group_by(TreatmentControl, household_questionnaire_id) %>% 
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season)) %>% 
  summarise(n(),mean(hr_per_ha,na.rm = T),mean(irrigate_hr,na.rm = T))



