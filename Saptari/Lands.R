S_Lands_Baseline_2017_ %>%filter(season=="SUMMER 2074",total_ownland_cultivated>0) %>% 
  group_by(TreatmentControl)%>%summarise(m=mean(total_ownland_cultivated),ha=m*0.0338,n())

S_Lands_I_Baseline_2017_ %>% group_by(TC) %>% tally()
rm(x)

S_Lands_Baseline_2017_ %>%filter(total_ownland_cultivated>0) %>%  group_by(household_questionnaire_id,season,TC)%>%
  summarise(sum=sum(total_ownland_cultivated)) %>%group_by(TC,season) %>%  summarise(mean(sum), n())
