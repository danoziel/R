x <- S_Lands_I_Baseline_2017_ %>% filter(land_for_aquaculture_ponds>0) %>%group_by(TreatmentControl)%>%
  summarise(m=mean(land_for_aquaculture_ponds),n(),ha=m*0.0339)

Aquaculture_Baseline_2017_ %>% filter(total_area_of_pond>0) %>%group_by(TreatmentControl)%>% 
  summarise(m=mean(total_area_of_pond),ha=m*0.0339,n())