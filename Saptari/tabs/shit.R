
,fig.show="hold", out.width="50%" 

ggplot( , aes(year, Mean, color = TreatmentControl)) +
  stat_summary(geom = 'line') +
  theme_minimal()+
  scale_x_continuous(breaks = c(2017,2018,2019))

ggplot( , aes(year, Mean, color = TreatmentControl)) +
  stat_summary(geom = 'line') +
  theme_minimal()+scale_x_continuous(breaks = c(2018,2019))

#-------------------------------------------------------------------------------------------

raqua <- lands_I_18_19 %>% filter(land_for_aquaculture_ponds>0,land_for_aquaculture_ponds<300) %>% 
  group_by(TreatmentControl,year) %>% 
  summarise(N=n(),Mean= mean(land_for_aquaculture_ponds,na.rm = T)*0.0339) %>% 
  mutate(across(is.numeric,round,2))

res <- 
  t.test(land_for_aquaculture_ponds ~ TreatmentControl, data = raqua)





Agriculture_17_18_19 %>%
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  group_by(TreatmentControl,year) %>% 
  summarise(N=n(),Mean=mean(cult_area)) %>% 
  mutate(across( is.numeric,round,2))