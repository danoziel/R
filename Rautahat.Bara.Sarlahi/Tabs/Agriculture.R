# frequency HH grow type of crop----
R.Agriculture_Baseline_2018_ %>%  
  group_by(household_questionnaire_id,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(name_of_crop) %>% 
  summarise(no.HH=n(),no.HH/133)

# How much of the cultivated area was under this crop: mean & %----

#Monsoon  Winter  Summer  Annual 
#   1       2       3       4 

#season-fixed
ses <- R.Agriculture_Baseline_2018_ %>%  
  group_by(household_questionnaire_id,season_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(season_of_crop) %>% 
  summarise(mean = mean(sum_cult_area),
            per=sum(sum_cult_area)/
              sum(R.Agriculture_Baseline_2018_$cult_area_under_crop,na.rm = T)
  )
# crop-fixed
ses <- R.Agriculture_Baseline_2018_ %>%  
  group_by(household_questionnaire_id,name_of_crop) %>%
  summarise(sum_cult_area=sum(cult_area_under_crop))%>%
  group_by(name_of_crop) %>% 
  summarise(mean = mean(sum_cult_area),
            per= sum(sum_cult_area)/
              sum(R.Agriculture_Baseline_2018_$cult_area_under_crop,na.rm = T)
            )

# area crop out of area seasom----
#season_of_crop / name_of_crop 

ses <- R.Agriculture_Baseline_2018_ %>%
  select(1,3,4,7) %>% 
  group_by(household_questionnaire_id,season_of_crop,name_of_crop) %>%
  summarise(sum1=sum(cult_area_under_crop)) %>% 
  group_by(season_of_crop,name_of_crop) %>% 
  summarise(n(),mean(sum1))

# sum of cult_area_under_crop in a season_of_crop
R.Agriculture_Baseline_2018_ %>%  
  group_by(season_of_crop) %>%
  summarise(sum(cult_area_under_crop))

#Monsoon
R.Agriculture_Baseline_2018_ %>% filter(season_of_crop=="1") %>% 
  group_by(name_of_crop)%>%summarise(pr=sum(cult_area_under_crop/7564))

#Winter
R.Agriculture_Baseline_2018_ %>%filter(season_of_crop=="2") %>% 
  group_by(name_of_crop)%>%summarise(pr=sum(cult_area_under_crop/7005))

#Summer
R.Agriculture_Baseline_2018_ %>%filter(season_of_crop=="3") %>% 
  group_by(name_of_crop)%>%summarise(p=sum(cult_area_under_crop/1061))

# Annual
R.Agriculture_Baseline_2018_ %>%filter(season_of_crop=="4") %>% 
  group_by(name_of_crop)%>%summarise(p=sum(cult_area_under_crop/526))

#irrigation- IN HOURS----fixed: n=HH ----
#irrigation for HH IN HOURS
ses <- R.Agriculture_Baseline_2018_ %>%
  select(household_questionnaire_id,cult_area_under_crop,irri_for_season) %>%
  filter(!is.na(irri_for_season))%>% 
  group_by(household_questionnaire_id) %>%
  summarise(n(),cult_area=sum(cult_area_under_crop),irri_season=sum(irri_for_season)) %>% 
  summarise(mean(irri_season),mean(cult_area),n.HH= n())


#irrigationfor one crop season IN HOURS

# season
ses <- R.Agriculture_Baseline_2018_ %>%
  filter(!is.na(irri_for_season),!is.na(season_of_crop))%>% 
  group_by(household_questionnaire_id,season_of_crop) %>% 
  summarise(count = n(),
            sum=sum(irri_for_season),
            m_irri=mean(irri_for_season)) %>% 
  group_by(season_of_crop) %>% 
              summarise(n(), mean(m_irri))

#crop

ses <- R.Agriculture_Baseline_2018_ %>%
  filter(!is.na(irri_for_season))%>% 
  group_by(household_questionnaire_id,name_of_crop) %>% 
  summarise(count = n(),
            sum=sum(irri_for_season),
            m_irri=mean(irri_for_season)) %>% 
  group_by(name_of_crop) %>% 
  summarise(n(), mean(m_irri))

#season/crop

ses <- R.Agriculture_Baseline_2018_ %>%
  filter(!is.na(irri_for_season))%>%
  group_by(household_questionnaire_id,season_of_crop,name_of_crop) %>%
  summarise(count = n(),
            sum_irri=sum(irri_for_season)) %>% 
  group_by(season_of_crop,name_of_crop) %>% 
  summarise(n(), mean(sum_irri))


#--------------irrigation IN HOURS------TC---
Treats <- subset(R.Agriculture_Baseline_2018_,  TC == 1)









#[5.6] most_imp_source--------

#Rain    Groundwater   River   Canal 
# 1          2           3       4    

R.Agriculture_Baseline_2018_ %>% 
  select(1,8,17) %>% 
  filter(most_imp_source>0) %>% 
  group_by(household_questionnaire_id,most_imp_source) %>% 
  summarise(n()) %>% 
  group_by(most_imp_source) %>% 
  summarise(n() ,mn=n()/133)

#when_is_crop_sowed/harvested----not fixed----
#when_is_crop_sowed
ses <- R.Agriculture_Baseline_2018_ %>% 
  select(when_is_crop_sowed, cult_area_under_crop) %>% 
  group_by(when_is_crop_sowed) %>% 
  summarise(sum=sum(cult_area_under_crop),
            per=sum(cult_area_under_crop)/
              sum(R.Agriculture_Baseline_2018_$cult_area_under_crop,na.rm = T)
  )

#when_is_crop_harvested
ses <- R.Agriculture_Baseline_2018_ %>% 
  select(cult_area_under_crop,when_is_crop_harvested) %>% 
  group_by(when_is_crop_harvested) %>% 
  summarise(sum=sum(cult_area_under_crop),
            per=sum(cult_area_under_crop)/
              sum(R.Agriculture_Baseline_2018_$cult_area_under_crop,na.rm = T)
  )

