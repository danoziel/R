library(tidyverse)
library(kableExtra)
library(formattable)
library(knitr)
library(MatchIt)
library(tableone)
library(gridExtra)

lsay <- Agriculture_17_18_19 %>% filter(season_of_crop=="Summer") %>%
  group_by(year,season_of_crop,TreatmentControl,household_questionnaire_id) %>%
  summarise(irrigate_hr=sum(irri_for_season),cult_area=sum(cult_area_under_crop)*0.0339) %>%
  mutate(irrigation_hr_per_ha= irrigate_hr/cult_area) %>%
  filter(!is.na(irrigation_hr_per_ha))

names(lsay)[names(lsay) == 'TreatmentControl'] <- 'Group'

lso <-
  lsay %>%  mutate(own_sp = Group == "Treatment")

lso_M <- filter(lso, year == 2017)
match.it <- matchit(own_sp ~ irrigation_hr_per_ha , data = lso_M, method="nearest", ratio=1)
df.match <- match.data(match.it)[4]
df.match <-
  inner_join (df.match,lso,by="household_questionnaire_id") %>% 
  select(year,Group,household_questionnaire_id,irrigation_hr_per_ha )%>%
  filter(Group=="Control")
df.match$Group <- "Match"

df <- rbind(lsay,df.match) 

ggplot(df, aes(year, irrigation_hr_per_ha, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y="income per ha") +
  scale_x_continuous(breaks = c(2017,2018,2019))+
  theme(legend.position = "none",axis.text.x = element_text(face="bold",size=14),axis.text.y = element_text(face="bold",size=16))

rm(df.match,match.it,df)
rm(lso , lso_M)
rm( lsi )


-------------------------------------------------------------------------------------------
df <- df %>% group_by(Group,year) %>% summarise(mean(irrigation_hr_per_ha))%>% mutate(across(is.numeric, round))
dt <- cbind(dt[1:3,-1],dt[4:6,3],dt[7:9,3])

kable(dt,col.names = c("", "Control", "Treatment", "Match")) %>%
  kable_styling() 
-------------------------------------------------------------------------------------------
  
lsayr <-Agriculture_18_19 %>% filter(season_of_crop=="Summer") %>% 
  group_by(year,season_of_crop,TreatmentControl,household_questionnaire_id) %>%
  summarise(irrigate_hr=sum(irri_for_season),cult_area=sum(cult_area_under_crop)*0.0339) %>%
  mutate(irrigation_hr_per_ha= irrigate_hr/cult_area) %>% 
  filter(!is.na(irrigation_hr_per_ha))


names(lsayr)[names(lsayr) == 'TreatmentControl'] <- 'Group'

lso <-
  lsayr %>% mutate(own_sp = Group == "Treatment") 

lso_M <- filter(lso, year == 2018)
match.it <- matchit(own_sp ~ irrigation_hr_per_ha , data = lso_M, method="nearest", ratio=1)
df.match <- match.data(match.it)[4]
df.match <-
  inner_join (df.match,lso,by="household_questionnaire_id") %>% 
  select(year,Group,household_questionnaire_id,irrigation_hr_per_ha)
df.match$Group <- "xMatch"

df <- rbind(lsayr,df.match)

ggplot(df, aes(year, irrigation_hr_per_ha, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y=" ") +
  scale_x_continuous(breaks = c(2018,2019))+
  theme(axis.text.x = element_text(face="bold",size=14),axis.text.y = element_text(face="bold",size=16))


dt <-
  df %>% group_by(Group,year) %>% summarise(mean(income_per_ha),n())%>% mutate(across(is.numeric, round, 2))

rm(df.match,match.it,df)
rm(lso , lso_M)


#----------------------------------------------------------------------------------------
harvest_ha<- Agriculture_17_18_19 %>% 
  mutate(kg=case_when(
    unit_harvest %in% c(1,2,5) ~37.32,
    unit_harvest == 4 ~100,
    unit_harvest == 3 ~1)) %>% 
  mutate(harvest_per_ha=what_was_your_total_harvest*kg/0.0339) %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  filter(harvest_per_ha>0) %>% 
  summarise(harvest_per_ha=sum(harvest_per_ha)) %>% 
  group_by(TreatmentControl,year) %>% 
  summarise(harvest_per_ha=mean(harvest_per_ha)) %>% 
  mutate(across( is.numeric,round))



income_ha <- Agriculture_17_18_19 %>% 
  mutate(kg=case_when(
    unit_harvest %in% c(1,2,5) ~37.32,
    unit_harvest == 4 ~100,
    unit_harvest == 3 ~1)) %>% 
  mutate(percent=percent_for_selling/100) %>% 
  mutate(income=what_was_your_total_harvest*cult_area_under_crop*kg*price_per_kg*percent) %>% 
  group_by(TreatmentControl,year, household_questionnaire_id) %>% 
  summarise(income=sum(income,na.rm = T),cult_area=sum(cult_area_under_crop,na.rm = T)*0.0339,income_per_ha=income/cult_area/1000) %>% 
  filter(!is.na(income_per_ha)) %>% 
  summarise(Mean=mean(income_per_ha)) %>% 
  mutate(across( is.numeric,round,2)) 

x <- spread(income_ha, TreatmentControl, Mean)

kable(x, align = "c",  booktabs = T) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1, valign = "top")
  
income2_ha<- Agriculture_18_19 %>% 
    mutate(kg=case_when(
      unit_harvest %in% c(1,2,5) ~37.32,
      unit_harvest == 4 ~100,
      unit_harvest == 3 ~1)) %>% 
    mutate(percent=percent_for_selling/100) %>% 
    mutate(income=what_was_your_total_harvest*cult_area_under_crop*kg*price_per_kg*percent) %>% 
    group_by(TreatmentControl,year, household_questionnaire_id) %>% 
    summarise(income=sum(income,na.rm = T),cult_area=sum(cult_area_under_crop,na.rm = T)*0.0339,income_per_ha=income/cult_area/1000) %>% 
    filter(!is.na(income_per_ha))  %>% 
  summarise(Mean=mean(income_per_ha)) %>% mutate(across( is.numeric,round,2))

x <- spread(income2_ha, TreatmentControl, Mean)

kable(x, align = "c",  booktabs = T) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1, valign = "top")



income2_ha<- Agriculture_18_19 %>% 
  mutate(kg=case_when(
    unit_harvest %in% c(1,2,5) ~37.32,
    unit_harvest == 4 ~100,
    unit_harvest == 3 ~1)) %>% 
  mutate(percent=percent_for_selling/100) %>% 
  mutate(income=what_was_your_total_harvest*cult_area_under_crop*kg*price_per_kg*percent) %>% 
  group_by(TreatmentControl,year, household_questionnaire_id) %>% 
  summarise(income=sum(income,na.rm = T),cult_area=sum(cult_area_under_crop,na.rm = T)*0.0339,income_per_ha=income/cult_area) %>% 
  filter(!is.na(income_per_ha)) %>% mutate(across( is.numeric,round,2)) 


