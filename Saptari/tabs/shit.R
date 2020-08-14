library(tidyverse)
library(kableExtra)
library(formattable)
library(knitr)
library(MatchIt)
library(tableone)
library(gridExtra)

# filter(name_of_crop %in% c("Paddy","Wheat","Maize")) %>% 
  

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

income_ha<- Agriculture_17_18_19 %>% 
  mutate(kg=case_when(
    unit_harvest %in% c(1,2,5) ~37.32,
    unit_harvest == 4 ~100,
    unit_harvest == 3 ~1)) %>% 
  mutate(income_per_ha=what_was_your_total_harvest*kg/0.0339*price_per_kg) %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  filter(income_per_ha>0) %>% 
  summarise(income_per_ha=sum(income_per_ha)) %>% 
  group_by(TreatmentControl,year) %>% 
  summarise(income_per_ha=mean(income_per_ha)/1000) %>% 
  mutate(across( is.numeric,round,3))

  
  
  
  
names(pwm)[names(pwm) == 'TreatmentControl'] <- 'Group'

lso <-
  pwm %>%  mutate(own_sp = Group == "Treatment")

lso_M <- filter(lso, year == 2017)
match.it <- matchit(own_sp ~ cult_area , data = lso_M, method="nearest", ratio=1)
df.match <- match.data(match.it)[3]
df.match <-
  inner_join (df.match,lso,by="household_questionnaire_id") %>% 
  select(year,Group,household_questionnaire_id,cult_area)
df.match$Group <- "xMatch"

df <- rbind(pwm,df.match)

ggplot(df, aes(year, cult_area, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y="cultiveted area under crop") +
  scale_x_continuous(breaks = c(2017,2018,2019))+
  theme(legend.position = "none",axis.text.x = element_text(face="bold",size=14),axis.text.y = element_text(face="bold",size=16))

rm(df.match,match.it,df)
rm(lso , lso_M)
rm( lsi )


-------------------------------------------------------------------------------------------
df <- df %>% group_by(Group,year) %>% summarise(mean(ci))%>% mutate(across(is.numeric, round))
dt <- cbind(dt[1:3,-1],dt[4:6,3],dt[7:9,3])

kable(dt,col.names = c("", "Control", "Treatment", "Match")) %>%
  kable_styling() 
-------------------------------------------------------------------------------------------
pwmr <- Agriculture_18_19 %>%
  filter(name_of_crop=="MAIZE") %>% 
  group_by(year,TreatmentControl, household_questionnaire_id) %>% 
  summarise(cult_area=sum(cult_area_under_crop)*0.0339) %>% 
  mutate(across( is.numeric,round,2))

names(pwmr)[names(pwmr) == 'TreatmentControl'] <- 'Group'

lso <-
  pwmr %>% mutate(own_sp = Group == "Treatment") 

lso_M <- filter(lso, year == 2018)
match.it <- matchit(own_sp ~ cult_area , data = lso_M, method="nearest", ratio=1)
df.match <- match.data(match.it)[3]
df.match <-
  inner_join (df.match,lso,by="household_questionnaire_id") %>% 
  select(year,Group,household_questionnaire_id,cult_area)
df.match$Group <- "xMatch"

df <- rbind(pwmr,df.match)

ggplot(df, aes(year, cult_area, color = Group)) +
  stat_summary(geom = 'line',size=1.1) +
  theme_minimal()+
  labs(x=" ", y=" ") +
  scale_x_continuous(breaks = c(2018,2019))+
  theme(axis.text.x = element_text(face="bold",size=14),axis.text.y = element_text(face="bold",size=16))


dt <-
  df %>% group_by(Group,year) %>% summarise(mean(irrigate_hr),n())%>% mutate(across(is.numeric, round, 2))

rm(df.match,match.it,df)
rm(lso , lso_M)