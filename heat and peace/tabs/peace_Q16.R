x <- peace_index %>% 
  group_by(educ) %>% 
  summarise(mean(peacesp,na.rm = T))
  
library(data.table)
peace_Q16 <- peace_index %>%
  select(date,date1,survey_year,
         oslosp,oslobl,negot_sp,negot_bl,peacesp,peacebl,
         gender,age,educ)
peace_Q16$oslosp [is.na(peace_Q16$oslosp)] <- 0 #replace NA to 0
peace_Q16$oslobl [is.na(peace_Q16$oslobl)] <- 0

peace_Q16$negot_sp [is.na(peace_Q16$negot_sp)] <- 0
peace_Q16$negot_bl [is.na(peace_Q16$negot_bl)] <- 0

peace_Q16$peacesp [is.na(peace_Q16$peacesp)] <- 0
peace_Q16$peacebl [is.na(peace_Q16$peacebl)] <- 0





Q1 <- peace_Q1Q2[,1:2] %>% group_by(date,oslosp) %>%
  summarise(n = n()) %>%  mutate(freq=n/sum(n)) %>% mutate(i=oslosp*freq) %>% 
  group_by(date) %>% summarise(q1=sum(i))