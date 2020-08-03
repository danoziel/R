library(tidyverse)

# X04_2018-----
xx04_2018 <- X04_2018 %>%
  rename(negot_sp = Q5,
         negot_bl = Q6,
         gender = Q1c,
         age =Q32,
         educ =Q37,
         political_spectrum =Q30 ,
         relig =Q3c ,
         incom =Q36 )

xx04_2018$age_range <- Peace_Index_April_2018_weighted_jews$Q2c

xx04_2018$month <- 04

xx00_0000$date <- "0000-00-00"
class(xx00_0000$date)
xx00_0000$date <-  as.Date(xx00_0000$date, "%Y-%m-%d")

xx00_0000$date1 <- rep(000)
class(xx00_0000$date1)

xx04_2018$survey_year <- rep(2018)
class(xx04_2018$survey_year)

xx04_2018$oslosp <- as.numeric(NA)
xx04_2018$oslobl <- as.numeric(NA)
xx04_2018$peacesp <- as.numeric(NA)
xx04_2018$peacebl <- as.numeric(NA)
class(xx04_2018$oslosp)

xx04_2018 <- xx04_2018 %>% 
  select(month,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,educ,political_spectrum,relig,incom,)

xx04_2018 <- xx04_2018 %>% 
  select(month,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)


# X06_2018 ----
xx06_2018 <- X06_2018 %>%filter(!is.na(Q3c)) %>%
  rename(negot_sp = Q5,
         negot_bl = Q6,
         gender = Q1c,
         age_range =Q2c ,
         educ =Q32 ,
         political_spectrum =Q25 ,
         relig = Q3c,
         incom =Q31 )

xx06_2018$age <- as.numeric(NA)
xx06_2018$month <-as.numeric(06)
xx06_2018$survey_year <- rep(2018)
class(xx06_2018$survey_year)

xx06_2018$oslosp <- as.numeric(NA)
xx06_2018$oslobl <- as.numeric(NA)
xx06_2018$peacesp <- as.numeric(NA)
xx06_2018$peacebl <- as.numeric(NA)
class(xx06_2018$oslosp)

xx06_2018 <- xx06_2018 %>% 
  select(month,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)
rm(X06_2018)


# X07_2018----
xx07_2018 <- X07_2018 %>% filter(!is.na(Q3c)) %>% 
  rename(negot_sp = Q5,
       negot_bl = Q6,
       gender = Q1c,
       age_range =Q2c ,
       educ =Q35 ,
       political_spectrum =Q28 ,
       relig = Q3c,
       incom =Q34 )


xx07_2018$month <- 07

xx07_2018$age <- as.numeric(NA)

xx07_2018$survey_year <- rep(2018)
class(xx04_2018$survey_year)

xx07_2018$oslosp <- as.numeric(NA)
xx07_2018$oslobl <- as.numeric(NA)
xx07_2018$peacesp <- as.numeric(NA)
xx07_2018$peacebl <- as.numeric(NA)
class(xx07_2018$oslosp)

xx07_2018 <- xx07_2018 %>% 
  select(month,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)
rm(X07_2018)

# X07_2017 --- 2017_07_25----

xx07_2017 <- X07_2017 %>% filter(!is.na(relig)) 

xx07_2017$month <- 07

xx07_2017$age <- as.numeric(NA)

xx07_2017$survey_year <- rep(2018)
class(xx04_2018$survey_year)

xx07_2017$oslosp <- as.numeric(NA)
xx07_2017$oslobl <- as.numeric(NA)
xx07_2017$peacesp <- as.numeric(NA)
xx07_2017$peacebl <- as.numeric(NA)
class(xx07_2017$oslosp)

xx07_2017 <- xx07_2017 %>% 
  select(month,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)


# 2017-08-30  ---- X08_2017----

xx08_2017 <- X08_2017 %>% filter(!is.na(Q3C)) %>% 
  rename(negot_sp =Q5 ,
         negot_bl =Q6 ,
         gender =Q1C ,
         age =Q33 ,
         age_range =Q2C ,
         educ =Q38 ,
         political_spectrum =Q31 ,
         relig =Q3C ,
         incom =Q37 )

xx08_2017$month <- 08

xx08_2017$survey_year <- rep(2018)
class(xx08_2017$survey_year)

xx08_2017$oslosp <- as.numeric(NA)
xx08_2017$oslobl <- as.numeric(NA)
xx08_2017$peacesp <- as.numeric(NA)
xx08_2017$peacebl <- as.numeric(NA)
class(xx08_2017$oslosp)

xx08_2017 <- xx08_2017 %>% 
  select(month,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)

# X10_2018----

xx10_2018 <- X10_2018 %>% filter(!is.na(Q3c)) %>% 
  rename(negot_sp = Q5,
         negot_bl = Q6,
         gender = Q1c,
         age_range =Q2c ,
         educ =Q32 ,
         political_spectrum =Q25 ,
         relig = Q3c,
         incom =Q31 )

xx10_2018$age <- as.numeric(NA) 

xx10_2018$month <- 10

xx10_2018$survey_year <- rep(2018)
class(xx10_2018$survey_year)

xx10_2018$oslosp <- as.numeric(NA)
xx10_2018$oslobl <- as.numeric(NA)
xx10_2018$peacesp <- as.numeric(NA)
xx10_2018$peacebl <- as.numeric(NA)
class(xx10_2018$oslosp)

xx10_2018 <- xx10_2018 %>% 
  select(month,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)

# X11_2018----
xx11_2018 <- X11_2018 %>% filter(!is.na(Q3c)) %>% 
  rename(negot_sp = Q5,
       negot_bl = Q6,
       gender = Q1c,
       age_range =Q2c ,
       educ =Q33 ,
       political_spectrum =Q25 ,
       relig = Q3c,
       incom =Q26 )

xx11_2018$age <- as.numeric(NA) 

xx11_2018$month <- 11

xx11_2018$survey_year <- rep(2018)
class(xx11_2018$survey_year)

xx11_2018$oslosp <- as.numeric(NA)
xx11_2018$oslobl <- as.numeric(NA)
xx11_2018$peacesp <- as.numeric(NA)
xx11_2018$peacebl <- as.numeric(NA)
class(xx11_2018$oslosp)

xx11_2018 <- xx11_2018 %>% 
  select(month,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,age_range,educ,political_spectrum,relig,incom,)



# -----
rename(negot_sp = ,
       negot_bl = ,
       gender = ,
       age = ,
       educ = ,
       political_spectrum = ,
       relig = ,
       incom = )