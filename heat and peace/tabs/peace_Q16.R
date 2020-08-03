# ------
library(tidyverse)
library(data.table)

class(peace_index$date)
peace_index$date <-  as.Date(peace_index$date, "%Y-%m-%d")

class(peace_index$date1)
peace_index$date1 <- as.numeric(peace_index$date1)

class(peace_index$oslobl)
class(peace_index$oslosp)

class(peace_index$negot_sp)
peace_index$negot_sp <- as.numeric(peace_index$negot_sp)

class(peace_index$negot_bl)
peace_index$negot_bl <- as.numeric(peace_index$negot_bl)

class(peace_index$peacesp)

class(peace_index$gender)
peace_index$gender <- as.numeric(peace_index$gender)

class(peace_index$age)

class(peace_index$relig )



political_spectrum,relig,incom

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

# political spectrum

peace_Q16$political_spectrum <- rep(0)

class(X17_05_2011_2913$date)

#X17_05_2011_2913----xx17_5_11----

X17_05_2011_2913$date <- "2011-05-17"
class(X17_05_2011_2913$date)
X17_05_2011_2913$date <-  as.Date(X17_05_2011_2913$date, "%Y-%m-%d")

X17_05_2011_2913$date1 <- rep(205)
class(X17_05_2011_2913$date1)

X17_05_2011_2913$survey_year <- rep(2011)

X17_05_2011_2913$oslosp <- as.numeric(NA)
class(X17_05_2011_2913$oslosp)

X17_05_2011_2913$oslobl <- as.numeric(NA)
class(X17_05_2011_2913$oslobl)

X17_05_2011_2913 <- X17_05_2011_2913 %>% 
  rename(negot_sp = q1,
         negot_bl = q2) 

class(X17_05_2011_2913$negot_sp)
X17_05_2011_2913$negot_sp <- as.numeric(X17_05_2011_2913$negot_sp)
X17_05_2011_2913$negot_bl <- as.numeric(X17_05_2011_2913$negot_bl)

X17_05_2011_2913$peacesp <- as.numeric(NA)
X17_05_2011_2913$peacebl <- as.numeric(NA)
class(X17_05_2011_2913$peacebl)

class(X17_05_2011_2913$q25)
class(X17_05_2011_2913$gender)

X17_05_2011_2913 <- X17_05_2011_2913 %>% 
  rename(gender = q23,
         age = q24,
         educ = q26,
         political_spectrum = q22,
         relig = q25,
         incom = q30)

X17_05_2011_2913$incom <- as.numeric(X17_05_2011_2913$incom)
X17_05_2011_2913$gender <- as.numeric(X17_05_2011_2913$gender)

xx17_5_11 <- X17_05_2011_2913 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,educ,political_spectrum,relig,incom)

xx17_5_11 <- xx17_5_11 %>% filter(relig<9)

# X29_11_12----
xx29_11_12 <- X29_11_12 %>% 
  rename(negot_sp = Q1,
         negot_bl = Q2,
         gender = Q1c,
         age = Q18,
         educ = Q21,
         political_spectrum = Q15,
         relig = Q19,
         incom = Q26)

xx29_11_12$date <- "2012-11-29"
class(xx29_11_12$date)
xx29_11_12$date <-  as.Date(xx29_11_12$date, "%Y-%m-%d")

xx29_11_12$date1 <- rep(220.5)
class(xx29_11_12$date1)

xx29_11_12$survey_year <- rep(2012)
class(xx29_11_12$survey_year)

xx29_11_12$oslosp <- as.numeric(NA)
xx29_11_12$oslobl <- as.numeric(NA)
xx29_11_12$peacesp <- as.numeric(NA)
xx29_11_12$peacebl <- as.numeric(NA)
class(xx29_11_12$oslosp)

xx29_11_12 <- xx29_11_12 %>% 
  select(date,date1,survey_year,oslosp,oslobl,negot_sp,negot_bl,
         peacesp,peacebl,gender,age,educ,political_spectrum,relig,incom)
