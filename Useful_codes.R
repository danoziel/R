write.csv(Agriculture_18_19, file = "C:/Users/Dan/Documents/R/Rautahat.Bara.Sarlahi/data/Agriculture_18_19.csv", row.names=FALSE)

# Save the data in different vector
women_weight <- genderweight %>%
  filter(group == "F") %>%
  pull(weight)

# t test
res <- 
  t.test(total_litres_consumed_dieselkero ~ TreatmentControl, data = df.match)



filter(!is.na(irri_for_season),name_of_crop %in% c("PADDY","WHEAT"))

rowwise() %>% # ignor NA in dplyr
  
  drop_na(irri_for_season,season_of_crop)%>% library(tidyverse)

#renam column----
R_intensity_Baseline %>% 
  rename(net_cropped_area = land_for_cultivation) #renam column land_ TO net_

# copy column----
R.Agriculture_Baseline_2018_ <- R.Agriculture_Baseline_2018_ %>%
  mutate( name_of_crop_detail = name_of_crop ) #(copy=original)

Treats_Lands <- subset(R.Lands_Endline_EPC_2019_,  TC == 1) #subset

mutate(across(is.numeric, round, 2)) #Controlling number of decimal digits


# freq by % symbol
mutate(freq = paste0(round(100 * total_land_cultivated/nca, 0), "%"))

mutate(freq = paste0(round(100 * n/sum(n), 0), "%"))

mutate(label_percent()(x))
--------
  
  x$land_cult [is.na(x$land_cult)] <- 0 #replace NA to 0


mutate(x= sub(NaN, NA, x)) %>% #replace NaN to Na, x=column
  
  Data$TC[Data$TC == 2] <- 1 #replace 2 to 1

R_Lands_I_Baseline_2018_[6,3]<- 40

Q1 <- spread(Q1, oslosp, freq) #from rows to column

#split column to two
at_btselem <- at_btselem %>% mutate(total_events_located_IL  = ifelse(location == 1, "1",NA)) %>%
  mutate(total_events_located_yesha  = ifelse(location == 2, "1",NA)) 
#location- old, total_events_located_IL-new


land_Treats <- subset(R.Lands_Baseline_2018_,  TC == 1)
colMeans(land_Treats)

Control_and_treatment_4_districts$TC[Control_and_treatment_4_districts$TC == 1] <- 0
Control_and_treatment_4_districts$TC[Control_and_treatment_4_districts$TC == 2] <- 1

mutate(avm_self = rowMeans(.[names(.)[7:8]], na.rm = T),#mean per row - by defined columns
       
       #-----  frequency in percentage   ----    
       R_Aquaculture_Baseline_2018_ %>% filter(!is.na(practice_aquaculture)) %>%
         group_by(practice_aquaculture,TreatmentControl) %>%summarise(n=n()) %>% 
         mutate(freq = n / sum(n)) #Percentage per sub group
       
       R_Aquaculture_Baseline_2018_ %>% filter(!is.na(practice_aquaculture)) %>%
         count(practice_aquaculture,TreatmentControl) %>%mutate(freq = n / sum(n))#Percentage per group
       
       #   R_Aquaculture_Baseline_2018_ %>%filter(!is.na(practice_aquaculture)) %>%
       #     group_by(practice_aquaculture)%>%summarise(n=n()) %>%mutate(freq = n / sum(n))
       #                                 _||_
       #                                 \  /
       #                                  \/
       R_Aquaculture_Baseline_2018_ %>%filter(!is.na(practice_aquaculture)) %>%
         count(practice_aquaculture) %>%mutate(freq = n / sum(n))
       
       
       
       Data$TC[Data$TC == 2] <- 1 #replace 2 to 1
       R_Agriculture_Baseline_2018_$season_of_crop
       
table(R_Agriculture_Baseline_2018_ %>%group_by(household_questionnaire_id,name_of_crop) %>% count())
# ---------------------------------
library(data.table)
peace_index$date <-  as.Date(peace_index$date, "%Y-%m-%d")
  
date <- seq(as.IDate("1994-01-01"), as.IDate("2019-12-31"), 1))
dt <- data.table(date=date)


summarise_at(c("harvest_KG_talya100","harvest_KG_CONTROL",
               "revenue_TALYA100","revenue_CONTROL"
), sum, na.rm = TRUE)
       

#split column to two
at_btselem <- at_btselem %>% mutate(total_events_located_IL  = ifelse(location == 1, "1",NA)) %>%
  mutate(total_events_located_yesha  = ifelse(location == 2, "1",NA))

peace_index_17_18 <- peace_index_17_18 %>%
  mutate(political_spectrum=case_when(
    party %in% c(2,3,4,5,7,9,10)~1,
    party %in% c(14:24,27:30)~4))


