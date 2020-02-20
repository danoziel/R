#name_of_crop----
table(Agriculture_Baseline_2018_$name_of_crop)
table(R.Agriculture_Baseline_2018_$name_of_crop)

# Replace values in the data frame---- 
R.Agriculture_Baseline_2018_$name_of_crop[
  R.Agriculture_Baseline_2018_$name_of_crop == "paddy"] <- "PADDY"

R.Agriculture_Baseline_2018_$name_of_crop[
  R.Agriculture_Baseline_2018_$name_of_crop == "SUAGRCANE"] <- "SUGARCANE"

R.Agriculture_Baseline_2018_$name_of_crop[
  R.Agriculture_Baseline_2018_$name_of_crop == "RED LENTLE"] <- "RED LENTIL"

R.Agriculture_Baseline_2018_$name_of_crop[
  R.Agriculture_Baseline_2018_$name_of_crop == "RED LENTI"] <- "RED LENTIL"




# How much of the cultivated area was under this crop in %----
#name_of_crop
ses <- R.Agriculture_Baseline_2018_ %>%
  select(name_of_crop,cult_area_under_crop) %>% 
  group_by(name_of_crop) %>%
  filter( name_of_crop %in%
            c("PADDY","WHEAT","SUGARCANE","MAIZE","RED LENTIL")) %>%
  summarise(count= n(),
            mean = mean(cult_area_under_crop,na.rm = T),
            per=sum(cult_area_under_crop,na.rm = T)/
              sum(R.Agriculture_Baseline_2018_$cult_area_under_crop,na.rm = T)
  )

#name_of_crop / season_of_crop
ses <- R.Agriculture_Baseline_2018_ %>%
  select(name_of_crop,cult_area_under_crop,season_of_crop) %>% 
  group_by(season_of_crop ,name_of_crop) %>%
  filter( name_of_crop %in%
            c("PADDY","WHEAT","SUGARCANE","MAIZE","RED LENTIL")) %>%
  summarise(count= n(),
            mean = mean(cult_area_under_crop,na.rm = T),
            per=sum(cult_area_under_crop)/
              sum(R.Agriculture_Baseline_2018_$cult_area_under_crop,na.rm = T)
  )

#season_of_crop----
ses <- R.Agriculture_Baseline_2018_ %>%
  select(cult_area_under_crop,season_of_crop) %>% 
  group_by(season_of_crop) %>%
  summarise(count= n(),
            mean = mean(cult_area_under_crop,na.rm = T),
            per=sum(cult_area_under_crop)/
              sum(R.Agriculture_Baseline_2018_$cult_area_under_crop,na.rm = T)
  )


#Monsoon  Winter  Summer  Annual 
#   1       2       3       4 


#when_is_crop_sowed----
wics<-table(Agriculture_Baseline_2018_$when_is_crop_sowed)
prop.table(wics)[c("3","7","8")]
barplot(prop.table(wics)[c("3","7","8")])


#when_is_crop_harvested----
wich<-table(Agriculture_Baseline_2018_$when_is_crop_harvested)
prop.table(wich)[c("8","11","12")]
barplot(prop.table(wich)[c("8","11","12")])

#cult_area_under_crop----
Agriculture_Baseline_2018_%>% filter(cult_area_under_crop>0) %>% 
  summarise(count= n(),
            mean = mean(cult_area_under_crop),
            mean=mean(cult_area_under_crop),
            median=median(cult_area_under_crop),
            SD=sd(cult_area_under_crop),
            min=min(cult_area_under_crop),
            max=max(cult_area_under_crop)
            )

#cult_area_under_crop by CR----
Agriculture_Baseline_2018_%>% group_by(name_of_crop) %>%
  summarise(
    count= n(),
    mean = mean(cult_area_under_crop),
    sd = sd(cult_area_under_crop)
    )


#most_imp_source----
mis<-table(Agriculture_Baseline_2018_$most_imp_source)[c(-1)]
prop.table(mis)
barplot(prop.table(mis))
#Rain    Groundwater   River   Canal 
# 1          2           3       4           


#irrigation for one crop season IN HOURS
#yaer
R.Agriculture_Baseline_2018_ %>%
  select(no_ir_hr_cropseason,cult_area_under_crop,season_of_crop) %>% 
  filter(cult_area_under_crop>0,
         no_ir_hr_cropseason>0,
         !is.na(season_of_crop)) %>% 
  summarise(count = n(),
            mean=mean(no_ir_hr_cropseason),
            min=min(no_ir_hr_cropseason),
            max=max(no_ir_hr_cropseason)
  )
# season
ses <- R.Agriculture_Baseline_2018_ %>%
  select(no_ir_hr_cropseason,cult_area_under_crop,season_of_crop) %>% 
  filter(cult_area_under_crop>0,
         no_ir_hr_cropseason>0,
         !is.na(season_of_crop)) %>% 
  group_by(season_of_crop) %>% 
  summarise(count = n(),
            mean=mean(no_ir_hr_cropseason),
            min=min(no_ir_hr_cropseason),
            max=max(no_ir_hr_cropseason)
  )

#crop
ses <- R.Agriculture_Baseline_2018_ %>%
  select(name_of_crop,no_ir_hr_cropseason,cult_area_under_crop) %>% 
  filter(cult_area_under_crop>0,
         no_ir_hr_cropseason>0,
         name_of_crop %in% c("RED LENTIL","PADDY","WHEAT","SUGARCANE","MAIZE")) %>% 
  group_by(name_of_crop) %>% 
  summarise(count = n(),
            mean=mean(no_ir_hr_cropseason),
            min=min(no_ir_hr_cropseason),
            max=max(no_ir_hr_cropseason)
  )


#season / crop
ses<- R.Agriculture_Baseline_2018_ %>%
  select(name_of_crop,no_ir_hr_cropseason,cult_area_under_crop,season_of_crop) %>% 
  filter(cult_area_under_crop>0,
         no_ir_hr_cropseason>0,
         name_of_crop %in% c("RED LENTIL","PADDY","WHEAT","SUGARCANE","MAIZE")) %>% 
  group_by(season_of_crop,name_of_crop) %>% 
  summarise(count = n(),
            mean=mean(no_ir_hr_cropseason),
            min=min(no_ir_hr_cropseason),
            max=max(no_ir_hr_cropseason)
  )

#what_was_your_total_harvest-דרוש תיקון לפי יחידות----
summary(what_was_your_total_harvest)

#----Maund=37.3242 KG----

Agriculture_Baseline_2018_$unit_harvest
Agriculture_Baseline_2018_$percent_for_selling
Agriculture_Baseline_2018_$price_per_kg
