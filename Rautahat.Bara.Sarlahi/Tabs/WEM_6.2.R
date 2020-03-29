
#|---------------------------------------------|#
#|  Water_extraction_mechanism_Baseline_2018_  |#
#|---------------------------------------------|#

# [6.17]What is the type of the pump?----
table(Water_extraction_mechanism_Baseline_2018_$pump_type__p_1) # 104 p for all HH
# 1=Electric pump 
# 2=Diesel pump
# 3= CD pump/Vuplator
# 4= Solar pump

ses <- R.Water_extraction_mechanism_Baseline_2018_ %>%
  filter(!is.na(pump_type__p_1)) %>% 
  group_by(pump_type__p_1) %>% 
  summarise(n(),Percent= n()/104)


# Editing the df:"total liter for a season"----

R.WEM_liter_hr_season <- R.Water_extraction_mechanism_Baseline_2018_ %>% 
  select(1,61:63,103:108,115:120,127:132)

# Replace all 0 values to NA
R.WEM_liter_hr_season[R.WEM_liter_hr_season == 0] <- NA

# rm liters [6.18]: higt amount & NA                      
R.WEM_liter_hr_season <- R.WEM_liter_hr_season %>%
  filter(!is.na(liters_of_fuels_p_hour_p_1)|!is.na(liters_of_fuels_p_hour_p_2),
         liters_of_fuels_p_hour_p_2>0|liters_of_fuels_p_hour_p_1<5)

# rm [6.31 6.35 6.39] hr>24
library(naniar)
R.WEM_liter_hr_season <- R.WEM_liter_hr_season %>%
  replace_with_na(replace = list(hours_per_day_use_s__p_1=c(25,80),
                                 hours_per_day_use_s__p_2=c(40,60),
                                 hours_per_day_use_m__p_1=c(25),
                                 hours_per_day_use_m__p_2=c(40),
                                 hours_per_day_use_w__p_1=c(24) ))

# "total liter for a season" - New VARs
R.WEM_liter_hr_season <- R.WEM_liter_hr_season %>% 
  mutate(p1_s=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_s_p1*hours_per_day_use_s__p_1,
         p2_s=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_s_p2*hours_per_day_use_s__p_2,
         p3_s=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_s_p3*hours_per_day_use_s__p_3) %>%
  mutate(p1_m=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_m_p1*hours_per_day_use_m__p_1,
         p2_m=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_m_p2*hours_per_day_use_m__p_2,
         p3_m=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_m_p3*hours_per_day_use_m__p_3) %>% 
  mutate(p1_w=liters_of_fuels_p_hour_p_1*days_in_a_season_pump_use_w_p1*hours_per_day_use_w__p_1,
         p2_w=liters_of_fuels_p_hour_p_2*days_in_a_season_pump_use_w_p2*hours_per_day_use_w__p_2,
         p3_w=liters_of_fuels_p_hour_p_3*days_in_a_season_pump_use_w_p3*hours_per_day_use_w__p_3)

R.WEM_liter_hr_season[, 23:31][is.na(R.WEM_liter_hr_season[, 23:31])] <- 0 # Replace  NA values to 0 on new VARs

R.WEM_liter_hr_season <- R.WEM_liter_hr_season %>%
  mutate(p123_s=p1_s+p2_s+p3_s, p123_m=p1_m+p2_m+p3_m, p123_w=p1_w+p2_w+p3_w,
         p123_year=p1_s+p2_s+p3_s+p1_m+p2_m+p3_m+p1_w+p2_w+p3_w)%>%
  filter(p123_year>0)

# how many liters of fuel in a season per HH (df: R.WEM_liter_hr_season)----
ses <- R.WEM_liter_hr_season %>%
  summarise(HH=n(),
            monsoon=mean(p123_m), summer=mean(p123_s), winter=mean(p123_w),
            Year=mean(p123_year))

# [6.19] How many horse power (HP)?----
R.Water_extraction_mechanism_Baseline_2018_ %>% 
  select(1,67:69) %>%
  filter(!is.na(horse_power__p_1), horse_power__p_1>0) %>% 
  mutate(HP123 = rowMeans(.[names(.)[2:4]], na.rm = T)) %>% 
  summarise(n(), mean_HP=mean(HP123))

# [6.21]How long does it take to irrigate 1 katha of land with this pump, in min?----				
R.Water_extraction_mechanism_Baseline_2018_ %>% select(1,73:75,142) %>% 
  filter(!is.na(time_to_irrigate_1_katha__p_1), time_to_irrigate_1_katha__p_1>1) %>% 
  mutate(time_irri_katha = rowMeans(.[names(.)[2:4]], na.rm = T)) %>%
  summarise(n(),mean(time_irri_katha))
# [6.26]What were the maintenance costs?-----
ses <- R.Water_extraction_mechanism_Baseline_2018_ %>%
  filter(!is.na(maintenance_costs_p_1)) %>% 
  summarise(HH=n(),
            Sum=sum(sum(maintenance_costs_p_1)+
                      sum(maintenance_costs_p_2,na.rm = T)+
                      sum(maintenance_costs_p_3,na.rm = T)),
            Mean=Sum/HH
  )

# [6.29] Do you sell water from this pump (or rent out) from Falgun 2072 to Falgun 2073?----					
table(R.Water_extraction_mechanism_Baseline_2018_$sell_water_or_rent_out__p_1) #all 18 have fuel pumps

# [6.33] what percentage was for selling?----		
ses <- R.Water_extraction_mechanism_Baseline_2018_ %>% 
  select(1,pump_type__p_1,100:101,112:113,124,125,136,137) %>% 
  filter(!is.na(pump_type__p_1),sell_water_or_rent_out__p_1=="1",
         !(household_questionnaire_id %in% c("T300901113","T300901112"))) %>%
  mutate(avm_sell = rowMeans(.[names(.)[7:8]], na.rm = T),
         avs_sell = rowMeans(.[names(.)[5:6]], na.rm = T),
         avw_sell = rowMeans(.[names(.)[9:10]], na.rm = T)) %>% 
  mutate(avy_sell=rowMeans(.[names(.)[5:10]], na.rm = T)) %>% 
  summarise(mean_per_sell_m=mean(avm_sell),mean_per_sell_s=mean(avs_sell),
            mean_per_sell_w=mean(avw_sell),mean_per_sell_y=mean(avy_sell))

# [6.32]what percentage was for self-use/own plot?----		
ses <- R.Water_extraction_mechanism_Baseline_2018_ %>% 
  select(1,pump_type__p_1,100:101,109:110,121,122,133,134) %>% 
  filter(!is.na(pump_type__p_1),sell_water_or_rent_out__p_1=="1",
         !(household_questionnaire_id %in% c("T300901113","T300901112"))) %>%
  mutate(avm_self = rowMeans(.[names(.)[7:8]], na.rm = T),
         avs_self = rowMeans(.[names(.)[5:6]], na.rm = T),
         avw_self = rowMeans(.[names(.)[9:10]], na.rm = T)) %>% 
  mutate(avy_self=rowMeans(.[names(.)[5:10]], na.rm = T)) %>% 
  summarise(mean_per_self_m=mean(avm_self),mean_per_self_s=mean(avs_self),
            mean_per_self_w=mean(avw_self,na.rm = T),mean_per_self_y=mean(avy_self))






