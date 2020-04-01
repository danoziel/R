#practice_aquaculture----
# omit sub.
R.Aquaculture_Baseline_2018_ <-
  filter(Aquaculture_Baseline_2018_,
         household_questionnaire_id !="T300608033",
         household_questionnaire_id != "T306102002")

table(R.Aquaculture_Baseline_2018_$practice_aquaculture)
# 1 Yes # 2 No #

#-ponds_cul_fish----
table(R.Aquaculture_Baseline_2018_$ponds_cul_fish)

# total_area_of_pond----
summary(R.Aquaculture_Baseline_2018_$total_area_of_pond)


#[11.4] Do all ponds have sufficient water for aquaculture throughout the year?----
table(R.Aquaculture_Baseline_2018_$suff_water_aquaculture)  # 1 Yes # 2 No #

group_by(R.Aquaculture_Baseline_2018_,suff_water_aquaculture)%>%
  filter(suff_water_aquaculture=="1") %>% 
  summarise(mean(total_area_of_pond),per=sum(total_area_of_pond,na.rm = T)/
              sum(R.Aquaculture_Baseline_2018_$total_area_of_pond,na.rm = T)
            )

#[11.5] Is a WEM already in use for filling and draining the ponds?----
table(R.Aquaculture_Baseline_2018_$wem_used_forpond)

group_by(R.Aquaculture_Baseline_2018_,wem_used_forpond)%>%
  filter(wem_used_forpond=="1") %>% 
  summarise(mean(total_area_of_pond),per=sum(total_area_of_pond,na.rm = T)/
              sum(R.Aquaculture_Baseline_2018_$total_area_of_pond,na.rm = T)
  )

#-------
table(Aquaculture_Baseline_2018_$type_wem_aquaculture) #  1 Electric / 2   Diesel/ 3 other
table(Aquaculture_Baseline_2018_$average_tofill_pond)
table(Aquaculture_Baseline_2018_$average_time_drainpond)
table(Aquaculture_Baseline_2018_$type_of_wem_aqua_p1) #  1 Electric / 2   Diesel
table(Aquaculture_Baseline_2018_$type_of_wem_aqua_p2)
table(Aquaculture_Baseline_2018_$cost_per_hour_p1)
table(Aquaculture_Baseline_2018_$cost_per_hour_p2)
table(Aquaculture_Baseline_2018_$total_no_of_hours_aqua_p1)
table(Aquaculture_Baseline_2018_$total_no_of_hours_aqua_p2)
table(Aquaculture_Baseline_2018_$annual_cost_aquaculture)
table(Aquaculture_Baseline_2018_$annual_revenue_aquaculture)

#Are you planning to use the SPIP for aquaculture?----
table(Aquaculture_Baseline_2018_$planning_spip_aqua)




