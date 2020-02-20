#practice_aquaculture----
pq<-table(Aquaculture_Baseline_2018_$practice_aquaculture)
prop.table(pq)
# 1 Yes # 2 No #

#-ponds_cul_fish----
pcf<-table(Aquaculture_Baseline_2018_$ponds_cul_fish)
prop.table(pcf)[c("1","2")]
mean(ponds_cul_fish,na.rm = T)

table(Aquaculture_Baseline_2018_$ponds_cul_fish)

# total_area_of_pond----
table(Aquaculture_Baseline_2018_$total_area_of_pond)
summary(total_area_of_pond)
taop<-total_area_of_pond[-133]
summary(taop)

#Do all ponds have sufficient water for aquaculture throughout the year?----
swa<-table(Aquaculture_Baseline_2018_$suff_water_aquaculture)
prop.table(swa)
# 1 Yes # 2 No #

#Is a WEM already in use for filling and draining the ponds?----
table(Aquaculture_Baseline_2018_$wem_used_forpond)
#-------
table(Aquaculture_Baseline_2018_$type_wem_aquaculture) # לברר ערכים
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




