hmbw<-table(Water_extraction_mechanism_Baseline_2018_$how_many_bore_wells_do_you_ow)
prop.table(hmbw)
summary(Water_extraction_mechanism_Baseline_2018_$how_many_bore_wells_do_you_ow)

table(Water_extraction_mechanism_Baseline_2018_$plot_no_bore_a)

table(Water_extraction_mechanism_Baseline_2018_$constructed_yr_bw_a)

summary(Water_extraction_mechanism_Baseline_2018_$cost_of_construction__bw_a)
table(Water_extraction_mechanism_Baseline_2018_$cost_of_construction__bw_a)

table(Water_extraction_mechanism_Baseline_2018_$fund_the_cost__bw_a)
# 1 Own funds #

summary(Water_extraction_mechanism_Baseline_2018_$depth_of_well__bw_a) #in feet
summary(Water_extraction_mechanism_Baseline_2018_$depth_of_well__bw_b)
summary(Water_extraction_mechanism_Baseline_2018_$depth_of_well__bw_c)

summary(Water_extraction_mechanism_Baseline_2018_$depth_of_water_monsoon_bw_a)
summary(Water_extraction_mechanism_Baseline_2018_$depth_of_water_monsoon_bw_b)

summary(Water_extraction_mechanism_Baseline_2018_$depth_of_water_summer_bw_a)
summary(Water_extraction_mechanism_Baseline_2018_$depth_of_water_summer_bw_b)

table(Water_extraction_mechanism_Baseline_2018_$pump_type__p_1)
table(Water_extraction_mechanism_Baseline_2018_$pump_type__p_2)
# 1 Electric pump & 3 CD pump vs 2 Diesel pump
p_1<-c(44,59,1)
p_2<-c(6,16,1)
pp<-p_1+p_2
prop.table(pp)

tchbw<-table(Water_extraction_mechanism_Baseline_2018_$water_dist_tech__bw_a)
prop.table(tchbw)

#What is the maximum distance to which this BW water reaches, in feet?
mean(Water_extraction_mechanism_Baseline_2018_$max_dist_reached__bw_a+
  Water_extraction_mechanism_Baseline_2018_$max_dist_reached__bw_b+
    Water_extraction_mechanism_Baseline_2018_$max_dist_reached__bw_c,na.rm = T)




