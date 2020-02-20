# SECTION 7.1 - BORE WELLS
hmbw<-table(Water_extraction_mechanism_Baseline_2018_$how_many_bore_wells_do_you_ow)
prop.table(hmbw)
summary(Water_extraction_mechanism_Baseline_2018_$how_many_bore_wells_do_you_ow)

#Water_extraction_mechanism_Baseline_2018_$plot_no_bore_a
#table(Water_extraction_mechanism_Baseline_2018_$constructed_yr_bw_a)

#cost_of_construction__bw_a/b/c
cost_of_construction__3bw <- data.frame(cost_of_construction__bw = unlist( Multiple_dataframe, use.names = FALSE))
cost_of_construction__3bw <- cost_of_construction__3bw[-9,]
summary(cost_of_construction__3bw)

#1=Own funds 2=Bank loan 5=Government subsidy
table(Water_extraction_mechanism_Baseline_2018_$fund_the_cost__bw_a)
table(Water_extraction_mechanism_Baseline_2018_$fund_the_cost__bw_b)
table(Water_extraction_mechanism_Baseline_2018_$fund_the_cost__bw_c)
 
#
table(Water_extraction_mechanism_Baseline_2018_$subsidy__bw_a)


table(Water_extraction_mechanism_Baseline_2018_$diameter_of_outlet_pipe__bw_a) 

summary(Water_extraction_mechanism_Baseline_2018_$depth_of_well__bw_a) #in feet
summary(Water_extraction_mechanism_Baseline_2018_$depth_of_well__bw_b)
summary(Water_extraction_mechanism_Baseline_2018_$depth_of_well__bw_c)

summary(Water_extraction_mechanism_Baseline_2018_$depth_of_water_monsoon_bw_a)
summary(Water_extraction_mechanism_Baseline_2018_$depth_of_water_monsoon_bw_b)

summary(Water_extraction_mechanism_Baseline_2018_$depth_of_water_summer_bw_a)
summary(Water_extraction_mechanism_Baseline_2018_$depth_of_water_summer_bw_b)

Water_extraction_mechanism_Baseline_2018_$more_than_one_pump__bw_a
Water_extraction_mechanism_Baseline_2018_$how_many__bw_a
Water_extraction_mechanism_Baseline_2018_$pump_type__bw_a
Water_extraction_mechanism_Baseline_2018_$water_dist_tech__bw_a
Water_extraction_mechanism_Baseline_2018_$max_dist_reached__bw_a

#SECTION 7.2 - PUMPS
Water_extraction_mechanism_Baseline_2018_$pump_count
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

<- 




