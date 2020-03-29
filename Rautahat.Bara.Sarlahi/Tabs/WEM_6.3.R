#-------------------------------------------------|
#   Water_extraction_mechanism_I_Baseline_2018_   |
#                                                 |
#    No 'treatment' observations in this file     |
#-------------------------------------------------|


# [6.42]To how many farmers did you sell water? 			
table(Water_extraction_mechanism_I_Baseline_2018_$area_served_by_water_selling__s)
table(Water_extraction_mechanism_I_Baseline_2018_$area_served_by_water_selling__m)
table(Water_extraction_mechanism_I_Baseline_2018_$area_served_by_water_selling__w)

# [6.50]Did you buy water for irrigation during Asadh -2072 to Asadh -2073? (1=Yes 2=No)
R.Water_extraction_mechanism_I_Baseline_2018_ %>%
  select(1,23:25) %>% 
  filter(TreatmentControl=="Treatment")

