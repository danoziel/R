
filter(!is.na(irri_for_season),name_of_crop %in%
         c("PADDY","WHEAT","SUGARCANE","MAIZE","RED LENTIL"))

rowwise() %>% # ignor NA in dplyr

mutate(x= sub(NaN, NA, x)) %>% #replace NaN to Na, x=col
  