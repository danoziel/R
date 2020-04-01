
filter(!is.na(irri_for_season),name_of_crop %in% c("PADDY","WHEAT"))

rowwise() %>% # ignor NA in dplyr

mutate(x= sub(NaN, NA, x)) %>% #replace NaN to Na, x=column
  
Data$TC[Data$TC == 2] <- 1 #replace 2 to 1

land_Treats <- subset(R.Lands_Baseline_2018_,  TC == 1)
colMeans(land_Treats)

Control_and_treatment_4_districts$TC[Control_and_treatment_4_districts$TC == 1] <- 0
Control_and_treatment_4_districts$TC[Control_and_treatment_4_districts$TC == 2] <- 1

