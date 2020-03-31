
filter(!is.na(irri_for_season),name_of_crop %in% c("PADDY","WHEAT"))

rowwise() %>% # ignor NA in dplyr

mutate(x= sub(NaN, NA, x)) %>% #replace NaN to Na, x=column
  
Data$TC[Data$TC == 2] <- 1 #replace 2 to 1

land_Treats <- subset(R.Lands_Baseline_2018_,  TC == 1)
colMeans(land_Treats)

R.Agriculture_Baseline_2018_$TC[R.Agriculture_Baseline_2018_$TC == 1] <- 0
R.Agriculture_Baseline_2018_$TC[R.Agriculture_Baseline_2018_$TC == 2] <- 1

