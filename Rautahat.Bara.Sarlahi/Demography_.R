# family members----
Demography_Baseline_2018_$household_questionnaire_id
hh<-table(Demography_Baseline_2018_$household_questionnaire_id)
mean(hh)

# caste----
Demography_2_Baseline_2018_$which_is_the_caste_of_you_househ
table(Demography_2_Baseline_2018_$which_is_the_caste_of_you_househ)
# 2 Tharu # 5 Yadavs  #



# how many houses do you own?----
aft<-table(Demography_2_Baseline_2018_$apart_from_this_main_house__h)
prop.table(aft)

# source of water for drinking----
wit<-table(Demography_2_Baseline_2018_$what_is_the_main_source_of_wa)
prop.table(wit)
# 1 Tap/Piped # 2 Tubewell/hand pump #

# source of energy for lighting----
sol<-table(Demography_2_Baseline_2018_$hh_source_of_lighting)
prop.table(sol)
# 3 Electricity #

#How many of the following items are owned by the household members?----
hty<-table(Demography_2_Baseline_2018_$b_hand_tubewell)
prop.table(hty)
noc<-table(Demography_2_Baseline_2018_$borewell)
prop.table(noc)
table(Demography_2_Baseline_2018_$treadle_pump)

ep<-table(Demography_2_Baseline_2018_$electric_pump)
prop.table(ep)
dp<-table(Demography_2_Baseline_2018_$diesel_pump)
prop.table(dp)

table(Demography_2_Baseline_2018_$plough) #3.9b Iron Plough
table(Demography_2_Baseline_2018_$power_tiller_tractor)#3.9b Power tiller/tractor
table(Demography_2_Baseline_2018_$threshing_machine)
table(Demography_2_Baseline_2018_$rambha) #Threshing & Husking Machine
table(Demography_2_Baseline_2018_$bullock)
table(Demography_2_Baseline_2018_$motorcycle)


