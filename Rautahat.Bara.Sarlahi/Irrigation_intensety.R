# Irrigation intensety----
# by year
x <- R_Lands_Baseline_2018_ %>% select(1,8) %>%filter(!is.na(irrigated_out_of_tot_land_cult)) %>% 
  group_by(household_questionnaire_id) %>%
  filter(irrigated_out_of_tot_land_cult == max(irrigated_out_of_tot_land_cult)) %>%
  group_by(household_questionnaire_id,irrigated_out_of_tot_land_cult) %>% tally() %>% 
  summarise(NIA=mean(irrigated_out_of_tot_land_cult))
R_Lands_I_Baseline_2018_ <- inner_join(R_Lands_I_Baseline_2018_,x)
xx <- R_Lands_I_Baseline_2018_ %>% select(1,3,41,37,40) %>% mutate(NIA/land_for_cultivation)

mean(xx$`NIA/land_for_cultivation`)
xx %>% mean(`NIA/land_for_cultivation`)
  
