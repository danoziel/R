
R_Lands_Baseline_2017_ %>%
  select(1,2,8,12) %>% 
  filter(TC==1) %>%
  group_by(season,household_questionnaire_id) %>%
  summarise(total_irri=sum(irrigated_out_of_tot_land_cult)) %>% 
  group_by(season) %>% 
  summarise(mean(total_irri)*0.0338)
  

S_Lands_Baseline_2017_ <- R_Lands_Baseline_2017_
S_Lands_I_Baseline_2017_ <- R_Lands_I_Baseline_2017_

saveRDS(S_Lands_Baseline_2017_, "S_Lands_Baseline_2017_.rds")
load(file = "S_Lands_Baseline_2017_.rda")
write.dta(S_Lands_I_Baseline_2017_, "S_Lands_I_Baseline_2017_.dta")
library(foreign)

