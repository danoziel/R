###-------------------------------------------------------###
###  water- befor and after at the treatment group B&A_T  ###
###-------------------------------------------------------###










#R.Lands_Baseline_2018_ vs Lands_Endline_EPC_2019_----
#[4.9]Irrigated area out of total land cultivated = $Ir_Retio
Treatment <- subset(R.Lands_Endline_EPC_2019_,  TC == 1)

# by HH / Year-Treats

as <- Treatment %>%
  select(household_questionnaire_id,irrigated_out_of_tot_land_cult,Ir_Retio) %>%
  filter(Ir_Retio<=1)%>% 
  group_by(household_questionnaire_id) %>%
  summarise(sum_Ir = mean(Ir_Retio), sum_total=sum(irrigated_out_of_tot_land_cult), n()) %>% 
  summarise(n(),mean(sum_total),mean(sum_Ir) )

#Season -Treatment
as <- Treatment %>%
  select(household_questionnaire_id,irrigated_out_of_tot_land_cult,season,Ir_Retio) %>%
  filter(Ir_Retio<=1)%>% 
  group_by(season) %>%
  summarise(n(),mean(irrigated_out_of_tot_land_cult),mean(Ir_Retio))




