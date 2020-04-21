#[4.8]total_land_cultivated-this data is not in docs 'Nepal SPIP- baseline- REWSSPC'----
#seasons
ses <- R.Lands_Baseline_2018_ %>% select(total_land_cultivated,season) %>% 
  filter(total_land_cultivated>0) %>%
  group_by(season) %>%
  summarise(count= n(),mean = mean(total_land_cultivated),
            sd = sd(total_land_cultivated)
  )
#Year
ses <- R.Lands_Baseline_2018_ %>% select(total_land_cultivated,season) %>%
  filter(total_land_cultivated>0)%>% 
  summarise(count= n(),mean = mean(total_land_cultivated),
            sd = sd(total_land_cultivated)
  )

#[4.9]Irrigated area out of total land cultivated = $Ir_Retio ----
# by HH / Year
as <- R.Lands_Baseline_2018_ %>%
  drop_na(total_land_cultivated,irrigated_out_of_tot_land_cult)%>% 
  group_by(household_questionnaire_id) %>%
  summarise(sum_total=sum(total_land_cultivated),sum_ir=sum(irrigated_out_of_tot_land_cult), n()) %>% 
  mutate(ir.total=sum_ir/sum_total) %>% filter(ir.total<=1) %>% 
  summarise(mean(sum_ir), mean(ir.total),n())

#Season
as <- R.Lands_Baseline_2018_ %>%
  select(household_questionnaire_id,irrigated_out_of_tot_land_cult,season,Ir_Retio) %>%
  filter(Ir_Retio<=1)%>% 
  group_by(season) %>%
  summarise(n(),mean(irrigated_out_of_tot_land_cult),mean(Ir_Retio))




# TC
#[4.9]Irrigated area out of total land cultivated = $Ir_Retio ----
Treatment <- subset(R.Lands_Baseline_2018_,  TC == 1)

# by HH / Year-Treatment
R.Lands_Baseline_2018_ %>% filter(TC == 1) %>% 
select(household_questionnaire_id,irrigated_out_of_tot_land_cult,Ir_Retio) %>%
  filter(Ir_Retio<=1)%>% 
  group_by(household_questionnaire_id) %>%
  summarise(sum_Ir = mean(Ir_Retio), sum_total=sum(irrigated_out_of_tot_land_cult), n()) %>% 
  summarise(mean(sum_Ir), mean(sum_total),n())

#Season -Treatment
R.Lands_Baseline_2018_ %>% filter(TC == 1) %>% 
  select(household_questionnaire_id,irrigated_out_of_tot_land_cult,season,Ir_Retio) %>%
  filter(Ir_Retio<=1)%>% 
  group_by(season) %>%
  summarise(n(),mean(irrigated_out_of_tot_land_cult),mean(Ir_Retio))
