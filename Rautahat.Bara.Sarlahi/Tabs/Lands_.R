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
ses <- R.Lands_Baseline_2018_ %>%
  select(household_questionnaire_id,total_land_cultivated,Ir_Retio) %>%
  filter(Ir_Retio<=1 & total_land_cultivated>0)%>% 
  group_by(household_questionnaire_id) %>%
  summarise(count = n(),m_t=mean(total_land_cultivated),m_irri = mean(Ir_Retio)) %>% 
  summarise(n(),mean(m_t), mean(m_irri))

#Season
ses <- R.Lands_Baseline_2018_ %>% select(total_land_cultivated,season,Ir_Retio) %>%
  filter(Ir_Retio<=1 & total_land_cultivated>0)%>% 
  group_by(season) %>%
  summarise(n(),mean(total_land_cultivated),mean(Ir_Retio))

# TC
#[4.9]Irrigated area out of total land cultivated = $Ir_Retio ----
Treats <- subset(R.Lands_Baseline_2018_,  TC == 1)

# by HH / Year-Treats
ses <- Treats %>%
  select(household_questionnaire_id,total_land_cultivated,Ir_Retio) %>%
  filter(Ir_Retio<=1 & total_land_cultivated>0)%>% 
  group_by(household_questionnaire_id) %>%
  summarise(count = n(),m_t=mean(total_land_cultivated),m_irri = mean(Ir_Retio)) %>% 
  summarise(n(),mean(m_t), mean(m_irri))

#season-Treats
ses <- Treats %>% select(total_land_cultivated,season,Ir_Retio) %>%
  filter(Ir_Retio<=1 & total_land_cultivated>0)%>% 
  group_by(season) %>%
  summarise(n(),mean(total_land_cultivated),mean(Ir_Retio))
