#total_land_cultivated----
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

#Irrigation ratio----
#season
ses <- R.Lands_Baseline_2018_ %>% select(total_land_cultivated,season,Ir_Retio) %>%
  filter(Ir_Retio<=1 & total_land_cultivated>0)%>% 
  group_by(season) %>%
  summarise(mean_area= mean(total_land_cultivated),
            mean_irri = mean(Ir_Retio)
  )

#year
R.Lands_Baseline_2018_ %>% select(total_land_cultivated,season,Ir_Retio) %>%
  filter(Ir_Retio<=1 & total_land_cultivated>0)%>% 
  summarise(count = n(),
            mean_area= mean(total_land_cultivated),
            mean_irri = mean(Ir_Retio),SD=sd(Ir_Retio)
  )

#crop
ses <- R.Lands_Baseline_2018_ %>% select(total_land_cultivated,season,Ir_Retio) %>%
  filter(Ir_Retio<=1 & total_land_cultivated>0)%>% 
  group_by(season) %>%
  summarise(count = n(),
            mean_area= mean(total_land_cultivated),
            mean_irri = mean(Ir_Retio),SD=sd(Ir_Retio)
  )



