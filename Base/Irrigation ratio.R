#Irrigation ratio By TreatmentControl p-value = 0.004418----
irrigation_ratio18<-land_TC_18 %>% filter(Ir_Retio<=1,season!="ANNAUL 2074-75")

#summarise- Irrigation ratio/CT
irrigation_ratio18 %>% 
  group_by(TreatmentControl) %>% 
  summarise(
    count = n(), 
    mean = mean(Ir_Retio),
    sd = sd(Ir_Retio))

#t test:Ir_Retio/TC p-value = 0.004418
t.test(irrigation_ratio18$Ir_Retio~irrigation_ratio18$TreatmentControl)

#Irrigation ratio/CT/season----
#table all seasons
tbl_Irrigation_ratio18<-irrigation_ratio18%>% group_by(season,TreatmentControl) %>%
  summarise(
    count = n(),
    mean = mean(Ir_Retio),
    sd = sd(Ir_Retio))

#monsoon p-value = 0.004061----
mi<-irrigation_ratio18%>% group_by(season,TreatmentControl) %>%
  filter(season=="MONSOON 2074")

# ttest -"MONSOON 2074"- p-value = 0.004061
t.test(mi$Ir_Retio~ mi$TreatmentControl, var.equal = T)

# winter p-value = 0.6432----
mw<-irrigation_ratio18%>% group_by(season,TreatmentControl) %>%
  filter(season=="WINTER 2074") 

# ttest -WINTER 2074- p-value = 0.6432
t.test(mw$Ir_Retio~ mw$TreatmentControl, var.equal = T)

