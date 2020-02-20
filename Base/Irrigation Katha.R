# irrigation for 1 katha by TreatmentControl-p-value =0.1335 ----
how_much_ir<- agri_TC_18 %>% filter(!is.na(ir_katha))

how_much_ir %>% group_by(TreatmentControl) %>%
  summarise(
  count= n(),
  mean = mean(ir_katha),
  sd = sd(ir_katha))

#t test:  p-value = 0.1335
t.test(how_much_ir$ir_katha~ how_much_ir$TreatmentControl,
       var.equal = T)

#irrigation for 1 katha - group_by: crop,TreatmentControl----
tbl_how_much_ir18<-how_much_ir%>% group_by(name_of_crop,TreatmentControl) %>%
  summarise(
  count= n(),
  mean = mean(ir_katha),
  sd = sd(ir_katha))

# PADDY	paddy-p-value = 0.0003789----	
Pa<-how_much_ir%>% group_by(name_of_crop,TreatmentControl) %>% 
  filter(name_of_crop=="PADDY")

# ttest -PADDY- p-value = 0.0003789
t.test(Pa$ir_katha ~ Pa$TreatmentControl, var.equal = T)

# WHEAT-p-value = 0.0258----	
Wh<-how_much_ir%>% group_by(name_of_crop,TreatmentControl) %>% 
  filter(name_of_crop=="WHEAT")

# ttest -WHEAT- p-value = 0.0258
t.test(Wh$ir_katha ~ Wh$TreatmentControl, var.equal = T)






# RED LENTIL	RED LENTI		RED LENTLE----		
# MAIZE	----
# SUGARCANE  SUAGRCANE----

