# cult_area_under_crop TreatmentControl-p-value =0.727 ----
cult_area_crop18<- agri_TC_18 %>% filter(!is.na(cult_area_under_crop))

cult_area_crop18 %>% group_by(TreatmentControl) %>%
  summarise(
    count= n(),
    mean = mean(cult_area_under_crop,na.rm = T),
    sd = sd(cult_area_under_crop,na.rm = T))

#t test:  p-value = 0.727
t.test(cult_area_crop18$cult_area_under_crop~cult_area_crop18$TreatmentControl,
       var.equal = T)
#cult_area_under_crop - group_by: crop,TreatmentControl----
tblcult_area_crop18<-cult_area_crop18%>% group_by(name_of_crop,TreatmentControl) %>%
  summarise(
    count= n(),
    mean = mean(cult_area_under_crop),
    sd = sd(cult_area_under_crop))

# PADDY	paddy-p-value =0.07605 ----	
Pa<-cult_area_crop18%>% group_by(name_of_crop,TreatmentControl) %>% 
  filter(name_of_crop=="PADDY")

# ttest -PADDY- p-value = 0.07605
t.test(Pa$cult_area_under_crop ~ Pa$TreatmentControl, var.equal = T)

# PADDY	paddy-p-value =  0.8244----	
Wh<-cult_area_crop18%>% group_by(name_of_crop,TreatmentControl) %>% 
  filter(name_of_crop=="WHEAT")

# ttest -PADDY- p-value =  0.8244
t.test(Wh$cult_area_under_crop ~ Wh$TreatmentControl, var.equal = T)