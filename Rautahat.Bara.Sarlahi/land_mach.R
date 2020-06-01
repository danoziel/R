
# ---- R_Lands_ ----
a <- inner_join(R_Lands_I_Baseline_2018_[,c(1,3)],R_Lands_I_Endline_EPC_2019_[,c(1,3,39)],
           by="household_questionnaire_id")

# nerow the 'total_ownland_cultivated' to HH and collect the CONTROL options
gross_land_base <- R_Lands_Baseline_2018_ %>% group_by(household_questionnaire_id) %>% 
  summarise(ownland=sum(total_ownland_cultivated)) %>% filter(ownland>0) 
gross_land_end <- R_Lands_Endline_EPC_2019_ %>% group_by(household_questionnaire_id) %>% 
  summarise(ownland=sum(total_ownland_cultivated)) %>% filter(ownland>0)

CTRL <- inner_join(R_Lands_I_Baseline_2018_[,c(1,3:4,39)],R_Lands_I_Endline_EPC_2019_[,1],
                    by="household_questionnaire_id") %>% inner_join(gross_land_base)%>%
  inner_join(gross_land_end,by="household_questionnaire_id") %>% 
  filter(TreatmentControl=="Control")

TRT <- inner_join(R_Lands_I_Baseline_2018_[,c(1,3:4,39)],R_Lands_I_Endline_EPC_2019_[,1],
                    by="household_questionnaire_id") %>% inner_join(gross_land_base)%>%
  filter(TreatmentControl=="Treatment")

binb_cult <- bind_rows(CTRL,TRT)

binb_cult <- binb_cult[,c(1:4)] %>% arrange(desc(land_for_cultivation))

trt <-binb_cult %>% filter(TreatmentControl=="Treatment")%>% arrange(desc(land_for_cultivation))
ctrl <-binb_cult %>% filter(household_questionnaire_id %in% c("T302606024","T305404026", "T307808108","T300603032",
                                           "T308601105","T303608010","T302607029","T302608026",
                                           "T307705125","T307006094","T309504024","T300401028",
                                           "T300409001","T301805004","T307001095","T302608003",
                                           "T304905099","T304808083","T302607006","T307103092",
                                           "T305105014","T303807077","T307408001","T303906006"))%>%
  arrange(desc(land_for_cultivation))

# total_ownland_cultivated
bt <- trt[,c(1,4)] %>%  inner_join(gross_land_base)
bc <- ctrl[,c(1,4)] %>%  inner_join(gross_land_base)
# baeline
bind_rows(bt,bc) %>% group_by(TreatmentControl) %>% summarise(mean(ownland))
# endline
bind_cols(bt,bc) %>% inner_join(gross_land_end[,1]) %>%
  summarise(Treatment=mean(ownland),Control=mean(ownland1))


# total_land_cultivated
gross_land_base <- R_Lands_Baseline_2018_ %>% group_by(household_questionnaire_id) %>% 
  summarise(land_cultivated=sum(total_land_cultivated)) %>% filter(land_cultivated>0) 
gross_land_end <- R_Lands_Endline_EPC_2019_ %>% group_by(household_questionnaire_id) %>% 
  summarise(land_cultivated=sum(total_land_cultivated)) %>% filter(land_cultivated>0)

bt <- trt[,c(1,4)] %>%  inner_join(gross_land_base)
bc <- ctrl[,c(1,4)] %>%  inner_join(gross_land_base)
# baeline
x <- bind_rows(bt,bc) %>% group_by(TreatmentControl) %>% summarise(mean(land_cultivated),n())
# endline
x <- bind_cols(bt,bc) %>% inner_join(gross_land_end[,1]) %>%
  summarise(Treatment=mean(land_cultivated),Control=mean(land_cultivated1),n())

# baeline-crop intensity
x <- bind_rows(bt,bc)%>%inner_join(binb_cult[,c(1,2)])%>%
  mutate(intensity=land_cultivated/land_for_cultivation*100) %>%
  group_by(TreatmentControl) %>% 
  summarise(mean(intensity),n())
  
# endline-crop intensity
x <- bind_rows(bt,bc)[,1:2] %>% inner_join(gross_land_end) %>%inner_join(binb_cult[,c(1,2)])%>%
  mutate(intensity=land_cultivated/land_for_cultivation*100) %>%
  group_by(TreatmentControl) %>% 
  summarise(mean(intensity),n())
  

# irrigated_out_of_tot_land_cult----
gross_land_base <- R_Lands_Baseline_2018_ %>% group_by(household_questionnaire_id) %>% 
  summarise(irrigated_land=sum(irrigated_out_of_tot_land_cult)) %>% filter(irrigated_land>0) 
x <- gross_land_end <- R_Lands_Endline_EPC_2019_ %>% group_by(household_questionnaire_id) %>% 
  summarise(irrigated_land=sum(irrigated_out_of_tot_land_cult)) %>% filter(irrigated_land>0)

bt <- trt[,c(1,4)] %>%  inner_join(gross_land_base)
bc <- ctrl[,c(1,4)] %>%  inner_join(gross_land_base)
# baeline
x <- bind_rows(bt,bc) %>% group_by(TreatmentControl) %>% summarise(mean(irrigated_land),n())
# endline
bt <- trt[,c(1,4)] %>%  inner_join(gross_land_end)
bc <- ctrl[,c(1,4)] %>%  inner_join(gross_land_end)

# baeline-irrigation intensity
x <- trt[,c(-3)] %>% inner_join(gross_land_base) %>% mutate(intensity=irrigated_land/land_for_cultivation*100) %>% 
  summarise(mean(intensity),n())
x <- ctrl[,c(-3)] %>%inner_join(gross_land_base) %>% mutate(intensity=irrigated_land/land_for_cultivation*100) %>% 
  summarise(mean(intensity),n())
# endline-irrigation intensity
x <- trt[,c(-3)] %>% inner_join(gross_land_end) %>% mutate(intensity=irrigated_land/land_for_cultivation*100) %>% 
  summarise(mean(intensity),n())
x <- ctrl[,c(-3)] %>%inner_join(gross_land_end) %>% mutate(intensity=irrigated_land/land_for_cultivation*100) %>% 
  summarise(mean(intensity),n())



