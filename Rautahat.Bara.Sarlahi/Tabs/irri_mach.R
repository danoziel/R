# -----
x <- R_Agriculture_Baseline_2018_ %>% group_by(household_questionnaire_id,TreatmentControl) %>%
  filter(!is.na(irri_for_season)) %>% summarise(total_irri=sum(irri_for_season)) %>% filter(total_irri>0) %>% 
  filter(TreatmentControl=="Control")

xx <- R_Agriculture_Baseline_2018_ %>% group_by(household_questionnaire_id,TreatmentControl) %>%
  filter(!is.na(irri_for_season)) %>% summarise(total_irri=sum(irri_for_season)) %>% filter(total_irri>0) %>% 
  filter(TreatmentControl=="Treatment")

y <- R_Agriculture_Endline_EPC_2019_ %>% group_by(household_questionnaire_id,TreatmentControl) %>%
  filter(!is.na(irri_for_season)) %>% summarise(total_irri=sum(irri_for_season)) %>% filter(total_irri>0) %>% 
  filter(TreatmentControl=="Control") %>% select(1)

yy <- R_Agriculture_Endline_EPC_2019_ %>% group_by(household_questionnaire_id,TreatmentControl) %>%
  filter(!is.na(irri_for_season)) %>% summarise(total_irri=sum(irri_for_season)) %>% filter(total_irri>0) %>% 
  filter(TreatmentControl=="Treatment") %>% select(1)


xy <- inner_join(x,y) %>% bind_rows(xx) %>%
  arrange(desc(total_irri)) %>%
  filter(TreatmentControl=="Treatment" |
           household_questionnaire_id %in% c("T305404025","T309306011","T304706130","T308601105",
                                             "T301808002","T302602008","T301503081","T304905099",
                                             "T309009013","T309304113","T302608032","T305404026",
                                             "T307103092","T307005097","T303806076","T304702127",
                                             "T301503073","T303603009","T304905101","T300509098",
                                             "T301502070","T303806078","T304901100","T307707110"))

d <- xy %>% group_by(TreatmentControl) %>% summarise(mean(total_irri),n())

# endline
trt <- xy %>% filter(TreatmentControl=="Treatment")
ctrl <- xy %>% filter(household_questionnaire_id %in% c("T305404025","T309306011","T304706130","T308601105",
                                                  "T301808002","T302602008","T301503081","T304905099",
                                                  "T309009013","T309304113","T302608032","T305404026",
                                                  "T307103092","T307005097","T303806076","T304702127",
                                                  "T301503073","T303603009","T304905101","T300509098",
                                                  "T301502070","T303806078","T304901100","T307707110"))
bind_cols(trt,ctrl) %>% inner_join(yy) %>% summarise(mean(total_irri),mean(total_irri1))

# time to irrigate 1 hectare----
x <- R_Agriculture_Baseline_2018_ %>% group_by(household_questionnaire_id,TreatmentControl) %>%
  filter(!is.na(hrs_irr_1katha)) %>% summarise(hr_irri=mean(hrs_irr_1katha/0.0338)) %>%
  filter(TreatmentControl=="Control") summarise(mean(hr_irri))

xx <- R_Agriculture_Baseline_2018_ %>% group_by(household_questionnaire_id,TreatmentControl) %>%
  filter(!is.na(hrs_irr_1katha)) %>% summarise(hr_irri=mean(hrs_irr_1katha)/0.0338)%>%
  filter(hr_irri>0,TreatmentControl=="Treatment")

y <- R_Agriculture_Endline_EPC_2019_ %>% group_by(household_questionnaire_id,TreatmentControl) %>%
  filter(!is.na(hrs_irr_1katha)) %>% summarise(hr_irri=mean(hrs_irr_1katha)/0.0338) %>% 
  filter(TreatmentControl=="Control") %>% select(1)

yy <- R_Agriculture_Endline_EPC_2019_ %>% group_by(household_questionnaire_id,TreatmentControl) %>%
  filter(!is.na(hrs_irr_1katha)) %>% summarise(hr_irri=mean(hrs_irr_1katha)/0.0338) %>% 
  filter(TreatmentControl=="Treatment") %>% select(1)


xy <- inner_join(x,y) %>% bind_rows(xx) %>%
  arrange(desc(hr_irri)) %>%
  filter(TreatmentControl=="Treatment" |
           household_questionnaire_id %in% c("T301502069","T302409104","T304707109","T304901100",
                                             "T304906102","T305004088","T307005097","T309504024",
                                             "T307808108","T307003117","T307103093","T307705125",
                                             "T307707110","T308601105","T300409001","T303608010",
                                             "T305404026","T309009013","T309304113","T309306011",
                                             "T304706131","T300607023","T300401028","T305602001",
                                             "T302608033"))

d <- xy %>% group_by(TreatmentControl) %>% summarise(mean(hr_irri),n())

# endline
trt <- xy %>% filter(TreatmentControl=="Treatment")
ctrl <- xy %>% filter(household_questionnaire_id %in% c("T301502069","T302409104","T304707109","T304901100",
                                                        "T304906102","T305004088","T307005097","T309504024",
                                                        "T307808108","T307003117","T307103093","T307705125",
                                                        "T307707110","T308601105","T300409001","T303608010",
                                                        "T305404026","T309009013","T309304113","T309306011",
                                                        "T304706131","T300607023","T300401028","T305602001",
                                                        "T302608033"))
xxy <- bind_cols(trt,ctrl) %>% inner_join(yy) %>% summarise(mean(hr_irri),mean(hr_irri1))

