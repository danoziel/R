
S_Agriculture_Baseline_2017_ <-inner_join(Agriculture_Baseline_2017_,Control_and_treatment_4_districts) %>%   
  filter(!household_questionnaire_id %in% c("T106006001", "T107609001","T106006002")) %>% 
  mutate(irri_for_season=cult_area_under_crop*hrs_irr_1katha*no_of_irrigation_for_1_katha) %>% 
  mutate( name_of_crop_detail = name_of_crop ) 

S_Agriculture_Baseline_2017_$name_of_crop[S_Agriculture_Baseline_2017_$name_of_crop %in%
                                            c("BELL PEPPER","BITTER GOURD","BOTTLE GOURD","BRINJAL",
                                              "CABBAGE","CAULIFLOWER","CHILLIES","CORIANDER","CUCUMBER",
                                              "CUUCMBER","GARLIC","GREEN PEA","GREENLEAFY VEGETABLE",
                                              "OKRA","ONIONS","PEA","POINTED GOURD", "POTAT0",
                                              "POTATO","SPONGE GOURD","TOMATO", "YARD LONG BEANS"
                                            )]<- "vegetables"
S_Agriculture_Baseline_2017_$name_of_crop[S_Agriculture_Baseline_2017_$name_of_crop %in%
                                            c("GRASS PEA","GREEN GRAM","HORSE GRAM","OTHER PULSES",
                                              "RED GRAM","RED LENTIL" )] <- "pulses"
S_Agriculture_Baseline_2017_$name_of_crop[S_Agriculture_Baseline_2017_$name_of_crop %in%
                                            c("LINSEED","MUSTARD","SESAME")] <- "oilseeds"
S_Agriculture_Baseline_2017_$name_of_crop[S_Agriculture_Baseline_2017_$name_of_crop %in%
                                            c("OTHER","OTHER CROPS","OTHER CEREALS")] <- "others"
                                            
