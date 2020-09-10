



WS2017 <-
  Water_extraction_mechanism_Baseline_2017_ %>%
  select(year,district, household_questionnaire_id,
         days_in_a_season_pump_use_s_p1,hours_per_day_use_s__p_1,
         days_in_a_season_pump_use_s_p2,hours_per_day_use_s__p_2,
         days_in_a_season_pump_use_s_p3,hours_per_day_use_s__p_3,
         days_in_a_season_pump_use_m_p1,hours_per_day_use_m__p_1,
         days_in_a_season_pump_use_m_p2,hours_per_day_use_m__p_2,
         days_in_a_season_pump_use_m_p3,hours_per_day_use_m__p_3,
         days_in_a_season_pump_use_w_p1,hours_per_day_use_w__p_1,
         days_in_a_season_pump_use_w_p2,hours_per_day_use_w__p_2,
         days_in_a_season_pump_use_w_p3,hours_per_day_use_w__p_3)

WS2018 <-
  Water_extraction_mechanism_Midline_2018_ %>%
  select(year,district, household_questionnaire_id,
         days_in_a_season_pump_use_s_p1,hours_per_day_use_s__p_1,
         days_in_a_season_pump_use_s_p2,hours_per_day_use_s__p_2,
         days_in_a_season_pump_use_s_p3,hours_per_day_use_s__p_3,
         days_in_a_season_pump_use_m_p1,hours_per_day_use_m__p_1,
         days_in_a_season_pump_use_m_p2,hours_per_day_use_m__p_2,
         days_in_a_season_pump_use_m_p3,hours_per_day_use_m__p_3,
         days_in_a_season_pump_use_w_p1,hours_per_day_use_w__p_1,
         days_in_a_season_pump_use_w_p2,hours_per_day_use_w__p_2,
         days_in_a_season_pump_use_w_p3,hours_per_day_use_w__p_3)


WS2019 <-
  Water_extraction_mechanisms_Endline_2019_Saptari %>%
  select(year,district, household_questionnaire_id,
         days_in_a_season_pump_use_s_p1,hours_per_day_use_s__p_1,
         days_in_a_season_pump_use_s_p2,hours_per_day_use_s__p_2,
         days_in_a_season_pump_use_s_p3,hours_per_day_use_s__p_3,
         days_in_a_season_pump_use_m_p1,hours_per_day_use_m__p_1,
         days_in_a_season_pump_use_m_p2,hours_per_day_use_m__p_2,
         days_in_a_season_pump_use_m_p3,hours_per_day_use_m__p_3,
         days_in_a_season_pump_use_w_p1,hours_per_day_use_w__p_1,
         days_in_a_season_pump_use_w_p2,hours_per_day_use_w__p_2,
         days_in_a_season_pump_use_w_p3,hours_per_day_use_w__p_3)

WRSB2018 <-
  Water_extraction_mechanism_Baseline_2018_ %>%
  select(year,district, household_questionnaire_id,
         days_in_a_season_pump_use_s_p1,hours_per_day_use_s__p_1,
         days_in_a_season_pump_use_s_p2,hours_per_day_use_s__p_2,
         days_in_a_season_pump_use_s_p3,hours_per_day_use_s__p_3,
         days_in_a_season_pump_use_m_p1,hours_per_day_use_m__p_1,
         days_in_a_season_pump_use_m_p2,hours_per_day_use_m__p_2,
         days_in_a_season_pump_use_m_p3,hours_per_day_use_m__p_3,
         days_in_a_season_pump_use_w_p1,hours_per_day_use_w__p_1,
         days_in_a_season_pump_use_w_p2,hours_per_day_use_w__p_2,
         days_in_a_season_pump_use_w_p3,hours_per_day_use_w__p_3)

WRSB2019 <-
  Water_extraction_mechanism_Endline_EPC_2019_ %>%
  select(year,district, household_questionnaire_id,
         days_in_a_season_pump_use_s_p1,hours_per_day_use_s__p_1,
         days_in_a_season_pump_use_s_p2,hours_per_day_use_s__p_2,
         days_in_a_season_pump_use_s_p3,hours_per_day_use_s__p_3,
         days_in_a_season_pump_use_m_p1,hours_per_day_use_m__p_1,
         days_in_a_season_pump_use_m_p2,hours_per_day_use_m__p_2,
         days_in_a_season_pump_use_m_p3,hours_per_day_use_m__p_3,
         days_in_a_season_pump_use_w_p1,hours_per_day_use_w__p_1,
         days_in_a_season_pump_use_w_p2,hours_per_day_use_w__p_2,
         days_in_a_season_pump_use_w_p3,hours_per_day_use_w__p_3)

water_use_6.2 <- rbind(WS2017,WS2018,WS2019,WRSB2018,WRSB2019) %>% 
  left_join(Control_and_treatment_4_districts)

water_use_6.2 <- water_use_6.2 %>% 
  mutate(ps1=days_in_a_season_pump_use_s_p1* hours_per_day_use_s__p_1,
         ps2=days_in_a_season_pump_use_s_p2* hours_per_day_use_s__p_2,
         ps3=days_in_a_season_pump_use_s_p3* hours_per_day_use_s__p_3,
         pm1=days_in_a_season_pump_use_m_p1* hours_per_day_use_m__p_1,
         pm2=days_in_a_season_pump_use_m_p2* hours_per_day_use_m__p_2,
         pm3=days_in_a_season_pump_use_m_p3* hours_per_day_use_m__p_3,
         pw1=days_in_a_season_pump_use_w_p1* hours_per_day_use_w__p_1,
         pw2=days_in_a_season_pump_use_w_p2* hours_per_day_use_w__p_2,
         pw3=days_in_a_season_pump_use_w_p3* hours_per_day_use_w__p_3) %>% 
  mutate(smr = rowSums(.[names(.)[24:26]], na.rm = T),
         mnsn = rowSums(.[names(.)[27:29]], na.rm = T),
         wntr = rowSums(.[names(.)[30:32]], na.rm = T),
         allyear = rowSums(.[names(.)[24:32]], na.rm = T),
         p1 = rowSums(.[names(.)[c(24,27,30)]], na.rm = T))


water_use_6.2$district [water_use_6.2$district %in% 1:3] <- "RSB"
water_use_6.2$district [water_use_6.2$district == "SAPTARI" ] <- "Saptari"

x <- water_use_6.2 %>%
  filter(TreatmentControl=="Treatment") %>% 
  select(household_questionnaire_id,year,district,mnsn)

xmnsn <- spread(x, year, mnsn)
xmnsn <- xmnsn %>% rename(monsoon_2017="2017",monsoon_2018="2018",monsoon_2019="2019")

x <- water_use_6.2 %>%
  filter(TreatmentControl=="Treatment") %>% 
  select(household_questionnaire_id,year,district,smr)

xsmr <- spread(x, year, smr)
xsmr <- xsmr %>% rename(summer_2017="2017",summer_2018="2018",summer_2019="2019")

x <- water_use_6.2 %>%
  filter(TreatmentControl=="Treatment") %>% 
  select(household_questionnaire_id,year,district,wntr)

xwntr <- spread(x, year, wntr)
xwntr <- xwntr %>% rename(winter_2017="2017",winter_2018="2018",winter_2019="2019")

xp1 <- spread(x, year, p1)
xp1 <- xp1 %>% rename(PUMP1_2017="2017",PUMP1_2018="2018",PUMP1_2019="2019")

xs <- inner_join(xmnsn,xsmr) %>% 
  inner_join(xwntr) 


  

  inner_join(xp1) %>% 
  arrange(district)

kable(xs) %>% kable_styling()


xx1 <- Water_extraction_mechanism_Baseline_2017_ %>% 
  select(household_questionnaire_id,year,district,pump_type__p_1,pump_type__p_2,pump_type__p_3)

xx2 <- Water_extraction_mechanism_Midline_2018_ %>% 
  select(household_questionnaire_id,year,district,pump_type__p_1,pump_type__p_2,pump_type__p_3)

xx3 <- Water_extraction_mechanisms_Endline_2019_Saptari %>% 
  select(household_questionnaire_id,year,district,pump_type__p_1,pump_type__p_2,pump_type__p_3)


# s
x1 <- Water_extraction_mechanism_Baseline_2017_ %>% 
  select(household_questionnaire_id,year,district,pump_type__p_1) %>% 
  rename(base_p_1=pump_type__p_1)

x2 <- Water_extraction_mechanism_Midline_2018_ %>% 
  select(household_questionnaire_id,pump_type__p_1)%>% 
  rename(mid_p_1=pump_type__p_1)

x3 <- Water_extraction_mechanisms_Endline_2019_Saptari %>% 
  select(household_questionnaire_id,pump_type__p_1)%>% 
  rename(end_p_1=pump_type__p_1)
 
x <-left_join(x1,x2) %>% inner_join(x3) %>% 
  inner_join(Control_and_treatment_4_districts)

# xxx
xxx1 <- Water_extraction_mechanism_Baseline_2018_ %>%
  select(household_questionnaire_id,year,district,pump_type__p_1) %>% 
  rename(base_p_1=pump_type__p_1)

xxx2 <- Water_extraction_mechanism_Endline_EPC_2019_ %>% 
  select(household_questionnaire_id,pump_type__p_1)%>% 
  rename(mid_p_1=pump_type__p_1)

xxx <-left_join(x1,x2) %>% 
  inner_join(Control_and_treatment_4_districts)

  
