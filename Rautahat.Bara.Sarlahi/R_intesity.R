# Cropping intensity----
by_year2018<-Lands_Baseline_2018_ %>% inner_join(Control_and_treatment_4_districts) %>% 
  inner_join(R_Lands_I_Baseline_2018_[,c(1,3)]) %>%
  filter(total_land_cultivated>0)%>%
  mutate(ci=total_land_cultivated/land_for_cultivation*100) %>%
  group_by(TreatmentControl,household_questionnaire_id) %>%
  summarise(a=sum(ci)) %>%inner_join(matchig) %>%
  group_by(TreatmentControl) %>% summarise(mean(a),n())

by_year2019<-R_Lands_Endline_EPC_2019_ %>% inner_join(R_Lands_I_Endline_EPC_2019_[,c(1,3)]) %>%
  filter(total_land_cultivated>0)%>%
  mutate(ci=total_land_cultivated/land_for_cultivation*100) %>%
  group_by(household_questionnaire_id,TreatmentControl) %>%
  summarise(a=sum(ci)) %>%inner_join(matchig) %>%
  group_by(TreatmentControl) %>% summarise(mean(a),n())

by_season18<-Lands_Baseline_2018_ %>% inner_join(Control_and_treatment_4_districts) %>% 
  inner_join(R_Lands_I_Baseline_2018_[,c(1,3)]) %>% filter(total_land_cultivated>0)%>%
  mutate(ci=total_land_cultivated/land_for_cultivation*100) %>%
  group_by(TreatmentControl,season) %>% summarise(m=mean(ci)) 

by_season19<-R_Lands_Endline_EPC_2019_ %>% 
  inner_join(R_Lands_I_Endline_EPC_2019_[,c(1,3)]) %>% filter(total_land_cultivated>0)%>%
  mutate(ci=total_land_cultivated/land_for_cultivation*100) %>%
  group_by(TreatmentControl,season) %>% summarise(m=mean(ci)) 

# matching
tret<-by_year2018[,c(1,12)] %>% group_by(household_questionnaire_id) %>% count()
matchig <- R_Lands_I_Baseline_2018_%>% inner_join(tret)
matchig <- matchig[,c(1,39,3,2,4:38,40)] %>% inner_join(bind)

            
matchig <- matchig %>% 
  filter(TC==1 | household_questionnaire_id %in% c("T302606024","T305404026","T307808108","T300603032",
                                                                        "T302004124","T303608010","T302608026","T307006094",
                                                                        "T304707129","T307705125","T302606012","T309504024",
                              "T300409001","T301805004","T304905099","T302608003",
                                                   "T307001095","T301503080","T302607006","T300509098",
                                                                        "T302608050","T301808003","T307408001","T303603009"))                                     
                                           
ce <- inner_join(by_year2018[,c(1,11,3)],by_year2019[,1]) %>%
  group_by(household_questionnaire_id,TreatmentControl) %>%
  filter(TreatmentControl=="Control") %>% tally()
 
tb <-by_year2018[,c(1,11,3)]%>%
  group_by(household_questionnaire_id,TreatmentControl) %>%
  filter(TreatmentControl=="Treatment") %>% tally()

bind <- bind_rows(ce,tb)
bind <- bind[,1]
------------------------------------------------------------------
R_Lands_Endline_EPC_2019_ <-  R_Lands_Endline_EPC_2019_ %>%
  rename(ii = irrigation_intensety) #renam column land_ TO net_

R_Lands_Endline_EPC_2019_ <-  R_Lands_Endline_EPC_2019_ %>%
  rename(nce = net_cropping_area) #renam column land_ TO net_

R_Lands_Endline_EPC_2019_ <- R_Lands_Endline_EPC_2019_ %>%filter(!is.na(total_land_cultivated)) %>%
  group_by(household_questionnaire_id) %>% 
  mutate(net_cropping_area = max(total_land_cultivated))

# Irrigation intensety----
# Baseline
x <- R_Lands_I_Baseline_2018_[,c(1,3,41)]
R_Lands_Baseline_2018_ <- inner_join(R_Lands_Baseline_2018_,x)
x <- R_Lands_Baseline_2018_ %>%select(1:8,15,14,16,everything())

x$net_irrigation_area <- NULL
colnames(R_Lands_Baseline_2018_)[9] <- "net_cropping_area"
R_Lands_Baseline_2018_ <- x

R_Lands_Baseline_2018_ <- R_Lands_Baseline_2018_ %>%
  mutate(II=irrigated_out_of_tot_land_cult/net_cropping_area*100)

R_Lands_Endline_EPC_2019[137,16]<- 60

# Endline
x <- R_Lands_I_Endline_EPC_2019_[,c(1,3)]
R_Lands_Endline_EPC_2019_ <- inner_join(R_Lands_Endline_EPC_2019_ ,x)
x <- R_Lands_Baseline_2018_ %>%select(1:8,15,14,16,everything())
R_Lands_Endline_EPC_2019_ <- R_Lands_Endline_EPC_2019_ %>%select(1:8,14,everything())

x$net_irrigation_area <- NULL
colnames(R_Lands_Endline_EPC_2019_)[9] <- "net_cropping_area"

R_Lands_Endline_EPC_2019_ <- R_Lands_Endline_EPC_2019_ %>%
  mutate(irrigation_intensety=irrigated_out_of_tot_land_cult/net_cropping_area*100)

R_Lands_Baseline_2018[181,9]<- 45

# by season----

base <-R_Lands_Baseline_2018_ %>%  filter(!household_questionnaire_id %in% c("T306102001","T306102002","T308707002"),total_land_cultivated>0) %>% 
  group_by(season)%>%summarise(mean(II),n=n()) %>% mutate(freq = n / 129)

tcb <- R_Lands_Baseline_2018_ %>% filter(!household_questionnaire_id %in% c("T306102001","T306102002","T308707002"),total_land_cultivated>0)%>%
  group_by(TC,season) %>%  summarise(mean(II),n=n())%>% mutate(freq = n / 105) #24

tca <- R_Lands_Endline_EPC_2019_ %>% group_by(TC,season) %>%
  filter(!household_questionnaire_id
         %in% c("T309708020","T301502075","T302603034","T300901113","T303007001",
                "T306102001","T308707002","T306102002"),total_land_cultivated>0) %>% 
  summarise(mean(irrigation_intensety),n=n()) %>% mutate(freq = n / 99) #23

# by year
ybase <- R_Lands_Baseline_2018_ %>% group_by(season) %>% 
  filter(!household_questionnaire_id %in% c("T306102001","T306102002","T308707002"),total_land_cultivated>0) %>% 
  summarise(mi=mean(II)) %>% summarise(sum(mi))

tcb <- R_Lands_Baseline_2018_ %>% group_by(TC,season) %>%
  filter(!household_questionnaire_id %in% c("T306102001","T306102002","T308707002"),total_land_cultivated>0) %>%
  summarise(mi=mean(II)) %>% summarise(sum(mi))

tca <- R_Lands_Endline_EPC_2019_ %>% group_by(TC,season) %>%
  filter(!household_questionnaire_id %in%
           c("T309708020","T301502075","T302603034","T300901113","T303007001","T306102001",
             "T308707002","T306102002"),total_land_cultivated>0) %>% 
  summarise(mi=mean(irrigation_intensety)) %>% summarise(sum(mi))


# total_land_cultivated>0
# by year
R_Lands_Baseline_2018_ %>% filter(total_land_cultivated>0) %>% group_by(household_questionnaire_id) %>%
  summarise(sum=sum(total_land_cultivated)) %>% summarise(mean(sum))
x <- R_Lands_Baseline_2018_ %>% filter(total_land_cultivated>0) %>% group_by(household_questionnaire_id,TC) %>%
  summarise(sum=sum(total_land_cultivated)) %>% group_by(TC) %>% summarise(mean(sum))
x <- R_Lands_Endline_EPC_2019_ %>% filter(total_land_cultivated>0) %>% group_by(household_questionnaire_id,TC) %>%
  summarise(sum=sum(total_land_cultivated)) %>% group_by(TC) %>% summarise(mean(sum))

# by season
base <- R_Lands_Baseline_2018_ %>% filter(total_land_cultivated>0) %>% group_by(household_questionnaire_id,season) %>%
  summarise(sum=sum(total_land_cultivated)) %>% group_by(season) %>%
  summarise(mean(sum),n=n(),freq = n / 129)

tcb <- R_Lands_Baseline_2018_ %>% filter(total_land_cultivated>0) %>% group_by(household_questionnaire_id,season,TC) %>%
  summarise(sum=sum(total_land_cultivated)) %>% group_by(TC,season) %>%
  summarise(mean(sum),n=n())

tca <- R_Lands_Endline_EPC_2019_ %>% filter(total_land_cultivated>0) %>% group_by(household_questionnaire_id,season,TC) %>%
  summarise(sum=sum(total_land_cultivated)) %>% group_by(TC,season) %>%
  summarise(mean(sum),n=n())

