#[7.1] Do you own/use a Krishi Meter?----
table(Procurement_Baseline_2018_$do_you_own_use_a_krishi_meter)
table(Procurement_Endline_EPC_2019_$do_you_own_use_a_krishi_meter)


# [7.4] How many electric pumps do you operate using this connection?----
table(Procurement_Baseline_2018_$how_many_electric_pumps_do_yo)
table(Procurement_Endline_EPC_2019_$how_many_electric_pumps_do_yo)

#[7.5]Do you use a domestic connection for agricultural needs?----
# RBS
table(Procurement_Baseline_2018_$do_you_use_a_domestic_connect)
table(Procurement_Endline_EPC_2019_$do_you_use_a_domestic_connect)

#[7.6d]How many electric pumps do you operate using this connection?----
# RBS
table(Procurement_Baseline_2018_$how_many_electric_pumps_do_yp)
table(Procurement_Endline_EPC_2019_$how_many_electric_pumps_do_yp)

# [7.12] Did you buy fuel for the pump?-----
# RBS
# "T302501007" "T305001121"- own electric pump-said yes to 'buy fuel' at 7.2 

as <- Procurement_Baseline_2018_ %>% 
  filter(!household_questionnaire_id %in% c("T302501007","T305001121")) %>%
  group_by(do_you_buy_fuel_for_the_pump) %>% 
  summarise(n())
  
table(Procurement_Baseline_2018_$do_you_buy_fuel_for_the_pump)
table(Procurement_Endline_EPC_2019_$do_you_buy_fuel_for_the_pump)

# [7.13]If yes, which fuel is predominantly used for your pump?----
table(Procurement_Baseline_2018_$if_yes__which_fuel_do_you_use)
table(Procurement_Endline_EPC_2019_$if_yes__which_fuel_do_you_use)

# [7.16]Total litres of diesel/kerosene consumed for agriculture pumps
summary(Procurement_Baseline_2018_$total_litres_consumed_dieselkero)
summary(TreatsB_proc$total_litres_consumed_dieselkero)
summary(TreatsE_proc$total_litres_consumed_dieselkero)


TreatsB_proc <- R_Procurement_Baseline_2018_ %>% filter(TC==1)
TreatsE_proc <- R_Procurement_Endline_EPC_2019_ %>% filter(TC==1)
table(rp$do_you_own_use_a_krishi_meter)
table(rp$how_many_electric_pumps_do_yo)
table(rp$do_you_use_a_domestic_connect)

table(rp$do_you_buy_fuel_for_the_pump)
summary(rp$how_many_electric_pumps_do_yp)

rp %>% 
  group_by(do_you_buy_fuel_for_the_pump) %>% tally()


