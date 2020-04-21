table(Procurement_Baseline_2018_$do_you_own_use_a_krishi_meter)
table(Procurement_Baseline_2018_$hrs_electricity_in_summer)

table(Procurement_Baseline_2018_$distance_of_electricity_mete)

table(Procurement_Baseline_2018_$total_expenses_on_electricit)
mean(Procurement_Baseline_2018_$total_expenses_on_electricit,na.rm = T)

# [7.12] Did you buy fuel for the pump?
R_Procurement_Baseline_2018_$do_you_buy_fuel_for_the_pump
table(R_Procurement_Baseline_2018_$do_you_buy_fuel_for_the_pump)

# [7.13] If yes, which fuel is predominantly used for your pump?
table(R_Procurement_Baseline_2018_$if_yes__which_fuel_do_you_use)
#     1 Diesel  # 2 Kerosene #

# [7.16] Total litres of diesel/kerosene consumed for agriculture pumps
R_Procurement_Baseline_2018_ %>% filter(!is.na(total_litres_consumed_dieselkero)) %>% 
  summarise(mean(total_litres_consumed_dieselkero),n())

table(Procurement_Baseline_2018_$at_which_price_did_you_buy_it)
summary(Procurement_Baseline_2018_$at_which_price_did_you_buy_it)

txo<-table(Procurement_Baseline_2018_$total_expenses_on_electricit)
summary(Procurement_Baseline_2018_$total_expenses_on_electricit)
as.data.frame(table(txo))


table(Procurement_Baseline_2018_$diesel_available_within_villa)
table(Procurement_Baseline_2018_$diesel_available_within_5_km)
table(Procurement_Baseline_2018_$diesel_available_when_needed)

table(Procurement_Baseline_2018_$have_to_pay_black_market_rat)





