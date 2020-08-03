TYfarmer

TALYAfarmer <- read.csv("~/R/TalYa/data/TALYAfarmer.csv")
area_talya_farmers <- read.csv("~/R/TalYa/data/area_talya_farmers.csv")

write.csv(TALYAfarmer,"C:/Users/Dan/Documents/R/TalYa/TALYAfarmer.csv")


# harvesting tomato data----
#eliminate: Vijaya narasimha (bitter guord Summer 2019)
#           R Chenna kista(maskmelon Rabi 2020)

talya_Yield <- TALYAfarmer %>%
  filter(farmer_name!="Vijaya narasimha") %>%
  filter(farmer_name!="R Chenna kista") %>%
  filter(harvest_yesno_talya100=="Yes") %>%
  rename(harvest_KG_CONTROL=harvest_KG,harvest_damage_CONTROL=harvest_damage,week_number=week.) %>%  
  select(starttime,mandal,village,farmer_name,username,
         harvest_KG_talya100,harvest_KG_CONTROL,harvest_damage_talya100,harvest_damage_CONTROL,
         KG_sold_TALYA100,KG_sold_CONTROL,average_price_TALYA100,
         revenue_TALYA100,revenue_CONTROL,
         week_number,year,harvest_week)

talya_Yield <- talya_Yield[-26,]

talya_Yield <- inner_join(talya_Yield,area_talya_farmers,by="farmer_name")

talya_Yield <- talya_Yield %>% mutate(ty_harvest_kg_ac = harvest_KG_talya100 /acre,
                          ctrl_harvest_kg_ac = harvest_KG_CONTROL /acre,
                          ty_damage_kg_ac = harvest_damage_talya100 /acre,
                          ctrl_damage_kg_ac = harvest_damage_CONTROL /acre,
                          ty_kg_sold_ac = KG_sold_TALYA100 /acre,
                          ctrl_kg_sold_ac = KG_sold_CONTROL /acre,
                          ty_income_ac = revenue_TALYA100 /acre,
                          ctrl_income_ac = revenue_CONTROL /acre,
                          ty_revenue_ac = KG_sold_TALYA100*average_price_TALYA100/ acre,
                          ctrl_revenue_ac = KG_sold_CONTROL*average_price_TALYA100/ acre)

attach(talya_Yield)

talya_Yield %>% group_by(farmer_name) %>% summarise(ty=sum(harvest_KG_talya100,na.rm = T)) %>% 
  summarise(mean(ty))
