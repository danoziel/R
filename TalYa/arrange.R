TAL.YA..farmer19_20 <- read.csv("~/R/TalYa/TAL-YA -farmer19_20.csv", stringsAsFactors=FALSE)

TAL.YA..farmer19_20 <- TAL.YA..farmer19_20%>% 
  rename(harvest_KG_CONTROL=harvest_KG,harvest_damage_CONTROL=harvest_damage) %>% 
  harvest_yesno_talya100

# harvesting tomato data
#eliminate: Vijaya narasimha (bitter guord Summer 2019)
#           R Chenna kista(maskmelon Rabi 2020)
talya <- TAL.YA..farmer19_20 %>%
  filter(farmer_name!="Vijaya narasimha") %>%
  filter(farmer_name!="R Chenna kista") %>%
  filter(harvest_yesno_talya100=="Yes") %>%
  select(2,18,19,20,harvest_KG_talya100,harvest_KG_CONTROL,harvest_damage_talya100,
         harvest_damage_CONTROL,KG_sold_TALYA100,KG_sold_CONTROL,average_price_TALYA100,
         revenue_TALYA100,revenue_CONTROL,average_price_TALYA100,8,harvest_yesno_talya100,
         ) 

x <- talya %>%group_by (farmer_name) %>% summarise(mean(harvest_KG_talya100))
at_btselem %>%
  group_by(yearweek,week_range) %>% 
  summarise_at(vars(total_killed:total_attacks), sum, na.rm = TRUE)



