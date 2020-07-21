library(tidyverse)

# country_txt == Incident Location
# natlty1_txt == This is the nationality of the target that was attacked
# nkill == The number includes  victims and attackers who died as a direct result of the incident.
# nkillter ==  only perpetrator fatalities

globalterrorismdb_il <-
  globalterrorismdb_0919dist %>%
  select(country_txt  , targtype1_txt, nkill, nwound ,
         provstate,city ,location ,attacktype1_txt,suicide,nkillter, 
         multiple ,related ,summary,addnotes, natlty1_txt,eventid , iyear , imonth , iday) %>% 
  filter(iyear>=1994,country_txt=="Israel"|
           country_txt=="West Bank and Gaza Strip" ,
         natlty1_txt== "Israel")

library(lubridate)

globalterrorismdb_il <- globalterrorismdb_il %>%
  mutate(date = make_date(iyear, imonth, iday))

globalterrorismdb_il$yearweek <-
  strftime(globalterrorismdb_il$date, format = "%Y-W%U")

# copy column targtype1_txt
globalterrorismdb_il <- globalterrorismdb_il %>%
  mutate( targtype1_txt_new = targtype1_txt )

globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Military"] <- "security force"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Police"] <- "security force"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Private Citizens & Property"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Educational Institution"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Business"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Government (General)"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Unknown"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Food or Water Supply"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Journalists & Media"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Airports & Aircraft"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Transportation"] <- "civilians"
 globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Government (Diplomatic)"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Tourists"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Utilities"] <- "civilians"
globalterrorismdb_il$targtype1_txt_new [globalterrorismdb_il$targtype1_txt_new=="Religious Figures/Institutions"] <- "civilians"

attach(globalterrorismdb_il)

globalterrorismdb_il$nincident <- rep(1)

# Creating a list of weeks numbers
library(data.table)
date <- seq(as.IDate("1994-01-01"), as.IDate("2019-12-31"), 1)
dt <- data.table(date=date)
dt$yearweek <-  strftime(date, format = "%Y-W%U") # this table: "date" and "yearweek"
calander_1994_2019 <- dt[,2] %>% count(yearweek) %>% select(1) # only "yearweek"

rm(dt,df,date)

# lets "group_by" The number of Incident----
# Incident Location in Israel
incident_location_israel <- globalterrorismdb_il %>%
  filter(targtype1_txt_new =="civilians", country_txt== "Israel") %>% 
  group_by(yearweek) %>%
  summarise(number_incident=sum(nincident)) %>% 
  right_join(calander_1994_2019)


# Incident Location in West Bank and Gaza Strip
# targtype1_txt "Military" "Police"
# targtype1_txt "Citizens"




