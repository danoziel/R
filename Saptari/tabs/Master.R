library(tidyverse)

water <-  Master_file_Saptari_REWSSPC_12_27_2019[1:14513,]

# water01----------------
water01 <- water
water01 <- water01 %>%
  rename(date=`Record Date (mm/dd/yyyy)`,Hours=`hr_n_diff`,HH=`HH ID`) %>% 
  fill(date) %>% 
  filter(date>"2017-06-01")

water01$HH [is.na(water01$HH)] <- "T3HH"

water01 <- water01 %>%
  mutate(season=case_when(
    date >= "2017-06-02" & date <= "2017-09-30" ~ "Monsoon_2017_2018",
    date >= "2017-10-01" & date <= "2018-01-31" ~ "Winter_2017_2018",
    date >= "2018-02-01" & date <= "2018-05-31" ~ "Summer_2017_2018",
    
    date >= "2018-06-01" & date <= "2018-09-30" ~ "Monsoon_2018_2019",
    date >= "2018-10-01" & date <= "2019-01-31" ~ "winter_2018_2019",
    date >= "2019-02-01" & date <= "2019-05-31" ~ "Summer_2018_2019",
    
    date >= "2019-06-01" & date <= "2019-09-30" ~ "Monsoon_2019_2020",
    date >= "2019-10-01" & date <= "2019-12-16" ~ "winter_2019_2020"))

water01 <- water01 %>% rename(pump_type=`Method (1= Solar, 2 = Diesel, 3 = Electric, 4 = Canal)`)
water01 <- water01 %>% rename(crop =`Crop Name (Separate row for each crop)`)
water01$crop[water01$crop == "Fish Pond"] <- "Fish Farming"

water01 <- water01 %>% 
  mutate(district  = ifelse(District %in% c("Rautahat", "Bara","Sarlahi"), "Rautahat_Bara_Sarlahi",
                                          "Saptari")) 

water01 <- water01 %>% filter (HH != "T300406135",HH != "Survey not done")

write.csv(water01,"C:/Users/Dan/Documents/R/Saptari/data/water01.csv", row.names = FALSE)

#  Pump usage days - as a percentage of all days   --------------------------------------
x0 <- water01 %>% 
  group_by(HH) %>% 
  summarise(start=min(date),end=max(date),irri_hr=mean(Hours)) %>% 
  mutate(total_days=end-start) %>% 
  select(HH,irri_hr,total_days)

x1 <- water01 %>% 
  filter(Hours>0) %>% 
  group_by(HH,date) %>% summarise(s=sum(Hours)) %>% 
  group_by(HH) %>% tally() %>% rename(irri_days=n)

days_use_hh <- left_join(x0,x1)
days_use_hh$total_days <- as.numeric(days_use_hh$total_days)

days_use_hh <- days_use_hh %>% mutate(percentage= irri_days / total_days  ) %>% mutate(across(is.numeric,round,2))

kable(days_use_hh) %>% kable_styling()

# histogram
ggplot(days_use_hh, aes(x = percentage))+
  geom_histogram(color= "gray20", fill = "royalblue1",
                 breaks=seq(0, 1, by =0.10)) +
  geom_vline(aes(xintercept = mean(percentage)), 
             linetype = "dashed", size = 0.7)+
  labs(title="Percentage of SIP use for HH") +
  labs(x="percentage", y="No. of HH") + 
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 10,2))


#fish farming
x1 <- water01 %>% 
  filter(crop=="Fish Farming") %>% 
  group_by(HH,date) %>% summarise(s=sum(Hours)) %>% 
  group_by(HH) %>% tally()%>% rename(irri_days=n)

days_use_hh <- left_join(x0,x1)
days_use_hh$total_days <- as.numeric(days_use_hh$total_days)

days_use_hh <- days_use_hh %>%
  filter(!is.na(irri_days)) %>% 
  mutate(percentage= irri_days / total_days  ) %>% 
  mutate(across(is.numeric,round,2))

kable(days_use_hh) %>% kable_styling()

# histogram
ggplot(days_use_hh, aes(x = percentage))+
  geom_histogram(color= "gray20", fill = "royalblue1",
                 breaks=seq(0, 1, by =0.10)) +
  geom_vline(aes(xintercept = mean(percentage)), 
             linetype = "dashed", size = 0.7)+
  labs(title="Aquaculture - percentage of SIP use for HH") +
  labs(x="percentage", y="No. of HH") + 
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 10,2))


#Cultivated land
x1 <- water01 %>% 
  filter(crop != "Fish Farming") %>% 
  group_by(HH,date) %>% summarise(s=sum(Hours)) %>% 
  group_by(HH) %>% tally() %>% rename(irri_days=n)

days_use_hh <- left_join(x0,x1)
days_use_hh$total_days <- as.numeric(days_use_hh$total_days)

days_use_hh <- days_use_hh %>%
  filter(!is.na(irri_days)) %>% 
  mutate(percentage= irri_days / total_days  ) %>%
  mutate(across(is.numeric,round,2))

kable(days_use_hh) %>% kable_styling()

# histogram
ggplot(days_use_hh, aes(x = percentage))+
  geom_histogram(color= "gray20", fill = "royalblue1",
                 breaks=seq(0, 1, by =0.10)) +
  geom_vline(aes(xintercept = mean(percentage)), 
             linetype = "dashed", size = 0.7)+
  labs(title="Cultivated Land - percentage of SIP use for HH") +
  labs(x="percentage", y="No. of HH") + 
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 15,2))

#Paddy
x1 <- water01 %>% 
  filter(crop %in% c("Paddy", "Summer Paddy",)) %>% 
  group_by(HH,date) %>% summarise(s=sum(Hours)) %>% 
  group_by(HH) %>% tally()

##   Monsoon
x0 <- water01 %>% 
  group_by(HH) %>% 
  filter(crop !="Fish Farming") %>% 
  filter(season %in% c("Monsoon_2017_2018", "Monsoon_2018_2019", "Monsoon_2019_2020" )) %>% 
  summarise(start=min(date),end=max(date),irri_hr=mean(Hours)) %>% 
  mutate(total_days=end-start) %>% 
  select(HH,irri_hr,total_days)

x1 <- water01 %>% 
  filter(crop !="Fish Farming") %>% 
  filter(season %in% c("Monsoon_2017_2018", "Monsoon_2018_2019", "Monsoon_2019_2020" )) %>% 
  group_by(HH,date) %>% summarise(s=sum(Hours)) %>% 
  group_by(HH) %>% tally()

days_use_hh <- left_join(x0,x1) %>% filter(total_days>3)
days_use_hh$total_days <- as.numeric(days_use_hh$total_days)

days_use_hh <- days_use_hh %>% mutate(percentage= n / total_days  ) %>% mutate(across(is.numeric,round,2))

kable(days_use_hh) %>% kable_styling()

# histogram
ggplot(days_use_hh, aes(x = percentage))+
  geom_histogram(color= "gray20", fill = "royalblue1",
                 breaks=seq(0, 1, by =0.10)) +
  geom_vline(aes(xintercept = mean(percentage)), 
             linetype = "dashed", size = 0.7)+
  labs(title="Monsoon - Cultivated Land - percentage of SIP use for HH") +
  labs(x="percentage", y="No. of HH") + 
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 26,2))+
  theme(plot.title = element_text( size =10))



#winter
x0 <- water01 %>% 
  group_by(HH) %>% 
  filter(crop !="Fish Farming") %>% 
  filter(season %in% c("Winter_2017_2018", "Winter_2018_2019","Winter_2019_2020" )) %>% 
  summarise(start=min(date),end=max(date),irri_hr=mean(Hours)) %>% 
  mutate(total_winter_days=end-start) %>% 
  select(HH,irri_hr,total_winter_days)

x1 <- water01 %>% 
  filter(crop !="Fish Farming") %>% 
  filter(season %in% c("Winter_2017_2018", "Winter_2018_2019","Winter_2019_2020" )) %>% 
  group_by(HH,date) %>% summarise(s=sum(Hours)) %>% 
  group_by(HH) %>% tally()

days_use_hh <- left_join(x0,x1) 
days_use_hh$total_winter_days <- as.numeric(days_use_hh$total_winter_days)

days_use_hh <- days_use_hh %>% mutate(percentage = n / total_winter_days  ) %>% mutate(across(is.numeric,round,2))

kable(days_use_hh) %>% kable_styling()

# histogram
ggplot(days_use_hh, aes(x = percentage))+
  geom_histogram(color= "gray20", fill = "royalblue1",
                 breaks=seq(0, 1, by =0.10)) +
  geom_vline(aes(xintercept = mean(percentage)), 
             linetype = "dashed", size = 0.7)+
  labs(title="Winter - Cultivated Land - percentage of SIP use for HH") +
  labs(x="percentage", y="No. of HH") + 
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 26,2))+
  theme(plot.title = element_text( size =10))

##  summer
x0 <- water01 %>% 
  group_by(HH) %>% 
  filter(crop !="Fish Farming") %>% 
  filter(season %in% c("Summer_2017_2018", "Summer_2018_2019" )) %>% 
  summarise(start=min(date),end=max(date),irri_hr=mean(Hours)) %>% 
  mutate(total_summer_days=end-start) %>% 
  select(HH,irri_hr,total_summer_days)

x1 <- water01 %>% 
  filter(crop !="Fish Farming") %>% 
  filter(season %in% c("Summer_2017_2018", "Summer_2018_2019" )) %>% 
  group_by(HH,date) %>% summarise(s=sum(Hours)) %>% 
  group_by(HH) %>% tally()

days_use_hh <- left_join(x0,x1) %>% filter(total_summer_days>8)
days_use_hh$total_summer_days <- as.numeric(days_use_hh$total_summer_days)

days_use_hh <- days_use_hh %>% mutate(percentage= n / total_summer_days  ) %>% mutate(across(is.numeric,round,2))

kable(days_use_hh) %>% kable_styling()

# histogram
ggplot(days_use_hh, aes(x = percentage))+
  geom_histogram(color= "gray20", fill = "royalblue1",
                 breaks=seq(0, 1, by =0.10)) +
  geom_vline(aes(xintercept = mean(percentage)), 
             linetype = "dashed", size = 0.7)+
  labs(title="Summer - Cultivated Land - percentage of SIP use for HH") +
  labs(x="percentage", y="No. of HH") + 
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 26,2))+
  theme(plot.title = element_text( size =10))
# -------Irrigated area vs. cultivated area  - per day (ha)  ---------------------

x53 <- water01 %>%
  group_by(HH) %>% 
  count()

#means  
x2 <- water01 %>%
  group_by(HH,date) %>% 
  filter(crop !="Fish Farming") %>% 
  summarise(`Total Area Cultivated`=sum(`Total Area Cultivated`)*0.0339,`Area Irrigated`=sum(`Area Irrigated`)*0.0339)%>%
  summarise_at(c("Total Area Cultivated" ,"Area Irrigated"), mean, na.rm = TRUE) %>% 
  summarise_at(c("Total Area Cultivated" ,"Area Irrigated"), mean, na.rm = TRUE)%>%
  mutate(across(is.numeric,round,2))
 
x2 <- x2 %>% gather("Area", "Hectar", 1:2)

kable(x2) %>% kable_styling()

ggplot(data=x2, aes(x=Area, y=Hectar, fill=Area)) +
  geom_bar(stat="identity", width=0.5) +
  geom_text(aes(label=Hectar), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  labs(title="Avg. irrigated area vs. Avg. cultivated area  - per day (ha)", x=" ")+
  theme(plot.title = element_text( size = 9))+
  guides(fill=FALSE)

#
x2 <- water01 %>%
  group_by(HH,date) %>% 
  filter(crop !="Fish Farming") %>% 
  summarise(`Total Area Cultivated`=sum(`Total Area Cultivated`)*0.0339,`Area Irrigated`=sum(`Area Irrigated`)*0.0339)%>%
  summarise_at(c("Total Area Cultivated" ,"Area Irrigated"), mean, na.rm = TRUE)

x22 <- x2 %>% gather("Area", "Hectar", 2:3) %>% filter(HH != "T301911010")

ggplot(x22, aes(x = Hectar, fill = Area)) +           
  geom_histogram(alpha = 0.5, position = "identity")+
  labs(title="Irrigated area vs. cultivated area") +
  labs(x="percentage", y="No. of HH") + 
  scale_x_continuous(breaks = seq(0, 1,0.1))+
  scale_y_continuous(breaks = seq(0, 12,2))

ggplot(x2, aes(x =`Total Area Cultivated`, y = `Area Irrigated`))+
  labs(title="Irrigated area & cultivated area") +
  geom_point(color = "#00AFBB", size = 2)+
  ylim(0, 0.7)+xlim(0, 0.7)



x2ff <- water01 %>%
  filter(crop=="Fish Farming") %>% 
  group_by(HH,date) %>% 
  summarise(irri_hr=sum(Hours),irrigated_area=sum(`Area Irrigated`))%>%
  summarise_at(c("irri_hr","irrigated_area"), mean, na.rm = TRUE) %>% 
  right_join(x53[,1]) %>% arrange(HH)
  
kable(x2ff) %>% kable_styling()

  


# library(scales)-----------------
library(scales)
g <- ggplot(days_use_hh, aes(x=HH, y=b)) + 
  geom_bar(stat="identity",width=0.4)+
  ggtitle("days_use_hh percent out of the total") +
  xlab(" ") +
  ylab(" ")+
  geom_text(aes(x=HH, y=b, label = percent(b), vjust=1.5),
            position = position_dodge(width=0.9))+
  scale_fill_manual(name="HH", values=c("#a1d99b"))+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%"))+
  coord_flip()


# Avg. irrigation hours for SPIP households------------------------------------------------------

# Cultivated Land
Hours_by_HH <- water01 %>% 
  filter(crop !="Fish Farming") %>% 
  group_by(HH,date) %>%  
  summarise(Hours=sum(Hours)) %>% 
  group_by(HH) %>% 
  summarise(Hours=mean(Hours)) %>% 
  mutate(across(is.numeric,round,2))
# histogram
ggplot(Hours_by_HH, aes(x = Hours))+
  geom_histogram(color= "gray20", fill = "royalblue2",
                 breaks=seq(0, 9, by =0.5)) +
  geom_vline(aes(xintercept = mean(Hours)), 
             linetype = "dashed", size = 0.7)+
  labs(title="Cultivated Land - Hours of SIP use for HH") +
  labs(x="Hours", y="No. of HH") + 
  scale_x_continuous(breaks = seq(0, 9, 1))+
  scale_y_continuous(breaks = seq(0, 15,2))

# Aquaculture
Hours_by_HH <- water01 %>% 
  filter(crop =="Fish Farming") %>% 
  group_by(HH,date) %>%  
  summarise(Hours=sum(Hours)) %>% 
  group_by(HH) %>% 
  summarise(Hours=mean(Hours)) %>% 
  mutate(across(is.numeric,round,2))
# histogram
ggplot(Hours_by_HH, aes(x = Hours))+
  geom_histogram(color= "gray20", fill = "royalblue2",
                 breaks=seq(0, 9, by =0.5)) +
  geom_vline(aes(xintercept = mean(Hours)), 
             linetype = "dashed", size = 0.7)+
  labs(title="Aquaculture - Hours of SIP use for HH") +
  labs(x="Hours", y="No. of HH") + 
  scale_x_continuous(breaks = seq(0, 9, 1))+
  scale_y_continuous(breaks = seq(0, 15,2))

  
ggplot(Hours_by_HH,
             aes(x=HH, y=Hours)) + 
  geom_bar(stat="identity")




# Graph 1: Hours_by_day  Saptari----
Hours_by_day <- water01 %>% 
# filter(!Seasons %in% c("Annual 2017-2018","Annual 2018-2019","Annual 2019-2020")) %>% 
  filter(district=="Saptari") %>% 
  group_by(season,date) %>%  # summarise(Flow_Meter=mean(DIFFERENCE))
  summarise(Hours=mean(Hours))

ggplot(data = Hours_by_day, aes(x = date, y = Hours))+
  labs(title=" Avg. irrigation hours for households- Saptari",
       subtitle=" monitored in 6/2017-12/2019",
       x=" ",y="No. of Houres") +
  geom_line(color = "#00AFBB", size = 0.5)+
  stat_smooth(color = "#FC4E07", fill = "#FC4E07",metho = "loess")

ggplot()+
  labs(title="Saptari by Seasons", 
       subtitle=" ",
       x=" ",y="No. of Houres") +
  geom_line(data=Hours_by_day, aes(x = date, y = Hours, color = season)) + 
  stat_smooth(data=Hours_by_day, aes(x = date, y = Hours, color = season),method = "loess")+
  theme(axis.text.x = element_text(angle = 65, vjust=0.5, size = 25), 
        panel.grid.minor = element_blank(),legend.title = element_blank(),legend.position = "none")


# Graph : Hours_by_day  Rautahat_Bara_Sarlahi-----------------------------------------------
Hours_by_day <- water01 %>% 
  filter(!Seasons %in% c("Annual 2017-2018","Annual 2018-2019","Annual 2019-2020")) %>% 
  filter(district=="Rautahat_Bara_Sarlahi") %>% 
  group_by(season,date) %>% # summarise(Flow_Meter=mean(DIFFERENCE))
  summarise(Hours=mean(Hours)) 


ggplot(data = Hours_by_day, aes(x = date, y = Hours))+
  labs(title="Rautahat Bara Sarlahi",
       subtitle=" monitored in 6/2018-12/2019",
       x=" ",y="No. of Houres") +
  geom_line(color = "#00AFBB", size = 0.5)+
  stat_smooth(color = "#FC4E07", fill = "#FC4E07",metho = "loess")+
  scale_x_date(limits = as.Date(c('2017-04-01','2020-01-01')))



ggplot()+
  labs(title="Rautahat Bara Sarlahi by Seasons", 
       subtitle=" ",
       x=" ",y="No. of Houres") +
  geom_line(data=Hours_by_day, aes(x = date, y = Hours, color = season)) + 
  stat_smooth(data=Hours_by_day, aes(x = date, y = Hours, color = season),method = "loess")+
  theme(axis.text.x = element_text(angle = 65, vjust=0.5,size= 25), 
        panel.grid.minor = element_blank(),legend.title = element_blank(),legend.position = "none")+
  scale_x_date(limits = as.Date(c('2017-04-01','2020-01-01')))

----------------------------------------------------------------------------------------
# Frequency of HH who irrigate each day -----------------
----------------------------------------------------------------------
# Graph 1
HH_by_day <- water01 %>% 
  select(HH,date,season)%>% 
  count(date)

ggplot() +
  geom_bar(aes(y = n, x = date),
           data = HH_by_day, stat="identity")+ geom_col(fill = "blue")

-----------------------------------------------------------------------
# Graph 2 
HH_by_day <- water01 %>% 
  select(HH,date,season)%>% 
  distinct()

ggplot(HH_by_day, aes(x=date, color = season)) +
  geom_histogram(binwidth=1)+
  labs(title="Daily frequency of households who irrigate",x=" ",
       y = "No. of HH")+
  theme(legend.position = "none")

-----------------------------------------------------------------------
# avg. hours - HH 
pump <- water01 %>% 
  filter (HH != "Survey not done") %>% 
  group_by(district, HH,pump_type) %>% 
  summarise (Hours=sum(Hours,na.rm = TRUE)) %>% 
  filter(Hours>0) %>% 
  mutate(across(is.numeric,round))

ggplot(data=pump_type, aes(x=HH, y=Hours, fill=pump_type)) +
  geom_bar(stat="identity")+
  labs(title="Avg. Irrigation hours for a HH", 
       subtitle="y axis is 53 HH who monitored in 6/2017-12/2019",
       x=" ",y=" ")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = rel(1), vjust=0.5),
        axis.text.y = element_text(size = rel(0.5), vjust=0.5),
        plot.title = element_text(size = rel(1), hjust = 0.5)) + 
  coord_flip()

ggplot(data=pump_type,aes(x=HH, y=Hours,fill=district)) +
  geom_col(show.legend = FALSE) +
  theme(axis.text.y = element_text(size = rel(0.5), vjust=0.5))+
  facet_wrap(~district, scales="free_y")+
  coord_flip()
---------------------------------------------------------------------
  ----------------------------------------------------------------------
  # Graph 1
  HH_by_day <- water01 %>% 
  filter (HH != "Survey not done") %>% 
  select(HH,date,season)%>% 
  count(HH) 

ggplot(data=pump_type,aes(y = n, x = HH)) +
  geom_col(fill="steelblue") + 
  coord_flip()+
  theme(axis.text.y = element_text(size = rel(2), vjust=0.5))+
  labs(title="Avg. Irrigation Days for a HH", 
       subtitle="y axis is 53 HH who monitored in 6/2017-12/2019",
       x=" ",y=" ")
  


-----------------------------------------------------------------------

  HH_by_day <- water01 %>% 
  filter (HH != "Survey not done") %>% 
  filter(season %in% c("Monsoon_2017_2018","Winter_2017_2018","Summer_2017_2018")) %>% 
  select(HH,date,season)%>% 
  distinct() %>% 
  group_by(season,HH) %>% 
  summarise(n=n() )

HH_by_day <- water01 %>% 
  filter (HH != "Survey not done") %>% 
  filter(season %in% c("Monsoon_2018_2019","Winter_2018_2019","Summer_2018_2019")) %>% 
  select(HH,date,season)%>% 
  distinct() %>% 
  group_by(season,HH) %>% 
  summarise(n=n() )

HH_by_day <- water01 %>% 
  filter (HH != "Survey not done") %>% 
  select(HH,date,season)%>% 
  distinct() %>% 
  group_by(season,HH) %>% 
  summarise(n=n() )

ggplot(HH_by_day, aes(x=HH, y=n ,fill=season))+
  geom_col(fill="steelblue") + 
  coord_flip()+
  facet_wrap(~season, scales="free_y")+
  theme(axis.text.y = element_text(size = rel(0.2), vjust=0.5))+
  labs(title="Avg. Irrigation Days for a HH by season", 
       subtitle="y axis is 53 HH who monitored ",
       x=" ",y=" ")+
  theme(axis.text.x = element_text(size =15, vjust=0.5))

ggplot(HH_by_day, aes(x=HH, y=n ,fill=season))+
  geom_col(fill="steelblue") + 
  coord_flip()+
  facet_wrap(~season, scales="free_y")+
  theme(axis.text.y = element_text(size = rel(0.2), vjust=0.5))+
  labs( x=" ",y=" ")+
  theme(axis.text.x = element_text(size =15, vjust=0.5))


  


-----------------------------------------------------------------------
  











# Saptari
xSaptari <- water01 %>% 
  filter (HH != "Survey not done") %>% 
  filter(district=="Saptari") %>% 
  select(HH,date,season) %>%
  distinct() %>% 
  group_by(season,date) %>% count(HH)%>% summarise(s=sum(n))

ggplot(xSaptari, aes(x = date, y = s,fill=season)) +
  geom_bar(stat="identity")+ 
  labs(title="Frequency of HH who irrigate each day", 
       subtitle="SAPTARI ",
       x=" ",y=" No. of HH") +
  theme(axis.text.x = element_text(angle = 65, vjust=0.5, size = 25), 
        panel.grid.minor = element_blank(),legend.title = element_blank(),legend.position = "none")

# Table: Number of monitoring of each farmer 
xxSaptari <- water01 %>% 
  filter (HH != "Survey not done") %>% 
  filter(district=="Saptari") %>% 
  select(HH,date,season) %>%
  distinct() %>% 
  group_by(HH) %>% count(date)%>%
  summarise(`Number of monitoring of each farmer`=sum(n)) %>% 
  arrange(`Number of monitoring of each farmer`)

kable(xxSaptari) %>% kable_styling()

# Rautahat_Bara_Sarlahi
xRautahat_Bara_Sarlahi <- water01 %>% 
  filter (HH != "Survey not done") %>% 
  filter(district=="Rautahat_Bara_Sarlahi") %>% 
  select(HH,date,season) %>%
  distinct() %>% 
  group_by(season,date) %>% count(HH)%>% summarise(s=sum(n))

ggplot(xRautahat_Bara_Sarlahi, aes(x = date, y = s,fill=season)) +
  geom_bar(stat="identity")+ 
  labs(title="Frequency of HH who irrigate each day", 
       subtitle="Rautahat Bara Sarlahi ",
       x=" ",y=" No. of HH") +
  theme(axis.text.x = element_text(angle = 65, vjust=0.5, size = 25), 
        panel.grid.minor = element_blank(),legend.title = element_blank(),legend.position = "none")+
  scale_x_date(limits = as.Date(c('2017-04-01','2020-01-01')))

# Table: Number of monitoring of each farmer 
xxRautahat_Bara_Sarlahi <- water01 %>% 
  filter (HH != "Survey not done") %>% 
  filter(district=="Rautahat_Bara_Sarlahi") %>% 
  select(HH,date,season) %>%
  distinct() %>% 
  group_by(HH) %>% count(date)%>%
  summarise(`Number of monitoring of each farmer`=sum(n)) %>% 
  arrange(`Number of monitoring of each farmer`)

kable(xxRautahat_Bara_Sarlahi) %>% kable_styling()
-------------------------------------------------------------------------
  
# list of HH with low days number_irri_days_per_season----

T300406135 <- water01 %>% 
  filter(HH=="T300406135")  %>% 
  mutate(Irrigated=`Total Area Irrigated`*0.0339)%>% 
  select(date,FarmerName,DIFFERENCE,Hours,Irrigated,
         Seasons,season,`Crop Name (Separate row for each crop)`,district)

T302603034 <- water01 %>% 
  filter(HH=="T302603034") %>% 
  mutate(Irrigated=`Total Area Irrigated`*0.0339)%>% 
  select(date,FarmerName,DIFFERENCE,Hours,Irrigated,
         Seasons,season,`Crop Name (Separate row for each crop)`,district)

T300901113 <- water01 %>% 
  filter(HH=="T300901113") %>% 
  mutate(Irrigated=`Total Area Irrigated`*0.0339)%>% 
  select(date,FarmerName,DIFFERENCE,Hours,Irrigated,
         Seasons,season,`Crop Name (Separate row for each crop)`,district)

T309708020 <- water01 %>% 
  filter(HH=="T309708020") %>% 
  mutate(Irrigated=`Total Area Irrigated`*0.0339)%>% 
  select(date,FarmerName,DIFFERENCE,Hours,Irrigated,
         Seasons,season,`Crop Name (Separate row for each crop)`,district)

T109902002 <- water01 %>% 
  filter(HH=="T109902002") %>% 
  mutate(Irrigated=`Total Area Irrigated`*0.0339)%>% 
  select(date,FarmerName,DIFFERENCE,Hours,Irrigated,
         Seasons,season,`Crop Name (Separate row for each crop)`,district)

T210701004 <- water01 %>% 
  filter(HH=="T210701004") %>% 
  mutate(Irrigated=`Total Area Irrigated`*0.0339)%>% 
  select(date,FarmerName,DIFFERENCE,Hours,Irrigated,
         Seasons,season,`Crop Name (Separate row for each crop)`,district)



rm(T300406135,T302603034,T300901113,T309708020,T109902002,T210701004)
-----------------------------------------------------------------------------
#by season
hr_per_season <- water %>%  
  filter (`HH ID` != "Survey not done") %>% 
  select(`HH ID`,Seasons,hr_n_diff,District) %>%
  group_by(District,`HH ID`,Seasons) %>%
  summarise_at(c("hr_n_diff"), sum, na.rm = TRUE) %>%
    mutate(across(is.numeric,round))
hr_per_season <-  spread(hr_per_season, Seasons, hr_n_diff)
hr_per_season[48,9] <- 71
hr_per_season[48,8] <- NA
hr_per_season <- select(hr_per_season,-`Monsoon 2017`)

District <- hr_per_season[,c(1,2)]


# Fish Farming
hr_aqua_season <- water %>%  
  filter (`HH ID` != "Survey not done",`Crop Name (Separate row for each crop)`== "Fish Farming") %>% 
  select(`HH ID`,Seasons,hr_n_diff) %>%
  group_by(`HH ID`,Seasons) %>%
  summarise_at(c("hr_n_diff"), sum, na.rm = TRUE) %>% 
  mutate(across(is.numeric,round,2))
hr_aqua_season <-  spread(hr_aqua_season, Seasons, hr_n_diff)

hr_aqua_season <- hr_aqua_season %>% 
  rename(aqua_17_18=`Annual 2017-2018`,
         aqua_18_19=`Annual 2018-2019`,
         aqua_19_20=`Annual 2019-2020`)
hr_aqua_season[10,4] <- 956
hr_aqua_season <- select(hr_aqua_season,1,2,3,4)
hr_aqua_season$fish_farming <- 1

# Fish Farming
hr_aqua_District <- water %>%  
  filter (`HH ID` != "Survey not done",`Crop Name (Separate row for each crop)`== "Fish Farming") %>% 
  select(`HH ID`,Seasons,hr_n_diff,District) %>%
  group_by(District, `HH ID`,Seasons) %>%
  summarise_at(c("hr_n_diff"), sum, na.rm = TRUE) %>% 
  mutate(across(is.numeric,round,2))
hr_aqua_District <-  spread(hr_aqua_District, Seasons, hr_n_diff)

trmt_water_hr_use <-
  hr_per_season %>% 
  left_join(hr_aqua_season) %>% 
  left_join(District)

write.csv(trmt_water_hr_use,"C:/Users/Dan/Documents/R/Saptari/data/trmt_water_hr_use.csv", row.names = FALSE)

------------------------------------------------------------------------------------------
#by season + Method info
hr_Method <- water %>%
  rename(metod= `Method (1= Solar, 2 = Diesel, 3 = Electric, 4 = Canal)`) %>% 
  filter (`HH ID` != "Survey not done") %>% 
  select(`HH ID`,metod,Seasons,hr_n_diff,District) %>%
  group_by(District,`HH ID`,Seasons,metod) %>%
  summarise_at(c("hr_n_diff"), sum, na.rm = TRUE) %>%
  mutate(across(is.numeric,round,2))
hr_Method <-  spread(hr_Method, Seasons, hr_n_diff)
hr_Method[59,10] <- 71 #T210709001
hr_Method[59,9] <- NA
hr_Method<- select(hr_Method,-`Monsoon 2017`)

hr_aqua_metod <- water %>%  
  rename(metod= `Method (1= Solar, 2 = Diesel, 3 = Electric, 4 = Canal)`) %>% 
  filter (`HH ID` != "Survey not done",`Crop Name (Separate row for each crop)`== "Fish Farming") %>% 
  select(`HH ID`,Seasons,hr_n_diff,metod) %>%
  group_by(`HH ID`,metod,Seasons) %>%
  summarise_at(c("hr_n_diff"), sum, na.rm = TRUE) %>% 
  mutate(across(is.numeric,round,2))
hr_aqua_metod <-  spread(hr_aqua_metod, Seasons, hr_n_diff)

hr_aqua_metod <- hr_aqua_metod %>% 
  rename(aqua_17_18=`Annual 2017-2018`,
         aqua_18_19=`Annual 2018-2019`,
         aqua_19_20=`Annual 2019-2020`)
hr_aqua_metod[13,4] <- 956
hr_aqua_metod[13,6] <- NA

hr_aqua_metod <- select(hr_aqua_metod,1,2,3,4,5)
hr_aqua_season$fish_farming <- 1

trmt_water_method_hr_use <-
  hr_Method %>% 
  left_join(hr_aqua_metod) %>% 
  left_join(District)

write.csv(trmt_water_hr_use,"C:/Users/Dan/Documents/R/Saptari/data/trmt_water_hr_use.csv", row.names = FALSE)

trmt_water_hr_use <- trmt_water_hr_use %>% inner_join(treatment_water,by="HH ID")

trmt_water <- trmt_water_hr_use %>% 
  filter(District=="Saptari") %>% 
  summarise(M2018=mean(`2017_2018`,na.rm = T,M2019=mean(`2018_2019`,na.rm = T)))
            
  summarise_at(c("2017_2018","2018_2019"), mean, na.rm = TRUE) %>% 
  mutate(across(is.numeric,round))

fix(trmt_water)
trmt_water$`2017_2018` <- as.numeric(trmt_water$`2017_2018`)
gather(year, hours, 2:3)

# aquacultur ponds-----


ponds <- 
  water %>%
  mutate(aqua_y_n  = ifelse(`Crop Name (Separate row for each crop)` == "Fish Farming",1,0))

avg_cultivated_area <- ponds %>%
  filter (`HH ID` != "Survey not done") %>% 
  group_by(aqua_y_n,Seasons,`HH ID`) %>% 
  summarise(cultivated_area=max(`Total Area Cultivated`,na.rm = TRUE)) %>% 
  group_by(aqua_y_n,Seasons) %>% 
  summarise(cultivated_area=mean(cultivated_area)) %>% 
  mutate(across(is.numeric,round))

total_cultivated_area <- ponds %>%
  filter (`HH ID` != "Survey not done") %>% 
  group_by(Seasons,`HH ID`) %>% 
  summarise(cultivated_area=max(`Total Area Cultivated`,na.rm = TRUE))

total_cultivated_area <-  spread(total_cultivated_area, Seasons, cultivated_area)
total_cultivated_area[is.na(total_cultivated_area) ] <- 0


x <- total_cultivated_area %>% 
  mutate(Y2017 = `Annual 2017-2018` + `Monsoon 2017-2018`+ `Monsoon 2017`+`Summer 2016-2017`+ `Winter 2017-2018`,
         Y2018 = `Annual 2018-2019` + `Monsoon 2018-2019`+`Summer 2017-2018`+ `Winter 2018-2019`,
         Y2019 = `Annual 2019-2020` + `Monsoon 2019-2020`+`Summer 2018-2019`+ `Winter 2019-2020`+`winter 2019-2020`) %>% 
  summarise(avg2017=mean(Y2017),avg2018=mean(Y2018),avg2019=mean(Y2019))
  
# --------------by day------------------------------------------------------------

class(water$`Record Date (mm/dd/yyyy)`)
water$`Record Date (mm/dd/yyyy)` <-  as.IDate(water$`Record Date (mm/dd/yyyy)`, "%Y-%m-%d")

--------------------------------------------------------------------------|
A <- water %>% 
  filter (`HH ID` != "Survey not done") %>% 
  rename(date=`Record Date (mm/dd/yyyy)`) %>% 
  rename(Hours=`hr_n_diff`) %>% 
  group_by(Seasons,`HH ID`,date) %>%
  summarise_at(c("Hours"), sum, na.rm = TRUE) %>% 
  count(Seasons) 

A <- spread(A,Seasons,n)

ff <- water %>%
  filter (`HH ID` != "Survey not done") %>% 
  filter(`Crop Name (Separate row for each crop)`%in% c("Fish Farming","Fish Pond")) %>%
  group_by(`HH ID`) %>%count() %>%  select(`HH ID`) %>%  mutate(1)

number_irri_days_per_season <-
  A %>% full_join(District,by="HH ID") %>%
  full_join(ff,by="HH ID") %>% arrange(District) %>% 
  rename(practice_aquaculture="1")

kable(number_irri_days_per_season) %>% kable_styling()


-----------------------------------------------------------------------------

A <- water %>% filter(`Crop Name (Separate row for each crop)`%in% c("Fish Farming","Fish Pond")) %>% 
  rename(date=`Record Date (mm/dd/yyyy)`) %>% 
  rename(Hours=`hr_n_diff`) %>% 
  group_by(date) %>% 
  summarise(av_Hours=mean(Hours,na.rm = TRUE))

A <- water %>% filter(`Crop Name (Separate row for each crop)`!="Fish Farming") %>% 
  rename(date=`Record Date (mm/dd/yyyy)`) %>% 
  rename(Hours=`hr_n_diff`) %>% 
  group_by(date) %>% 
  summarise(av_Hours=mean(Hours,na.rm = TRUE))

Sunny <- water %>% filter(`Crop Name (Separate row for each crop)`!="Fish Farming") %>% 
  filter(`Weather (1=Sunny , 2= Partly Cloudy, 3=Cloudy, 4=Highly Cloudy)`==1) %>% 
  rename(date=`Record Date (mm/dd/yyyy)`) %>% 
  rename(Hours=`hr_n_diff`) %>% 
  group_by(date) %>% 
  summarise(Sunny=mean(Hours,na.rm = TRUE))

Partly_Cloudy <- water %>% filter(`Crop Name (Separate row for each crop)`!="Fish Farming") %>% 
  filter(`Weather (1=Sunny , 2= Partly Cloudy, 3=Cloudy, 4=Highly Cloudy)`==2) %>% 
  rename(date=`Record Date (mm/dd/yyyy)`) %>% 
  rename(Hours=`hr_n_diff`) %>% 
  group_by(date) %>% 
  summarise(Partly_Cloudy=mean(Hours,na.rm = TRUE))

A <- full_join(Sunny,Partly_Cloudy,by="date")

Cloudy <- water %>% filter(`Crop Name (Separate row for each crop)`!="Fish Farming") %>% 
  filter(`Weather (1=Sunny , 2= Partly Cloudy, 3=Cloudy, 4=Highly Cloudy)`%in% c (3,4)) %>% 
  rename(date=`Record Date (mm/dd/yyyy)`) %>% 
  rename(Hours=`hr_n_diff`) %>% 
  group_by(date) %>% 
  summarise(Cloudy=mean(Hours,na.rm = TRUE))

A <- full_join(A,Cloudy,by="date")

A<- right_join (A,dt,by="date") %>% 
  mutate(across(is.numeric,round,1))

kable(A) %>% kable_styling()

# Weather (1=Sunny , 2= Partly Cloudy, 3=Cloudy, 4=Highly Cloudy)

class(A$date)

date <- seq(as.IDate("2017-01-02"), as.IDate("2019-11-01"), 1)
dt <- data.table(date=date)

# ------------------   scatterplot MONITORING vs. SURVEY--------------------------------------------
# the `xs` dataset from `WEM_irri` tab

B <- water %>% 
  filter (`HH ID` != "Survey not done") %>% 
  rename(date=`Record Date (mm/dd/yyyy)`) %>% 
  rename(Hours=`hr_n_diff`) %>% 
  filter(Seasons %in% c("Monsoon 2017-2018","Winter 2017-2018","Summer 2017-2018",
                        "Monsoon 2018-2019","Winter 2018-2019","Summer 2018-2019",
                        "Monsoon 2019-2020","Winter 2019-2020")) %>% 
  group_by(Seasons,`HH ID`) %>%
  summarise_at(c("Hours"), sum, na.rm = TRUE) %>% 
  mutate(across(is.numeric,round))

B <- spread(B,Seasons,Hours)

Bxs <- xs %>% rename(`HH ID`=household_questionnaire_id) %>% 
  select(`HH ID`,monsoon_2018,monsoon_2019,
         summer_2018,summer_2019,
         winter_2018,winter_2019,district)

Bxs <- B %>% left_join(Bxs) %>% 
  mutate(survey_2018=monsoon_2018+summer_2018+winter_2018,
         survey_2019=monsoon_2019+summer_2019+winter_2019,
         monitoring_2017_2018=`Monsoon 2017-2018`+`Summer 2017-2018`+`Winter 2017-2018`,
         monitoring_2018_2019=`Monsoon 2018-2019`+`Summer 2018-2019`+`Winter 2018-2019`)
Bxs[Bxs==0] <- NA
kable(Bxs) %>% kable_styling()

Bxs <- Bxs %>% rename(Monsoon_2017_2018 =`Monsoon 2017-2018`,
                      Summer_2017_2018 = `Summer 2017-2018`,
                      Winter_2017_2018 =`Winter 2017-2018`,
                      Monsoon_2018_2019 =`Monsoon 2018-2019`,
                      Summer_2018_2019= `Summer 2018-2019`,
                      Winter_2018_2019 =`Winter 2018-2019`)
write.csv(Bxs,"C:/Users/Dan/Documents/R/Saptari/data/Bxs.csv", row.names = FALSE)


ggplot(Bxs, aes(x =monitoring_2017_2018, y =  survey_2018))+
  geom_point(color = "#00AFBB", size = 2)+
  labs(x="monitoring data", y= "Survey data",
       title="Irrigation Houres YEAR 2017-2018")+
  theme_bw(base_size = 12)+
  ylim(0, 900)

ggplot(Bxs, aes(x =monitoring_2018_2019, y =  survey_2019))+
  geom_point(color = "#00AFBB", size = 2)+
  labs(x="monitoring data", y= "Survey data",
       title="Irrigation Houres YEAR 2018-2019")+
  theme_bw(base_size = 12)+
  ylim(0, 1500)

---------------------
# seasons 2017-2018
---------------------
ggplot(Bxs, aes(x =`Monsoon 2017-2018`, y = monsoon_2018))+
  geom_point(color = "#00AFBB", size = 2)+
  labs(x="monitoring data", y= "Survey data",
       title="Irrigation Houres MONSOON 2017-2018")+
  theme_bw(base_size = 12)+
  ylim(0, 600)

ggplot(Bxs, aes(x =`Winter 2017-2018`, y = winter_2018))+
  geom_point(color = "#00AFBB", size = 2)+
  labs(x="monitoring data", y= "Survey data",
       title="Irrigation Houres WINTER 2017-2018")+
  theme_bw(base_size = 12)+
  ylim(0, 500)

ggplot(Bxs, aes(x =`Summer 2017-2018`, y = summer_2018))+
  geom_point(color = "#00AFBB", size = 2)+
  labs(x="monitoring data", y= "Survey data",
       title="Irrigation Houres SUMMER 2017-2018")+
  theme_bw(base_size = 12)+
  ylim(0, 500)

--------------------
# seasons 2018-2019
-------------------
ggplot(Bxs, aes(x =`Monsoon 2018-2019`, y = monsoon_2019))+
  geom_point(color = "#00AFBB", size = 2)+
  labs(x="monitoring data", y= "Survey data",
       title="Irrigation Houres MONSOON 2018-2019")+
  theme_bw(base_size = 12)

ggplot(Bxs, aes(x =`Winter 2018-2019`, y = winter_2019))+
  geom_point(color = "#00AFBB", size = 2)+
  labs(x="monitoring data", y= "Survey data",
       title="Irrigation Houres WINTER 2018-2019")+
  theme_bw(base_size = 12)

ggplot(Bxs, aes(x =`Summer 2018-2019`, y = summer_2019))+
  geom_point(color = "#00AFBB", size = 2)+
  labs(x="monitoring data", y= "Survey data",
       title="Irrigation Houres SUMMER 2018-2019")+
  theme_bw(base_size = 12)

--------------------------------------------------------------
x <- Agriculture_17_18_19 %>% filter(year != "2017", TreatmentControl=="Treatment",irri_for_season<400)
ggplot(x, aes(x = irri_for_season, y = name_of_crop))+
  geom_point(color = "#00AFBB", size = 2)+
  labs(x="Irrigation Houres", y= "crop",
       title="Survey - Irrigation Houres- Saptari ")+
  theme_bw(base_size = 12)

ggplot(data=x,aes(x = irri_for_season, y = name_of_crop)) +
  geom_bar(stat="identity", fill="#00AFCC")

x <- Agriculture_18_19 %>% filter(year != "2018", TreatmentControl=="Treatment",irri_for_season<400)
ggplot(x, aes(x = irri_for_season, y = name_of_crop))+
  geom_point(color = "#00AFBB", size = 2)+
  labs(x="Irrigation Houres", y= "crop",
       title="Survey- Irrigation Houres - Rautahat Bara Sarlahi ")+
  theme_bw(base_size = 12)

x <- water01 %>%
  filter(`Crop Name (Separate row for each crop)` %in% c( "Fish Farming", "Paddy", "Summer Paddy", "Onion", "Cauliflower", "Wheat","Potato", "Garlic", "Mustard" ,"Cucumber" )) 

ggplot(x, aes(x = Hours, y = `Crop Name (Separate row for each crop)`))+
  geom_point(color = "#00AFCC", size = 2)+
  labs(x="Irrigation Houres", y= "crop",
       title="Irrigation Houres from monitoring data")+
  theme_bw(base_size = 12)

ggplot(x, aes(x = HH, y = `Crop Name (Separate row for each crop)`))+
  geom_point(color = "#00AFCC", size = 2)+
  labs(x="Irrigation Houres", y= "crop",
       title="Irrigation Houres from monitoring data")+
  theme_bw(base_size = 12)

x <- water01 %>% group_by(HH) %>% 
  filter(`Crop Name (Separate row for each crop)` %in% c( "Fish Farming", "Paddy", "Summer Paddy", "Onion", "Cauliflower", "Wheat","Potato", "Garlic", "Mustard" ,"Cucumber" )) %>% 
  count(`Crop Name (Separate row for each crop)`) %>% 
  group_by(`Crop Name (Separate row for each crop)`) %>%
  summarise(Mean=mean(n)) %>% 
  ggplot(aes(x=`Crop Name (Separate row for each crop)`, y=Mean)) +
  geom_bar(stat="identity", fill="#00AFCC")+
  coord_flip()+
  labs(title = "Avg. Irrigation houres for HH ",
       subtitle = "from monitoring Excel data ",
       x = " ", y = " No. of houres") 

x <- water01 %>% group_by(HH,`Crop Name (Separate row for each crop)`) %>% 
  filter (HH != "Survey not done") %>% 
  filter(`Crop Name (Separate row for each crop)` %in% c( "Fish Farming", "Paddy", "Summer Paddy", "Onion", "Cauliflower", "Wheat","Potato", "Garlic", "Mustard" ,"Cucumber" )) %>% 
  summarise(Hours=sum(Hours)) %>% 
  group_by(`Crop Name (Separate row for each crop)`) %>%
  summarise(Hours=mean(Hours)) %>% 
  ggplot(aes(x=`Crop Name (Separate row for each crop)`, y=Hours)) +
  geom_bar(stat="identity", fill="#00AFBB")+
  coord_flip()+
  labs(title = "Avg. Irrigation days for HH ",
     subtitle = "from monitoring Excel data ",
     x = " ", y = " No. of days")


A_days <- water01 %>% 
  group_by(HH,date) %>%
  summarise(hr= sum(Hours)) %>%
  add_column(irri = "A") %>% 
  select(1,2,4)

A_days_start_end <- A_days %>% 
  full_join (A_days_list) %>% 
  summarise(start=min(date),end=max(date)) %>% 
  mutate(total_days=end-start)

A_days_HH_col <- spread(A_days,HH,irri)

write.csv(A1,"C:/Users/Dan/Documents/R/Saptari/data/A1.csv", row.names = FALSE)

A_days_list <- A_days[,2] %>%   distinct()

# 1 - A_days_start_end 
A0110402001 <- 
  A_days %>% 
  filter(HH=="A0110402001") %>%
  full_join (A_days_list) %>% 
  filter(date >="2018-02-14",date <= "2019-11-01") %>% 
  mutate(A0110402001=ifelse(is.na(irri),"no", "yes")) 
A0110402001 <- A0110402001[,c(2,4)]
  
# 2 - A_days_start_end 
A104507035 <- 
  A_days %>% 
  filter(HH=="A104507035") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-06-02",date <= "2019-12-11") %>% 
  mutate(A104507035=ifelse(is.na(irri),"no", "yes")) 
A104507035 <- A104507035[,c(2,4)]

A1 <- full_join(A104507035,A0110402001)

# 3 - A_days_start_end 
E0104705010 <- 
  A_days %>% 
  filter(HH=="E0104705010") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-12-19",date <= "2018-03-29") %>% 
  mutate(E0104705010=ifelse(is.na(irri),"no", "yes")) 
E0104705010 <- E0104705010[,c(2,4)]

A1 <- full_join(A1,E0104705010)

# 4 - A_days_start_end 
T100503002	 <- 
  A_days %>% 
  filter(HH=="T100503002") %>%
  full_join (A_days_list) %>% 
  filter(date >="2017-07-02",date <= "2019-09-12") %>% 
  mutate(T100503002=ifelse(is.na(irri),"no", "yes")) 
T100503002 <- T100503002[,c(2,4)]

A1 <- full_join(A1,T100503002)

rm(A104507035,A0110402001,E0104705010,T100503002)


A_days_HH_per <- A1 %>% 
  rowwise() %>% 
  mutate(sumVar = across(c(A104507035:T100503002),~ifelse(. %in% c("yes", "no"),1,0)) %>% sum) %>% 
  mutate(irri = across(c(A104507035:T100503002),~ifelse(. %in% c("yes"),1,0)) %>% sum) %>% 
  mutate(per=irri/ sumVar)


