library(tidyverse)
library(kableExtra)
library(formattable)
library(tableone)
library(gridExtra)
library(scales)
library(extrafont)


water <-  Master_file_Saptari_REWSSPC_12_27_2019[1:14513,]

# water01      ----------------
water01 <- water
water01 <- water01 %>%
  rename(date=`Record Date (mm/dd/yyyy)`,Hours=`hr_n_diff`,HH=`HH ID`) %>% 
  fill(date) %>% 
  filter(date>"2017-06-01")

water01$HH [is.na(water01$HH)] <- "T3HH"
water01$HH <- ifelse(water01$FarmerName=="Nirmala Devi","T309800000",x$HH)
water01$HH [water01$HH== "T3HH"] <- "T309900000"


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
# T300406135 returned the pump
water01 <- water01 %>% filter (HH != "T300406135",HH != "Survey not done")

water01 <- water01 %>%  filter(pump_type==1)

write.csv(water01,"C:/Users/Dan/Documents/R/Saptari/data/water01.csv", row.names = FALSE)

# HH 54 count  ###----
x54 <- water01 %>%
  group_by(HH) %>% 
  count()

--------------------------------------------------------------
##  Pump usage days - as a percentage of all days  
  
# colors       ----

"Aquaculture" : dodgerblue 
"Cultivated Land" : #a1d99b

"Saptari": darkolivegreen4
"Rautahat\nBara Sarlahi":lightsalmon4 

"Total Area Cultivated" :steelblue2
"Area Irrigated" :steelblue

  # --------------------------------------------------- -----------
  # Irrigation Days ##                                         ----
### x0 fish farming+Cultivated Land  ###----
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

days_use_hh <- 
  days_use_hh %>%
  mutate(percentage= irri_days / total_days  ) %>%
  mutate(across(is.numeric,round,2))  # %>% summarise(n(),mean(percentage))

# histogram 400/700
ggplot(days_use_hh) + 
  geom_histogram(aes(x = percentage ,y=stat(count)/sum(stat(count))),
                 color= "gray20", fill = "dodgerblue3", breaks=seq(0, 1, by =0.10)) +
  scale_y_continuous(labels = scales::percent)+
  geom_vline(aes(xintercept = mean(percentage)),linetype = "dashed", size = 0.7)+
  labs(title = "Percentage of SPIP use for HH",x="Percentages of usage days", y="HH frequency",
       subtitle = " Number of days a farmer has used a pump  \nout of all the days the pump is in his possession.",
       caption = "*54 HH in sample")+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 20, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))



  

###    fish farming                  ###----
x1 <- water01 %>% 
  filter(crop %in% c("Fish Farming","Kurli")) %>% 
  group_by(HH,date) %>% summarise(s=sum(Hours)) %>% 
  group_by(HH) %>% tally()%>% rename(irri_days=n)

days_use_hh <- left_join(x0,x1)
days_use_hh$total_days <- as.numeric(days_use_hh$total_days)

days_use_hh <-
  days_use_hh %>%
  filter(!is.na(irri_days)) %>% 
  mutate(percentage= irri_days / total_days  ) %>% 
  mutate(across(is.numeric,round,2))  #%>% summarise(n(),mean(percentage))
# N=27 , Mean=0.4025926

kable(days_use_hh) %>% kable_styling()

# histogram
ggplot(days_use_hh) + 
  geom_histogram(aes(x = percentage ,y=stat(count)/sum(stat(count))),
                 color= "gray20", fill = "dodgerblue", breaks=seq(0, 1, by =0.10)) +
  scale_y_continuous(labels = scales::percent)+
  geom_vline(aes(xintercept = mean(percentage)),linetype = "dashed", size = 0.7)+
  labs(title = "Aquaculture",x="Percentages of usage days", y="HH frequency",
       caption = "*27 HH in sample")+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 15, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))


ggplot(days_use_hh, aes(x = percentage))+
  geom_histogram(color= "gray20", fill = "royalblue1",
                 breaks=seq(0, 1, by =0.10)) +
  geom_vline(aes(xintercept = mean(percentage)), 
             linetype = "dashed", size = 0.7)+
  labs(title="Aquaculture - percentage of SIP use for HH") +
  labs(x="percentage", y="No. of HH") + 
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 10,2))





###    Cultivated Land               ###----
x1 <- water01 %>% 
  filter(crop != "Fish Farming") %>% 
  group_by(HH,date) %>% summarise(s=sum(Hours)) %>% 
  group_by(HH) %>% tally() %>% rename(irri_days=n)

days_use_hh <- left_join(x0,x1)
days_use_hh$total_days <- as.numeric(days_use_hh$total_days)

days_use_hh <- days_use_hh %>%
  filter(!is.na(irri_days)) %>% 
  mutate(percentage= irri_days / total_days  ) %>%
  mutate(across(is.numeric,round,2)) %>% summarise(n(),mean(percentage))
# N=44	Mean= 0.2206818
kable(days_use_hh) %>% kable_styling()

# histogram
ggplot(days_use_hh) + 
  geom_histogram(aes(x = percentage ,y=stat(count)/sum(stat(count))),
                 color= "gray20", fill = "#a1d99b", breaks=seq(0, 1, by =0.10)) +
  scale_y_continuous(labels = scales::percent)+
  geom_vline(aes(xintercept = mean(percentage)),linetype = "dashed", size = 0.7)+
  labs(title = "Cultivated Land",x="Percentages of usage days", y="HH frequency",
       caption = "*44 HH in sample")+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 15, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))




###  Monsoon   ###----
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

days_use_hh_Monsoon <-
  days_use_hh %>% 
  mutate(percentage= n / total_days  ) %>%
  summarise(N=n(),Mean=mean(percentage), SD=sd(percentage)) %>% 
  mutate(across(is.numeric,round,2)) 
  
kable(days_use_hh_Monsoon) %>% kable_styling()

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



###  winter    ###----
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

days_use_hh_Winter <-
  days_use_hh %>% 
  mutate(percentage= n / total_winter_days  ) %>%
  summarise(N=n(),Mean=mean(percentage), SD=sd(percentage)) %>% 
  mutate(across(is.numeric,round,2)) 

kable(days_use_hh_Winter) %>% kable_styling()

# histogram
ggplot(days_use_hh_Winter, aes(x = percentage))+
  geom_histogram(color= "gray20", fill = "royalblue1",
                 breaks=seq(0, 1, by =0.10)) +
  geom_vline(aes(xintercept = mean(percentage)), 
             linetype = "dashed", size = 0.7)+
  labs(title="Winter - Cultivated Land - percentage of SIP use for HH") +
  labs(x="percentage", y="No. of HH") + 
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 26,2))+
  theme(plot.title = element_text( size =10))

###  summer    ###----
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

days_use_hh_Summer <- 
  days_use_hh %>% 
  mutate(percentage= n / total_summer_days  ) %>%
  summarise(N=n(),Mean=mean(percentage), SD=sd(percentage)) %>% 
  mutate(across(is.numeric,round,2)) 

kable(days_use_hh_Summer) %>% kable_styling()

# histogram
ggplot(days_use_hh_Summer, aes(x = percentage))+
  geom_histogram(color= "gray20", fill = "royalblue1",
                 breaks=seq(0, 1, by =0.10)) +
  geom_vline(aes(xintercept = mean(percentage)), 
             linetype = "dashed", size = 0.7)+
  labs(title="Summer - Cultivated Land - percentage of SIP use for HH") +
  labs(x="percentage", y="No. of HH") + 
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(breaks = seq(0, 26,2))+
  theme(plot.title = element_text( size =10))

###  days_use_hh_Seasons   ###----
days_use_hh_Seasons <- rbind(days_use_hh_Monsoon,days_use_hh_Winter,days_use_hh_Summer)
days_use_hh_Seasons$Season <- c("Monsoon","Winter","Summer")
days_use_hh_Seasons <- days_use_hh_Seasons[,c(4,1,2,3)]

kable(days_use_hh_Seasons,caption = "Cultivated Land by Seasons") %>% kable_styling()


  #----------------------------------------------------------------
  #  Total Area Cultivated" vs. "Area Irrigated per day (ha)   ----
###  Cereals      ###----              
Irrigated_Cereals <- water01 %>% 
  mutate(total_area_cultivated_ha=`Total Area Cultivated`*0.0339,
         area_irrigated_ha=`Area Irrigated`*0.0339) %>% 
  filter(crop %in% c("Paddy", "Summer Paddy","paddy","Maize","Wheat")) %>%
  group_by(HH,date) %>%
  summarise_at(c("total_area_cultivated_ha","area_irrigated_ha"), sum, na.rm = TRUE) %>% 
  group_by(HH) %>%
  summarise_at(c("total_area_cultivated_ha","area_irrigated_ha"), mean, na.rm = TRUE) %>% 
  summarise(N=n(),`Total Area Cultivated`=mean(total_area_cultivated_ha),
            SD1=sd(total_area_cultivated_ha),
            `Area Irrigated`=mean(area_irrigated_ha),
            SD2=sd(area_irrigated_ha)) %>% 
  mutate(across(is.numeric,round,2))



###  Vegetables   ###----
Irrigated_Vegetables <- water01 %>% 
  mutate(total_area_cultivated_ha=`Total Area Cultivated`*0.0339,
         area_irrigated_ha=`Area Irrigated`*0.0339) %>% 
  filter(crop %in% c("Bitter gourd" , "Bitter Gourd" , "Bottle Gourd","Brinjal","Cabbage",
                     "cauliflower","Cauliflower","Chilli","Garlic","Grass",
                     "Green Leafy Vegetables","Lady's Finger","Long Yard Beans","Mango Plant",
                     "Luffa Gourd","Onion","Potato","Pumpkin","Radish","Ridge Gourd",
                     "Sponge Gourd","Sugarcane","Sunflower","Tomato","vegetable","vegetables")) %>% 
  group_by(HH,date) %>%
  summarise_at(c("total_area_cultivated_ha","area_irrigated_ha"), sum, na.rm = TRUE) %>% 
  group_by(HH) %>%
  summarise_at(c("total_area_cultivated_ha","area_irrigated_ha"), mean, na.rm = TRUE) %>% 
  summarise(N=n(),`Total Area Cultivated`=mean(total_area_cultivated_ha),
            SD1=sd(total_area_cultivated_ha),
            `Area Irrigated`=mean(area_irrigated_ha),
            SD2=sd(area_irrigated_ha))%>% 
  mutate(across(is.numeric,round,2))
  
  

###  Oilseeds     ###----
Irrigated_Oilseeds <- water01 %>% 
  mutate(total_area_cultivated_ha=`Total Area Cultivated`*0.0339,
         area_irrigated_ha=`Area Irrigated`*0.0339) %>% 
  filter(crop %in% c("Fenugreek","Linseed","Mustard","Oil","Sesame Seeds")) %>% 
  group_by(HH,date) %>%
  summarise_at(c("total_area_cultivated_ha","area_irrigated_ha"), sum, na.rm = TRUE) %>% 
  group_by(HH) %>%
  summarise_at(c("total_area_cultivated_ha","area_irrigated_ha"), mean, na.rm = TRUE) %>% 
  summarise(N=n(),`Total Area Cultivated`=mean(total_area_cultivated_ha),
            SD1=sd(total_area_cultivated_ha),
            `Area Irrigated`=mean(area_irrigated_ha),
            SD2=sd(area_irrigated_ha))%>% 
  mutate(across(is.numeric,round,2))



###  Pulses       ###----
Irrigated_Pulses <- water01 %>% 
  mutate(total_area_cultivated_ha=`Total Area Cultivated`*0.0339,
         area_irrigated_ha=`Area Irrigated`*0.0339) %>% 
  filter(crop %in% c("Beans" , "Black Eyed Beans","Horse Gram","Lentil","Red Kidney Beans")) %>% 
  group_by(HH,date) %>%
  summarise_at(c("total_area_cultivated_ha","area_irrigated_ha"), sum, na.rm = TRUE) %>% 
  group_by(HH) %>%
  summarise_at(c("total_area_cultivated_ha","area_irrigated_ha"), mean, na.rm = TRUE) %>% 
  summarise(N=n(),`Total Area Cultivated`=mean(total_area_cultivated_ha),
            SD1=sd(total_area_cultivated_ha),
            `Area Irrigated`=mean(area_irrigated_ha),
            SD2=sd(area_irrigated_ha))%>% 
  mutate(across(is.numeric,round,2))
###  cereal_veg_oil_puls - table                        ###----
cereal_veg_oil_puls <- rbind(Irrigated_Cereals,Irrigated_Vegetables,
                             Irrigated_Oilseeds,Irrigated_Pulses)
cereal_veg_oil_puls$Crop <- c("Cereals", "Vegetables", "Oilseeds", "Pulses")
cereal_veg_oil_puls <- cereal_veg_oil_puls[,c(6,1:5)]

###  Cultivated Land in general - barplot scatterplot   ###----

x2 <- water01 %>%
  group_by(HH,date) %>% 
  filter(!crop %in% c("Fish Farming","Kurli")) %>% 
  summarise(`Total Area Cultivated`=sum(`Total Area Cultivated`)*0.0339,`Area Irrigated`=sum(`Area Irrigated`)*0.0339)%>%
  summarise_at(c("Total Area Cultivated" ,"Area Irrigated"), mean, na.rm = TRUE) %>% 
  summarise_at(c("Area Irrigated","Total Area Cultivated"), mean, na.rm = TRUE)%>%
  mutate(across(is.numeric,round,2))
 
x2 <- x2 %>% gather("Area", "Hectar", 1:2)

#bar plot
ggplot(data=x2, aes(x=Area, y=Hectar, fill=Area)) +
  geom_bar(stat="identity", width=0.5) +
  geom_text(aes(label=Hectar), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)+
  labs(title="Avg. irrigated area vs. Avg. cultivated area  - per day (ha)", x=" ")+
  theme(plot.title = element_text( size = 9))+
  guides(fill=FALSE)+
  theme_minimal() +
  scale_fill_manual(values=c("steelblue", "steelblue2"))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 10, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))

#
x22 <- water01 %>%
  group_by(HH,date,district) %>% 
  filter(!crop %in% c("Fish Farming","Kurli")) %>% 
  mutate(`Total Area Cultivated`=`Total Area Cultivated`*0.0339,
            `Area Irrigated`=`Area Irrigated`*0.0339) %>%
  summarise_at(c("Total Area Cultivated" ,"Area Irrigated"), sum, na.rm = TRUE) %>% 
  group_by(HH,district) %>% 
  summarise_at(c("Total Area Cultivated" ,"Area Irrigated"), mean, na.rm = TRUE)
x22$District <- ifelse(x22$district=="Saptari","Saptari","Rautahat\nBara Sarlahi")

#scatter plot
ggplot(x22, aes(x =`Total Area Cultivated`, y = `Area Irrigated`, color= District))+
  geom_point()+
  labs(title="Irrigated area & cultivated area",
       caption = "*44 HH in sample")+
  ylim(0, 0.7)+xlim(0, 0.7)+  
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 15, margin = margin(b = 10)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))+
  guides(color = guide_legend(override.aes = list(size = 4)))+
  scale_color_manual(values=c("lightsalmon4", "darkolivegreen4"))

## by district - table   ----
x2 <- water01 %>%
  group_by(district,HH,date) %>% 
  filter(!crop %in% c("Fish Farming","Kurli")) %>% 
  summarise(`Total Area Cultivated`=sum(`Total Area Cultivated`)*0.0339,`Area Irrigated`=sum(`Area Irrigated`)*0.0339)%>%
  summarise_at(c("Total Area Cultivated" ,"Area Irrigated"), mean, na.rm = TRUE) %>% 
  summarise(N=n(),`Total Area Cultivated`=mean(`Total Area Cultivated`),
            SD1=sd(`Total Area Cultivated`),
            `Area Irrigated`=mean(`Area Irrigated`),
            SD2=sd(`Area Irrigated`)) %>% 
  mutate(across(is.numeric,round,2))

## by season   - table   ----
x2 <- water01 %>%
  mutate(SEASONs=case_when(
    season == "Monsoon_2017_2018" | season == "Monsoon_2018_2019" |
      season == "Monsoon_2019_2020" ~ "Monsoon",
    season == "Summer_2017_2018" |season == "Summer_2018_2019" ~ "Summer",
    season == "Winter_2017_2018" | season == "winter_2018_2019" |
      season == "winter_2019_2020"~"Winter")) %>% 
  group_by(SEASONs,HH,date) %>% 
  filter(!crop %in% c("Fish Farming","Kurli")) %>% 
  summarise(`Total Area Cultivated`=sum(`Total Area Cultivated`)*0.0339,`Area Irrigated`=sum(`Area Irrigated`)*0.0339)%>%
  summarise_at(c("Total Area Cultivated" ,"Area Irrigated"), mean, na.rm = TRUE) %>% 
  summarise(N=n(),`Total Area Cultivated`=mean(`Total Area Cultivated`),
            SD1=sd(`Total Area Cultivated`),
            `Area Irrigated`=mean(`Area Irrigated`),
            SD2=sd(`Area Irrigated`)) %>% 
  mutate(across(is.numeric,round,2))



# library(scales)

  #----------------------------------------------------------------
  # Irrigation Hours                                           ----

# Avg. irrigation hours per day                                ----
# Cultivated Land    ----
#all
xc <- water01 %>% 
  filter(!crop %in% c("Fish Farming","Kurli")) %>% 
  group_by(HH,date) %>%  
  summarise(Hours=sum(Hours)) %>% 
  group_by(HH) %>% 
  summarise(Hours=mean(Hours)) %>% 
  summarise(N=n(),`Hours per day`=mean(Hours),SD=sd(Hours))
kable(xc) %>% kable_styling()

# district
xcd <- water01 %>% 
  filter(!crop %in% c("Fish Farming","Kurli")) %>% 
  group_by(district,HH,date) %>%  
  summarise(Hours=sum(Hours)) %>% 
  group_by(district,HH) %>% 
  summarise(Hours=mean(Hours)) %>% 
  summarise(N=n(),`Hours per day`=mean(Hours),SD=sd(Hours))
kable(xcd) %>% kable_styling()


# Aquaculture        ----
# omit A104507035 bcoz NAs
# all
xa <- water01 %>% 
  filter(crop %in% c("Fish Farming","Kurli"),HH != "A104507035") %>% 
  group_by(HH,date) %>%  
  summarise(Hours=sum(Hours)) %>% 
  group_by(HH) %>% 
  summarise(Hours=mean(Hours)) %>% 
  summarise(N=n(),`Hours per day` =mean(Hours),SD=sd(Hours))
kable(xa) %>% kable_styling()

#district
xad <- water01 %>% 
  filter(crop %in% c("Fish Farming","Kurli"),HH != "A104507035") %>% 
  group_by(district,HH,date) %>%  
  summarise(Hours=sum(Hours)) %>% 
  group_by(district,HH) %>% 
  summarise(Hours=mean(Hours)) %>% 
  summarise(N=n(),`Hours per day`=mean(Hours),SD=sd(Hours))
kable(xad) %>% kable_styling()

#   barplot  400/300        ----
rbind(xa,xc) %>% 
  mutate(Crop= c("Aquaculture","Cultivated Land")) %>%
  select(4,everything()) %>% 
  mutate(across(is.numeric,round,2)) %>% 

  ggplot(aes(x=Crop, y= `Hours per day`, fill=Crop)) +
  geom_bar(stat="identity", position=position_dodge(), width=.5) +
  geom_errorbar(aes(ymin= `Hours per day`-SD, ymax=`Hours per day`+SD), width=.2,
                position=position_dodge(.9))+
  
  geom_text(aes(label=`Hours per day`), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)+
  labs(title="HH Avg. irrigation hours  - per day", x=" ")+
  theme(plot.title = element_text( size = 9))+
  guides(fill=FALSE)+
  theme_minimal() +
  scale_fill_manual(values=c("dodgerblue", "#a1d99b"))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 10, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))

#   Districts table     ----
rbind(xad,xcd) %>% 
  mutate(Crop= c("Aquaculture"," " ,"Cultivated Land", " ")) %>%
  select(5,everything()) %>% 
  mutate(across(is.numeric,round,2)) %>% 
  kable() %>% kable_styling()

# total irrigation hours per HH                                ----
#   barplot      ----
xc <- water01 %>% 
  filter(!crop %in% c("Fish Farming","Kurli")) %>% 
  group_by(HH) %>%  
  summarise(Hours=sum(Hours)) %>% 
  summarise(N=n(),`Total Hours`=mean(Hours),SD=sd(Hours)) %>% 
  mutate(across (is.numeric,round)) %>% 
  mutate(district= c("Total district"))%>% 
  select(4,everything())

xa <- water01 %>% 
  filter(crop %in% c("Fish Farming","Kurli"),HH != "A104507035") %>% 
  group_by(HH) %>%  
  summarise(Hours=sum(Hours)) %>% 
  summarise(N=n(),`Total Hours`=mean(Hours),SD=sd(Hours)) %>% 
  mutate(across (is.numeric,round))%>% 
  mutate(district= c("Total district")) %>% 
  select(4,everything())

xcd <- water01 %>% 
  filter(!crop %in% c("Fish Farming","Kurli")) %>% 
  group_by(district,HH) %>%  
  summarise(Hours=sum(Hours)) %>% 
  summarise(N=n(),`Total Hours`=mean(Hours),SD=sd(Hours)) %>% 
  mutate(across (is.numeric,round))

xad <- water01 %>% 
  filter(crop %in% c("Fish Farming","Kurli"),HH != "A104507035") %>% 
  group_by(district,HH) %>%  
  summarise(Hours=sum(Hours)) %>% 
  summarise(N=n(),`Total Hours`=mean(Hours),SD=sd(Hours)) %>% 
  mutate(across (is.numeric,round))

#   plot 600/500 
rbind(xa,xad,xc,xcd) %>% rename(District = district) %>% 
  mutate(District = ifelse(District == "Rautahat_Bara_Sarlahi",
                           "Rautahat\nBara Sarlahi", District)) %>% 
  mutate(crop= c("Aquaculture","Aquaculture","Aquaculture",
                 "Cultivated Land","Cultivated Land","Cultivated Land")) %>%

  ggplot(aes(x=crop, y= `Total Hours`, fill=District)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=`Total Hours`-SD, ymax=`Total Hours`+SD), width=.2,
                position=position_dodge(.9))+

  geom_text(aes(label=`Total Hours`), vjust=1.6, color="white",
            position = position_dodge(.9), size=3.5)+
  labs(title="HH Total irrigation hours  - per day", x=" ")+
  theme(plot.title = element_text( size = 9))+
  theme_minimal() +
  scale_fill_manual(values=c("lightsalmon4", "darkolivegreen4","dimgrey"))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 20, margin = margin(b = 10)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))



#----------------------------------------------------------------
#----------------------------------------------------------------
#    Not in paper        
# Hours_by_day  Saptari Timeline ----
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

# Hours_by_day  Rautahat_Bara_Sarlahi Timeline -----
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
  
# Cultivated Land - HH freq - Hours per day ----
sh_ <- water01 %>% 
  filter(!crop %in% c("Fish Farming","Kurli")) %>% 
  group_by(HH,date) %>%  
  summarise(Hours=sum(Hours)) %>% 
  group_by(HH) %>% 
  summarise(Hours=mean(Hours)) %>% 
  ggplot(aes(x = Hours))+
  geom_histogram(color= "gray20", fill = "royalblue2",
                 breaks=seq(0, 9, by =0.5)) +
  geom_vline(aes(xintercept = mean(Hours)), 
             linetype = "dashed", size = 0.7)+
  labs(title="Cultivated Land - Hours of SIP use for HH") +
  labs(x="Hours", y="No. of HH") + 
  scale_x_continuous(breaks = seq(0, 9, 1))+
  scale_y_continuous(breaks = seq(0, 15,2))
# HH list - percentage of SPIP usege        ----
# The "days_use_hh" from "x0 fish farming+Cultivated Land"

library(scales)
ggplot(days_use_hh, aes(x=HH, y=percentage)) + 
  geom_bar(stat="identity",width=0.4)+
  ggtitle("days_use_hh percent out of the total") +
  xlab(" ") +
  ylab(" ")+
  geom_text(aes(x=HH, y=percentage, label = percent(percentage), vjust=1.5),
            position = position_dodge(width=0.9))+
  scale_fill_manual(name="HH", values=c("#a1d99b"))+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%"))+
  coord_flip()



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
  # avg. hours - for every HH 
  pump <- water01 %>% 
  filter (HH != "Survey not done") %>% 
  group_by(district, HH,pump_type) %>% 
  summarise (Hours=sum(Hours,na.rm = TRUE)) %>% 
  filter(Hours>0) %>% 
  mutate(across(is.numeric,round))

ggplot(data=pump, aes(x=HH, y=Hours, fill=pump_type)) +
  geom_bar(stat="identity")+
  labs(title="Avg. Irrigation hours for a HH", 
       subtitle="y axis is 53 HH who monitored in 6/2017-12/2019",
       x=" ",y=" ")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = rel(1), vjust=0.5),
        axis.text.y = element_text(size = rel(0.5), vjust=0.5),
        plot.title = element_text(size = rel(1), hjust = 0.5)) + 
  coord_flip()

ggplot(data=pump,aes(x=HH, y=Hours,fill=district)) +
  geom_col(show.legend = FALSE) +
  theme(axis.text.y = element_text(size = rel(0.5), vjust=0.5))+
  facet_wrap(~district, scales="free_y")+
  coord_flip()


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
  
  -------------------------------------------------------------------------
  # row: HH , col: seasons ----
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

District <- hr_per_season[,c(1,2)]

trmt_water_hr_use <-
  hr_per_season %>% 
  left_join(hr_aqua_season) %>% 
  left_join(District)

write.csv(trmt_water_hr_use,"C:/Users/Dan/Documents/R/Saptari/data/trmt_water_hr_use.csv", row.names = FALSE)

------------------------------------------------------------------------------------------
  
  
  
  
  # by day------------------------------------------------------------

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


# scatterplot MONITORING vs. SURVEY         --------------------------------------------
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


