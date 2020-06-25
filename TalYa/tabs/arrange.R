TAL.YA..farmer19_20 <- read.csv("~/R/TalYa/TAL-YA -farmer19_20.csv", stringsAsFactors=FALSE)

write.csv(talya,"C:/Users/Dan/Documents/R/TalYa/talya11.csv")
# harvesting tomato data----
#eliminate: Vijaya narasimha (bitter guord Summer 2019)
#           R Chenna kista(maskmelon Rabi 2020)
talya <- TALYA_farmer %>%
  filter(farmer_name!="Vijaya narasimha") %>%
  filter(farmer_name!="R Chenna kista") %>%
  filter(harvest_yesno_talya100=="Yes") %>%
  rename(harvest_KG_CONTROL=harvest_KG,harvest_damage_CONTROL=harvest_damage,week_number=week.) %>%  
  select(2,18,19,20,harvest_KG_talya100,harvest_KG_CONTROL,harvest_damage_talya100,
         harvest_damage_CONTROL,KG_sold_TALYA100,KG_sold_CONTROL,average_price_TALYA100,
         revenue_TALYA100,revenue_CONTROL,average_price_TALYA100,8,harvest_yesno_talya100,
         week_number,year) 

talya <- full_join(talya,area_talya_farmers)

fix(talya)

talya <- talya %>% mutate(ty_harvest_kg_ac = harvest_KG_talya100 /talya_ac,
    ctrl_harvest_kg_ac = harvest_KG_CONTROL /control_ac,
    ty_damage_kg_ac = harvest_damage_talya100 /talya_ac,
    ctrl_damage_kg_ac = harvest_damage_CONTROL /control_ac,
    ty_kg_sold_ac = KG_sold_TALYA100 /talya_ac,
    ctrl_kg_sold_ac = KG_sold_CONTROL /control_ac,
    ty_revenue_ac = revenue_TALYA100 /talya_ac,
    ctrl_revenue_ac = revenue_CONTROL /control_ac)

# g-harvest----

g_harvest <- talya %>% summarise(`Tal-Ya plot`=mean(ty_harvest_kg_ac,na.rm = T),
                               `Control Plot`=mean(ctrl_harvest_kg_ac,na.rm = T)) %>%
  mutate(across(is.numeric, round))

g_harvest <- g_harvest %>% tidyr::gather("plot", "harvest", 1:2)

g_harvest <- ggplot(g_harvest, 
       aes(x=plot, y=harvest, fill=plot)) + 
  geom_bar(stat="identity",width=0.4)+
  theme_gray()+
  ggtitle("Harvest Per Acre (In Kg)") +
  xlab(" ") +
  ylab("Kg ")+
  geom_text(aes(label=harvest), vjust=1.5, colour="white", size=4)+ 
  scale_fill_manual(name="Plot", values=c("#a1d99b","#31a354"))+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))
  

# g-sold------
g_sold <- talya %>%
  summarise(`Tal-Ya plot`=mean(ty_kg_sold_ac,na.rm = T),
                               `Control Plot`=mean(ctrl_kg_sold_ac,na.rm = T)) %>%
  mutate(across(is.numeric, round))

g_sold <- g_sold %>% tidyr::gather("plot", "sold", 1:2)

g_sold <- ggplot(g_sold, 
       aes(x=plot, y=sold, fill=plot)) + 
  geom_bar(stat="identity",width=0.4)+
  theme_gray()+
  ggtitle("Kg Sold Per Acre") +
  xlab(" ") +
  ylab("Kg ")+
  geom_text(aes(label=sold), vjust=1.5, colour="white", size=4)+ 
  scale_fill_manual(name="Plot", values=c("#a1d99b","#31a354"))+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))

g_sold

# g_revenue-----

g_revenue <- talya %>% summarise(`Tal-Ya plot`=mean(ty_revenue_ac,na.rm = T),
                         `Control Plot`=mean(ctrl_revenue_ac,na.rm = T)) %>% 
  mutate(across(is.numeric, round))


g_revenue <- g_revenue %>% tidyr::gather("plot", "Revenue", 1:2)

g_revenue <- ggplot(g_revenue, 
       aes(x=plot, y=Revenue, fill=plot)) + 
  geom_bar(stat="identity",width=0.4)+
  theme_gray()+
  ggtitle("Revenue Per Acre") +
  xlab(" ") +
  ylab("Revenue ")+
  geom_text(aes(label=Revenue), vjust=1.5, colour="white", size=4)+ 
  scale_fill_manual(name="Plot", values=c("#a1d99b","#31a354"))+
  theme(legend.position = "none",
 plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))

g_revenue

# g_damage-----

# damaged harvest as percent of the total harvest
g_damage <- talya %>% mutate(AVt=harvest_damage_talya100/harvest_KG_talya100,
                 AVc=harvest_damage_CONTROL/harvest_KG_CONTROL) %>% 
  summarise(`Tal-Ya plot`=mean(AVt)*100,`Control Plot`=mean(AVc)*100) %>% 
  mutate(across(is.numeric, round,2)) 

g_damage <- g_damage %>% tidyr::gather("plot", "damage", 1:2)

g_damage <- ggplot(g_damage, aes(x=plot, y=damage, fill=plot)) + 
  geom_bar(stat="identity",width=0.4)+
  theme_gray()+
  ggtitle("Damaged harvest percent out of the total") +
  xlab(" ") +
  ylab("Damage % ")+
  geom_text(aes(x=plot, y=damage, label = percent(damage/100), vjust=1.5),
            position = position_dodge(width=0.9))+
  scale_fill_manual(name="Plot", values=c("#a1d99b","#31a354"))+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%"))



# ----sctters-harvest vs. sold-harvest vs. revenue----

plot(ty_harvest_kg_ac,ty_kg_sold_ac)
plot(ctrl_harvest_kg_ac,ctrl_kg_sold_ac)


p1 <- ggplot(talya,aes(x = ty_harvest_kg_ac, y = ty_kg_sold_ac, color = week_number))+
  geom_point()+
  ggtitle("tal ya -harvest vs. sold") +
  labs(x = "talya harvest kg per ac", y = "talya kg sold kg ac")

p2 <- ggplot(talya,aes(x = ctrl_harvest_kg_ac, y = ctrl_kg_sold_ac,color= farmer_name)) +
  geom_point()+
  ggtitle("control -harvest vs. sold") +
  labs(x = "control harvest kg per ac", y = "control kg sold kg ac")

p3 <- ggplot(talya,aes(x = ty_harvest_kg_ac, y = ty_revenue_ac,color= farmer_name)) +
  geom_point()+
  ggtitle("tal ya -harvest vs. revenue") +
  labs(x = "talya harvest kg per ac", y = "talya kg revenue kg ac")

p4 <- ggplot(talya,aes(x = ctrl_harvest_kg_ac, y = ctrl_revenue_ac,color= farmer_name)) +
  geom_point()+
  
gridar
  ggtitle("control -harvest vs. revenue") +
  labs(x = "control harvest kg per ac", y = "control kg revenue kg ac")

  
  
  
  (p1, p2, p3, p4, cols=2)


p1 + facet_wrap( ~ farmer_name, nrow = 1) + theme(legend.position = "none") +
  ggtitle("facetted plot")
#----farmer level----

# harvest

farmer_harvest <- talya %>% select(id,ty_harvest_kg_ac,ctrl_harvest_kg_ac) %>% 
  group_by(id) %>% filter(ctrl_harvest_kg_ac>0) %>% 
  summarise(`Tal-Ya`=mean(ty_harvest_kg_ac),
            `Control`= mean(ctrl_harvest_kg_ac)) %>% 
  mutate(across(is.numeric, round))

farmer_harvest <- gather(farmer_harvest, "Group", "value", 2:3)

farmer_harvest <- ggplot(data=farmer_harvest, aes(x=id, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_update()+
  ggtitle("Harvest Kg Per Acre- Farmer Level") +
  xlab("Farmer id") +
  ylab("Kg")+
  geom_text(
    aes(x = id, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)

#----revenue

farmer_revenue <- talya %>% select(id,ty_revenue_ac,ctrl_revenue_ac) %>% 
  group_by(id) %>% filter(ctrl_revenue_ac >0,ty_revenue_ac>0) %>% 
  summarise(`Tal-Ya`=mean(ty_revenue_ac),
            `Control`= mean(ctrl_revenue_ac)) %>% 
  mutate(across(is.numeric, round))

farmer_revenue <- gather(farmer_revenue, "Group", "value", 2:3)

farmer_revenue <- ggplot(data=farmer_revenue, aes(x=id, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_update()+
  ggtitle("Revenue Per Acre- Farmers Level") +
  xlab("Farmer id") +
  ylab("Revenue")+
  geom_text(
    aes(x = id, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)

-----
price <- talya %>% group_by(week_number) %>%filter(average_price_TALYA100!=8) %>% 
  summarise(average_price= mean(average_price_TALYA100,na.rm = T)) %>% 
  mutate(across(is.numeric, round,2))

  
ggplot(price) + 
  geom_line(aes(y = average_price, x = week_number),
            size=1, stat="identity", color = "darkred")+
  ggtitle("Prices per kg of tomatoes 20/2/2020-3/6/2020 (In IRs.)") +
  labs(x="Week number", y="Kg Price")+
  geom_vline(xintercept = 15)+
  geom_vline(xintercept = 18)+
  scale_x_continuous(breaks=seq(8,22,1))+
  scale_y_continuous(breaks=seq(1.5,8,1))
  
  


