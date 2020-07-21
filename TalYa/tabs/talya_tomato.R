TALYAplot[127,50] <- 0

TALYAplot$Plant_Flowers_control_3 <- as.numeric(TALYAplot$Plant_Flowers_control_3)

talya_tomato <- TALYAplot %>%
  filter(farmer_name!="Vijaya narasimha") %>%
  filter(farmer_name!="R Chenna kista")

# tomato_height   ----
talya_tomato_height <- talya_tomato %>% 
  filter(Plant_height_control_1>0) %>%
  select(Plant_height_control_1,Plant_height_control_2,Plant_height_control_3,
         Plant_height_talya100_plot_1,Plant_height_talya100_plot_2,
         Plant_height_talya100_plot_3,year,week.,weeknum.year) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Plant_height_control_1:Plant_height_control_3)),
            MeanT = rowMeans(select(., Plant_height_talya100_plot_1:Plant_height_talya100_plot_3))) %>% 
  group_by(weeknum.year) %>% 
  summarise(`Tal-Ya`=mean(MeanT),`Control`=mean(MeanC)) %>% 
  mutate(across(is.numeric, round)) %>% 
  mutate(Week=1:17)
View(talya_tomato_height)

# graph

tomato_height <- gather(talya_tomato_height, "Group", "value", 2:3)

g_tomato_height <- ggplot(data=tomato_height, aes(x=Week, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Plant Height (In cm)") +
  xlab("Week ") +
  ylab("Height")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = Week, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)
g_tomato_height



# tomato_fruits   ----
TALYAplot[90,31] <- 42

tomato_fruits <- talya_tomato %>% 
  select(Plant_Fruits_control_1,Plant_Fruits_control_2,Plant_Fruits_control_3,
         Plant_Fruits_talya100_plot_1,Plant_Fruits_talya100_plot_2,Plant_Fruits_talya100_plot_3,
         weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Plant_Fruits_control_1:Plant_Fruits_control_3),na.rm = T),
            MeanT = rowMeans(select(., Plant_Fruits_talya100_plot_1:Plant_Fruits_talya100_plot_3),na.rm = T)) %>% 
  group_by(weeknum.year) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) %>% 
  filter(`Tal-Ya`>0) %>% 
  mutate(Week=1:11)
View(tomato_fruits)

# graph
  
tomato_fruits <- gather(tomato_fruits, "Group", "value", 2:3)

g_tomato_fruits <- ggplot(data=tomato_fruits, aes(x=Week, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Average Number of Fruits per Plant") +
  xlab("Week ") +
  ylab("No. Fruits")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = Week, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)
g_tomato_fruits





# tomato_flowers  ----
TALYAplot[127,50] <- 0
TALYAplot$Plant_Flowers_control_3 <- as.numeric(TALYAplot$Plant_Flowers_control_3)

tomato_flowers <- talya_tomato %>% 
  select(Plant_Flowers_control_1,Plant_Flowers_control_2,Plant_Flowers_control_3,
         Plant_Flowers_talya100_plot_1,Plant_Flowers_talya100_plot_2,Plant_Flowers_talya100_plot_3,
         weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Plant_Flowers_control_1:Plant_Flowers_control_3),na.rm = T),
            MeanT = rowMeans(select(., Plant_Flowers_talya100_plot_1:Plant_Flowers_talya100_plot_3),na.rm = T)) %>% 
  group_by(weeknum.year) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) %>% 
  filter(`Control`>0) %>% 
  mutate_all(funs(replace_na(.,0))) %>% 
  mutate(Week=1:13)
View(tomato_flowers)

# graph

tomato_flowers <- gather(tomato_flowers, "Group", "value", 2:3)

g_tomato_flowers <- ggplot(data=tomato_flowers, aes(x=Week, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Average Number of Flowers per Plant") +
  xlab("Week ") +
  ylab("No. flowers")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = Week, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 2)
g_tomato_flowers


# tomato_branches ----
Plant_Branches

# tomato_Weight   ----
tomato_Weight <- talya_tomato %>% 
  select(Fruit_Weight_control_1,Fruit_Weight_control_2,Fruit_Weight_control_3,
         Fruit_Weight_talya100_plot_1,
         Fruit_Weight_talya100_plot_2,
         Fruit_Weight_talya100_plot_3,weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Fruit_Weight_control_1:Fruit_Weight_control_3),na.rm = T),
            MeanT = rowMeans(select(., Fruit_Weight_talya100_plot_1:Fruit_Weight_talya100_plot_3),na.rm = T)) %>% 
  group_by(weeknum.year) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) %>% 
  filter(`Tal-Ya`> ) %>% 
  mutate(Week=1: )
View(tomato_fruits)











# Fruit_Circumference-----
tomato_circumference <-
  talya_tomato %>% select(Fruit_Circumference_control_1,Fruit_Circumference_control_2,Fruit_Circumference_control_3,
                          Fruit_Circumference_talya100_plot_1,Fruit_Circumference_talya100_plot_2,
                          Fruit_Circumference_talya100_plot_3,weeknum.year) %>% 
  na_if(0) %>% 
  transmute(weeknum.year,MeanC = rowMeans(select(., Fruit_Circumference_control_1:Fruit_Circumference_control_3),na.rm = T),
            MeanT = rowMeans(select(., Fruit_Circumference_talya100_plot_1:Fruit_Circumference_talya100_plot_3),na.rm = T)) %>% 
  group_by(weeknum.year) %>% 
  summarise(`Tal-Ya`=mean(MeanT,na.rm = T),`Control`=mean(MeanC,na.rm = T)) %>% 
  mutate(across(is.numeric, round)) %>% 
  filter(`Tal-Ya`>0) %>% 
  mutate(Week=1:11)
View(tomato_circumference)

# graph

tomato_circumference <- gather(tomato_circumference, "Group", "value", 2:3)

g_tomato_circumference <- ggplot(data=tomato_circumference, aes(x=Week, y=value, fill=Group)) +
  geom_bar(stat="identity" ,width=0.8, position=position_dodge())+
  theme_minimal()+
  ggtitle("Average Circumference of The Fruit (In cm)") +
  xlab("Week ") +
  ylab("Circumference")+
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5))+
  geom_text(
    aes(x = Week, y = value, label = value, group = Group),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 4)
g_tomato_circumference

