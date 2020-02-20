
Lands_Baseline_2018_$total_land_cultivated
Control_and_treatment_4_districts$TreatmentControl
  
  
Graf12<-ggplot(land_TC_18,aes(x=TreatmentControl,y=total_land_cultivated))+theme_bw()+
  geom_boxplot(fill = "tan3", colour = "tan4"
               ,alpha = 0.5)+
  scale_x_discrete(name =" ") +
  scale_y_continuous(name = "land cultivation",
                     breaks = seq(0, 150,25),
                     limits=c(0, 150))+ ggtitle("Boxplot of mean Total land cultivation")

summarise(
count= n(),
sum=sum(total_land_cultivated),
av=sum/count,
mean = mean(total_land_cultivated),
sd = sd(total_land_cultivated))
