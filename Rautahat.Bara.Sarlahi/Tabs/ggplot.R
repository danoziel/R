https://www.learnbyexample.org/r-bar-plot-base-graph/
# ------
data <- bind_rows(xx,yy)
data <- data.frame(year=rep(c("2018", "2019"),each=2),
                   group=c("Control", "Treatment","Control", "Treatment"),
                   average=c(0.65,1.06 ,0.53,1.31),
                   n=c(19,16,18,17))

ggplot(data, aes(x=year, y=average, fill=group)) +
  geom_bar(stat="identity",width=0.5,position=position_dodge(width=0.6))+
  scale_fill_brewer(palette="Dark2")+
  ggtitle("              Total area of pond")+ 
  theme(plot.title = element_text(lineheight=.8, face="bold",size=10))+
  xlab("") +
  ylab("hectare")+
  scale_y_continuous(breaks=seq(0,1.5,0.25))+
  geom_text(aes(label=average), position=position_dodge(width=0.6), vjust=-0.25, size=2.8)

# -----
my_bar <- barplot(height=data$average ,
                  border="black" ,
                  cex.names=0.7,
                  las=1 , 
                  xlab="2018                           2019", 
                  ylab="hectare",
                  col=c("#FC8D62","#A6D854") ,
                  space=c(2,0.1,0.5,0.1),
                  width=c(2,2,2,2),
                  font.axis=1, 
                  col.axis="dimgrey", 
                  cex.axis=0.8,
                  ylim=c(0,1.8) , 
                  main="Total area of pond",
                  font.lab=1, 
                  col.lab="black", 
                  cex.lab=1)

text(my_bar, data$average+0.2 , paste(data$average) ,cex=0.8,col="gray29")

legend("topright", legend = c("Control","Treatment") , 
       col = c("#FC8D62","#A6D854") , 
       bty = "n", pch=20 , pt.cex = 1.5, cex = 0.7, horiz = T, inset = c(0,-0.08))

# ------
# 
hist(R_Agriculture_Baseline_2018_$percent_for_selling, 
     main="Percent for selling", 
     xlab="Percent", 
     border="black", 
     col="cadetblue2",
     xlim=c(0,100),
     las=1, 
     breaks=10)

# 
counts <- table(R_Agriculture_Baseline_2018_$name_of_crop)
HHcrop <- barplot(counts, main="No. HH grow the crop",
        ylab="HH",las=2,
        cex.names=0.8,
        cex.axis =0.8,
        col = c("lightblue", "mistyrose", "lightcyan","lavender", "cornsilk"),
        )


