plot(model)

plot(week1ago[intifada==0] ,q3[intifada==0],col="blue",
     ylim = c(0,5),xlim = c(0,15))
points(week1ago[intifada==1] ,q3[intifada==1],col="red",pch = 16 )
abline(a=2.036, b=0.0026,col="blue",lwd=1)
abline(a=2.08, b=0.0026,col="red",lwd=1)

pairs(bind11[,2:7], pch = 19, upper.panel = NULL)