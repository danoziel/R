
ggplot(dt_temp[1:7,], aes(x=date, y=max_temp)) +   
  geom_point() +   
  geom_line() 

ggplot(dt_attack, aes(x=date, y=`number of killed`)) +   
  geom_point() +   
  geom_line() 

dt_temp <- temperature_BeitDagan[1:366,3:4] 
colnames(dt_temp)[1] <-"date"

dt_attack <-Attacks_Btselem_data [c(305:321,323:333,335:342),c(1,3)] 
colnames(dt_attack)[1] <-"date"

dt_peace <- peace_index %>% filter(date>"2004-01-01" & date<"2005-01-01")
dt_peace <- dt_peace[,1:5] %>% mutate(ind=oslosp+oslobl+negot_sp+negot_bl) %>% 
  filter(!is.na(ind)) %>% group_by(date) %>% summarise(sum(ind)/n())





dt_temp$date <- as.POSIXct(dt_temp$date, format="%d-%m-%Y")
dt_temp$date = as.Date(dt_temp$date)  
dt_attack$date = as.Date(dt_attack$date)  
dt_peace$date <- as.POSIXct(dt_peace$date)
dt_peace$date <- as.Date.POSIXct(dt_peace$date)



class(dt_temp$date)
class(dt_attack$date)
class(dt_peace$date)


x <- right_join(dt_peace,dt_attack,by="date")



plot(dt_temp, type ="l", ylab = "temperature",col = "blue")
par(new = TRUE)
plot(dt_attack, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "",col = "red")
legend("topleft", c("temperature", "attack"),
       col = c("blue", "red"), lty = c(1, 1), cex=0.6)

