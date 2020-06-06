# org----
data <- tibble::rowid_to_column(panel_jews_273_panels, "ID")

data <- panel_jews_273_panels %>% select(survey_year,yishuv) %>%
  filter(!yishuv=="**",!yishuv=="",!yishuv=="'רושלים",!yishuv=="-")

class (data$yishuv)
data$yishuv <- as.numeric (data$yishuv)

with_yishuv_name <- data %>% filter(is.na(yishuv)) %>% group_by(survey_year)%>% tally()

sub_by_year <- data %>% select(survey_year,yishuv) %>%group_by(survey_year)%>% tally()

# -----
civilians_yesha <- Suicide_and_Bombing_Attacks_in_Israel_
civilians_il <- Suicide_and_Bombing_Attacks_in_Israel_
security_forces_yesha<- Suicide_and_Bombing_Attacks_in_Israel_
security_forces_il <- Suicide_and_Bombing_Attacks_in_Israel_

# reformat exel date to d/m/y
library(openxlsx)
civilians_IL_Attacks_in_Israel$`Date of the attack` <- convertToDateTime(civilians_IL_Attacks_in_Israel$`Date of the attack`,origin = "1900-01-01")

Attacks_Btselem_data <- Attacks_Btselem_data[,c(1,3,4,7,8,5,2,6,10,11)]
Attacks_Foreign_Office_data <- Attacks_Foreign_Office_data[,c(1,3,4,5,6,7,2,10)]



colnames(Attacks_in_Israel)[2] <-"attack_location" 
colnames(Attacks_in_Israel)[c(3,5)] <- c("injured","type_of_attack")
levels(`location: IL(1)  Yesha(2)`)
Attacks_in_Israel$`location: IL(1)  Yesha(2)` <- as.factor(`location: IL(1)  Yesha(2)`)

`location: IL(1)  Yesha(2)` <- factor(c(1,2) , levels = c("israel", "aza",))
`target: civilian(1) security-forces(2)` <-as.factor(`target: civilian(1) security-forces(2)`)
`target: civilian(1) security-forces(2)` <- factor(`target: civilian(1) security-forces(2)`,c("1","2") , levels = c("civi", "sf",))
levels(`target: civilian(1) security-forces(2)`)[1] <- "low"
levels(`target: civilian(1) security-forces(2)`)[2] <- "high"
Attacks_in_Israel$`target: civilian(1) security-forces(2)`
Attacks_in_Israel[,7:9]

# 22  6 94 - 27  2 95 :Q1
#27  3 95 -27  3 95 :Q1 Q2
# 25  4 95- 25  5 95 :Q1
#26  6 95- 26 06 01 :Q1 Q2
# 31 07 01 - 25 03 02 :all 4Q
# :Q1 Q2
# 31 05 02 - 31 08 02 :all 4Q
# :Q1 Q2
# 30 10 02 - 30 04 03 :all 4Q
# :Q1 Q2
# 31 07 03 - 28 12 05 :all 4Q
# :Q1 Q2
# 28 02 06 - 30 06 06 :all 4Q
# 31 07 06 - 31 08 06 : only Q3 Q4
# 26 09 06 :all 4Q
# 31 10 06 :Q1 Q2
# 30 11 06 - 29 02 08 :all 4Q
# 31 03 08 - 31 05 08 : only Q3 Q4
# 30 06 08 - 30 06 08 all 4Q
# 31 07 08 - 30 04 10 : only Q3 Q4
# May 2010 - april 2017 : only Q3 Q4


attach(panels)
panels$date <-  as.Date(panels$date,"%d%m%y")
class(panels$date) 

panel_date <- pd


colnames(temperature_BeitDagan)[c(4,5,6)] <- c("max_temp","min_temp","min_temp_near_ground")

colnames(temperature_BeitDagan)[3] <-"date"


