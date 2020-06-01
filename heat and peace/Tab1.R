attach(panel_jews_273_panels)
library(dplyr)
# org----
data <- tibble::rowid_to_column(panel_jews_273_panels, "ID")

data <- panel_jews_273_panels %>% select(survey_year,yishuv) %>%
  filter(!yishuv=="**",!yishuv=="",!yishuv=="'רושלים",!yishuv=="-")

class (data$yishuv)
data$yishuv <- as.numeric (data$yishuv)

with_yishuv_name <- data %>% filter(is.na(yishuv)) %>% group_by(survey_year)%>% tally()

sub_by_year <- data %>% select(survey_year,yishuv) %>%group_by(survey_year)%>% tally()

# library(googlesheets)----
# op 1
library(googlesheets)

url <- "https://docs.google.com/spreadsheets/d/1wCk_OY4p7GlkxiyqRFxd7oILNo0J67mjiTzds-2ZOPw/edit#gid=2109757684"
dat <- gs_read(url)
# op2
library(googlesheets)
gs_auth()

name <- "name of google sheet"
spreadsheet_object <- gs_title(name)

dat <- gs_read(spreadsheet_object)
# -----
civilians_yesha <- Suicide_and_Bombing_Attacks_in_Israel_
civilians_il <- Suicide_and_Bombing_Attacks_in_Israel_
security_forces_yesha<- Suicide_and_Bombing_Attacks_in_Israel_
security_forces_il <- Suicide_and_Bombing_Attacks_in_Israel_

attach(security_forces_il)
attach(security_forces_IL_Attacks_in_Israel)

# reformat exel date to d/m/y
library(openxlsx)
civilians_IL_Attacks_in_Israel$`Date of the attack` <- convertToDateTime(civilians_IL_Attacks_in_Israel$`Date of the attack`,origin = "1900-01-01")

df<- rbind(civilians_yesha,civilians_il,security_forces_yesha,security_forces_il)
df <- df %>% arrange(`date of the attack`)

library(openxlsx)
write.xlsx(df, 'Attacks_in_IsraelX2.xlsx')
