

```{r base, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}

Land_18_19 <- read.csv("~/R/Rautahat.Bara.Sarlahi/data/Land_18_19.csv")

dfb <- Land_18_19 %>% 
  filter(year==2018,total_land_cultivated_year>0) %>%
  mutate(ci=total_land_cultivated/nca,ii=irrigated_out_of_tot_land_cult/nca) %>%
  group_by( household_questionnaire_id) %>%
  summarise(own=sum(total_ownland_cultivated)*0.0338,
            cult=sum(total_land_cultivated)*0.0338) %>% 
  summarise(N=n(), `Ownland Cultivated`=mean(own),`Land Cultivated`=mean(cult)) %>% 
  mutate(across(is.numeric, round, 2))

kable(dfb,"latex", booktabs = T ,align = "lcc") %>% row_spec(0, bold = T, color = "black", background = NULL )

--------------------------------------------------------------------------------------------------















-------------------------------------------------------------------------------------------
c <- Agriculture_18_19 %>% filter(season_of_crop!="Annual") %>% 
  group_by(year,season_of_crop,TreatmentControl,household_questionnaire_id) %>%
  summarise(hr_per_ha=mean(hrs_irr_1katha)/0.0339,irrigate_hr=sum(irri_for_season))%>% 
  group_by(TreatmentControl,season_of_crop,year) %>%
  summarise(n=n(),average_hr_per_ha=mean(hr_per_ha,na.rm = T),
            total_irrigate_hr=mean(irrigate_hr,na.rm = T)) %>% 
  mutate(across(is.numeric, round, 2))

cc <- c[1:6,]
ct <- c[7:12,4:6]
cx <- cbind(cc,ct)[,-1]

kable(cx,col.names = c("Season","Year","N","mean 1 ha","Total","N","mean 1 ha","Total"),align = "c") %>%
  kable_styling(latex_options = "striped",full_width = F) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1, valign = "top")
-------------------------------------------------------------------------
# year
irri_inten_y <- Land_18_19 %>% filter(season!="Annual") %>% 
  filter(total_land_cultivated_year>0) %>%
  mutate(ii=irrigated_out_of_tot_land_cult/nca) %>%
  group_by(TreatmentControl, season,year,household_questionnaire_id) %>%
  summarise(irrigate_intensity=sum(ii)) %>% 
  group_by(TreatmentControl,year,household_questionnaire_id) %>%
  summarise(ir_in=sum(irrigate_intensity)) %>% 
  summarise(N=n(),`Irrigation Intensity`=mean(ir_in)*100) %>% 
  mutate(across(is.numeric, round, 2))

iyc <- irri_inten_y[1:2,]
iyt <- irri_inten_y[3:4,3:4]
iyc_iyt <- cbind(iyc,iyt)[,-1]

kable(iyc_iyt, "latex",col.names = c("Year","N","Intensity","N","Intensity"), booktabs = T,align = "lcccc",linesep = "") %>%
  column_spec(1, bold = T) %>%
  kable_styling(latex_options = "striped", position = "left") %>% 
  column_spec(1:5, width = "1.5cm",border_left = F) %>%
  column_spec(3,border_right = T ,width = "1.5cm") %>%
  row_spec(0, font_size= 9) %>% 
  add_header_above(c(" " = 1, "Control" = 2, "Treatment" = 2), bold = F, align = "c")

---------------------------------------------------------------------------------------
# seasons
irri_inten_s <- Land_18_19 %>% filter(season!="Annual") %>% 
  filter(total_land_cultivated_year>0) %>%
  mutate(ii=irrigated_out_of_tot_land_cult/nca) %>%
  group_by(TreatmentControl, season,year,household_questionnaire_id) %>%
  summarise(irrigate_intensity=sum(ii)) %>% 
  summarise(N=n(),`Irrigation Intensity`=mean(irrigate_intensity)*100) %>% 
  mutate(across(is.numeric, round, 2))

iic <-irri_inten[1:6,]
iit <- irri_inten[7:12,4:5]
iic_iit <- cbind(iic,iit)[,-1]

kable(iic_iit,col.names = c("Season","Year","N","mean","N","mean"), booktabs = T,align = "llcccc",linesep = "") %>%
  column_spec(1, bold = T) %>%
  kable_styling(latex_options = "striped", position = "left") %>% 
  column_spec(1:6, width = "1.5cm") %>%
  column_spec(4,border_right = T ,width = "1.5cm") %>% 
  collapse_rows(columns = 1, valign = "top") %>% 
  row_spec(0, font_size= 9) %>% 
  add_header_above(c(" " = 2, "Control" = 2, "Treatment" = 2), bold = F, align = "c")

--------------------------------------------------------------------------------------

