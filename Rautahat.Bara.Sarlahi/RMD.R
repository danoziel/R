

```{r base, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}

Land_18_19 <- read.csv("~/R/Rautahat.Bara.Sarlahi/data/Land_18_19.csv")


--------------------------------------------------------------------------------------------------
  irrigate_hr
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

