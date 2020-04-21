attach(panel_jews_273_panels)
library(dplyr)
data <- tibble::rowid_to_column(panel_jews_273_panels, "ID")

data <- panel_jews_273_panels %>% select(survey_year,yishuv) %>%
  filter(!yishuv=="**",!yishuv=="",!yishuv=="'רושלים",!yishuv=="-")

class (data$yishuv)
data$yishuv <- as.numeric (data$yishuv)

with_yishuv_name <- data %>% filter(is.na(yishuv)) %>% group_by(survey_year)%>% tally()

sub_by_year <- data %>% select(survey_year,yishuv) %>%group_by(survey_year)%>% tally()
