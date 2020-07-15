library(MatchIt)
library(tableone)
# grid for matching----





# total_litres_consumed_dieselkero -- S ----

litres_S <-  Procurement_17_18_19 %>%
  filter(!is.na(total_litres_consumed_dieselkero))%>%
  mutate(after_1Y = year == 2018, 
         after_2Y = year == 2019, 
         own_sp = TC == 1)

litres_S_M <- 
  select(filter(litres_S, year == 2017)
         ,c(household_questionnaire_id,at_which_price_did_you_buy_it,
            total_litres_consumed_dieselkero:own_sp))

tableBEFORE <- CreateTableOne(vars = 'total_litres_consumed_dieselkero', 
                          data = litres_S_M, 
                          strata = 'TreatmentControl')

match.it <- matchit(own_sp ~ total_litres_consumed_dieselkero , data = litres_S_M, method="nearest", ratio=1)
a <- summary(match.it)

plot(match.it, type = 'jitter', interactive = FALSE)

df.match <- match.data(match.it)[1]

df.match <-inner_join (df.match,litres_S,by="household_questionnaire_id")


# after 1 year
df.match18 <- filter(df.match,year==2018)

pacman::p_load(tableone)
table18 <- CreateTableOne(vars = c('total_litres_consumed_dieselkero'), 
                         data = df.match18, 
                         strata = 'TreatmentControl')

ggplot(data = df.match18, mapping = aes(x=TreatmentControl, 
                                        y=total_litres_consumed_dieselkero,
                                        fill=TreatmentControl)) +
  geom_boxplot() + theme_bw() +
  scale_fill_brewer(palette = "Accent")


########## lm
pro <- filter(df.match,year==2018)
fit <- lm(total_litres_consumed_dieselkero ~ TC, data=pro)
summary(fit)

# after 2 year
df.match19 <- filter(df.match,year==2019)

pacman::p_load(tableone)
table19 <- CreateTableOne(vars = c('total_litres_consumed_dieselkero'), 
                         data = df.match19, 
                         strata = 'TreatmentControl')


table199 <- print(table19, 
                printToggle = FALSE, 
                noSpaces = TRUE)

table199 [,1:3]%>%
  kable() %>%
  kable_styling()
rm (tableBEFORE,match.it,df.match,df.match18,df.match19)
rm(litres_RBS,litres_S,litres_S_M)
# Irrigation Intensity -------------- S -------
irri_intens_S <- land_17_18_19 %>% filter(total_land_cultivated_year>0,season!="Annual") %>% 
  group_by(TreatmentControl,year,household_questionnaire_id) %>%
  summarise(gross_irri=sum(irrigated_out_of_tot_land_cult),gross_crop=sum(total_land_cultivated),ii=gross_irri/gross_crop*100) %>% 
  filter(ii>= 0,ii<=100) 

irri_intens_S_M <- irri_intens_S %>% 
  mutate(after_1Y = year == 2018, 
         after_2Y = year == 2019, 
         own_sp = TreatmentControl == "Treatment")

tableBEFORE <- CreateTableOne(vars = 'ii', 
                              data = irri_intens_S_M, 
                              strata = 'TreatmentControl')

irri_intens_S_M <- filter(irri_intens_S_M, year == 2017)

match.it <- matchit(own_sp ~ ii , data = irri_intens_S_M, method="nearest", ratio=1)
a <- summary(match.it)

plot(match.it, type = 'jitter', interactive = FALSE)

df.match <- match.data(match.it)[3]

df.match <-inner_join (df.match,irri_intens_S,by="household_questionnaire_id")


# after 1 year
df.match18 <- filter(df.match,year==2018)

pacman::p_load(tableone)
table18 <- CreateTableOne(vars = c('ii'), 
                          data = df.match18, 
                          strata = 'TreatmentControl')

ggplot(data = df.match18, mapping = aes(x=TreatmentControl, 
                                        y=ii,
                                        fill=TreatmentControl)) +
  geom_boxplot() + theme_bw() +
  scale_fill_brewer(palette = "Accent")


# after 2 year
df.match19 <- filter(df.match,year==2019)

pacman::p_load(tableone)
table19 <- CreateTableOne(vars = c('ii'), 
                          data = df.match19, 
                          strata = 'TreatmentControl')

# lm
x <- inner_join(irri_intens_S,Control_and_treatment_4_districts)
pro <- filter(x,year==2019)
fit <- lm(ii ~ TC, data=x)
summary(fit)

rm (tableBEFORE,match.it,df.match,df.match18,df.match19,fit)
rm(irri_intens_S,irri_intens_S_M)


