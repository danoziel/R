attach(R.Agriculture_Baseline_2018_)
library(tableone)
library(MatchIt)
library(dplyr)
library(Matching)
library(data.table)
require(MatchIt)
require(Matching)
#    outcome = irri_for_season #total irrigation for one-crop-season IN HOURS
#   outcome treatment = TC
#    x= name_of_crop,season_of_crop,cult_area_under_crop,most_imp_source
 
Data <- cbind(season_of_crop,cult_area_under_crop,
                most_imp_source,TC,irri_for_season)
Data <- data.frame(Data)
#-------------------------------------------------------------------------
Data <- Data %>% na.omit()

Data <- Data %>% 
  mutate(sell = sample(0:1, n(), replace = TRUE))

Data$TC[Data$TC == 1] <- 0
Data$TC[Data$TC == 2] <- 1
#-------------------------------------------------------------------------

dim(Data)
Treats <- subset(Data,  TC == 1)
colMeans(Treats)

Control <- subset(Data,  TC == 0)
colMeans(Control)

dim(Treats)
dim(Control)
# Linear regression model-------------------------------------------------------------
model_1 <- lm(irri_for_season ~ TC + most_imp_source + cult_area_under_crop,data=Data)
model_1

Effect <- model_1$coefficients[2]
Effect

# Propensity score matching--------------------------------------------------

pscores.model <- glm(TC ~ most_imp_source + cult_area_under_crop,family= binomial,data = Data)
m.out <- matchit(TC ~ most_imp_source + cult_area_under_crop,data = Data,method="subclass")
summary(m.out)
plot(m.out, type="jitter")
plot(m.out, type="hist")

summary(pscores.model)

matchedIns <- match.data(m.out)

pscores <- pscores.model$fitted.values

Propensity_scores <- pscores.model
Data$PScores <- pscores.model$fitted.values

hist(Data$PScores[TC==1],main = "PScores of Response = 1")
hist(Data$PScores[TC==0],main = "PScores of Response = 0")

library(MatchIt)
match1 <- matchit(pscores.model, method="exact",data=Data)
summary(match1, covariates = T)

match1.data <- match.data(match1)
View(match1.data)

table_match1 <- CreateTableOne(vars =xvars ,strata = "solar_pump",data = match1.data,test = FALSE)
print(table_match1, smd = TRUE)

match2 <- matchit(pscores.model, method="nearest", radio=1,data=Data)
plot(match2, type="jitter")

plot(match2, type="hist")

