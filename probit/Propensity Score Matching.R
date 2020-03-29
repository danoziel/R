#---------------
attach(R_Agriculture_Baseline_2018_)
library(tableone)
library(MatchIt)
library(dplyr)
library(Matching)
library(data.table)
require(MatchIt)
require(Matching)
#    outcome = irri_for_season #total irrigation for one-crop-season IN HOURS-----

Data <- cbind(season_of_crop,cult_area_under_crop,
                most_imp_source,TC,irri_for_season)
Data <- Data %>% filter(most_imp_source!=0) %>%  na.omit()

Data <- data.frame(Data)

DDDDDD <- Data %>% 
  mutate(sell = sample(0:1, n(), replace = T))

Data$TC[Data$TC == 1] <- 0
Data$TC[Data$TC == 2] <- 1

Treats <- subset(Data,  TC == 1)
colMeans(Treats)

Control <- subset(Data,  TC == 0)
colMeans(Control)

# Linear regression model-------------------------------------------------------------
model_1 <- lm(irri_for_season ~ TC + most_imp_source + cult_area_under_crop,data=Data)
model_1



Effect <- model_1$coefficients[2]
Effect

#------------------------------------Propensity score matching---------------------------

pscores.model <- glm(TC ~ most_imp_source + cult_area_under_crop,family= binomial,data = Data)
summary(pscores.model)

Propensity_scores <- pscores.model
Data$PScores <- pscores.model$fitted.values

hist(Data$PScores[TC==1])
hist(Data$PScores[TC==0])

xvars <- c("most_imp_source", "cult_area_under_crop")

####### exact_matching----
match1 <- matchit(pscores.model, method="exact",data=Data)
summary(match1, covariates = T)

match1.data <- match.data(match1)

table_match1 <- CreateTableOne(vars =xvars ,strata = "TC",data = match1.data,test = FALSE)
print(table_match1, smd = TRUE)

####### Nearest Neighbour Matching-----
####### Nearest Neighbour Matching-----
match2 <- matchit(pscores.model, method="nearest", radio=1,data=Data)
match2.data <- match.data(match2)

plot(match2, type="jitter")
plot(match2, type="hist")

table_match2 <- CreateTableOne(vars = xvars,strata = "TC",data = match2.data,test = FALSE)
print(table_match2, smd = TRUE)

# Outcome Analysis (testing our hypothesis)----

y_trt <- match2.data$irri_for_season [match2.data$TC == 1]
y_con <- match2.data$irri_for_season[match2.data$TC == 0]
difference <- y_trt - y_con
t.test(difference)
