# Propensity Score Matching#
library(Matching)
library(rbounds)
attach(R.Agriculture_Baseline_2018_)
Tr <- cbind(TC)
Y <-  cbind(irri_for_season)
X <- cbind(cult_area_under_crop,most_imp_source,name_of_crop)
 # outcome of Difference-in-differences
#Y <-  cbind(DIFF)

# descriptive statistics
summary(Tr)
summary(Y)
summary(X)

# Propensity Score Model
glm1 <- glm(Tr ~ X, family =binomial, data = R.Agriculture_Baseline_2018_)
summary(glm1)


library(tableone)
library(dplyr)

df <- tbl_df(data.frame(x = rep(1:0, each = 40)))
df

df %>% 
  mutate(pop = sample(0:1, n(), replace = TRUE))


