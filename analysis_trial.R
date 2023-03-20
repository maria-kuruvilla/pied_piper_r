# Live code - try some GLM with twice daily catch data, daily catch data and deviation from average catch data. 
# Discuss why residuals look so bad - is it only because of correlated data or could it be other things?
#   What if there is no pool of fish that is being drained when there is a large spike?
#   Discuss how ARMA models can be fit to this data? Will it be useful?
#   Discuss how to scale variance. Is it okay to do that? 
#   Should we try hierarchical modeling/STAN? 

library(here)
library(tidyverse)

agg_data_w_ma <- read.csv(here("Documents","data","pied_piper","dungeness_2005-2020_combine_ma.csv"),header=TRUE)#,na.strings=c("[nan]"))

agg_data <- read.csv(here("Documents","data","pied_piper","combine_2005-2020_dungeness.csv"),header=TRUE)#,na.strings=c("[nan]"))

unagg_data <- read.csv(here("Documents","data","pied_piper","combine_2005-2020_dungeness_all.csv"),header=TRUE)#,na.strings=c("[nan]"))

hist(agg_data$chinook0_wild_perhour)
hist(agg_data_w_ma$chinook0_wild_perhour_x)
hist(agg_data_w_ma$chinook0_deviation,30)
hist(unagg_data$chinook0_wild_perhour,30)

hist(agg_data_w_ma$flow_deviation)
hist(agg_data_w_ma$temp_deviation)

#make deviations
env_model <- lm(chinook0_deviation ~ temp_x + flow_x, agg_data_w_ma)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))


env_model <- lm(chinook0_deviation/moving_sd ~ temp_x + flow_x, agg_data_w_ma)
summary(env_model)
plot(fitted(env_model), residuals(env_model))


glm_model <- 
  
  it1 <- lme(height ~ type * time, random= ~ 1|box/plant, correlation=corARMA(0.2, form=~time|box/plant, p=1, q=0), data=mydata)


env_model <- lm(chinook0_deviation/moving_sd ~ temp_x + flow_x + chinook0_hatchery_perhour_x, agg_data_w_ma)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))


