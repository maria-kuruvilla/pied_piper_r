#Goal - read the dungeness river smolt trap data files
# - build model with only env variables
# - try ARIMA model

library(here)
library(tidyverse)

data <- read.csv(here("Documents","data","pied_piper","dungeness_2005-2020_combine_doy.csv"),header=TRUE)#,na.strings=c("[nan]"))

model_lm <- lm(chinook0_wild_perhour_x ~ temp + I(temp^2) + flow + I(flow^2) ,data)
summary(model_lm)
rsq(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.17 #this works
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


model_lm <- glm(chinook0_wild_perhour_x ~ temp + I(temp^2) + flow + I(flow^2) , family = poisson ,data)
summary(model_lm)
rsq(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.17 #this works
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

pacf(data$chinook0_wild_perhour_x)
acf(data$chinook0_wild_perhour_x)
require(forecast)

auto.arima(data$chinook0_hatchery_perhour_x, start.p = 0, max.p = 30, start.q = 0, max.q = 30)
