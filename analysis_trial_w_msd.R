#read new ma msd file
#transform data by dividing by moving standard deviation
#try glm 
#try glmm with correlation


library(here)

agg_data_w_ma_msd <- read.csv(here("Documents","data","pied_piper","dungeness_2005-2020_combine_ma_msd.csv"),header=TRUE)#,na.strings=c("[nan]"))

#try fish per hour and regular glm
  #with msd


env_model <- lm(chinook0_wild_perhour/msd_chinook0_wild_perhour ~ temp + flow, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))

hist(log((agg_data_w_ma_msd$chinook0_wild_perhour/agg_data_w_ma_msd$msd_chinook0_wild_perhour)+1),30)
hist((agg_data_w_ma_msd$chinook0_wild_perhour/agg_data_w_ma_msd$msd_chinook0_wild_perhour)^0.5,30)



env_model <- lm(chinook0_wild_perhour/msd_chinook0_wild_perhour ~ temp + I(temp^2) + flow + I(flow^2), agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))

env_model <- lm(log(chinook0_wild_perhour/msd_chinook0_wild_perhour)+1 ~ temp + I(temp^2) + flow + I(flow^2), agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))

#zero inflated wtih transformations

  #with msd of mean
env_model <- lm(chinook0_wild_perhour/msd_chinook0_wild_perhour_mean ~ temp + I(temp^2) + flow + I(flow^2), agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))

#try without msd
env_model <- lm(log(chinook0_wild_perhour+1) ~ temp + I(temp^2) + flow + I(flow^2), agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))

#try deviation of fish per hour and regular glm
hist((agg_data_w_ma_msd$chinook0_wild_perhour - agg_data_w_ma_msd$ma_chinook0_wild_perhour),30)
hist(log((agg_data_w_ma_msd$chinook0_wild_perhour - agg_data_w_ma_msd$ma_chinook0_wild_perhour)+20),30)

hist((agg_data_w_ma_msd$chinook0_wild_perhour - agg_data_w_ma_msd$ma_chinook0_wild_perhour),30)


env_model <- lm((chinook0_wild_perhour-ma_chinook0_wild_perhour)/msd_chinook0_wild_perhour ~ temp + I(temp^2) + flow + I(flow^2), agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#this is the best residuals so far


env_model <- lm((chinook0_wild_perhour-ma_chinook0_wild_perhour_mean)/msd_chinook0_wild_perhour_mean ~ temp + I(temp^2) + flow + I(flow^2), agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#not good residuals

env_model <- lm((chinook0_wild_perhour-ma_chinook0_wild_perhour)/msd_chinook0_wild_perhour_mean ~ temp + I(temp^2) + flow + I(flow^2), agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#good residuals


env_model <- lm((chinook0_wild_perhour-ma_chinook0_wild_perhour) ~ temp + I(temp^2) + flow + I(flow^2), agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#good residuals

env_model <- lm((chinook0_wild_perhour-ma_chinook0_wild_perhour)/msd_chinook0_wild_perhour ~ temp + flow, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#this is the best residuals so far


env_model <- lm((chinook0_wild_perhour-ma_chinook0_wild_perhour) ~ temp + flow, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#not great


hist((agg_data_w_ma_msd$chinook0_wild_perhour - 
        agg_data_w_ma_msd$ma_chinook0_wild_perhour)/agg_data_w_ma_msd$msd_chinook0_wild_perhour,30)


#trying hatchery
env_model <- lm((chinook0_wild_perhour-ma_chinook0_wild_perhour)/msd_chinook0_wild_perhour ~ temp + flow + chinook0_hatchery_perhour, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#this is the best residuals so far

#trying doy
env_model <- lm((chinook0_wild_perhour-ma_chinook0_wild_perhour)/msd_chinook0_wild_perhour ~ temp + flow + chinook0_hatchery_perhour + I(doy^2), agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#this is the best residuals so far


#trying chinook1

#just temp and flow
env_model <- lm((chinook1_wild_perhour-ma_chinook1_wild_perhour)/msd_chinook1_wild_perhour ~ temp + flow, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#worst qq plot

#trying chinook1

#just temp and flow
env_model <- lm((coho1_wild_perhour-ma_coho1_wild_perhour)/
                  (msd_coho1_wild_perhour+1) ~ temp + flow, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#this is the best residuals so far



#try raw counts and Poisson
env_model <- glm(chinook0_wild_num ~ temp + flow, family = poisson(link = "log"), data = agg_data_w_ma_msd)
summary(env_model)
plot(residuals(env_model) ~ predict(env_model,type="response"),xlab=expression(hat(mu)),ylab="Deviance residuals",pch=20,col="red")


#trying negative binomial
require(MASS)
env_model <- glm.nb(chinook0_wild_num ~ temp + flow, data = agg_data_w_ma_msd, link = "log")
summary(env_model)
plot(residuals(env_model) ~ predict(env_model,type="link"),xlab=expression(hat(mu)),ylab="Deviance residuals",pch=20,col="red")

plot(predict(env_model,type="link"),env_model$resid)


#trying simple deviation model with deviations scaled by variation
#for all species

#temp and flow deviaitons
agg_data_w_ma_msd$temp_deviation_day <- agg_data_w_ma_msd$temp - 
  agg_data_w_ma_msd$ma_temp

agg_data_w_ma_msd$temp_deviation_year <- agg_data_w_ma_msd$temp - 
  agg_data_w_ma_msd$ma_temp_mean

agg_data_w_ma_msd$flow_deviation_day <- agg_data_w_ma_msd$flow - 
  agg_data_w_ma_msd$ma_flow

agg_data_w_ma_msd$flow_deviation_year <- agg_data_w_ma_msd$flow - 
  agg_data_w_ma_msd$ma_flow_mean

#chinook0
agg_data_w_ma_msd$chinook0_deviation_day <- agg_data_w_ma_msd$chinook0_wild_perhour - 
  agg_data_w_ma_msd$ma_chinook0_wild_perhour

agg_data_w_ma_msd$chinook0_deviation_year <- agg_data_w_ma_msd$chinook0_wild_perhour - 
  agg_data_w_ma_msd$ma_chinook0_wild_perhour_mean

agg_data_w_ma_msd$chinook0_deviation_scaled_year <- (
  agg_data_w_ma_msd$chinook0_deviation_year)/(agg_data_w_ma_msd$msd_chinook0_wild_perhour+1)

agg_data_w_ma_msd$chinook0_deviation_scaled_day <- (
  agg_data_w_ma_msd$chinook0_deviation_day)/(agg_data_w_ma_msd$msd_chinook0_wild_perhour+1)

hist(agg_data_w_ma_msd$chinook0_deviation_day,30)
hist(agg_data_w_ma_msd$chinook0_deviation_year,30)

hist(agg_data_w_ma_msd$chinook0_deviation_scaled_day,30)
hist(agg_data_w_ma_msd$chinook0_deviation_scaled_year,30)
#similar


env_model <- lm((chinook0_wild_perhour-ma_chinook0_wild_perhour)/msd_chinook0_wild_perhour ~ temp + flow, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#this is the best residuals so far
#qq plot shows some non normality


env_model <- lm((chinook0_wild_perhour-ma_chinook0_wild_perhour)/msd_chinook0_wild_perhour ~ temp_deviation_day + flow_deviation_day, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#this is the best residuals so far
#qq plot shows some non normality


env_model <- lm((chinook0_wild_perhour-ma_chinook0_wild_perhour)/msd_chinook0_wild_perhour ~ temp_deviation_year + flow_deviation_year, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#this is the best residuals so far
#qq plot shows some non normality


env_model <- lm((chinook0_wild_perhour-ma_chinook0_wild_perhour)/msd_chinook0_wild_perhour ~ temp_deviation_day + flow_deviation_day + temp + flow, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#this is the best residuals so far
#qq plot shows some non normality


env_model1 <- lm(chinook0_deviation_scaled_day ~ temp_deviation_day + flow_deviation_day + temp + flow, agg_data_w_ma_msd)
summary(env_model1)
plot(fitted(env_model1), residuals(env_model1))
qqnorm(residuals(env_model1), main= "")
qqline(residuals(env_model1))
#this is the best residuals so far
#qq plot shows some non normality

# my_data<-data.frame("chinook0_dev_scaled_day" = agg_data_w_ma_msd$chinook0_deviation_scaled_day[complete.cases(agg_data_w_ma_msd$chinook0_deviation_scaled_day)],
#                     "temp"=agg_data_w_ma_msd$temp[complete.cases(agg_data_w_ma_msd$chinook0_deviation_scaled_day)],
#                     "flow"=agg_data_w_ma_msd$flow[complete.cases(agg_data_w_ma_msd$chinook0_deviation_scaled_day)], 
#                     "temp_deviation_day" = agg_data_w_ma_msd$temp_deviation_day[complete.cases(agg_data_w_ma_msd$chinook0_deviation_scaled_day)],
#                     "flow_deviation_day" = agg_data_w_ma_msd$flow_deviation_day[complete.cases(agg_data_w_ma_msd$chinook0_deviation_scaled_day)]
#                     
# )
# 
# my_data <- my_data[is.finite(my_data$chinook0_dev_scaled_day),]
env_model <- lm(chinook0_deviation_scaled_day ~ temp + flow + temp_deviation_day + flow_deviation_day, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))

env_model <- lm(chinook0_deviation_scaled_year ~ temp + flow + temp_deviation_day + flow_deviation_day, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))


hist(agg_data_w_ma_msd$chinook0_deviation_scaled_day,30)
hist((agg_data_w_ma_msd$chinook0_deviation_day/(agg_data_w_ma_msd$msd_chinook0_wild_perhour+1)),30)
hist((agg_data_w_ma_msd$chinook0_wild_perhour- agg_data_w_ma_msd$ma_chinook0_wild_perhour),30)
hist((agg_data_w_ma_msd$chinook0_deviation_day),30)
      #/agg_data_w_ma_msd$msd_chinook0_wild_perhour),30)


#chinook1
agg_data_w_ma_msd$chinook1_deviation_day <- agg_data_w_ma_msd$chinook1_wild_perhour - 
  agg_data_w_ma_msd$ma_chinook1_wild_perhour

agg_data_w_ma_msd$chinook1_deviation_year <- agg_data_w_ma_msd$chinook1_wild_perhour - 
  agg_data_w_ma_msd$ma_chinook1_wild_perhour_mean

agg_data_w_ma_msd$chinook1_deviation_scaled_year <- (
  agg_data_w_ma_msd$chinook1_deviation_year)/(agg_data_w_ma_msd$msd_chinook1_wild_perhour+1)

agg_data_w_ma_msd$chinook1_deviation_scaled_day <- (
  agg_data_w_ma_msd$chinook1_deviation_day)/(agg_data_w_ma_msd$msd_chinook1_wild_perhour+1)


env_model <- lm(chinook1_deviation_scaled_year ~ temp + flow + temp_deviation_day + flow_deviation_day, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))

env_model <- lm(chinook1_deviation_scaled_day ~ temp + flow + temp_deviation_day + flow_deviation_day, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))

env_model <- lm(chinook1_deviation_year ~ temp + flow + temp_deviation_day + flow_deviation_day, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))

env_model <- lm(chinook1_deviation_day ~ temp + flow + temp_deviation_day + flow_deviation_day + doy + I(doy^2), agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))


hist(agg_data_w_ma_msd$chinook1_deviation_day,30)
#none of the above models have okay residuals and qq plot

#coho1
agg_data_w_ma_msd$coho1_deviation_day <- agg_data_w_ma_msd$coho1_wild_perhour - 
  agg_data_w_ma_msd$ma_coho1_wild_perhour

agg_data_w_ma_msd$coho1_deviation_year <- agg_data_w_ma_msd$coho1_wild_perhour - 
  agg_data_w_ma_msd$ma_coho1_wild_perhour_mean

agg_data_w_ma_msd$coho1_deviation_scaled_year <- (
  agg_data_w_ma_msd$coho1_deviation_year)/(agg_data_w_ma_msd$msd_coho1_wild_perhour+1)

agg_data_w_ma_msd$coho1_deviation_scaled_day <- (
  agg_data_w_ma_msd$coho1_deviation_day)/(agg_data_w_ma_msd$msd_coho1_wild_perhour+1)


hist((agg_data_w_ma_msd$coho1_deviation_scaled_day),30)

env_model <- lm(coho1_deviation_day ~ temp + flow + temp_deviation_day + flow_deviation_day + doy + I(doy^2), agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))

env_model <- lm(coho1_deviation_scaled_day ~ temp + flow + temp_deviation_day + flow_deviation_day + doy + I(doy^2), agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))

#qqplot sucks

#steelhead
agg_data_w_ma_msd$steelheadsmolt_deviation_day <- agg_data_w_ma_msd$steelheadsmolt_wild_perhour - 
  agg_data_w_ma_msd$ma_steelheadsmolt_wild_perhour

agg_data_w_ma_msd$steelheadsmolt_deviation_year <- agg_data_w_ma_msd$steelheadsmolt_wild_perhour - 
  agg_data_w_ma_msd$ma_steelheadsmolt_wild_perhour_mean

agg_data_w_ma_msd$steelheadsmolt_deviation_scaled_year <- (
  agg_data_w_ma_msd$steelheadsmolt_deviation_year)/(agg_data_w_ma_msd$msd_steelheadsmolt_wild_perhour+1)

agg_data_w_ma_msd$steelheadsmolt_deviation_scaled_day <- (
  agg_data_w_ma_msd$steelheadsmolt_deviation_day)/(agg_data_w_ma_msd$msd_steelheadsmolt_wild_perhour+1)


hist((agg_data_w_ma_msd$steelheadsmolt_deviation_day),30)


env_model <- lm(steelheadsmolt_deviation_scaled_day ~ temp + flow + temp_deviation_day + flow_deviation_day, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#qqplot is bad

#chum
agg_data_w_ma_msd$chum0_deviation_day <- agg_data_w_ma_msd$chum0_wild_perhour - 
  agg_data_w_ma_msd$ma_chum0_wild_perhour

agg_data_w_ma_msd$chum0_deviation_year <- agg_data_w_ma_msd$chum0_wild_perhour - 
  agg_data_w_ma_msd$ma_chum0_wild_perhour_mean

agg_data_w_ma_msd$chum0_deviation_scaled_year <- (
  agg_data_w_ma_msd$chum0_deviation_year)/(agg_data_w_ma_msd$msd_chum0_wild_perhour+1)

agg_data_w_ma_msd$chum0_deviation_scaled_day <- (
  agg_data_w_ma_msd$chum0_deviation_day)/(agg_data_w_ma_msd$msd_chum0_wild_perhour+1)

hist((agg_data_w_ma_msd$chum0_deviation_scaled_day)^(3),30)

env_model <- lm(chum0_deviation_scaled_day ~ temp + flow + temp_deviation_day + flow_deviation_day, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#qqplot is bad


#pink
agg_data_w_ma_msd$pink0_deviation_day <- agg_data_w_ma_msd$pink0_wild_perhour - 
  agg_data_w_ma_msd$ma_pink0_wild_perhour

agg_data_w_ma_msd$pink0_deviation_year <- agg_data_w_ma_msd$pink0_wild_perhour - 
  agg_data_w_ma_msd$ma_pink0_wild_perhour_mean

agg_data_w_ma_msd$pink0_deviation_scaled_year <- (
  agg_data_w_ma_msd$pink0_deviation_year)/(agg_data_w_ma_msd$msd_pink0_wild_perhour+1)

agg_data_w_ma_msd$pink0_deviation_scaled_day <- (
  agg_data_w_ma_msd$pink0_deviation_day)/(agg_data_w_ma_msd$msd_pink0_wild_perhour+1)

hist((agg_data_w_ma_msd$pink0_deviation_scaled_day),30)

env_model <- lm(pink0_deviation_scaled_day ~ temp + flow + temp_deviation_day + flow_deviation_day, agg_data_w_ma_msd)
summary(env_model)
plot(fitted(env_model), residuals(env_model))
qqnorm(residuals(env_model), main= "")
qqline(residuals(env_model))
#qqplot is bad and residuals are bad

require(lme4)
require(nlme)
mixed_model <- lmer(chinook0_deviation_scaled_day ~ temp + flow + temp_deviation_day + flow_deviation_day, correlation = corAR1(), data = agg_data_w_ma_msd)

mm <- lme(chinook0_wild_num ~ temp + flow, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = agg_data_w_ma_msd, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
pacf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

mm <- lme(chinook0_deviation_scaled_day  ~ temp + flow, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = agg_data_w_ma_msd, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
pacf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

mm <- lme(chinook0_deviation_scaled_day  ~ temp + flow + temp_deviation_day + flow_deviation_day, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = agg_data_w_ma_msd, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
pacf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))


library(MuMIn)
r.squaredGLMM(mm) 


mm <- lme(chinook1_deviation_scaled_day  ~ temp + flow + temp_deviation_day + flow_deviation_day, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = agg_data_w_ma_msd, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
pacf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))
r.squaredGLMM(mm) 


mm <- lme(coho1_deviation_scaled_day  ~ temp + flow + temp_deviation_day + flow_deviation_day, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = agg_data_w_ma_msd, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
pacf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))
r.squaredGLMM(mm) 

mm <- lme(steelheadsmolt_deviation_scaled_day  ~ temp + flow + temp_deviation_day + flow_deviation_day, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = agg_data_w_ma_msd, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
pacf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))
r.squaredGLMM(mm) 

mm <- lme(steelheadsmolt_deviation_scaled_day  ~ temp + flow + temp_deviation_day + flow_deviation_day, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = agg_data_w_ma_msd, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
pacf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))
r.squaredGLMM(mm) 

mm <- lme(chum0_deviation_scaled_day  ~ temp + flow + temp_deviation_day + flow_deviation_day, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = agg_data_w_ma_msd, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
pacf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))
r.squaredGLMM(mm) 


mm <- lme(pink0_deviation_scaled_day  ~ temp + flow + temp_deviation_day + flow_deviation_day, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = agg_data_w_ma_msd, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
pacf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))
r.squaredGLMM(mm) 




require(MASS)
glmm <- glmmPQL(chinook0_wild_num ~ temp + flow, random = ~ 1|year, correlation = corAR1(form = ~doy|year), family = poisson, data = agg_data_w_ma_msd, na.action=na.exclude)
summary(glmm)
plot(fitted(glmm), residuals(glmm))
acf(residuals(glmm, retype="normalized"), na.action=na.exclude)


#Let's try new variables


#same variables but different correlation structure
#chinook0
agg_data_w_ma_msd$chinook0_deviation_day <- agg_data_w_ma_msd$chinook0_wild_perhour - 
  agg_data_w_ma_msd$ma_chinook0_wild_perhour

agg_data_w_ma_msd$chinook0_deviation_year <- agg_data_w_ma_msd$chinook0_wild_perhour - 
  agg_data_w_ma_msd$ma_chinook0_wild_perhour_mean

agg_data_w_ma_msd$chinook0_deviation_scaled_year <- (
  agg_data_w_ma_msd$chinook0_deviation_year)/(agg_data_w_ma_msd$msd_chinook0_wild_perhour+1)

agg_data_w_ma_msd$chinook0_deviation_scaled_day <- (
  agg_data_w_ma_msd$chinook0_deviation_day)/(agg_data_w_ma_msd$msd_chinook0_wild_perhour+1)


agg_data_w_ma_msd$chinook0_num_scaled_day <- (
  agg_data_w_ma_msd$chinook0_wild_num)/(agg_data_w_ma_msd$msd_chinook0_wild_perhour+1)

hist(agg_data_w_ma_msd$chinook0_num_scaled_day)
hist(agg_data_w_ma_msd$chinook0_wild_num)


df <- agg_data_w_ma_msd %>% drop_na(c(chinook0_wild_num,temp, flow, chinook0_num_scaled_day))

lme_model <- lme(chinook0_wild_num ~ temp+flow, data = df,
                 random = ~1|year,
                 correlation = corCAR1(form = ~doy|year))
summary(lme_model)
plot(lme_model,type=c("p","smooth"))
plot(ACF(lme_model,resType="normalized"),alpha=0.05)
plot(lme_model,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"))
plot(lme_model,resid(.,type="pearson")~temp,
     type=c("p","smooth"))


lme_model <- lme(chinook0_num_scaled_day ~ temp+flow, data = df,
                 random = ~1|year,
                 correlation = corCAR1(form = ~doy|year))
summary(lme_model)
plot(lme_model,type=c("p","smooth"))
plot(ACF(lme_model,resType="normalized"),alpha=0.05)
plot(lme_model,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"))
plot(lme_model,resid(.,type="pearson")~temp,
     type=c("p","smooth"))


lme_model <- lme(chinook0_num_scaled_day ~ temp+flow, data = df,
                 random = ~1|year,
                 correlation = corCAR1(form = ~doy|year))
summary(lme_model)

acf(residuals(lme_model, retype="normalized"), na.action=na.exclude)
pacf(residuals(lme_model, retype="normalized"), na.action=na.exclude)
plot(fitted(lme_model), residuals(lme_model))



df <- agg_data_w_ma_msd %>% drop_na(c(chinook0_wild_num,temp, flow, chinook0_num_scaled_day, chinook0_deviation_scaled_day,chinook0_hatchery_num))


lme_model <- lme(chinook0_deviation_scaled_day ~ temp+flow+
                   chinook0_hatchery_num,data = df,
                 random = ~1|year,
                 correlation = corCAR1(form = ~doy|year))
summary(lme_model)
acf(residuals(lme_model, retype="normalized"), na.action=na.exclude)
pacf(residuals(lme_model, retype="normalized"), na.action=na.exclude)
plot(fitted(lme_model), residuals(lme_model))




df <- agg_data_w_ma_msd %>% drop_na(c(chinook0_wild_num,temp, flow, 
                                      chinook0_num_scaled_day, 
                                      chinook0_deviation_scaled_day,
                                      chinook0_hatchery_perhour,
                                      chinook1_hatchery_perhour,
                                      coho1_hatchery_perhour,
                                      steelheadsmolt_hatchery_perhour))



lme_model <- lme(chinook0_deviation_scaled_day ~ temp+flow+
                   chinook0_hatchery_perhour + chinook1_hatchery_perhour+
                   coho1_hatchery_perhour+ steelheadsmolt_hatchery_perhour,
                 data = df,
                 random = ~1|year,
                 correlation = corCAR1(form = ~doy|year))
summary(lme_model)
acf(residuals(lme_model, retype="normalized"), na.action=na.exclude)
pacf(residuals(lme_model, retype="normalized"), na.action=na.exclude)
plot(fitted(lme_model), residuals(lme_model))


lme_model <- lme(chinook0_deviation_scaled_day ~ temp+flow+
                   chinook0_hatchery_perhour + chinook1_hatchery_perhour+
                   coho1_hatchery_perhour+ steelheadsmolt_hatchery_perhour +
                   photoperiod + photoperiod^2,
                 data = df,
                 random = ~1|year,
                 correlation = corCAR1(form = ~doy|year))
summary(lme_model)
acf(residuals(lme_model, retype="normalized"), na.action=na.exclude)
pacf(residuals(lme_model, retype="normalized"), na.action=na.exclude)
plot(fitted(lme_model), residuals(lme_model))






