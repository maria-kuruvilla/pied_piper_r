# Goal - to analyse deviation for all species

library(here)
library(forecast)
library(modelr)
library(MASS)

########## read data ##############
agg_data_w_ma_msd <- read.csv(here("Documents","data","pied_piper","dungeness_2005-2020_combine_ma_msd_atu_photoperiod.csv"),header=TRUE)#,na.strings=c("[nan]"))




########## modifying environmental variables #####
auto.arima(agg_data_w_ma_msd$temp, trace = TRUE) #ARIMA(5,0,0) with non-zero mean 

lm_temp <- lme(temp ~ doy, random = ~1+doy|year, data = agg_data_w_ma_msd, 
               correlation =  corARMA(value = c(0.8,0.08,0.02,0.03,0.03), form = ~doy|year, p =5, q = 0), na.action=na.exclude)
summary(lm_temp)
plot(lm_temp$fitted, lm_temp$residuals)
acf(residuals(lm_temp), na.action = na.exclude) # there is still autocorrelation
qqnorm(residuals(lm_temp))
qqline((residuals(lm_temp)))
#adding temp residuals to data

data <- agg_data_w_ma_msd %>% add_residuals(lm_temp)

#adding flow deviation to data
data$flow_deviation <- data$flow - data$ma_flow




########## chinook 0 ###########

#chinook0 data transformation
data$chinook0_deviation_day <- data$chinook0_wild_perhour - 
  data$ma_chinook0_wild_perhour

data$chinook0_deviation_scaled_day <- (
  data$chinook0_deviation_day)/(data$msd_chinook0_wild_perhour+1)

hist(data$chinook0_deviation_scaled_day,30)
hist(data$chinook0_deviation_scaled_day^2,30)
hist(data$chinook0_deviation_scaled_day+2,30)
hist((data$chinook0_deviation_scaled_day+2)^0.5,30) # might be best
hist(log(data$chinook0_deviation_scaled_day+2),30)

#boxcox transformation

boxcox(chinook0_deviation_scaled_day+2~1, data = data, 
       lambda= seq(-0.1, 0.5, length = 10))

hist((data$chinook0_deviation_scaled_day+2)^0.25,30) #according to boxcox


auto.arima((data$chinook0_deviation_scaled_day+2)^0.25, trace = TRUE)#p=5,q=5
auto.arima((data$chinook0_deviation_scaled_day+2)^0.5, trace = TRUE)#p=5,q=1

# chinook0 model


lmm <- lme((chinook0_deviation_scaled_day+2)^0.5 ~ temp + resid + flow +
                 flow_deviation + atu_solstice + photoperiod, 
               random = ~1+doy|year, 
               data, correlation =  corARMA(value = c(0.5,-0.2,-0.2,0.02,-0.07,-0.7), 
                                            form = ~doy|year, p =5, q = 1), 
               na.action=na.exclude)
summary(lmm)
plot(lmm$fitted, lmm$residuals)
acf(residuals(lmm), na.action = na.exclude) # there is negative autocorrelation at 3
qqnorm(residuals(lmm)) # heavy tails
qqline((residuals(lmm)))
r.squaredGLMM(lmm)
