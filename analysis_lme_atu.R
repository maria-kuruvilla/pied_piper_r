#trying the mixed model with atu 

library(here)
require(MASS)
require(lme4)
require(nlme)
library(MuMIn)

agg_data_w_ma_msd <- read.csv(here("Documents","data","pied_piper","dungeness_2005-2020_combine_ma_msd_atu_photoperiod.csv"),header=TRUE)#,na.strings=c("[nan]"))



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

#coho1
agg_data_w_ma_msd$coho1_deviation_day <- agg_data_w_ma_msd$coho1_wild_perhour - 
  agg_data_w_ma_msd$ma_coho1_wild_perhour

agg_data_w_ma_msd$coho1_deviation_year <- agg_data_w_ma_msd$coho1_wild_perhour - 
  agg_data_w_ma_msd$ma_coho1_wild_perhour_mean

agg_data_w_ma_msd$coho1_deviation_scaled_year <- (
  agg_data_w_ma_msd$coho1_deviation_year)/(agg_data_w_ma_msd$msd_coho1_wild_perhour+1)

agg_data_w_ma_msd$coho1_deviation_scaled_day <- (
  agg_data_w_ma_msd$coho1_deviation_day)/(agg_data_w_ma_msd$msd_coho1_wild_perhour+1)

#steelhead
agg_data_w_ma_msd$steelheadsmolt_deviation_day <- agg_data_w_ma_msd$steelheadsmolt_wild_perhour - 
  agg_data_w_ma_msd$ma_steelheadsmolt_wild_perhour

agg_data_w_ma_msd$steelheadsmolt_deviation_year <- agg_data_w_ma_msd$steelheadsmolt_wild_perhour - 
  agg_data_w_ma_msd$ma_steelheadsmolt_wild_perhour_mean

agg_data_w_ma_msd$steelheadsmolt_deviation_scaled_year <- (
  agg_data_w_ma_msd$steelheadsmolt_deviation_year)/(agg_data_w_ma_msd$msd_steelheadsmolt_wild_perhour+1)

agg_data_w_ma_msd$steelheadsmolt_deviation_scaled_day <- (
  agg_data_w_ma_msd$steelheadsmolt_deviation_day)/(agg_data_w_ma_msd$msd_steelheadsmolt_wild_perhour+1)




my_data = agg_data_w_ma_msd

mm <- lme(chinook0_deviation_scaled_day  ~ temp + flow + temp_deviation_day + flow_deviation_day + atu_solstice + I(atu_solstice^2) + photoperiod, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = my_data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
pacf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))
r.squaredGLMM(mm) 


mm <- lme(coho1_deviation_scaled_day  ~ temp + flow + temp_deviation_day + flow_deviation_day + atu + I(atu^2) + photoperiod, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = my_data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
pacf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))
r.squaredGLMM(mm) 

mm <- lme(steelheadsmolt_deviation_scaled_day  ~ temp + flow + temp_deviation_day + flow_deviation_day + atu + I(atu^2) + photoperiod, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = my_data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
pacf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))
r.squaredGLMM(mm) 


#trying with correlation

model1 <- lm(chinook0_deviation_scaled_day  ~ temp + flow + temp_deviation_day + flow_deviation_day + atu_solstice + I(atu_solstice^2) + photoperiod, data = my_data)
plot(fitted(model1), residuals(model1))
acf(residuals(model1))
pacf(residuals(model1))
summary(model1)
