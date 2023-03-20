
library(here)
library(rsq)
library(lmtest)
library(sandwich)
library(pscl)
require(tscount)
require(MASS)
# require(lme4)
# require(nlme)
# library(MuMIn)
# require(modelr)


############## reading data #################
agg_data_w_ma_msd <- read.csv(here("Documents","data","pied_piper","dungeness_2005-2020_combine_ma_msd_atu_photoperiod.csv"),header=TRUE)#,na.strings=c("[nan]"))


############### modifying env data ############

#use residual of temperature as covariate
hist(agg_data_w_ma_msd$temp)

lm_temp <- lme(temp ~ doy, random = ~1+doy|year, data = agg_data_w_ma_msd, 
               correlation =  corAR1(form = ~doy|year), na.action=na.exclude)
summary(lm_temp)
plot(lm_temp$fitted, lm_temp$residuals)
acf(lm_temp$residuals)

#adding temp residuals to data

data <- agg_data_w_ma_msd %>% add_residuals(lm_temp)

#adding flow deviation to data
data$flow_deviation <- data$flow - data$ma_flow


############# trying poisson without correlation #########
hist(data$chinook0_wild_num,50)
#zero inflated model is probably best

#but let us try poisson
chinook_glm <- glm(chinook0_wild_num ~ resid + flow_deviation + temp + flow +
                     photoperiod + atu_solstice, family = poisson, data = data,
                   na.action = na.exclude)
summary(chinook_glm)
rsq(chinook_glm) #not working after adding na.exclude
coeftest(chinook_glm, vcov = sandwich)


#let's try zeo inflated
chinook0_zinf <- zeroinfl(chinook0_wild_num ~ resid + flow_deviation + temp + flow +
                            photoperiod + atu_solstice, dist = "poisson",
                          data = data, link = "logit", na.action = na.exclude)
summary(chinook0_zinf)

data_na <- na.omit(data)
#let's try tsglm for time series count data
ts <- tsglm(data_na$chinook0_wild_num, model= list(past_obs = 1), 
            xreg = cbind(data_na$temp, data_na$flow, data_na$resid, 
                         data_na$flow_deviation,data_na$photoperiod, data_na$atu_solstice), 
            distr = "poisson",link = "log")
summary(ts)
acf(residuals(ts)) #not good
pit(ts) #not good
marcal(ts) #not good


ts <- tsglm(data_na$chinook0_wild_num, model= list(past_obs = c(1,2,3,4)), 
            xreg = cbind(data_na$temp, data_na$flow, data_na$resid, 
                         data_na$flow_deviation,data_na$photoperiod, data_na$atu_solstice), 
            distr = "poisson",link = "log")
summary(ts)
acf(residuals(ts))
pit(ts) #not good
marcal(ts) #not good


ts <- tsglm(data_na$chinook0_wild_num, model= list(past_obs = c(1,2,3,4)), 
            xreg = cbind(data_na$temp, data_na$flow, data_na$resid, 
                         data_na$flow_deviation,data_na$photoperiod, data_na$atu_solstice), 
            distr = "nbinom", link = "log")
summary(ts)
acf(residuals(ts))
pit(ts) #not good
marcal(ts) #not good

# all estmates are nan or inf

ts <- tsglm(data_na$chinook0_wild_num, model= list(past_obs = c(1,2,3,4)), 
            distr = "nbinom", link = "log")
summary(ts)
acf(residuals(ts))
pit(ts) #not good
marcal(ts) #not good

glmm <- glmmPQL(chinook0_wild_num ~ 1, random = ~ 1|year, data = data,
                family = poisson, correlation = corARMA(c(0.3,-0.8), 
                                                        form = ~ doy|year, 
                                                        p = 1, q = 1))

summary(glmm)
acf(residuals(glmm))
# all estmates are nan or inf


##trying to see what log transformed counts look like
hist(data$chinook0_wild_num,30)
hist(log(data$chinook0_wild_num+1),20)


#trying zero inflated model again
hist(data$chinook0_wild_num,30)
chinook0_zinf <- zeroinfl(chinook0_wild_num ~ resid + flow_deviation + temp + flow +
                            photoperiod + atu_solstice, dist = "poisson",
                          data = data, link = "logit")#, na.action = na.exclude)
summary(chinook0_zinf)
plot(fitted(chinook0_zinf), residuals(chinook0_zinf))
X2 <- sum((chinook0_zinf$y - fitted(chinook0_zinf))^2/fitted(chinook0_zinf))
pchisq(X2, df = 1780-14, lower.tail = FALSE)
c_hat <- X2/(1780 - 14)
c_hat
#c_hat >> 1 -> overdispersed data

chinook0_zinb <- zeroinfl(chinook0_wild_num ~ resid + flow_deviation + temp + flow +
                            photoperiod + atu_solstice, dist = "negbin",
                          data = data, link = "logit")#, na.action = na.exclude)
summary(chinook0_zinb)
plot(fitted(chinook0_zinb), residuals(chinook0_zinb)) # residuals are bad
X2 <- sum((chinook0_zinb$y - fitted(chinook0_zinb))^2/fitted(chinook0_zinb))
pchisq(X2, df = 1780-14, lower.tail = FALSE)
c_hat <- X2/(1780 - 14)
c_hat
#c_hat >> 1 -> overdispersed data

#trying higher powers
chinook0_zinb_2 <- zeroinfl(chinook0_wild_num ~ resid + I(resid^2) + flow_deviation + 
                            I(flow_deviation^2) + temp + I(temp^2) + flow +
                            I(flow^2) + photoperiod + I(photoperiod^2) + 
                            atu_solstice + I(atu_solstice^2), dist = "negbin",
                          data = data, link = "logit")#, na.action = na.exclude)
summary(chinook0_zinb_2)
plot(fitted(chinook0_zinb_2), residuals(chinook0_zinb_2)) # residuals are bad
X2 <- sum((chinook0_zinb$y - fitted(chinook0_zinb))^2/fitted(chinook0_zinb))
pchisq(X2, df = 1780-14, lower.tail = FALSE)
c_hat <- X2/(1780 - 14)
c_hat
#c_hat >> 1 -> overdispersed data


