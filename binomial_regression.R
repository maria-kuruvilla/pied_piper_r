#reformat data to have (success,failure)
#success = number of fish that migrated that day
#failure = total number of fish - cumsum of fish that migrated on the previous day

library(here)
library(tidyverse)
agg_data_w_ma_msd <- read.csv(here("Documents","data","pied_piper","dungeness_2005-2020_combine_ma_msd_atu_photoperiod.csv"),header=TRUE)#,na.strings=c("[nan]"))

years = unique(agg_data_w_ma_msd$year)

agg_data_w_ma_msd$cum_chinook0_wild <- ave(agg_data_w_ma_msd$chinook0_wild_num, 
                                           cumsum(c(F, diff(agg_data_w_ma_msd$year) == 1)), FUN=cumsum)

agg_data_w_ma_msd$cum_chinook1_wild <- ave(agg_data_w_ma_msd$chinook1_wild_num, 
                                           cumsum(c(F, diff(agg_data_w_ma_msd$year) == 1)), FUN=cumsum)


agg_data_w_ma_msd$cum_coho1_wild <- ave(agg_data_w_ma_msd$coho1_wild_num, 
                                           cumsum(c(F, diff(agg_data_w_ma_msd$year) == 1)), FUN=cumsum)

agg_data_w_ma_msd$cum_steelheadsmolt_wild <- ave(agg_data_w_ma_msd$steelheadsmolt_wild_num, 
                                           cumsum(c(F, diff(agg_data_w_ma_msd$year) == 1)), FUN=cumsum)

agg_data_w_ma_msd$cum_chum0_wild <- ave(agg_data_w_ma_msd$chum0_wild_num, 
                                           cumsum(c(F, diff(agg_data_w_ma_msd$year) == 1)), FUN=cumsum)

agg_data_w_ma_msd$cum_pink0_wild <- ave(agg_data_w_ma_msd$pink0_wild_num, 
                                           cumsum(c(F, diff(agg_data_w_ma_msd$year) == 1)), FUN=cumsum)


#cumulative for each year

cum_year <- agg_data_w_ma_msd$cum_chinook0_wild[which(diff(agg_data_w_ma_msd$year) == 1)]
cum_year <- append(cum_year,agg_data_w_ma_msd$cum_chinook0_wild[length(agg_data_w_ma_msd$cum_chinook0_wild)])

datapoints_year <- which(diff(agg_data_w_ma_msd$year) == 1)
datapoints_year <- append(datapoints_year,length(agg_data_w_ma_msd$cum_chinook0_wild))



for(i in 1:length(cum_year)){
  if(i==1){
    cum_df = c()
    x= 0
  }
  else{
    x = datapoints_year[i-1]
  }
  
  cum_df <- c(cum_df,rep(cum_year[i],datapoints_year[i]-x))
  
}

agg_data_w_ma_msd$chinook0_wild_total <- cum_df 

agg_data_w_ma_msd$chinook0_wild_fail <- number_failed(agg_data_w_ma_msd)

#We need the first datapoint of every year to be NA. Total - cum of previous day
#would be NA. 
agg_data_w_ma_msd$chinook0_wild_fail[datapoints_year[-length(datapoints_year)] + 1] <- NA

number_failed <- function(x){
  y <- x$chinook0_wild_total - c(NA, x$cum_chinook0_wild[-nrow(x)])
  return(y)
} 

#trying GLM

glm_probability <- glm(cbind(chinook0_wild_num,chinook0_wild_fail) ~ temp + 
                         flow + photoperiod + atu_solstice, agg_data_w_ma_msd,
                       family = "binomial")

summary(glm_probability)
require(arm)
binnedplot(x = predict(glm_probability), y= resid(glm_probability))
acf(resid(glm_probability))

#use gee
require(gee)

agg_data_w_ma_msd$id <- factor(agg_data_w_ma_msd$year-2004)
df <- agg_data_w_ma_msd[order(agg_data_w_ma_msd$id), ]
dep_gee3 <- gee(cbind(chinook0_wild_num,chinook0_wild_fail) ~ temp + 
                  flow + photoperiod + atu_solstice,
                data =  agg_data_w_ma_msd,
                id = year,
                family = binomial,
                corstr = "AR-M", Mv = 1)

dep_gee3 <- gee(chinook0_wild_num~ temp + flow + photoperiod + atu_solstice,
                data =  df,
                id = id,
                family = poisson,
                corstr = "AR-M", Mv = 1)

dep_gee3 <- gee(chinook0_wild_num~ temp + flow,
                data =  df,
                id = id,
                family = poisson,
                corstr = "AR-M", Mv = 1)
### not working


## trying Glmm again

require(nlme)
require(tidyr)
df <- df %>% drop_na(c(chinook0_wild_num,temp, flow,doy,id))
lme_model <- lme(chinook0_wild_num ~ temp+flow, data = df, random = ~1 + doy|id,
                 correlation = corCAR1(form = ~doy|id))
summary(lme_model)
plot(lme_model,type=c("p","smooth"))
plot(ACF(lme_model,resType="normalized"),alpha=0.05)
plot(lme_model,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"))
plot(lme_model,resid(.,type="pearson")~temp,
     type=c("p","smooth"))

qqnorm(lme_model, abline=c(0,1))     


lme_model <- lme(chinook0_wild_num ~ temp+flow, data = df, family = poisson,
                 random = ~1 + doy|id,
                 correlation = corCAR1(form = ~doy|id))


####################################################################





data <- agg_data_w_ma_msd %>% drop_na(c(chinook0_wild_num,chinook0_wild_fail,temp, flow,doy,atu_solstice,
                                        photoperiod, chinook0_hatchery_perhour,
                                        chinook1_hatchery_perhour, coho1_hatchery_perhour,
                                        steelheadsmolt_hatchery_perhour, pink0_hatchery_perhour, year))
data <- cbind(data$chinook0_wild_num, data$chinook0_wild_fail, data$temp, data$flow,
              data$doy,data$atu_solstice,
              data$photoperiod, data$chinook0_hatchery_perhour,
              data$chinook1_hatchery_perhour, data$coho1_hatchery_perhour,
              data$steelheadsmolt_hatchery_perhour, data$pink0_hatchery_perhour, 
              data$year)
data <- as.data.frame(data)

colnames(data) <- c('chinook0_wild_num','chinook0_wild_fail','temp', 'flow','doy',
                    'atu_solstice',
                    'photoperiod', 'chinook0_hatchery_perhour',
                    'chinook1_hatchery_perhour', 'coho1_hatchery_perhour',
                    'steelheadsmolt_hatchery_perhour', 'pink0_hatchery_perhour', 'year')

glmm <- glmmPQL(cbind(chinook0_wild_num,chinook0_wild_fail)~ temp + flow +
                  atu_solstice + I(atu_solstice^2) + 
                  photoperiod + chinook0_hatchery_perhour + 
                  chinook1_hatchery_perhour + coho1_hatchery_perhour + 
                  steelheadsmolt_hatchery_perhour + pink0_hatchery_perhour, 
                random = ~ 1|year, 
                family =  binomial,
                data = data) #not working

glmm <- glm(cbind(chinook0_wild_num,chinook0_wild_fail)~ temp + flow +
                  atu_solstice + I(atu_solstice^2) + 
                  photoperiod + chinook0_hatchery_perhour + 
                  chinook1_hatchery_perhour + coho1_hatchery_perhour + 
                  steelheadsmolt_hatchery_perhour + pink0_hatchery_perhour,
                family =  "binomial",
                data = data) #working
summary(glmm)


glmm <- glm(cbind(chinook0_wild_num,chinook0_wild_fail)~ temp + flow +
              atu_solstice + I(atu_solstice^2) + 
              photoperiod + chinook0_hatchery_perhour + 
              chinook1_hatchery_perhour + coho1_hatchery_perhour + 
              steelheadsmolt_hatchery_perhour + pink0_hatchery_perhour,
            family =  "binomial",
            data = data) #working
require(arm)
binnedplot(x = predict(glmm), y= resid(glmm))

glmm <- glmmPQL(cbind(chinook0_wild_num,chinook0_wild_fail)~ temp + flow +
              atu_solstice + I(atu_solstice^2) + 
              photoperiod + chinook0_hatchery_perhour + 
              chinook1_hatchery_perhour + coho1_hatchery_perhour + 
              steelheadsmolt_hatchery_perhour + pink0_hatchery_perhour,
              random = ~1|year,
              correlation = corAR1(),
            family =  binomial,
            data = data, verbose = FALSE) #working

glmm <- glmmPQL(chinook0_wild_num~ temp + flow +
                  atu_solstice + I(atu_solstice^2) + 
                  photoperiod + chinook0_hatchery_perhour + 
                  chinook1_hatchery_perhour + coho1_hatchery_perhour + 
                  steelheadsmolt_hatchery_perhour + pink0_hatchery_perhour,
                random = ~1|year,
                correlation = corAR1(),
                family =  poisson,
                data = data, verbose = FALSE,
                na.action=na.exclude) #working

summary(glmm)

data_trial <- data[1:100,]
glmm <- glmmPQL(chinook0_wild_num~ temp + flow,
                random = ~1|year,
                correlation = corAR1(),
                family =  poisson,
                data = data_trial, verbose = FALSE,
                na.action=na.exclude) #working

summary(glmm)


glmm <- glmmPQL(cbind(chinook0_wild_num, chinook0_wild_fail)~ temp + flow +
                  chinook1_hatchery_perhour,
                random = ~1|year,
                correlation = corAR1(form = ~doy|year),
                family =  binomial,
                data = data_trial, verbose = FALSE,
                na.action=na.exclude) #working

summary(glmm)
binnedplot(x = predict(glmm), y= resid(glmm))

glm <- gls(cbind(chinook0_wild_num,chinook0_wild_fail)~ temp + flow +
                  atu_solstice + I(atu_solstice^2) + 
                  photoperiod + chinook0_hatchery_perhour + 
                  chinook1_hatchery_perhour + coho1_hatchery_perhour + 
                  steelheadsmolt_hatchery_perhour + pink0_hatchery_perhour, 
                correlation = corAR1(form = ~doy|year), family =  "binomial",
                data = data, na.action=na.exclude) #not working
summary(mm)
binnedplot(x = predict(glm_probability), y= resid(glm_probability))

# agg_data_w_ma_msd$chinook0_wild_fail <- ave(agg_data_w_ma_msd, 
#     cumsum(c(F, diff(agg_data_w_ma_msd$year) == 1)), FUN=number_failed)

# agg_data_w_ma_msd$chinook0_wild_fail <- cum_df - 
# value <- 100
# nout <- 10
# # v[i+1]  =  function(v[i], x[i])
# v <- Reduce(function(v, x) .9*v  + 9, x=numeric(nout),  init=value, accumulate=TRUE)
# cbind(step = 0:nout, v)
