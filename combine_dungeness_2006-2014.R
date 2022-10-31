#Goal - combine data from dungeness 2006-2014 so that we can make average

library(here)
library(tidyverse)
library(tidyquant)
library(RColorBrewer)
library(patchwork)
library(magick)
library(cowplot)
library(rsq)

data <- read.csv(here("Documents","data","pied_piper","dungeness_2006-2014_combine_doy.csv"),header=TRUE)#,na.strings=c("[nan]"))

data$chinook0_deviation <-  data$chinook0_wild_perhour_x - data$ma_chinook0_wild_perhour
png(here("Documents","output","pied_piper","chinook0_deviation_hist.png"), width = 465, height = 225, units = 'mm', res = 300)
hist(data$chinook0_deviation,50, main = "Chinook 0+", xlab = "Deviation from moving average")
dev.off()
hist(log(data$chinook0_deviation+10),50, main = "Chinook 0+", xlab = "Log Deviation from moving average")


data$chinook1_deviation <-  data$chinook1_wild_perhour_x - data$ma_chinook1_wild_perhour
png(here("Documents","output","pied_piper","chinook1_deviation_hist.png"), width = 465, height = 225, units = 'mm', res = 300)
hist(data$chinook1_deviation,50, main = "Chinook 1+", xlab = "Deviation from moving average")
dev.off()

hist(log(data$chinook1_deviation+10),50, main = "Chinook 1+", xlab = "Log Deviation from moving average")

data$coho1_deviation <-  data$coho1_wild_perhour_x - data$ma_coho1_wild_perhour
png(here("Documents","output","pied_piper","coho1_deviation_hist.png"), width = 465, height = 225, units = 'mm', res = 300)
hist(data$coho1_deviation,50, main = "Coho 1+", xlab = "Deviation from moving average")
dev.off()

hist(log(data$coho1_deviation + 10),50, main = "Coho 1+", xlab = "Log Deviation from moving average")



model_lm <- lm(
  chinook0_deviation ~ doy + I(doy^2) + temp + 
    I(temp^2) + flow + I(flow^2) + 
    chinook0_hatchery_perhour_x + chinook1_hatchery_perhour_x, data)
summary(model_lm)
rsq(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.05 #this works
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


model_lm <- lm(
  chinook0_deviation ~  chinook0_hatchery_perhour_x + chinook1_hatchery_perhour_x, data)
summary(model_lm)
rsq(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.05 #this works
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


model_lm <- lm(
  log(chinook0_deviation+10) ~ year + doy + I(doy^2) + temp + 
    I(temp^2) + flow + I(flow^2) + 
    chinook0_hatchery_perhour_x + chinook1_hatchery_perhour_x, data)
summary(model_lm)
rsq(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.05 #this works
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
