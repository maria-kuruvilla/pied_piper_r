#Goal - install phenomix, use it for both count and real

library(phenomix)
library(TMB)
library(dplyr)
library(here)
library(ggplot2)

glimpse(fishdist)

agg_data_w_ma_msd <- read.csv(here("..","..","data","pied_piper","dungeness_2005-2020_combine_ma_msd_atu_photoperiod.csv"),header=TRUE)

chum_2005 <- data.frame(year = agg_data_w_ma_msd$year, doy = agg_data_w_ma_msd$doy, chum_num=agg_data_w_ma_msd$chum0_wild_num)
glimpse(chum_2005)

plot(chum_2005$doy, chum_2005$chum_num)

cov_dat = data.frame(nyear = unique(chum_2005$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

datalist = create_data(chum_2005, 
                       min_number=0, 
                       variable = "chum_num", 
                       time="year", 
                       date = "doy",
                       asymmetric_model = FALSE, 
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "gaussian")
set.seed(1)
fitted = fit(datalist)
fitted$pars$convergence


cov_dat = data.frame(nyear = unique(fishdist$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

datalist = create_data(fishdist, 
                       min_number=0, 
                       variable = "number", 
                       time="year", 
                       date = "doy",
                       asymmetric_model = FALSE, 
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "gaussian")
set.seed(1)
fitted = fit(datalist)
fitted$pars$convergence



chum <- data.frame(year = agg_data_w_ma_msd$year, doy = agg_data_w_ma_msd$doy, chum_num=agg_data_w_ma_msd$chum0_wild_num)
glimpse(chum)

plot(chum_2005$doy, chum_2005$chum_num)

cov_dat = data.frame(nyear = unique(chum$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

datalist_chum = create_data(chum, 
                       min_number=0, 
                       variable = "chum_num", 
                       time="year", 
                       date = "doy",
                       asymmetric_model = TRUE, 
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "student_t")
set.seed(1)
fitted_chum = fit(datalist_chum)
fitted_chum$pars$convergence



#let's try coho
plot(agg_data_w_ma_msd$doy[agg_data_w_ma_msd$year==2015],agg_data_w_ma_msd$coho1_wild_num[agg_data_w_ma_msd$year==2015], type = "l")

coho_2016 <- data.frame(doy = agg_data_w_ma_msd$doy[agg_data_w_ma_msd$year==2015], number=agg_data_w_ma_msd$coho1_wild_num[agg_data_w_ma_msd$year==2015])

datalist_coho = create_data(coho_2016, 
                            min_number=0, 
                            variable = "number",
                            time = "doy",
                            asymmetric_model = TRUE,
                            est_sigma_re = TRUE,
                            est_mu_re = TRUE,
                            tail_model = "student_t")
set.seed(1)
fitted_coho = fit(datalist_coho)
fitted_coho$pars$convergence


#let's see what fishdist data looks like
year = 1962
plot(fishdist$doy[fishdist$year == year], 
     fishdist$number[fishdist$year == year], type = "l")

chum_1962 <- data.frame(doy = fishdist$doy[fishdist$year == year], 
                        number  = fishdist$number[fishdist$year == year],
                        year  = fishdist$year[fishdist$year == year])

datalist_1962 <- create_data(chum_1962, 
                             min_number=0, 
                             variable = "number",
                             time = "doy",
                             asymmetric_model = TRUE,
                             est_sigma_re = TRUE,
                             est_mu_re = TRUE,
                             tail_model = "student_t")

set.seed(1)
fitted_chum_1962 = fit(datalist_1962)
fitted_chum_1962$pars$convergence
#did not work

cov_dat = data.frame(nyear = unique(chum_1962$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 



datalist = create_data(chum_1962, 
                       min_number=0, 
                       variable = "number", 
                       time="year", 
                       date = "doy",
                       asymmetric_model = FALSE, 
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "gaussian")
set.seed(1)
fitted = fit(datalist)
fitted$pars$convergence

g = plot_diagnostics(fitted, type="timing", logspace=FALSE)
g


#trying single year of data for coho1 from Dungeness

year = 2020
plot(agg_data_w_ma_msd$doy[agg_data_w_ma_msd$year == year], 
     agg_data_w_ma_msd$coho1_wild_num[agg_data_w_ma_msd$year == year], type = "l")

coho_2020 <- data.frame(doy = agg_data_w_ma_msd$doy[agg_data_w_ma_msd$year == year],
                        year = agg_data_w_ma_msd$year[agg_data_w_ma_msd$year == year],
                        number = agg_data_w_ma_msd$coho1_wild_num[agg_data_w_ma_msd$year == year])

cov_dat = data.frame(nyear = unique(coho_2020$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

datalist_coho = create_data(coho_2020, 
                       min_number=0, 
                       variable = "number", 
                       time="year", 
                       date = "doy",
                       asymmetric_model = FALSE, 
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "student_t")
set.seed(1)
fitted_coho = fit(datalist_coho)
fitted_coho$pars$convergence

#trying with pink

year = 2011

x = seq(0,200,1)
y <- dnorm(x,87,5)*4000
points(x,y)

plot(x,y)

plot(agg_data_w_ma_msd$doy[agg_data_w_ma_msd$year == year], 
     agg_data_w_ma_msd$pink0_wild_num[agg_data_w_ma_msd$year == year], type = "l")


pink_2011 <- data.frame(doy = agg_data_w_ma_msd$doy[agg_data_w_ma_msd$year == year],
                        year = agg_data_w_ma_msd$year[agg_data_w_ma_msd$year == year],
                        number = agg_data_w_ma_msd$pink0_wild_num[agg_data_w_ma_msd$year == year])
plot(pink_2011$doy, pink_2011$number)

cov_dat = data.frame(nyear = unique(pink_2011$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 


datalist_pink = create_data(pink_2011, 
                            min_number=0, 
                            variable = "number", 
                            time="year", 
                            date = "doy",
                            asymmetric_model = FALSE, 
                            mu = ~ nyear,
                            sigma = ~ nyear,
                            covar_data = cov_dat,
                            est_sigma_re = TRUE,
                            est_mu_re = TRUE,
                            tail_model = "gaussian")
set.seed(1)
fitted_pink = fit(datalist_pink)
fitted_pink$pars$convergence

#troubleshooting
datalist_pink = create_data(pink_2011, 
                            asymmetric_model = FALSE,
                            est_sigma_re = FALSE,
                            est_mu_re = FALSE,
                            tail_model = "gaussian")
set.seed(1)
fitted_pink = fit(datalist_pink, silent = TRUE, control = list(eval.max = 4000, 
                                                               iter.max = 5000, 
                                                               rel.tol = 1e-7))
fitted_pink$pars$convergence



##trying the trouble shooting example given online
set.seed(123)
df <- expand.grid("doy" = 100:200, "year"=1:20)
df$mu <- rnorm(unique(df$year), 150, 5)[df$year]
df$sig1 <- rnorm(unique(df$year),30,5)[df$year]
df$sig2 <- rnorm(unique(df$year), 30, 5)[df$year]
df$sig <- ifelse(df$doy < df$mu, df$sig1, df$sig2)
df$pred <- dnorm(df$doy, df$mu, sd = df$sig, log = TRUE)
df$pred <- exp(df$pred+8)
df$number <- round(rnorm(nrow(df), df$pred, 0.1))

set.seed(1)
fit_t <- fit(create_data(df, asymmetric_model = TRUE, min_number = 1,
                         tail_model = "student_t"),
             silent = TRUE,
             control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)

fit_t$pars$convergence

fit_gnorm <- fit(create_data(df, asymmetric_model = TRUE, min_number = 1,
                             tail_model = "gnorm"),
                 silent = TRUE,limits=TRUE,
                 control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)

fit_gnorm$pars$convergence


fit_gnorm <- fit(create_data(df, asymmetric_model = TRUE, min_number = 1,
                             tail_model = "gnorm",
                             est_mu_re = FALSE,
                             est_sigma_re = FALSE),
                 silent = TRUE,
                 control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)

pink_2011 <- data.frame(doy = agg_data_w_ma_msd$doy[agg_data_w_ma_msd$year == year],
                        year = agg_data_w_ma_msd$year[agg_data_w_ma_msd$year == year],
                        number = agg_data_w_ma_msd$pink0_wild_num[agg_data_w_ma_msd$year == year])

pink_2011_gnorm <- fit(create_data(pink_2011, asymmetric_model = TRUE, min_number = 1,
                             tail_model = "gnorm",
                             est_mu_re = FALSE,
                             est_sigma_re = FALSE),
                 silent = TRUE,
                 control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)
#it fit!
g = plot_diagnostics(pink_2011_gnorm, type="timing", logspace = TRUE)
g


pink_2011_student_t <- fit(create_data(pink_2011, asymmetric_model = TRUE, min_number = 1,
                                   tail_model = "student_t"),
                       control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)

g2 = plot_diagnostics(pink_2011_student_t, type="timing", logspace = TRUE)
g2

#let's try for all pink data < 2017

pink <- data.frame(doy = agg_data_w_ma_msd$doy[agg_data_w_ma_msd$year < 2017],
                        year = agg_data_w_ma_msd$year[agg_data_w_ma_msd$year< 2017],
                        number = agg_data_w_ma_msd$pink0_wild_num[agg_data_w_ma_msd$year <2017])

pink_student_t <- fit(create_data(pink, asymmetric_model = TRUE, min_number = 1,
                                       tail_model = "student_t"),
                           control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)

year = 2020
pink_year <- data.frame(doy = agg_data_w_ma_msd$doy[agg_data_w_ma_msd$year == year],
                        year = agg_data_w_ma_msd$year[agg_data_w_ma_msd$year == year],
                        number = agg_data_w_ma_msd$pink0_wild_num[agg_data_w_ma_msd$year == year])
plot(pink_year$doy, pink_year$number)

pink_student_t <- fit(create_data(pink_year, asymmetric_model = TRUE, min_number = 1,
                                  tail_model = "student_t"),
                      control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)

g = plot_diagnostics(pink_student_t, type="timing", logspace = TRUE)
g

#success for 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015
# 2016, 2017, 2018, 2019, 2020

pink <- data.frame(doy = agg_data_w_ma_msd$doy,
                   year = agg_data_w_ma_msd$year,
                   number = agg_data_w_ma_msd$pink0_wild_num)


cov_dat = data.frame(nyear = unique(pink$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

datalist = create_data(pink, 
                       min_number=1, 
                       variable = "number", 
                       time="year", 
                       date = "doy",
                       asymmetric_model = FALSE, 
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "student_t")
#set.seed(1)
fitted = fit(datalist)

pink_all = plot_diagnostics(fitted, type="timing", logspace = FALSE)
pink_all
#why does it work only when min_num = 1 and not when min_num = 0


#trouble shooting to send eric

#read data
dungeness_data <- read.csv(here("..","..","data","pied_piper","dungeness","dungeness_subset.csv"),header=TRUE)
year = 2011
plot(dungeness_data$doy[dungeness_data$year == year], 
     dungeness_data$pink0_wild_num[dungeness_data$year == year], type = "l")

pink_year <- data.frame(doy = dungeness_data$doy[dungeness_data$year == year],
                                     year = dungeness_data$year[dungeness_data$year == year],
                                     number = dungeness_data$pink0_wild_num[dungeness_data$year == year])

pink_student_t <- fit(create_data(pink_year, asymmetric_model = TRUE, min_number = 0,
                                  tail_model = "student_t"),
                      control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)
plot(pink_year$doy[pink_year$number >0], pink_year$number[pink_year$number >0],)

pink_year_subset <- pink_year[pink_year$number>0,]
pink_student_t <- fit(create_data(pink_year_subset, asymmetric_model = TRUE, min_number = 0,
                                  tail_model = "student_t"),
                      control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)

#trying real data instead of count data

dungeness_data <- read.csv(here("..","..","data","pied_piper","dungeness","dungeness_subset.csv"),header=TRUE)
year = 2020
plot(dungeness_data$doy[dungeness_data$year == year], 
     dungeness_data$pink0_wild_num[dungeness_data$year == year])

pink_year <- data.frame(doy = dungeness_data$doy[dungeness_data$year == year],
                        year = dungeness_data$year[dungeness_data$year == year],
                        number = dungeness_data$pink0_wild_perhour[dungeness_data$year == year])


pink_student_t <- fit(create_data(pink_year, asymmetric_model = TRUE, min_number = 1,
                                  tail_model = "student_t"),
                      control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)
plot_diagnostics(pink_student_t, logspace = FALSE)


#making plot for pinks for all years
# count
# student t
# skewed

pink_data <- data.frame(doy = dungeness_data$doy[dungeness_data$doy < 366],
                        year = dungeness_data$year[dungeness_data$doy < 366],
                        number = dungeness_data$pink0_wild_num[dungeness_data$doy < 366])

cov_dat = data.frame(nyear = unique(pink_data$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

pink_student_t <- fit(create_data(pink_data,
                                  min_number=1, 
                                  variable = "number", 
                                  time="year", 
                                  date = "doy",
                                  asymmetric_model = FALSE, 
                                  mu = ~ nyear,
                                  sigma = ~ nyear,
                                  covar_data = cov_dat,
                                  est_sigma_re = TRUE,
                                  est_mu_re = TRUE,
                                  tail_model = "student_t"),
                      control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)
plot_diagnostics(pink_student_t, logspace = FALSE)

#2020 not good. trying per hour data now
#making plot for pinks for all years
# per hour
# student t
# skewed

pink_data <- data.frame(doy = dungeness_data$doy[dungeness_data$doy < 366],
                        year = dungeness_data$year[dungeness_data$doy < 366],
                        number = dungeness_data$pink0_wild_perhour[dungeness_data$doy < 366])

cov_dat = data.frame(nyear = unique(pink_data$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

pink_student_t <- fit(create_data(pink_data,
                                  min_number=1, 
                                  variable = "number", 
                                  time="year", 
                                  date = "doy",
                                  asymmetric_model = FALSE, 
                                  mu = ~ nyear,
                                  sigma = ~ nyear,
                                  covar_data = cov_dat,
                                  est_sigma_re = TRUE,
                                  est_mu_re = TRUE,
                                  tail_model = "student_t"),
                      control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)
plot_diagnostics(pink_student_t, logspace = FALSE)

# that does not work for some years
# how to plot for years that it does work
df <- predict(pink_student_t)

#try for individual years
year = 2017

pink_data <- data.frame(doy = dungeness_data$doy[dungeness_data$doy < 366 & dungeness_data$year == year],
                        year = dungeness_data$year[dungeness_data$doy < 366 & dungeness_data$year == year],
                        number = dungeness_data$pink0_wild_perhour[dungeness_data$doy < 366 & dungeness_data$year == year])

cov_dat = data.frame(nyear = unique(pink_data$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

pink_student_t_2017 <- fit(create_data(pink_data,
                                  min_number=1, 
                                  variable = "number", 
                                  time="year", 
                                  date = "doy",
                                  asymmetric_model = FALSE, 
                                  mu = ~ nyear,
                                  sigma = ~ nyear,
                                  covar_data = cov_dat,
                                  est_sigma_re = TRUE,
                                  est_mu_re = TRUE,
                                  tail_model = "student_t"),
                      control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)
plot_diagnostics(pink_student_t_2017, logspace = FALSE)
#2017 does not converge


#trying gnorm
pink_data <- data.frame(doy = dungeness_data$doy[dungeness_data$doy < 366],
                        year = dungeness_data$year[dungeness_data$doy < 366],
                        number = dungeness_data$pink0_wild_perhour[dungeness_data$doy < 366])

cov_dat = data.frame(nyear = unique(pink_data$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

pink_student_t <- fit(create_data(pink_data,
                                  min_number=1, 
                                  variable = "number", 
                                  time="year", 
                                  date = "doy",
                                  asymmetric_model = FALSE, 
                                  mu = ~ nyear,
                                  sigma = ~ nyear,
                                  covar_data = cov_dat,
                                  est_sigma_re = TRUE,
                                  est_mu_re = TRUE,
                                  tail_model = "gnorm"),
                      control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)
plot_diagnostics(pink_student_t, logspace = FALSE)
# gnorm also has same problem
#how about gaussian

pink_data <- data.frame(doy = dungeness_data$doy[dungeness_data$doy < 366],
                        year = dungeness_data$year[dungeness_data$doy < 366],
                        number = dungeness_data$pink0_wild_perhour[dungeness_data$doy < 366])

cov_dat = data.frame(nyear = unique(pink_data$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

pink_student_t <- fit(create_data(pink_data,
                                  min_number=1, 
                                  variable = "number", 
                                  time="year", 
                                  date = "doy",
                                  asymmetric_model = FALSE, 
                                  mu = ~ nyear,
                                  sigma = ~ nyear,
                                  covar_data = cov_dat,
                                  est_sigma_re = TRUE,
                                  est_mu_re = TRUE,
                                  tail_model = "gaussian"),
                      control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)
plot_diagnostics(pink_student_t, logspace = FALSE)

df <- predict(pink_student_t)
mus <- data.frame(
  mu = pink_student_t$sdreport$value[which(names(pink_student_t$sdreport$value) == "mu")]
)


#quickly trying other species

chum_data <- data.frame(doy = dungeness_data$doy[dungeness_data$doy < 366],
                        year = dungeness_data$year[dungeness_data$doy < 366],
                        number = dungeness_data$chum0_wild_perhour[dungeness_data$doy < 366])

cov_dat = data.frame(nyear = unique(chum_data$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

chum_student_t <- fit(create_data(chum_data,
                                  min_number=1, 
                                  variable = "number", 
                                  time="year", 
                                  date = "doy",
                                  asymmetric_model = FALSE, 
                                  mu = ~ nyear,
                                  sigma = ~ nyear,
                                  covar_data = cov_dat,
                                  est_sigma_re = TRUE,
                                  est_mu_re = TRUE,
                                  tail_model = "student_t"),
                      control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)
plot_diagnostics(chum_student_t, logspace = FALSE)
ggsave(here("..","..","output","pied_piper","dungeness_chum_phenomix.pdf"), plot = last_plot(), width = 15, height = 20)#, units = c("in", "cm", "mm"))


#coho
coho_data <- data.frame(doy = dungeness_data$doy[dungeness_data$doy < 366],
                        year = dungeness_data$year[dungeness_data$doy < 366],
                        number = dungeness_data$coho1_wild_perhour[dungeness_data$doy < 366])

cov_dat = data.frame(nyear = unique(coho_data$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

coho_student_t <- fit(create_data(coho_data,
                                  min_number=1, 
                                  variable = "number", 
                                  time="year", 
                                  date = "doy",
                                  asymmetric_model = FALSE, 
                                  mu = ~ nyear,
                                  sigma = ~ nyear,
                                  covar_data = cov_dat,
                                  est_sigma_re = TRUE,
                                  est_mu_re = TRUE,
                                  tail_model = "student_t"),
                      control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)
plot_diagnostics(coho_student_t, logspace = FALSE)

coho_gaussian <- fit(create_data(coho_data,
                                  min_number=1, 
                                  variable = "number", 
                                  time="year", 
                                  date = "doy",
                                  asymmetric_model = FALSE, 
                                  mu = ~ nyear,
                                  sigma = ~ nyear,
                                  covar_data = cov_dat,
                                  est_sigma_re = TRUE,
                                  est_mu_re = TRUE,
                                  tail_model = "gaussian"),
                      control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)
plot_diagnostics(coho_gaussian, logspace = FALSE)


coho_gnorm <- fit(create_data(coho_data,
                                 min_number=1, 
                                 variable = "number", 
                                 time="year", 
                                 date = "doy",
                                 asymmetric_model = FALSE, 
                                 mu = ~ nyear,
                                 sigma = ~ nyear,
                                 covar_data = cov_dat,
                                 est_sigma_re = TRUE,
                                 est_mu_re = TRUE,
                                 tail_model = "gnorm"),
                     control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)
plot_diagnostics(coho_gnorm, logspace = FALSE)


ggsave(here("..","..","output","pied_piper","dungeness_chum_phenomix.pdf"), plot = last_plot(), width = 15, height = 20)#, units = c("in", "cm", "mm"))


#chinook1

chinook1_data <- data.frame(doy = dungeness_data$doy[dungeness_data$doy < 366],
                        year = dungeness_data$year[dungeness_data$doy < 366],
                        number = dungeness_data$chinook1_wild_perhour[dungeness_data$doy < 366])

cov_dat = data.frame(nyear = unique(chinook1_data$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

chinook1_student_t <- fit(create_data(chinook1_data,
                                  min_number=1, 
                                  variable = "number", 
                                  time="year", 
                                  date = "doy",
                                  asymmetric_model = FALSE, 
                                  mu = ~ nyear,
                                  sigma = ~ nyear,
                                  covar_data = cov_dat,
                                  est_sigma_re = TRUE,
                                  est_mu_re = TRUE,
                                  tail_model = "student_t"),
                      control = list(eval.max = 4000, iter.max = 5000, rel.tol = 1e-7)
)
plot_diagnostics(coho_student_t, logspace = FALSE)