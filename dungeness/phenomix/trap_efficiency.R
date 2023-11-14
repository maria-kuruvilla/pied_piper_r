#Goal
#try phenomix with trap efficiency data and without to look at the difference in
#peak migration

#load packages
library(ggplot2)
library(phenomix)
library(dplyr)
library(TMB)
library(here)
#readin file
d <- read.csv(here("data",
                   "dungeness","dungeness_aggregated_w_trap_efficiency.csv"),header=TRUE)

#chinook0 - late spring

#negbin and gaussian
#trying with covariate data

cov_dat = data.frame(nyear = unique(d$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

datalist_chinook0_1_t = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
                                    min_number=0, 
                                    variable = "chinook0_wild_num",
                                    time="year", 
                                    date = "doy",
                                    family = "negbin",
                                    asymmetric_model = TRUE, 
                                    mu = ~ nyear,
                                    sigma = ~ nyear,
                                    covar_data = cov_dat,
                                    est_sigma_re = FALSE,
                                    est_mu_re = TRUE,
                                    tail_model = "student_t")

set.seed(1)
fitted_year_chinook0_1_t = fit(datalist_chinook0_1_t,
                                      control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chinook0_1_t$pars$convergence
AIC(fitted_year_chinook0_1_t) #11582

means_chinook0_num <- extract_means(fitted_year_chinook0_1_t)


#perhour
datalist_chinook0_perhour_1_t = create_data(dplyr::filter(d, !is.na(chinook0_wild_perhour) & doy >= 120),
                                    min_number=0, 
                                    variable = "chinook0_wild_perhour",
                                    time="year", 
                                    date = "doy",
                                    family = "negbin",
                                    asymmetric_model = TRUE, 
                                    mu = ~ nyear,
                                    sigma = ~ nyear,
                                    covar_data = cov_dat,
                                    est_sigma_re = FALSE,
                                    est_mu_re = TRUE,
                                    tail_model = "student_t")


set.seed(1)
fitted_year_chinook0_perhour_1_t = fit(datalist_chinook0_perhour_1_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chinook0_perhour_1_t$pars$convergence
AIC(fitted_year_chinook0_perhour_1_t) #11582

means_chinook0_perhour <- extract_means(fitted_year_chinook0_perhour_1_t)


#using estimates
datalist_chinook0_perhour_estimate_1_t = create_data(dplyr::filter(d, !is.na(chinook0_wild_perhour_estimate) & doy >= 120),
                                            min_number=0, 
                                            variable = "chinook0_wild_perhour_estimate",
                                            time="year", 
                                            date = "doy",
                                            family = "negbin",
                                            asymmetric_model = TRUE, 
                                            mu = ~ nyear,
                                            sigma = ~ nyear,
                                            covar_data = cov_dat,
                                            est_sigma_re = FALSE,
                                            est_mu_re = TRUE,
                                            tail_model = "student_t")

set.seed(1)
fitted_year_chinook0_perhour_estimate_1_t = fit(datalist_chinook0_perhour_estimate_1_t,
                                       control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chinook0_perhour_estimate_1_t$pars$convergence
AIC(fitted_year_chinook0_perhour_estimate_1_t) #7840

means_chinook0_perhour_estimate <- extract_means(fitted_year_chinook0_perhour_estimate_1_t)
means_chinook0_perhour_estimate$year <- 2005:2020
means_chinook0_perhour$year <- 2005:2020
percentage_diff <- (means_chinook0_perhour$value - means_chinook0_perhour_estimate$value)*100/means_chinook0_perhour$value

percentage_diff

day_diff <- means_chinook0_perhour$value - means_chinook0_perhour_estimate$value

day_diff
avg_day_diff <- mean(abs(day_diff))
avg_day_diff

#combine means_chinook0_perhour_estimate and means_chinook0_perhour using tidyverse and add
#a column to indicate which is which

means_chinook0_perhour_estimate <- mutate(means_chinook0_perhour_estimate, type = "estimate")
means_chinook0_perhour <- mutate(means_chinook0_perhour, type = "raw")
means_chinook0 <- rbind(means_chinook0_perhour_estimate, means_chinook0_perhour)


#save file
write.csv(means_chinook0, here("data", "dungeness", "means_chinook0.csv"), row.names = FALSE)


#use ggplot to plot the values of means_chinook0 by year, use different colors for type

ggplot(means_chinook0, aes(x = year, y = value, color = as.factor(type))) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Peak Migration (day of year)", title = "Peak Migration of Chinook0 (late spring) by Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = c("#337a64", "#7b6b80"))

predicted_values <- predict(fitted_year_chinook0_perhour_estimate_1_t)

ggplot(predicted_values, aes(x = x, y = log(y))) +
  geom_point(alpha = 0.5) +
  geom_line(aes(x=x,y=log(pred))) +
  facet_wrap(~years, ncol = 4, scales = 'free')+
  labs(x = "doy", y = "Fish per hour")
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
#I dont think this is right
