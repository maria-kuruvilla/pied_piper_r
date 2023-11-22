#Goal
#try phenomix with trap efficiency data and without to look at the difference in
#peak migration

#load packages
library(ggplot2)
library(phenomix)
library(dplyr)
library(TMB)
library(here)
library(tidyverse)
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

#extract upper and lower quantile
upper_quantile_chinook0 <- extract_upper(fitted_year_chinook0_perhour_estimate_1_t)
lower_quantile_chinook0 <- extract_lower(fitted_year_chinook0_perhour_estimate_1_t)
upper_quantile_chinook0_raw <- extract_upper(fitted_year_chinook0_perhour_1_t)
lower_quantile_chinook0_raw <- extract_lower(fitted_year_chinook0_perhour_1_t)

#combine upper and lower quantile and add column to indicate which is which and add column to
#indicate whether type is estimate or raw

upper_quantile_chinook0 <- mutate(upper_quantile_chinook0, type = "estimate")
lower_quantile_chinook0 <- mutate(lower_quantile_chinook0, type = "estimate")
upper_quantile_chinook0_raw <- mutate(upper_quantile_chinook0_raw, type = "raw")
lower_quantile_chinook0_raw <- mutate(lower_quantile_chinook0_raw, type = "raw")

quantile_chinook0 <- rbind(upper_quantile_chinook0, upper_quantile_chinook0_raw,
                            lower_quantile_chinook0, lower_quantile_chinook0_raw)

#calculate the difference between upper and lower
diff_quantile_chinook0 <- upper_quantile_chinook0$value - lower_quantile_chinook0$value
diff_quantile_chinook0_raw <- upper_quantile_chinook0_raw$value - lower_quantile_chinook0_raw$value

#in dataframe d, calculate the cumulative sum of chinook0_hatchery_num for every year
#ignore na values

d_sum_chinook0 <- d %>%
  select(year, chinook0_hatchery_num) %>% 
  group_by(year) %>%
  summarize(sum_chinook0 = sum(chinook0_hatchery_num, na.rm = TRUE))

d_sum_chinook0$diff <- diff_quantile_chinook0_raw

#plot diff vs sum_chinook0 and make x axis a log
ggplot(d_sum_chinook0, aes(x = sum_chinook0, y = diff)) +
  geom_point() +
  labs(x = "Sum of Chinook0 Hatchery Counts", y = "Difference between Upper and Lower Quantile") +
  theme_bw()

#plot year vs diff
ggplot(d_sum_chinook0, aes(x = year, y = diff)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Sum of Chinook0 Hatchery Counts") +
  theme_bw()

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
  
  
#means are changin with year . I do not want that
  
#using estimates
datalist_chinook0_perhour_estimate_1_t_edited = create_data(dplyr::filter(d, !is.na(chinook0_wild_perhour_estimate) & doy >= 120),
                                                       min_number=0, 
                                                       variable = "chinook0_wild_perhour_estimate",
                                                       time="year", 
                                                       date = "doy",
                                                       family = "negbin",
                                                       asymmetric_model = TRUE,
                                                       covar_data = cov_dat,
                                                       est_sigma_re = FALSE,
                                                       est_mu_re = TRUE,
                                                       tail_model = "student_t")
  
set.seed(1)
fitted_year_chinook0_perhour_estimate_1_t_edited = fit(datalist_chinook0_perhour_estimate_1_t_edited,
                                                control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chinook0_perhour_estimate_1_t_edited$pars$convergence
AIC(fitted_year_chinook0_perhour_estimate_1_t_edited) #7910

#extract means
means_chinook0_perhour_estimate_edited <- extract_means(fitted_year_chinook0_perhour_estimate_1_t_edited)
means_chinook0_perhour_estimate_edited
means_chinook0_perhour_estimate_edited$year <- 2005:2020

#extract upper and lower quantile and add it to the means_chinook0_perhour_estimate_edited dataframe

upper_quantile_chinook0_perhour_estimate_edited <- extract_upper(fitted_year_chinook0_perhour_estimate_1_t_edited)
lower_quantile_chinook0_perhour_estimate_edited <- extract_lower(fitted_year_chinook0_perhour_estimate_1_t_edited)

upper_quantile_chinook0_perhour_estimate_edited <- mutate(upper_quantile_chinook0_perhour_estimate_edited, type = "upper")
lower_quantile_chinook0_perhour_estimate_edited <- mutate(lower_quantile_chinook0_perhour_estimate_edited, type = "lower")

quantile_chinook0_perhour_estimate_edited <- rbind(upper_quantile_chinook0_perhour_estimate_edited, lower_quantile_chinook0_perhour_estimate_edited)

#calculate the difference between upper and lower
diff_quantile_chinook0_perhour_estimate_edited <- upper_quantile_chinook0_perhour_estimate_edited$value - lower_quantile_chinook0_perhour_estimate_edited$value

#add year to diff_quantile_chinook0_perhour_estimate_edited
diff_quantile_chinook0_perhour_estimate_edited <- mutate(diff_quantile_chinook0_perhour_estimate_edited, year = 2005:2020)
