<<<<<<< HEAD
#trying phenomix with est_mu and est_sigma = FALSE

#load packages
library(ggplot2)
library(tidyverse)
library(here)
library(dplyr)
library(phenomix)

#readin file
d <- read.csv(here("data",
                   "dungeness","dungeness_aggregated_w_trap_efficiency.csv"),header=TRUE)

datalist_chinook0_1_t = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
                                    min_number=0, 
                                    variable = "chinook0_wild_num",
                                    time="year", 
                                    date = "doy",
                                    family = "negbin",
                                    asymmetric_model = TRUE, 
                                    # mu = ~ nyear,
                                    # sigma = ~ nyear,
                                    # covar_data = cov_dat,
                                    est_sigma_re = FALSE,
                                    est_mu_re = FALSE,
                                    tail_model = "student_t")


set.seed(1)
fitted_year_chinook0_1_t = fit(datalist_chinook0_1_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))
fitted_year_chinook0_1_t$pars$convergence


g = plot_diagnostics(fitted_year_chinook0_1_t, type="timing", logspace=TRUE)
g

means <- extract_means(fitted_year_chinook0_1_t)
upper <- extract_upper(fitted_year_chinook0_1_t)
lower <- extract_lower(fitted_year_chinook0_1_t)
upper$value-lower$value



datalist_chinook0_1_t = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
                                    min_number=0, 
                                    variable = "chinook0_wild_num",
                                    time="year", 
                                    date = "doy",
                                    family = "negbin",
                                    asymmetric_model = TRUE, 
                                    # mu = ~ nyear,
                                    # sigma = ~ nyear,
                                    # covar_data = cov_dat,
                                    # est_sigma_re = FALSE,
                                    # est_mu_re = FALSE,
                                    tail_model = "student_t")
set.seed(1)
fitted_year_chinook0_1_t = fit(datalist_chinook0_1_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))
fitted_year_chinook0_1_t$pars$convergence
#did not work


datalist_chinook0_1_t = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
                                    min_number=0, 
                                    variable = "chinook0_wild_num",
                                    time="year", 
                                    date = "doy",
                                    family = "negbin",
                                    asymmetric_model = FALSE, 
                                    # mu = ~ nyear,
                                    # sigma = ~ nyear,
                                    # covar_data = cov_dat,
                                    # est_sigma_re = FALSE,
                                    # est_mu_re = FALSE,
                                    tail_model = "student_t")
set.seed(1)
fitted_year_chinook0_1_t = fit(datalist_chinook0_1_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))
fitted_year_chinook0_1_t$pars$convergence
#did not work

datalist_chinook0_1_t = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
                                    min_number=0, 
                                    variable = "chinook0_wild_num",
                                    time="year", 
                                    date = "doy",
                                    family = "negbin",
                                    asymmetric_model = FALSE, 
                                    # mu = ~ nyear,
                                    # sigma = ~ nyear,
                                    # covar_data = cov_dat,
                                    # est_sigma_re = FALSE,
                                    # est_mu_re = FALSE,
                                    tail_model = "gnorm")
set.seed(1)
fitted_year_chinook0_1_t = fit(datalist_chinook0_1_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))
fitted_year_chinook0_1_t$pars$convergence
#did not work


datalist_chinook0_1_t = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
                                    min_number=0, 
                                    variable = "chinook0_wild_num",
                                    time="year", 
                                    date = "doy",
                                    # family = "negbin",
                                    asymmetric_model = FALSE, 
                                    # mu = ~ nyear,
                                    # sigma = ~ nyear,
                                    # covar_data = cov_dat,
                                    # est_sigma_re = FALSE,
                                    # est_mu_re = FALSE,
                                    tail_model = "gnorm")
set.seed(1)
fitted_year_chinook0_1_t = fit(datalist_chinook0_1_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))
fitted_year_chinook0_1_t$pars$convergence
#did not work

datalist_chinook0_1_t = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
                                    min_number=0, 
                                    variable = "chinook0_wild_num",
                                    time="year", 
                                    date = "doy")
set.seed(1)
fitted_year_chinook0_1_t = fit(datalist_chinook0_1_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))
fitted_year_chinook0_1_t$pars$convergence
#did not work

#trying Eric's code

datalist = create_data(dplyr::filter(d, !is.na(chinook0_wild_num)),
                       min_number=0,
                       variable = "chinook0_wild_num",
                       time="year",
                       date = "doy",
                       family = "negbin",
                       asymmetric_model = TRUE,
                       est_sigma_re = FALSE,
                       est_mu_re = TRUE,
                       tail_model = "gaussian")
set.seed(123)
fitted = fit(datalist,
             control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

datalist = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120 & doy < 200),
                       min_number=0,
                       variable = "chinook0_wild_num",
                       time="year",
                       date = "doy",
                       family = "negbin",
                       asymmetric_model = TRUE,
                       est_sigma_re = FALSE,
                       est_mu_re = TRUE,
                       tail_model = "gaussian")
set.seed(1)
fitted = fit(datalist,
             control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted$pars$convergence

means_chinook0 <- extract_means(fitted)
upper_chinook0 <- extract_upper(fitted)
lower_chinook0 <- extract_lower(fitted)
range_chinook0 <- upper_chinook0$value - lower_chinook0$value

range_chinook0

datalist_wo_trend = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120 & doy < 200),
                       min_number=0,
                       variable = "chinook0_wild_num",
                       time="year",
                       date = "doy",
                       family = "negbin",
                       asymmetric_model = TRUE,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "gaussian")
set.seed(1)
fitted_wo_trend = fit(datalist_wo_trend,
             control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_wo_trend$pars$convergence

means_chinook0_wo_trend <- extract_means(fitted_wo_trend)
upper_chinook0_wo_trend <- extract_upper(fitted_wo_trend)
lower_chinook0_wo_trend <- extract_lower(fitted_wo_trend)
range_chinook0_wo_trend <- upper_chinook0_wo_trend$value - lower_chinook0_wo_trend$value

range_chinook0_wo_trend

plot_diagnostics(fitted_wo_trend)


#calculate total number of chinook0_hatchery_num for each year

chinook0_hatchery_total <- d %>%
  group_by(year) %>%
  summarise(chinook0_hatchery_total = sum(chinook0_hatchery_num, na.rm = TRUE))

chinook0_hatchery_total

#plot total number of chinook0_hatchery_num for each year on the x axis and
#range on the y axis

#first add range and mean to chinook0_hatchery_total

chinook0_hatchery_total$range <- range_chinook0_wo_trend
chinook0_hatchery_total$mean <- means_chinook0_wo_trend$value

chinook0_hatchery_total

#plot

ggplot(chinook0_hatchery_total, 
       aes(x = chinook0_hatchery_total, y = range)) +
  geom_point() +
  geom_text(aes(label = year), hjust = 0, vjust = 0) +
  labs(x = "Total number of chinook0_hatchery_num", y = "Range of chinook0_hatchery_num") +
  theme_bw()

ggplot(chinook0_hatchery_total, 
       aes(x = chinook0_hatchery_total, y = mean)) +
  geom_point() +
  geom_text(aes(label = year), hjust = 0, vjust = 0) +
  labs(x = "Total number of chinook0_hatchery_num", y = "Range of chinook0_hatchery_num") +
  theme_bw()


datalist_wo_trend2 = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
                                min_number=0,
                                variable = "chinook0_wild_num",
                                time="year",
                                date = "doy",
                                family = "negbin",
                                asymmetric_model = TRUE,
                                est_sigma_re = TRUE,
                                est_mu_re = TRUE,
                                tail_model = "gaussian")
set.seed(1)
fitted_wo_trend2 = fit(datalist_wo_trend2,
                      control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_wo_trend2$pars$convergence


means_chinook0_wo_trend2 <- extract_means(fitted_wo_trend2)
upper_chinook0_wo_trend2 <- extract_upper(fitted_wo_trend2)
lower_chinook0_wo_trend2 <- extract_lower(fitted_wo_trend2)
range_chinook0_wo_trend2 <- upper_chinook0_wo_trend2$value - lower_chinook0_wo_trend2$value

range_chinook0_wo_trend2


chinook0_hatchery_total$range2 <- range_chinook0_wo_trend2
chinook0_hatchery_total$mean2 <- means_chinook0_wo_trend2$value

chinook0_hatchery_total

ggplot(chinook0_hatchery_total, 
       aes(x = chinook0_hatchery_total, y = range2)) +
  geom_point() +
  geom_text(aes(label = year), hjust = 0, vjust = 0) +
  labs(x = "Total number of chinook0_hatchery_num", y = "Range of chinook0_hatchery_num") +
  theme_bw()

ggplot(chinook0_hatchery_total, 
       aes(x = chinook0_hatchery_total, y = mean2)) +
  geom_point() +
  geom_text(aes(label = year), hjust = 0, vjust = 0) +
  labs(x = "Total number of chinook0_hatchery_num", y = "peak doy of migration") +
  theme_bw()

=======
#trying phenomix with est_mu and est_sigma = FALSE

#load packages
library(ggplot2)
library(tidyverse)
library(here)
library(dplyr)
library(phenomix)

#readin file
d <- read.csv(here("data",
                   "dungeness","dungeness_aggregated_w_trap_efficiency.csv"),header=TRUE)

datalist_chinook0_1_t = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
                                    min_number=0, 
                                    variable = "chinook0_wild_num",
                                    time="year", 
                                    date = "doy",
                                    family = "negbin",
                                    asymmetric_model = TRUE, 
                                    # mu = ~ nyear,
                                    # sigma = ~ nyear,
                                    # covar_data = cov_dat,
                                    est_sigma_re = FALSE,
                                    est_mu_re = FALSE,
                                    tail_model = "student_t")


set.seed(1)
fitted_year_chinook0_1_t = fit(datalist_chinook0_1_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))
fitted_year_chinook0_1_t$pars$convergence


g = plot_diagnostics(fitted_year_chinook0_1_t, type="timing", logspace=TRUE)
g

means <- extract_means(fitted_year_chinook0_1_t)
upper <- extract_upper(fitted_year_chinook0_1_t)
lower <- extract_lower(fitted_year_chinook0_1_t)
upper$value-lower$value

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
                                    est_sigma_re = TRUE,
                                    est_mu_re = TRUE,
                                    tail_model = "student_t")
#sigma = TRUE and mu = FALSE is not working. I am setting both to TRUE
set.seed(1)
fitted_year_chinook0_1_t = fit(datalist_chinook0_1_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))
fitted_year_chinook0_1_t$pars$convergence

means <- extract_means(fitted_year_chinook0_1_t)
upper <- extract_upper(fitted_year_chinook0_1_t)
lower <- extract_lower(fitted_year_chinook0_1_t)
upper$value-lower$value

>>>>>>> d66512284322217344f7020d9952df15aa0b130b
