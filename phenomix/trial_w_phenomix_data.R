#trying phenomix with skagit data that is already
#in the package

#load packages
library(ggplot2)
library(tidyverse)
library(here)
library(dplyr)
library(phenomix)


glimpse(fishdist)

years <- unique(fishdist$year)
years


#plot the data

ggplot(fishdist, aes(x=doy, y=number)) +
  geom_point()+
  labs(x="Year", y="Salmon number") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~year)

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

sdrep_df = data.frame("par"=names(fitted$sdreport$value),
                      "value"=fitted$sdreport$value, "sd"=fitted$sdreport$sd)
head(sdrep_df)

pars(fitted)

extract_means(fitted)

upper <- extract_upper(fitted)
lower <- extract_lower(fitted)  

upper
lower

range <- upper$value-lower$value

range


#let me try to do this for skagit coho data

skagit <- read.csv(here("data",
                        "skagit","skagit_aggregated_w_trap_efficiency.csv"),header=TRUE)


glimpse(skagit)

skagit_agg <- skagit %>% 
  filter(daytime_category == 'night') %>% 
  group_by(year,doy) %>%
  summarise(number = sum(coho1_wild_num)
            ) %>%
  ungroup() %>%
  mutate(year = as.numeric(year))
  
skagit_df <- as.data.frame(skagit_agg)

#plot the data

ggplot(skagit_agg, aes(x=doy, y=number)) +
  geom_point()+
  labs(x="Year", y="Coho number") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~year)


cov_dat = data.frame(nyear = unique(skagit_agg$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

cov_dat$nyear_edited <- seq(0, 11, by = 1)

glimpse(skagit_agg)
unique(skagit_agg$year)
unique(skagit_agg$doy)
datalist_coho = create_data(filter(skagit_df, !is.na(number)), 
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
fitted = fit(datalist_coho)

datalist_coho2 = create_data(filter(skagit_df, !is.na(number)), 
                            min_number=0, 
                            variable = "number", 
                            time="year", 
                            date = "doy",
                            asymmetric_model = FALSE, 
                            mu = ~ nyear,
                            sigma = ~ nyear,
                            covar_data = cov_dat,
                            est_sigma_re = FALSE,
                            est_mu_re = FALSE,
                            tail_model = "gaussian")

set.seed(1)
fitted = fit(datalist_coho2)


datalist_coho3 = create_data(filter(skagit_df, !is.na(number)), 
                             min_number=0, 
                             variable = "number", 
                             time="year", 
                             date = "doy",
                             asymmetric_model = FALSE, 
                             mu = ~ nyear,
                             sigma = ~ nyear,
                             covar_data = cov_dat,
                             est_sigma_re = FALSE,
                             est_mu_re = FALSE,
                             tail_model = "gaussian",
                             family = "negbin")

set.seed(1)
fitted = fit(datalist_coho3)
#this worked



datalist_coho4 = create_data(filter(skagit_df, !is.na(number)), 
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
                             tail_model = "gaussian",
                             family = "negbin")

set.seed(1)
fitted = fit(datalist_coho4)
#this worked


fitted$pars$convergence

sdrep_df = data.frame("par"=names(fitted$sdreport$value),
                      "value"=fitted$sdreport$value, "sd"=fitted$sdreport$sd)
head(sdrep_df)

extract_means(fitted)

upper <- extract_upper(fitted)
lower <- extract_lower(fitted)
range <- upper$value-lower$value
range

plot_diagnostics(fitted)


#calculate total number of coho1_hatchery_num for each year

skagit_coho1_hatchery <- skagit %>% 
  group_by(year) %>%
  summarise(coho1_hatchery_total = sum(coho1_hatchery_num, na.rm = TRUE)
            ) %>%
  ungroup() %>%
  mutate(year = as.numeric(year))

skagit_coho1_hatchery



#plot the raw coho1_hatchery data

ggplot(skagit, aes(x=doy, y=coho1_hatchery_num)) +
  geom_point()+
  labs(x="Year", y="Coho hatchery number") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~year, scales = "free_y")


#add the upper, lower, range, and mean to the skagit_coho1_hatchery data frame

skagit_coho1_hatchery$upper <- upper$value
skagit_coho1_hatchery$lower <- lower$value
skagit_coho1_hatchery$range <- range
skagit_coho1_hatchery$mean <- extract_means(fitted)$value

skagit_coho1_hatchery

#plot the coho1_hatchery data with the upper, lower, range, and mean

ggplot(skagit_coho1_hatchery, aes(x=coho1_hatchery_total, y = range)) +
  geom_point()+
  # geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  labs(x="Hatchery", y="Range") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(skagit_coho1_hatchery, aes(x=coho1_hatchery_total, y = mean)) +
  geom_point()+
  # geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  labs(x="Hatchery", y="Peak migration doy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#negbin is important

