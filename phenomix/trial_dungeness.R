#trying phenomix with est_mu and est_sigma = TRUE

# looking at relationship between ATU and peak of migration

#load packages
library(ggplot2)
library(tidyverse)
library(here)
library(dplyr)
library(phenomix)

plot_diagnostics <- function(fitted, type = "timing", logspace = TRUE) {
  
  # rebuild data frame
  df <- predict(fitted)
  
  # join in mean
  mus <- data.frame(
    years = unique(df$years),
    mu = fitted$sdreport$value[which(names(fitted$sdreport$value) == "mu")]
  )
  df <- left_join(df, mus)
  df$timing <- as.factor(ifelse(df$x < df$mu, "pre", "post"))
  
  if (type == "scatter") {
    if (logspace == TRUE) {
      if(fitted$data_list$family %in% c(2,3,5)) {
        g <- ggplot(df, aes(pred, log(y), fill = timing, col = timing)) +
          geom_point(alpha = 0.5) +
          facet_wrap(~years, labeller = labeller(years = 
                                                   c("1" = "2005",
                                                     "2" = "2006",
                                                     "3" = "2007",
                                                     "4" = "2008",
                                                     "5" = "2009",
                                                     "6" = "2010",
                                                     "7" = "2011",
                                                     "8" = "2012",
                                                     "9" = "2013",
                                                     "10" = "2014",
                                                     "11" = "2015",
                                                     "12" = "2016",
                                                     "13" = "2017",
                                                     "14" = "2018",
                                                     "15" = "2019",
                                                     "16" = "2020")), scales = "free") +
          geom_abline(intercept = 0, slope = 1) +
          xlab("Ln predicted") +
          ylab("Ln obs")
      }
      if(fitted$data_list$family %in% c(1)) {
        g <- ggplot(df, aes(log(pred), log(y), fill = timing, col = timing)) +
          geom_point(alpha = 0.5) +
          facet_wrap(~years, labeller = labeller(years = 
                                                   c("1" = "2005",
                                                     "2" = "2006",
                                                     "3" = "2007",
                                                     "4" = "2008",
                                                     "5" = "2009",
                                                     "6" = "2010",
                                                     "7" = "2011",
                                                     "8" = "2012",
                                                     "9" = "2013",
                                                     "10" = "2014",
                                                     "11" = "2015",
                                                     "12" = "2016",
                                                     "13" = "2017",
                                                     "14" = "2018",
                                                     "15" = "2019",
                                                     "16" = "2020")), scales = "free") +
          geom_abline(intercept = 0, slope = 1) +
          xlab("Ln predicted") +
          ylab("Ln obs")
      }
    } else {
      if(fitted$data_list$family %in% c(2,3,5)) {
        g <- ggplot(df, aes(exp(pred), y, fill = timing, col = timing)) +
          geom_point(alpha = 0.5) +
          facet_wrap(~years, labeller = labeller(years = 
                                                   c("1" = "2005",
                                                     "2" = "2006",
                                                     "3" = "2007",
                                                     "4" = "2008",
                                                     "5" = "2009",
                                                     "6" = "2010",
                                                     "7" = "2011",
                                                     "8" = "2012",
                                                     "9" = "2013",
                                                     "10" = "2014",
                                                     "11" = "2015",
                                                     "12" = "2016",
                                                     "13" = "2017",
                                                     "14" = "2018",
                                                     "15" = "2019",
                                                     "16" = "2020")), scales = "free") +
          geom_abline(intercept = 0, slope = 1) +
          xlab("Ln predicted") +
          ylab("Ln obs")
      }
      if(fitted$data_list$family %in% c(1)) {
        g <- ggplot(df, aes(pred, y, fill = timing, col = timing)) +
          geom_point(alpha = 0.5) +
          facet_wrap(~years, labeller = labeller(years = 
                                                   c("1" = "2005",
                                                     "2" = "2006",
                                                     "3" = "2007",
                                                     "4" = "2008",
                                                     "5" = "2009",
                                                     "6" = "2010",
                                                     "7" = "2011",
                                                     "8" = "2012",
                                                     "9" = "2013",
                                                     "10" = "2014",
                                                     "11" = "2015",
                                                     "12" = "2016",
                                                     "13" = "2017",
                                                     "14" = "2018",
                                                     "15" = "2019",
                                                     "16" = "2020")), scales = "free") +
          geom_abline(intercept = 0, slope = 1) +
          xlab("Predicted") +
          ylab("Obs")
      }
    }
  }
  if (type == "timing") {
    if (logspace == TRUE) {
      if(fitted$data_list$family %in% c(2,3,5)) {
        g <- ggplot(df, aes(x, pred, fill = timing, col = timing)) +
          facet_wrap(~years, labeller = labeller(years = 
                                                   c("1" = "2005",
                                                     "2" = "2006",
                                                     "3" = "2007",
                                                     "4" = "2008",
                                                     "5" = "2009",
                                                     "6" = "2010",
                                                     "7" = "2011",
                                                     "8" = "2012",
                                                     "9" = "2013",
                                                     "10" = "2014",
                                                     "11" = "2015",
                                                     "12" = "2016",
                                                     "13" = "2017",
                                                     "14" = "2018",
                                                     "15" = "2019",
                                                     "16" = "2020")), scales = "free") +
          xlab("Calendar day") +
          ylab("Ln pred and obs") +
          geom_point(aes(x, log(y), fill = timing, col = timing), size = 1, alpha = 0.5) +
          geom_line(col = "black")
      }
      if(fitted$data_list$family %in% c(1)) {
        g <- ggplot(df, aes(x, log(pred), fill = timing, col = timing)) +
          facet_wrap(~years, labeller = labeller(years = 
                                                   c("1" = "2005",
                                                     "2" = "2006",
                                                     "3" = "2007",
                                                     "4" = "2008",
                                                     "5" = "2009",
                                                     "6" = "2010",
                                                     "7" = "2011",
                                                     "8" = "2012",
                                                     "9" = "2013",
                                                     "10" = "2014",
                                                     "11" = "2015",
                                                     "12" = "2016",
                                                     "13" = "2017",
                                                     "14" = "2018",
                                                     "15" = "2019",
                                                     "16" = "2020")), scales = "free") +
          xlab("Calendar day") +
          ylab("Ln pred and obs") +
          geom_point(aes(x, log(y), fill = timing, col = timing), size = 1, alpha = 0.5) +
          geom_line(col = "black")
      }
    } else {
      if(fitted$data_list$family %in% c(2,3,5)) {
        g <- ggplot(df, aes(x, exp(pred), fill = timing, col = timing)) +
          facet_wrap(~years, labeller = labeller(years = 
                                                   c("1" = "2005",
                                                     "2" = "2006",
                                                     "3" = "2007",
                                                     "4" = "2008",
                                                     "5" = "2009",
                                                     "6" = "2010",
                                                     "7" = "2011",
                                                     "8" = "2012",
                                                     "9" = "2013",
                                                     "10" = "2014",
                                                     "11" = "2015",
                                                     "12" = "2016",
                                                     "13" = "2017",
                                                     "14" = "2018",
                                                     "15" = "2019",
                                                     "16" = "2020")), scales = "free") +
          xlab("Calendar day") +
          ylab("Ln pred and obs") +
          geom_point(aes(x, y, fill = timing, col = timing), size = 1, alpha = 0.5) +
          geom_line(col = "black")
      }
      if(fitted$data_list$family %in% c(1)) {
        g <- ggplot(df, aes(x, pred, fill = timing, col = timing)) +
          facet_wrap(~years, labeller = labeller(years = 
                                                   c("1" = "2005",
                                                     "2" = "2006",
                                                     "3" = "2007",
                                                     "4" = "2008",
                                                     "5" = "2009",
                                                     "6" = "2010",
                                                     "7" = "2011",
                                                     "8" = "2012",
                                                     "9" = "2013",
                                                     "10" = "2014",
                                                     "11" = "2015",
                                                     "12" = "2016",
                                                     "13" = "2017",
                                                     "14" = "2018",
                                                     "15" = "2019",
                                                     "16" = "2020")), scales = "free") +
          xlab("Calendar day") +
          ylab("Ln pred and obs") +
          geom_point(aes(x, y, fill = timing, col = timing), size = 1, alpha = 0.5) +
          geom_line(col = "black")
      }
    }
  }
  return(g)
}


#readin file
d <- read.csv(here("data",
                   "dungeness","dungeness_aggregated_w_trap_efficiency.csv"),header=TRUE)


dungeness_chinook0 = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
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
fitted_dungeness_chinook0 = fit(dungeness_chinook0,
                                control = list(eval.max = 5000, 
                                               iter.max = 2000, 
                                               rel.tol = 1e-8))
fitted_dungeness_chinook0$pars$convergence

dungeness_chinook0_means <- extract_means(fitted_dungeness_chinook0)
dungeness_chinook0_means <- dungeness_chinook0_means %>% 
  mutate(year = rownames(dungeness_chinook0_means))

dungeness_chinoook0_upper <- extract_upper(fitted_dungeness_chinook0) %>% 
  mutate(year = rownames(extract_upper(fitted_dungeness_chinook0)))

dungeness_chinook0_lower <- extract_lower(fitted_dungeness_chinook0) %>% 
  mutate(year = rownames(extract_lower(fitted_dungeness_chinook0)))

dungeness_chinook0_means <- rbind(dungeness_chinook0_means,
                                  dungeness_chinoook0_upper,
                                  dungeness_chinook0_lower)

ggplot(dungeness_chinook0_means %>% 
         filter(par == 'mu')) +
  geom_point(aes(x=as.numeric(year), y=value))+
  geom_smooth(aes(x=as.numeric(year), y=value), method = 'lm')

#drop year 2015
atu_150 <- d %>% 
  filter(doy == 150) %>% 
  select(atu_april,year) %>% 
  mutate(year = as.numeric(year - 2004))

#merge the atu file with means_chinook0 on year

means_chinook0_atu <- merge(atu_150, dungeness_chinook0_means, by="year", all.x = TRUE)


ggplot(means_chinook0_atu %>% 
         filter(par == 'upper75'),aes(x=atu_april, y=value)) +
  geom_point()+
  geom_smooth(method = 'lm')

plot_diagnostics(fitted_dungeness_chinook0)+theme_classic()+
  theme(strip.background = element_blank())

ggsave(here("phenomix_output","dungeness_chinook0.png"), 
       width = 10, height = 10, units = "in")


#### 

# in d, make new column for cumulative sum of chinook0_hatchery_num 
#for each year
#ignore NA values

d <- d %>% 
  group_by(year) %>% 
  mutate(chinook0_hatchery_num_cumsum = cumsum(
    ifelse(is.na(chinook0_hatchery_num), 0, chinook0_hatchery_num)),
    flow_cumsum = cumsum(
      ifelse(is.na(flow), 0, flow))) %>% 
  ungroup()

hatchery_150 <- d %>% 
  filter(doy == 150) %>% 
  select(chinook0_hatchery_num_cumsum,year) %>% 
  mutate(year = as.numeric(year - 2004)) %>%
  rename(chinook0_hatchery_num_cumsum_150 = chinook0_hatchery_num_cumsum)

hatchery_200 <- d %>% 
  filter(doy == 200) %>% 
  select(chinook0_hatchery_num_cumsum,year) %>% 
  mutate(year = as.numeric(year - 2004)) %>% 
  rename(chinook0_hatchery_num_cumsum_200 = chinook0_hatchery_num_cumsum)

flow_150 <- d %>% 
  filter(doy == 150) %>% 
  select(flow_cumsum,year) %>% 
  mutate(year = as.numeric(year - 2004)) %>% 
  rename(flow_cumsum_150 = flow_cumsum)

flow_100 <- d %>% 
  filter(doy == 100) %>% 
  select(flow_cumsum,year) %>% 
  mutate(year = as.numeric(year - 2004)) %>% 
  rename(flow_cumsum_100 = flow_cumsum)


means_chinook0_hatchery <- merge(hatchery_150, means_chinook0_atu, by="year", all.x = TRUE)

means_chinook0_hatchery

ggplot(means_chinook0_hatchery %>% 
         filter(par == 'upper75'),aes(x=chinook0_hatchery_num_cumsum, y=value)) +
  geom_point()+
  geom_smooth(method = 'lm')

#calculate the difference between the upper75 and lower25 and call it range

dungeness_chinook0_means_wide <- dungeness_chinook0_means %>% 
  pivot_wider(names_from = par, 
              values_from = c(value,sd)) %>% 
  unnest() %>% 
  mutate(range = value_upper75 - value_lower25) %>% 
  merge(hatchery_150, ., by="year", all.x = TRUE) %>% 
  merge(atu_150, ., by="year", all.x = TRUE) %>%
  merge(hatchery_200, ., by="year", all.x = TRUE) %>%
  merge(flow_150, ., by="year", all.x = TRUE) %>%
  merge(flow_100, ., by="year", all.x = TRUE) %>%
  mutate(flow_100_150 = flow_cumsum_150 - flow_cumsum_100)


#plot the range against the cumulative sum of hatchery fish on doy 200

ggplot(dungeness_chinook0_means_wide, aes(x=chinook0_hatchery_num_cumsum_200, y=range)) +
  geom_point()+
  geom_smooth(method = 'lm')

#plot the mean against the cumulative sum of hatchery flow of 150

ggplot(dungeness_chinook0_means_wide, aes(x=flow_100_150, y=value_mu)) +
  geom_point()+
  geom_smooth(method = 'lm')



#make linear model with range as response 
#and cumulative sum of hatchery fish on doy 200 and atu_april as predictor


model <- lm(range ~ chinook0_hatchery_num_cumsum_200 + atu_april, 
            data = dungeness_chinook0_means_wide)
summary(model)


#plot atu and mean

ggplot(dungeness_chinook0_means_wide, aes(x=atu_april, y=value_mu)) +
  geom_point(size = 4, alpha = 0.5)+
  geom_smooth(method = 'lm', color = "black")+
  theme_classic()+
  labs(x = "ATU on Day 150", y = "Peak day of migration")+
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))
ggsave(here("phenomix_output","dungeness_chinook0_parr_atu.png"), 
       width = 12, height = 8, units = "in")

#fry migration

dungeness_chinook0_fry = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy < 120),
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
fitted_dungeness_chinook0_fry = fit(dungeness_chinook0_fry,
                                control = list(eval.max = 5000, 
                                               iter.max = 2000, 
                                               rel.tol = 1e-8))
fitted_dungeness_chinook0_fry$pars$convergence
#did not converge

dungeness_chinook0_fry = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy < 120),
                                     min_number=0,
                                     variable = "chinook0_wild_num",
                                     time="year",
                                     date = "doy",
                                     family = "negbin",
                                     asymmetric_model = TRUE,
                                     est_sigma_re = TRUE,
                                     est_mu_re = TRUE,
                                     tail_model = "student_t")
set.seed(123)
fitted_dungeness_chinook0_fry = fit(dungeness_chinook0_fry,
                                    control = list(eval.max = 5000, 
                                                   iter.max = 2000, 
                                                   rel.tol = 1e-8))
fitted_dungeness_chinook0_fry$pars$convergence


dungeness_chinook0_fry = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy < 120),
                                     min_number=0,
                                     variable = "chinook0_wild_num",
                                     time="year",
                                     date = "doy",
                                     family = "negbin",
                                     asymmetric_model = TRUE,
                                     est_sigma_re = TRUE,
                                     est_mu_re = TRUE,
                                     tail_model = "gnorm")
set.seed(1)
fitted_dungeness_chinook0_fry = fit(dungeness_chinook0_fry,
                                    control = list(eval.max = 5000, 
                                                   iter.max = 2000, 
                                                   rel.tol = 1e-8))
fitted_dungeness_chinook0_fry$pars$convergence

dungeness_chinook0_fry = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy < 120),
                                     min_number=0,
                                     variable = "chinook0_wild_num",
                                     time="year",
                                     date = "doy",
                                     family = "lognormal",
                                     asymmetric_model = TRUE,
                                     est_sigma_re = TRUE,
                                     est_mu_re = TRUE,
                                     tail_model = "student_t")
set.seed(1)
fitted_dungeness_chinook0_fry = fit(dungeness_chinook0_fry,
                                    control = list(eval.max = 5000, 
                                                   iter.max = 2000, 
                                                   rel.tol = 1e-8))
fitted_dungeness_chinook0_fry$pars$convergence

dungeness_chinook0_fry = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy < 120),
                                     min_number=0,
                                     variable = "chinook0_wild_num",
                                     time="year",
                                     date = "doy",
                                     family = "lognormal",
                                     asymmetric_model = TRUE,
                                     est_sigma_re = TRUE,
                                     est_mu_re = TRUE,
                                     tail_model = "student_t")
set.seed(1)
fitted_dungeness_chinook0_fry = fit(dungeness_chinook0_fry,
                                    control = list(eval.max = 5000, 
                                                   iter.max = 2000, 
                                                   rel.tol = 1e-8))
fitted_dungeness_chinook0_fry$pars$convergence


cov_dat = data.frame(nyear = unique(d$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 


dungeness_chinook0_fry = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy < 120),
                                     min_number=0,
                                     variable = "chinook0_wild_num",
                                     time="year",
                                     date = "doy",
                                     family = "negbin",
                                     mu = ~ nyear,
                                     sigma = ~ nyear,
                                     covar_data = cov_dat,
                                     asymmetric_model = TRUE,
                                     est_sigma_re = TRUE,
                                     est_mu_re = TRUE,
                                     tail_model = "student_t")
set.seed(123)
fitted_dungeness_chinook0_fry = fit(dungeness_chinook0_fry,
                                    control = list(eval.max = 5000, 
                                                   iter.max = 2000, 
                                                   rel.tol = 1e-8))
fitted_dungeness_chinook0_fry$pars$convergence

dungeness_chinook0_fry = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy < 120),
                                     min_number=0,
                                     variable = "chinook0_wild_num",
                                     time="year",
                                     date = "doy",
                                     family = "negbin",
                                     mu = ~ nyear,
                                     sigma = ~ nyear,
                                     covar_data = cov_dat,
                                     asymmetric_model = TRUE,
                                     est_sigma_re = FALSE,
                                     est_mu_re = TRUE,
                                     tail_model = "student_t")
set.seed(123)
fitted_dungeness_chinook0_fry = fit(dungeness_chinook0_fry,
                                    control = list(eval.max = 5000, 
                                                   iter.max = 2000, 
                                                   rel.tol = 1e-8))
fitted_dungeness_chinook0_fry$pars$convergence
#converges only when est_sigma_re = FALSE

extract_means(fitted_dungeness_chinook0_fry)
extract_upper(fitted_dungeness_chinook0_fry)
extract_lower(fitted_dungeness_chinook0_fry)

plot_diagnostics(fitted_dungeness_chinook0_fry)

extract_upper(fitted_dungeness_chinook0_fry)$value - extract_lower(fitted_dungeness_chinook0_fry)$value

dungeness_chinook0_fry = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy < 120),
                                     min_number=0,
                                     variable = "chinook0_wild_num",
                                     time="year",
                                     date = "doy",
                                     family = "negbin",
                                     asymmetric_model = TRUE,
                                     est_sigma_re = FALSE,
                                     est_mu_re = TRUE,
                                     tail_model = "student_t")
set.seed(123)
fitted_dungeness_chinook0_fry = fit(dungeness_chinook0_fry,
                                    control = list(eval.max = 5000, 
                                                   iter.max = 2000, 
                                                   rel.tol = 1e-8))
fitted_dungeness_chinook0_fry$pars$convergence
#converges only when est_sigma_re = FALSE

extract_means(fitted_dungeness_chinook0_fry)
extract_upper(fitted_dungeness_chinook0_fry)
extract_lower(fitted_dungeness_chinook0_fry)

plot_diagnostics(fitted_dungeness_chinook0_fry)+theme_classic()+
  theme(strip.background = element_blank())

ggsave(here("phenomix_output", "dungeness_chinook0_fry.png"), width = 10, height = 10)

extract_upper(fitted_dungeness_chinook0_fry)$value - extract_lower(fitted_dungeness_chinook0_fry)$value
#if I do not put covariate data, it estimates the 
#same value for range

dungeness_chinook0_fry_means <- rbind(extract_means(fitted_dungeness_chinook0_fry),
                                      extract_upper(fitted_dungeness_chinook0_fry),
                                      extract_lower(fitted_dungeness_chinook0_fry))

#Ido nto have atu data fro doy=60
#maybe it doesn't make sense to do this for fry, but I also do not have it for
#chum and pink
#maybe just temperature?


dungeness_chinook0_fry_means_wide <- dungeness_chinook0_fry_means %>% 
  pivot_wider(names_from = par, 
              values_from = c(value,sd)) %>% 
  unnest()


#chum

dungeness_chum0 = create_data(dplyr::filter(d, !is.na(chum0_wild_num)),
                                     min_number=0,
                                     variable = "chum0_wild_num",
                                     time="year",
                                     date = "doy",
                                     family = "negbin",
                                     asymmetric_model = TRUE,
                                     est_sigma_re = TRUE,
                                     est_mu_re = TRUE,
                                     tail_model = "gaussian")
set.seed(123)
fitted_dungeness_chum0 = fit(dungeness_chum0,
                                    control = list(eval.max = 5000, 
                                                   iter.max = 2000, 
                                                   rel.tol = 1e-8))
#failed

dungeness_chum0 = create_data(dplyr::filter(d, !is.na(chum0_wild_num)),
                              min_number=0,
                              variable = "chum0_wild_num",
                              time="year",
                              date = "doy",
                              family = "negbin",
                              asymmetric_model = TRUE,
                              est_sigma_re = TRUE,
                              est_mu_re = TRUE,
                              tail_model = "student_t")
set.seed(123)
fitted_dungeness_chum0 = fit(dungeness_chum0,
                             control = list(eval.max = 5000, 
                                            iter.max = 2000, 
                                            rel.tol = 1e-8))
#failed

dungeness_chum0 = create_data(dplyr::filter(d, !is.na(chum0_wild_num)),
                              min_number=0,
                              variable = "chum0_wild_num",
                              time="year",
                              date = "doy",
                              family = "negbin",
                              mu = ~ nyear,
                              sigma = ~ nyear,
                              covar_data = cov_dat,
                              asymmetric_model = TRUE,
                              est_sigma_re = TRUE,
                              est_mu_re = TRUE,
                              tail_model = "gnorm")

set.seed(123)
fitted_dungeness_chum0 = fit(dungeness_chum0,
                             control = list(eval.max = 5000, 
                                            iter.max = 2000, 
                                            rel.tol = 1e-8))
#failed



dungeness_chum0 = create_data(dplyr::filter(d, !is.na(chum0_wild_num)),
                              min_number=0,
                              variable = "chum0_wild_num",
                              time="year",
                              date = "doy",
                              family = "negbin",
                              mu = ~ nyear,
                              sigma = ~ nyear,
                              covar_data = cov_dat,
                              asymmetric_model = TRUE,
                              est_sigma_re = FALSE,
                              est_mu_re = TRUE,
                              tail_model = "gaussian")

set.seed(123)
fitted_dungeness_chum0 = fit(dungeness_chum0,
                             control = list(eval.max = 5000, 
                                            iter.max = 2000, 
                                            rel.tol = 1e-8))
fitted_dungeness_chum0$pars$convergence


plot_diagnostics(fitted_dungeness_chum0)+theme_classic()+
  theme(strip.background = element_blank())

extract_means(fitted_dungeness_chum0)
extract_upper(fitted_dungeness_chum0)
extract_lower(fitted_dungeness_chum0)

extract_upper(fitted_dungeness_chum0)$value - extract_lower(fitted_dungeness_chum0)$value



dungeness_chum0 = create_data(dplyr::filter(d, !is.na(chum0_wild_num)),
                              min_number=0,
                              variable = "chum0_wild_num",
                              time="year",
                              date = "doy",
                              family = "negbin",
                              asymmetric_model = TRUE,
                              est_sigma_re = FALSE,
                              est_mu_re = TRUE,
                              tail_model = "gaussian")
set.seed(123)
fitted_dungeness_chum0 = fit(dungeness_chum0,
                             control = list(eval.max = 5000, 
                                            iter.max = 2000, 
                                            rel.tol = 1e-8))
fitted_dungeness_chum0$pars$convergence


plot_diagnostics(fitted_dungeness_chum0)+theme_classic()+
  theme(strip.background = element_blank())


dungeness_chum0 = create_data(dplyr::filter(d, !is.na(chum0_wild_num) & doy < 150),
                              min_number=0,
                              variable = "chum0_wild_num",
                              time="year",
                              date = "doy",
                              family = "negbin",
                              mu = ~ nyear,
                              sigma = ~ nyear,
                              covar_data = cov_dat,
                              asymmetric_model = TRUE,
                              est_sigma_re = FALSE,
                              est_mu_re = TRUE,
                              tail_model = "gaussian")

set.seed(123)
fitted_dungeness_chum0 = fit(dungeness_chum0,
                             control = list(eval.max = 5000, 
                                            iter.max = 2000, 
                                            rel.tol = 1e-8))
fitted_dungeness_chum0$pars$convergence


plot_diagnostics(fitted_dungeness_chum0)+theme_classic()+
  theme(strip.background = element_blank())
ggsave(here("phenomix_output","dungeness_chum0.png"),width=10,height=10)



dungeness_pink0 = create_data(dplyr::filter(d, !is.na(pink0_wild_num) & doy < 150),
                              min_number=0,
                              variable = "pink0_wild_num",
                              time="year",
                              date = "doy",
                              family = "negbin",
                              mu = ~ nyear,
                              sigma = ~ nyear,
                              covar_data = cov_dat,
                              asymmetric_model = TRUE,
                              est_sigma_re = TRUE,
                              est_mu_re = TRUE,
                              tail_model = "student_t")

set.seed(123)
fitted_dungeness_pink0 = fit(dungeness_pink0,
                             control = list(eval.max = 5000, 
                                            iter.max = 2000, 
                                            rel.tol = 1e-8))

dungeness_pink0 = create_data(dplyr::filter(d, !is.na(pink0_wild_num) & doy < 150),
                              min_number=0,
                              variable = "pink0_wild_num",
                              time="year",
                              date = "doy",
                              family = "negbin",
                              mu = ~ nyear,
                              sigma = ~ nyear,
                              covar_data = cov_dat,
                              asymmetric_model = TRUE,
                              est_sigma_re = TRUE,
                              est_mu_re = TRUE,
                              tail_model = "gaussian")

set.seed(123)
fitted_dungeness_pink0 = fit(dungeness_pink0,
                             control = list(eval.max = 5000, 
                                            iter.max = 2000, 
                                            rel.tol = 1e-8))

fitted_dungeness_pink0$pars$convergence


dungeness_pink0 = create_data(dplyr::filter(d, !is.na(pink0_wild_num) & doy < 150),
                              min_number=0,
                              variable = "pink0_wild_num",
                              time="year",
                              date = "doy",
                              family = "negbin",
                              mu = ~ nyear,
                              sigma = ~ nyear,
                              covar_data = cov_dat,
                              asymmetric_model = TRUE,
                              est_sigma_re = FALSE,
                              est_mu_re = TRUE,
                              tail_model = "student_t")


set.seed(123)
fitted_dungeness_pink0 = fit(dungeness_pink0,
                             control = list(eval.max = 5000, 
                                            iter.max = 2000, 
                                            rel.tol = 1e-8))

fitted_dungeness_pink0$pars$convergence


plot_diagnostics(fitted_dungeness_pink0)+theme_classic()+
  theme(strip.background = element_blank())
ggsave(here("phenomix_output","dungeness_pink0.png"),width=10,height=10)


dungeness_pink0 = create_data(dplyr::filter(d, !is.na(pink0_wild_num) & doy < 150 & year%%2 == 0),
                              min_number=0,
                              variable = "pink0_wild_num",
                              time="year",
                              date = "doy",
                              family = "negbin",
                              asymmetric_model = TRUE,
                              est_sigma_re = TRUE,
                              est_mu_re = TRUE,
                              tail_model = "student_t")

set.seed(123)

fitted_dungeness_pink0 = fit(dungeness_pink0,
                             control = list(eval.max = 5000, 
                                            iter.max = 2000, 
                                            rel.tol = 1e-8))

dungeness_pink0 = create_data(dplyr::filter(d, !is.na(pink0_wild_num) & doy < 150 & year%%2 == 0),
                              min_number=0,
                              variable = "pink0_wild_num",
                              time="year",
                              date = "doy",
                              family = "negbin",
                              asymmetric_model = TRUE,
                              est_sigma_re = FALSE,
                              est_mu_re = TRUE,
                              tail_model = "student_t")

set.seed(123)

fitted_dungeness_pink0 = fit(dungeness_pink0,
                             control = list(eval.max = 5000, 
                                            iter.max = 2000, 
                                            rel.tol = 1e-8))

fitted_dungeness_pink0$pars$convergence
years <- c("2006","2008","2010","2012","2014","2016","2018","2020")
plot_diagnostics(fitted_dungeness_pink0)+theme_classic()+
  theme(strip.background = element_blank())
ggsave(here("phenomix_output","dungeness_pink0.png"),width=10,height=10)


df <- predict(fitted_dungeness_pink0)

# join in mean
mus <- data.frame(
  years = unique(df$years),
  mu = fitted$sdreport$value[which(names(fitted$sdreport$value) == "mu")]
)
df <- left_join(df, mus)
df$timing <- as.factor(ifelse(df$x < df$mu, "pre", "post"))



g <- ggplot(df, aes(x, pred, fill = timing, col = timing)) +
  facet_wrap(~years, scales = "free") +
  xlab("Calendar day") +
  ylab("Ln pred and obs") +
  geom_point(aes(x, log(y), fill = timing, col = timing), size = 1, alpha = 0.5) +
  geom_line(col = "black")

g

