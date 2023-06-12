# goal - use best model from model selection for each species and 
# extract means and then plot it for each year

#tried using per hour and changing the doy but decided against it and stuck with
#what I tried for model selection code




library(ggplot2)
library(phenomix)
library(dplyr)
library(TMB)
library(here)
library(patchwork)

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
d <- read.csv(here("..","..","..","Onedrive","Documents","data","pied_piper",
                   "dungeness","dungeness_subset.csv"),header=TRUE)
cov_dat = data.frame(nyear = unique(d$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 


set.seed(1)
datalist_chinook0_0_t = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy < 120),
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


fitted_year_chinook0_0_t = fit(datalist_chinook0_0_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chinook0_0_t$pars$convergence #converged

chinook0_0_means <- extract_means(fitted_year_chinook0_0_t)

plot_diagnostics(fitted_year_chinook0_0_t)


chinook0_0_means$ year <- 2005:2020

p_chinook0_0 <- ggplot(data = chinook0_0_means) + 
  geom_point(aes(year,value), size = 3, color = "black", alpha = 0.6) + 
  labs(x = "Year", y = "Day of year at Peak migration", title = "Chinook subyearlings - spring") + 
  theme_minimal() +
  theme(axis.text = element_text(size = 14),axis.title = element_text(size = 16),
        plot.title = element_text(size = 20)) + 
  ylim(30,130)

ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_chinook0_early_means.jpeg"), 
       p_chinook0_0, width = 12, height = 10, units = "in")



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

fitted_year_chinook0_1_t$pars$convergence #converged


chinook0_1_means <- extract_means(fitted_year_chinook0_1_t)


chinook0_1_means$year <- 2005:2020


p_chinook0_1 <- ggplot(data = chinook0_1_means) + 
  geom_point(aes(year,value), size = 3, color = "black", alpha = 0.6) + 
  labs(x = "Year", y = "Day of year at Peak migration", title = "Chinook subyearlings - summer") + 
  theme_minimal() +
  theme(axis.text = element_text(size = 14),axis.title = element_text(size = 16),
        plot.title = element_text(size = 20)) + 
  ylim(140,210)


ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_chinook0_late_means.jpeg"), 
       p_chinook0_1, width = 12, height = 10, units = "in")





datalist_coho1_t = create_data(dplyr::filter(d, !is.na(coho1_wild_num)),
                               min_number=0, 
                               variable = "coho1_wild_num",
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
fitted_year_coho1_t = fit(datalist_coho1_t,
                          control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_coho1_t$pars$convergence 

coho1_means <- extract_means(fitted_year_coho1_t)


coho1_means$year <- 2005:2020


p_coho1 <- ggplot(data = coho1_means) + 
  geom_point(aes(year,value), size = 3, color = "black", alpha = 0.6) + 
  labs(x = "Year", y = "Day of year at Peak migration", title = "Coho yearlings") + 
  theme_minimal() +
  theme(axis.text = element_text(size = 14),axis.title = element_text(size = 16),
        plot.title = element_text(size = 20)) + 
  ylim(110,160)


ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_coho1_means.jpeg"), 
       p_coho1, width = 12, height = 10, units = "in")


######### steelhead

##########

datalist_steel_t = create_data(dplyr::filter(d, !is.na(steelheadsmolt_wild_num)),
                               min_number=0, 
                               variable = "steelheadsmolt_wild_num",
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
fitted_year_steel_t = fit(datalist_steel_t,
                          control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_steel_t$pars$convergence 


steelhead_means <- extract_means(fitted_year_steel_t)


steelhead_means$year <- 2005:2020


p_steel <- ggplot(data = steelhead_means) + 
  geom_point(aes(year,value), size = 3, color = "black", alpha = 0.6) + 
  labs(x = "Year", y = "Day of year at Peak migration", title = "Steelhead smolts") + 
  theme_minimal() +
  theme(axis.text = element_text(size = 14),axis.title = element_text(size = 16),
        plot.title = element_text(size = 20)) + 
  ylim(125,155)


ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_steelheadsmolts_means.jpeg"), 
       p_steel, width = 12, height = 10, units = "in")

#############


### chum



########
datalist_chum0_t = create_data(dplyr::filter(d, !is.na(chum0_wild_num)),
                               min_number=0, 
                               variable = "chum0_wild_num",
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
fitted_year_chum0_t = fit(datalist_chum0_t,
                          control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chum0_t$pars$convergence #did not converge

AIC(fitted_year_chum0_t) #17170

#t is better than gaussian and gnorm
###########
datalist_chum0_g = create_data(dplyr::filter(d, !is.na(chum0_wild_num)),
                               min_number=0, 
                               variable = "chum0_wild_num",
                               time="year", 
                               date = "doy",
                               family = "negbin",
                               asymmetric_model = TRUE, 
                               mu = ~ nyear,
                               sigma = ~ nyear,
                               covar_data = cov_dat,
                               est_sigma_re = FALSE,
                               est_mu_re = TRUE,
                               tail_model = "gaussian")

set.seed(1)
fitted_year_chum0_g = fit(datalist_chum0_g,
                          control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chum0_g$pars$convergence #converge

AIC(fitted_year_chum0_g) #17451

############
datalist_chum0_gnorm = create_data(dplyr::filter(d, !is.na(chum0_wild_num)),
                               min_number=0, 
                               variable = "chum0_wild_num",
                               time="year", 
                               date = "doy",
                               family = "negbin",
                               asymmetric_model = TRUE, 
                               mu = ~ nyear,
                               sigma = ~ nyear,
                               covar_data = cov_dat,
                               est_sigma_re = FALSE,
                               est_mu_re = TRUE,
                               tail_model = "gnorm")

set.seed(1)
fitted_year_chum0_gnorm = fit(datalist_chum0_gnorm,
                          control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chum0_gnorm$pars$convergence #did not converge

AIC(fitted_year_chum0_gnorm) #17170



#########

# chum
plot_chum <- plot_diagnostics(fitted_year_chum0_t, type = "timing",
                              logspace = TRUE) + ggtitle("Chum subyearlings") +
  ylim(c(0,10))

ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_chum0.jpeg"), plot_chum, width = 10, height = 10, units = "in")



chum0_means <- extract_means(fitted_year_chum0_t)


chum0_means$year <- 2005:2020


p_chum0 <- ggplot(data = chum0_means) + 
  geom_point(aes(year,value), size = 3, color = "black", alpha = 0.6) + 
  labs(x = "Year", y = "Day of year at Peak migration", title = "Chum subyearlings") + 
  theme_minimal() +
  theme(axis.text = element_text(size = 14),axis.title = element_text(size = 16),
        plot.title = element_text(size = 20)) + 
  ylim(70,120)

ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_chum0_means.jpeg"), 
       p_chum0, width = 12, height = 10, units = "in")


# 

datalist_pink0_t = create_data(dplyr::filter(d, !is.na(pink0_wild_num)),
                               min_number=0, 
                               variable = "pink0_wild_num",
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
fitted_year_pink0_t = fit(datalist_pink0_t,
                          control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_pink0_t$pars$convergence # did not work
AIC(fitted_year_pink0_t) #117451


datalist_pink0_gaussian = create_data(dplyr::filter(d, !is.na(pink0_wild_num)),
                                      min_number=0, 
                                      variable = "pink0_wild_num",
                                      time="year", 
                                      date = "doy",
                                      family = "negbin",
                                      asymmetric_model = TRUE, 
                                      mu = ~ nyear,
                                      sigma = ~ nyear,
                                      covar_data = cov_dat,
                                      est_sigma_re = FALSE,
                                      est_mu_re = TRUE,
                                      tail_model = "gaussian")

set.seed(2)
fitted_year_pink0_gaussian = fit(datalist_pink0_gaussian,
                                 control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_pink0_gaussian$pars$convergence # did not work



pink0_means <- extract_means(fitted_year_pink0_gaussian)


pink0_means$year <- 2005:2020


p_pink0 <- ggplot(data = pink0_means) + 
  geom_point(aes(year,value), size = 3, color = "black", alpha = 0.6) + 
  labs(x = "Year", y = "Day of year at Peak migration", title = "Pink subyearlings") + 
  theme_minimal() +
  theme(axis.text = element_text(size = 14),axis.title = element_text(size = 16),
        plot.title = element_text(size = 20)) + 
  ylim(40,120)

ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_pink0_means.jpeg"), 
       p_pink0, width = 12, height = 10, units = "in")



p <- (p_chinook0_0 + p_chinook0_1 + p_coho1) / (p_steel + p_chum0 + p_pink0)


ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_all_means.jpeg"), 
       p, width = 16, height = 12, units = "in")
