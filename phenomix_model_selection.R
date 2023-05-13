# try different families and different tail models and compare AIC
# to find best model that fits data for each species/age


library(ggplot2)
library(phenomix)
library(dplyr)
library(TMB)
library(here)


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

#chinook0 - late spring

#negbin and gaussian
#trying with covariate data

cov_dat = data.frame(nyear = unique(d$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

datalist_chinook0_1_gaussian = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
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
                       tail_model = "gaussian")

set.seed(1)
fitted_year_chinook0_1_gaussian = fit(datalist_chinook0_1_gaussian,
                  control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chinook0_1_gaussian$pars$convergence
AIC(fitted_year_chinook0_1_gaussian) #11681.82


#negbin and gnorm
datalist_chinook0_1_gnorm = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
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
                                           tail_model = "gnorm")

set.seed(1)
fitted_year_chinook0_1_gnorm = fit(datalist_chinook0_1_gnorm,
                                      control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chinook0_1_gnorm$pars$convergence #did not converge
AIC(fitted_year_chinook0_1_gnorm) #1886242


#negbin and student-t
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

fitted_year_chinook0_1_t$pars$convergence #did not converge
AIC(fitted_year_chinook0_1_t) #11589.68


#[possion and student-t
datalist_chinook0_1_poisson_t = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
                                    min_number=0, 
                                    variable = "chinook0_wild_num",
                                    time="year", 
                                    date = "doy",
                                    family = "poisson",
                                    asymmetric_model = TRUE, 
                                    mu = ~ nyear,
                                    sigma = ~ nyear,
                                    covar_data = cov_dat,
                                    est_sigma_re = FALSE,
                                    est_mu_re = TRUE,
                                    tail_model = "student_t")

set.seed(1)
fitted_year_chinook0_1_poisson_t = fit(datalist_chinook0_1_poisson_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chinook0_1_poisson_t$pars$convergence #did not converge
AIC(fitted_year_chinook0_1_poisson_t) #22594

#negbin and student-t for chinook 0 early spring 
#negbin and t is best
###### 
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

set.seed(1)
fitted_year_chinook0_0_t = fit(datalist_chinook0_0_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chinook0_0_t$pars$convergence #did not converge
AIC(fitted_year_chinook0_0_t) #7916


#negbin and gaussian
#######

datalist_chinook0_0_gaussian = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy < 120),
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
                                    tail_model = "gaussian")

set.seed(1)
fitted_year_chinook0_0_gaussian = fit(datalist_chinook0_0_gaussian,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chinook0_0_gaussian$pars$convergence #did not converge
AIC(fitted_year_chinook0_0_gaussian) #7969


########

#######
# chinook1 - very few fish

datalist_chinook1_t = create_data(dplyr::filter(d, !is.na(chinook1_wild_num)),
                                    min_number=0, 
                                    variable = "chinook1_wild_num",
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
fitted_year_chinook1_t = fit(datalist_chinook1_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chinook1_t$pars$convergence #did not converge
AIC(fitted_year_chinook1_t) #2243

plot_diagnostics(fitted_year_chinook1_t, type="timing", logspace=TRUE)

#######

# coho

# neg bin and t this is best


######

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
AIC(fitted_year_coho1_t) #11318.95

plot_diagnostics(fitted_year_coho1_t, type="timing", logspace=TRUE)

##########


#neg bin and guassian

#####
datalist_coho1_gaussian = create_data(dplyr::filter(d, !is.na(coho1_wild_num)),
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
                               tail_model = "gaussian")

set.seed(1)
fitted_year_coho1_gaussian = fit(datalist_coho1_gaussian,
                          control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_coho1_gaussian$pars$convergence 
AIC(fitted_year_coho1_gaussian) #11676.3

plot_diagnostics(fitted_year_coho1_gaussian, type="timing", logspace=TRUE)

#######



######


#steelhead


#####
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
AIC(fitted_year_steel_t) #6480

plot_diagnostics(fitted_year_steel_t, type="timing", logspace=TRUE)


##########


#gaussian


######

datalist_steel_gaussian = create_data(dplyr::filter(d, !is.na(steelheadsmolt_wild_num)),
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
                               tail_model = "gaussian")

set.seed(1)
fitted_year_steel_gaussian = fit(datalist_steel_gaussian,
                          control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_steel_gaussian$pars$convergence 
AIC(fitted_year_steel_gaussian) #6965

plot_diagnostics(fitted_year_steel_gaussian, type="timing", logspace=TRUE)

######

#chum

# t did not work. trying gaussian
#######

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
                                    tail_model = "gaussian")

set.seed(2)
fitted_year_chum0_t = fit(datalist_chum0_t,
                               control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_chum0_t$pars$convergence #did not converge
AIC(fitted_year_chum0_t) #117451

plot_diagnostics(fitted_year_chum0_t, type="timing", logspace=TRUE)


#######


#pink

#negbin and t


######

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

set.seed(2)
fitted_year_pink0_t = fit(datalist_pink0_t,
                          control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

fitted_year_pink0_t$pars$convergence # did not work
AIC(fitted_year_pink0_t) #117451

plot_diagnostics(fitted_year_pink0_t, type="timing", logspace=TRUE)

######


#gaussian

##########

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
AIC(fitted_year_pink0_gaussian) #12740

plot_pink <- plot_diagnostics(fitted_year_pink0_gaussian, type="timing", logspace=TRUE)

plot_pink + ylim(c(0,10))

#######


## plotting figures for all species

# chinook0 - 0

plot_chinook0_1 <- plot_diagnostics(fitted_year_chinook0_1_t, type = "timing",
                                    logspace = TRUE) + ggtitle("Chinook subyearlings - Late spring") +
  ylim(c(0,10))

ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_chinook0_late.jpeg"), plot_chinook0_1r, width = 10, height = 10, units = "in")



# chinook0 - 1

plot_chinook0_0 <- plot_diagnostics(fitted_year_chinook0_0_t, type = "timing",
                                    logspace = TRUE) + ggtitle("Chinook subyearlings - Early spring") +
  ylim(c(0,10))

ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_chinook0_early.jpeg"), plot_chinook0_0, width = 10, height = 10, units = "in")


# coho
plot_coho1 <- plot_diagnostics(fitted_year_coho1_t, type = "timing",
                                    logspace = TRUE) + ggtitle("Coho yearlings") +
  ylim(c(0,10))

ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_coho1.jpeg"), plot_coho1, width = 10, height = 10, units = "in")


#steelhead


plot_steel <- plot_diagnostics(fitted_year_steel_t, type = "timing",
                               logspace = TRUE) + ggtitle("Steelhead smolt") +
  ylim(c(0,10))

ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_steelheadsmolt.jpeg"), plot_steel, width = 10, height = 10, units = "in")


# chum
plot_chum <- plot_diagnostics(fitted_year_chum0_t, type = "timing",
                               logspace = TRUE) + ggtitle("Chum subyearlings") +
  ylim(c(0,10))

ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_chum0.jpeg"), plot_chum, width = 10, height = 10, units = "in")

# pink
plot_pink <- plot_diagnostics(fitted_year_pink0_gaussian, type = "timing",
                              logspace = TRUE) + ggtitle("Pink subyearlings") +
  ylim(c(0,10))

ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_pink0.jpeg"), plot_pink, width = 10, height = 10, units = "in")


