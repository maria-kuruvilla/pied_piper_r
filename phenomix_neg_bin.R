# goal - to try Eric's code for all species/ages


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

#plotting chinook  subyearlings
plot(d$doy,d$chinook0_wild_num, xlim = c(10,250))

#migration is bimodal. cut off point is probably around doy = 120

dplyr::filter(d, !is.na(chinook0_wild_num) & doy < 120)

#looking at just chinook0 subyearlins that migrate in early spring
datalist = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy < 120),
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

p <- plot_diagnostics(fitted, type="timing", logspace=TRUE)
p   


#looking at chinook0 subyearlins that migrate in early summer/late spring
datalist = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
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

p2 <- plot_diagnostics(fitted, type="timing", logspace=TRUE)
p2   


#trying with covariate data

cov_dat = data.frame(nyear = unique(d$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear) 

datalist = create_data(dplyr::filter(d, !is.na(chinook0_wild_num) & doy >= 120),
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
fitted_year = fit(datalist,
             control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

extract_means(fitted_year) # this works!
p_year <- plot_diagnostics(fitted_year, type="timing", logspace=TRUE)
p_year <- p_year + ggtitle("Chinook subyearlings - Late spring")
p_year
ggsave(here("..","..","..","Onedrive","Documents","output","pied_piper",
            "dungeness","phenomix_chinook0_late.jpeg"), p_year, width = 10, height = 10, units = "in")
