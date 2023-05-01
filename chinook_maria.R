library(ggplot2)
library(phenomix)
library(dplyr)
library(TMB)
library(here)

 # = dplyr::filter(d, !is.na())

d <- read.csv(here("..","..","..","Onedrive","Documents","data","pied_piper","dungeness_2005-2020_combine_ma_msd_atu_photoperiod.csv"),header=TRUE)


# Chinook
datalist = create_data(dplyr::filter(d, !is.na(chinook0_hatchery_num)),
                       min_number=0,
                       variable = "chinook0_hatchery_num",
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
set.seed(1)
fitted = fit(datalist,
             control = list(eval.max = 5000, iter.max = 2000, rel.tol = 1e-8))

plot_diagnostics(fitted, type="timing", logspace=TRUE)

datalist = create_data(dplyr::filter(d, !is.na(chinook1_hatchery_num)),
                       min_number=0,
                       variable = "chinook1_hatchery_num",
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

datalist = create_data(dplyr::filter(d, !is.na(chinook1_wild_num)),
                       min_number=0,
                       variable = "chinook1_wild_num",
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

plot_diagnostics(fitted, type="timing", logspace=FALSE)


datalist = create_data(dplyr::filter(d, !is.na(chinook1_wild_perhour)),
                       min_number=0,
                       variable = "chinook1_wild_perhour",
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

plot_diagnostics(fitted, type="timing", logspace=FALSE)


datalist = create_data(dplyr::filter(d, !is.na(chum0_wild_perhour)),
                       min_number=0,
                       variable = "chum0_wild_perhour",
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

plot_diagnostics(fitted, type="timing", logspace=FALSE)




datalist = create_data(dplyr::filter(d, !is.na(pink0_wild_perhour)),
                       min_number=0,
                       variable = "pink0_wild_perhour",
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

plot_diagnostics(fitted, type="timing", logspace=FALSE)