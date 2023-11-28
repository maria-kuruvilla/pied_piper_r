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
