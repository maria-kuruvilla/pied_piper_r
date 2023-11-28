#Goal - to produce fingures for paper
#first aggregate all the data for all rivers
#1 - plot daily catch data for each year and each river and each species for 
#hatchery and wild
#2 - plot daily environmental data for each year and each river

#load packages

library(ggplot2)
library(tidyverse)
library(here)


#readin file
dungeness <- read.csv(here("data",
                   "dungeness","dungeness_aggregated_w_trap_efficiency.csv"),
              header=TRUE)

dungeness$river <- "dungeness"

green_day <- read.csv(here("data",
                   "green","green_covariates_day.csv"),
              header=TRUE)
green_night <- read.csv(here("data",
                   "green","green_covariates_night.csv"),
              header=TRUE)

#aggregate day and night values

green_day$daytime_category <- 'day'

green_night$daytime_category <- 'night'

#sum all the values for each Date

green <- rbind(green_day, green_night) %>% 
  mutate(river = "green") %>%
  select(chinook0_hatchery_num, chinook0_wild_num,
         chinook1_hatchery_num, chinook1_wild_num,
         coho1_hatchery_num, coho1_wild_num,
         steelheadsmolt_hatchery_num, steelheadsmolt_wild_num,
         doy,year, river, daytime_category, In) %>%
  pivot_longer(cols = c(chinook0_hatchery_num, chinook0_wild_num,
                        chinook1_hatchery_num, chinook1_wild_num,
                        coho1_hatchery_num, coho1_wild_num,
                        steelheadsmolt_hatchery_num, steelheadsmolt_wild_num),
               names_to = c("species", "origin"),
               names_pattern = "(.*)_(.*)_num",
               values_to = "catch") %>%
  group_by(year, doy, river, species, origin) %>%
  summarise(catch = sum(catch, na.rm = TRUE), hours = sum(In, na.rm = TRUE)) %>%
  mutate(catch_per_unit_effort = catch/hours)
  

puyallup <- read.csv(here("data",
                   "puyallup",
                   "puyallup_2004-2021_all_days_w_covariates.csv"),
              header=TRUE)
skagit <- read.csv(here("data",
                   "skagit",
                   "skagit_2010-2019_w_covariates.csv"), header=TRUE)

#plot green river flows and temperature

ggplot(green_day, aes(x = doy, y = flow)) +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(green_day, aes(x = doy, y = temp)) +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(skagit, aes(x = doy, y = flow)) +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(dungeness, aes(x = doy, y = flow)) +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(puyallup, aes(x = doy, y = flow)) +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(green_day, aes(x = doy, y = c(NA,diff(flow)))) +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(skagit, aes(x = doy, y = c(NA,diff(flow)))) +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(puyallup, aes(x = doy, y = c(NA,diff(flow)))) +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(dungeness, aes(x = doy, y = c(NA,diff(flow)))) +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

  