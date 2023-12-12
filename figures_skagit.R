#Goal - plot the number of hatchery and wild fish 
#caught per hour
#For each year and each species

#load packages

library(ggplot2)
library(tidyverse)
library(here)
library(cowplot)

skagit <- read.csv(here("data",
                        "skagit",
                        "skagit_2010-2022_w_covariates.csv"), header=TRUE)


skagit_agg <- skagit %>%
  mutate(river = "skagit") %>% #making new column for river
  select(chinook0_hatchery_num, chinook0_wild_num,
         chinook1_hatchery_num, chinook1_wild_num,
         coho1_hatchery_num, coho1_wild_num,
         steelheadsmolt_hatchery_num, steelheadsmolt_wild_num,
         doy,year, river, In) %>% #selecting columns to work with
  pivot_longer(cols = c(chinook0_hatchery_num, chinook0_wild_num,
                        chinook1_hatchery_num, chinook1_wild_num,
                        coho1_hatchery_num, coho1_wild_num,
                        steelheadsmolt_hatchery_num, steelheadsmolt_wild_num),
               names_to = c("species", "origin"), #making the data set so that 
               #there is only one column for catch
               #one column for species (eg chinook0)
               #and one column for origin (eg wild)
               names_pattern = "(.*)_(.*)_num",
               values_to = "catch") %>%
  group_by(year, doy, river, species, origin) %>% #if there are multiple entries
  #for a day, sum the catch and sum the In
  summarise(catch = sum(catch, na.rm = TRUE), hours = sum(In, na.rm = TRUE)) %>%
  mutate(catch_per_unit_effort = catch/hours) #calculate catch per unit effort


#plot the catch_per_unit_effort for all species and years

ggplot(skagit_agg)+
  geom_line(aes(x = doy, y = catch_per_unit_effort, group = year), linewidth = 1,
            alpha = 0.1)+
  #each year should be separate line and opaqueness 0.1
  facet_wrap(~species + origin, scales = "free_y")+ #separare plot for species
#and origin
  xlim(50,250)


ggplot(skagit_agg %>% 
         filter(species == "chinook0" & origin == "wild"))+
  #only wild chinook0
  geom_line(aes(x = doy, y = catch_per_unit_effort),
            alpha = 0.8)+
  #each year should be separate line and opaqueness 0.8
  facet_wrap(~year, scales = "free_y")+ #separare plot for each year
  xlim(50,250)

#2022 seems to have very few chinook0
