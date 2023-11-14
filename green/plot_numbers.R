#read all the green river data files and plot numbers by species

library(here)
library(tidyverse)


#read data

data_2017 <- read.csv(here("data","green", "2017_green_all_R_new.csv"))


#using tidyverse to make the data long format

data_long <- data_2017 %>% 
  pivot_longer(cols = c("chum0_mixed_num", "steelheadsmolt_wild_num",
                        "steelheadsmolt_hatchery_num","coho0_wild_num","coho1_hatchery_num",
                        "chinook1_hatchery_num",  "chinook0_wild_num", "coho1_wild_num",
                        "chinook0_hatchery_num"), names_to = "species", values_to = "num_caught")


#plot num caught for each species
#formatting date first
data_long$StartDate <- as.Date(data_long$StartDate, format = "%Y-%m-%d")


ggplot(data_long, aes( x = StartDate, y = num_caught)) +
  geom_point() +
  facet_wrap(~species, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Date", y = "Number of Fish Caught", title = "Number of Fish Caught by Species in 2017") 

#save plot
ggsave(here("output","green", "2017_plot_species.png"), width = 10, height = 10, units = "in")

#repeat same for 2020

data_2020 <- read.csv(here("data","green", "2020_green_all_R_new.csv"))

data_long_2020 <- data_2020 %>% 
  pivot_longer(cols = c("chum0_mixed_num", "steelheadsmolt_wild_num","pink0_wild_num",
                        "steelheadsmolt_hatchery_num","coho0_wild_num","coho1_mixed_num",
                        "chinook1_hatchery_num",  "chinook0_wild_num", "coho0_wild_num",
                        "chinook0_hatchery_num"), names_to = "species", values_to = "num_caught")

data_long_2020$StartDate <- as.Date(data_long_2020$StartDate, format = "%Y-%m-%d")

ggplot(data_long_2020, aes( x = StartDate, y = num_caught)) +
  geom_point() +
  facet_wrap(~species, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Date", y = "Number of Fish Caught", title = "Number of Fish Caught by Species in 2020")

#save plot

ggsave(here("output","green", "2020_plot_species.png"), width = 10, height = 10, units = "in")

