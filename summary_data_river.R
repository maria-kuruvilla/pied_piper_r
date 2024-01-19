# Goal
# read data for each river
# filter the data based on day of year
# sum the number of fish caught each year separated by species and origin
# output summary table

# read data for dungeness river

dungeness <- read.csv(here("data",
                           "dungeness","dungeness_aggregated_w_trap_efficiency.csv"),header=TRUE)

# filter the data based on day of year

trial <- dungeness %>% 
  filter(doy >130 & doy < 200) %>%
  pivot_longer(cols = c(chinook0_wild_num, chinook0_hatchery_num, 
                        coho1_wild_num, coho1_hatchery_num), names_to = c("species", "origin"), 
               values_to = "num", names_pattern = "(.*)_(.*)_num") %>% 
  select(year, doy, num, origin, species) 
