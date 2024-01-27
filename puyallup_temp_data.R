#goal - to read white_temp_data.csv and 
#puyallup trap data
#make the white_temp_data wider
#merge with puyallup_data
#save csv

#load libraries
library(tidyverse)

#read in data
white_temp_data <- read_csv(here("data","puyallup","white_temp_data.csv"))
head(white_temp_data)

#make the white_temp_data wider
white_temp_data_wide <- white_temp_data %>%
  select(Date, daynight_category, temp) %>%
  group_by(Date, daynight_category) %>%
  pivot_wider(names_from = "daynight_category", values_from = "temp") %>% 
  rename(temp_day = "day", temp_night = "night") %>% 
  ungroup()

head(white_temp_data_wide)

white_temp_data_wide$Date <- as.Date(white_temp_data_wide$Date, format = "%Y/%m/%d")

#read in puyallup data
puyallup_data <- read_csv(here("data","puyallup","puyallup_2004-2021_all_days_w_covariates_edited.csv"))
head(puyallup_data)
puyallup_data$Date <- as.Date(puyallup_data$Date, format = "%m/%d/%Y")



#merge white_temp_data_wide with puyallup_data on Date

puyallup_data_w_temp <- puyallup_data %>%
  left_join(white_temp_data_wide, by = "Date")

head(puyallup_data_w_temp)

#plot temp by year
puyallup_data_w_temp %>%
  filter(year > 2010) %>% 
  ggplot(aes(x = doy, y = temp_day)) +
  geom_point() +
  geom_line() +
  facet_wrap(~year, ncol = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#save csv

write_csv(puyallup_data_w_temp, here("data","puyallup","puyallup_2004-2021_all_days_w_covariates_w_temp.csv"))
