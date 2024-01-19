#get peak migration doy from phenomix with trap efficiency data

#use env covaraites like atu at a particular day in regression model

#load packages
library(ggplot2)
library(tidyverse)
library(here)
library(dplyr)


#read means chinook0

means_chinook0 <- read.csv(here("data",
                                    "dungeness","means_chinook0.csv"),header=TRUE)

#read trap_efficiency data

trap_efficiency <- read.csv(here("data",
                                    "dungeness","dungeness_aggregated_w_trap_efficiency.csv"),header=TRUE)

#lets look at atu for doy = 150

#make subset of atu values at doy = 150

atu_150 <- trap_efficiency %>% 
  filter(doy == 150) %>% 
  select(atu_april,year)

#merge the atu file with means_chinook0 on year

means_chinook0_atu <- merge(means_chinook0, atu_150, by="year", all.x = TRUE)

#make ggplot scatter plot of atu and peak migration

ggplot(means_chinook0_atu, aes(x=atu_april, y=value, color = type)) +
  geom_point() +
  labs(x="Accumulated Temperature Units", y="Peak Migration (day of year)") +
  theme_bw() +
  theme(text = element_text(size=20))+
  scale_color_manual(values = c("#337a64", "#7b6b80"))

#calculating different flood metric

#plot histogram of flow_diff
hist(trap_efficiency$flow_diff,30)
hist(trap_efficiency$flow,30)
#let's say anything above 100 is a flood

#make a new column in trap_efficiency called flood_count which is count of flood days in a year

trap_efficiency <- trap_efficiency %>% 
  mutate(flood = ifelse(flow_diff > 100, 1, 0), high_flow = ifelse(flow > 1000, 1, 0))
#make column of cumulative sum of flood reset every year

trap_efficiency <- trap_efficiency %>% 
  group_by(year) %>% 
  mutate(flood_count = cumsum(ifelse(is.na(flood),0,flood)), 
         high_flow_count = cumsum(ifelse(is.na(high_flow),0,high_flow)),
         cum_flow = cumsum(ifelse(is.na(flow),0,flow)))

#merge dataframes with means_chinook0 by year

flow_150 <- trap_efficiency %>% 
  filter(doy == 150) %>% 
  select(flood_count, high_flow_count, cum_flow, year)

means_chinook0_flow <- merge(means_chinook0_atu, flow_150, by="year", all.x = TRUE)

#make ggplot scatter plot of flood count and peak migration

ggplot(means_chinook0_flow, aes(x=flood_count, y=value, color = type)) +
  geom_point() +
  labs(x="Flood Count", y="Peak Migration (day of year)") +
  theme_bw() +
  theme(text = element_text(size=20))+
  scale_color_manual(values = c("#337a64", "#7b6b80"))

#make ggplot scatter plot of high flow count and peak migration

ggplot(means_chinook0_flow, aes(x=high_flow_count, y=value, color = type)) +
  geom_point() +
  labs(x="High Flow Count", y="Peak Migration (day of year)") +
  theme_bw() +
  theme(text = element_text(size=20))+
  scale_color_manual(values = c("#337a64", "#7b6b80"))

#make ggplot scatter plot of cumulative flow and peak migration

ggplot(means_chinook0_flow, aes(x=cum_flow, y=value, color = type)) +
  geom_point() +
  labs(x="Cumulative Flow", y="Peak Migration (day of year)") +
  theme_bw() +
  theme(text = element_text(size=20))+
  scale_color_manual(values = c("#337a64", "#7b6b80"))

#ggplot of hatchery chinook with doy

ggplot(trap_efficiency, aes(y = chinook0_hatchery_perhour, x = doy)) +
  geom_point(alpha = 0.5) +
  labs(y="Hatchery Chinook (per hour)", x="Day of Year") +
  theme_bw() +
  theme(text = element_text(size=20))
