#Goal - to produce fingures for paper
#first aggregate all the data for all rivers
#1 - plot daily catch data for each year and each river and each species for 
#hatchery and wild
#2 - plot daily environmental data for each year and each river

#load packages

library(ggplot2)
library(tidyverse)
library(here)
library(cowplot)


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

puyallup <- read.csv(here("data",
                          "puyallup",
                          "puyallup_2004-2021_all_days_w_covariates.csv"),
                     header=TRUE)
puyallup$river <- "puyallup"

skagit <- read.csv(here("data",
                        "skagit",
                        "skagit_2010-2022_w_covariates.csv"), header=TRUE)

skagit$river <- "skagit"

#sum all the values for each Date

green_agg <- rbind(green_day, green_night) %>% 
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
  
#do the same for the other rivers

skagit_agg <- skagit %>%
  mutate(river = "skagit") %>%
  select(chinook0_hatchery_num, chinook0_wild_num,
         chinook1_hatchery_num, chinook1_wild_num,
         coho1_hatchery_num, coho1_wild_num,
         steelheadsmolt_hatchery_num, steelheadsmolt_wild_num,
         doy,year, river, In) %>%
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

#using tidyverse
#first select Date, chinook0_wild_num, chinook0_hatchery_num, river
#make column for day of year and year
#make new column for In which is the sum of In_day and In_night while ignoring NAs
#make new column for chinook0_wild_num 
#which is the sum of chinook0_wild_num_day and chinook0_wild_num_night 
#while ignoring NAs
#make new column for chinook0_hatchery_num
#pivot longer to make a column for species and origin
#check if hours = 0, then convert catch_per_unit_effort to NA

puyallup_agg <- puyallup %>%
  mutate(river = "puyallup") %>%
  select(chinook0_hatchery_num_day, chinook0_wild_num_day,
         chinook0_hatchery_num_night, chinook0_wild_num_night,
         Date, In_day, In_night,
         doy, river) %>%
  mutate(year = year(Date), doy = yday(Date)) %>%
  group_by(year, doy, river) %>%
  summarise(hours = sum(In_day, In_night, na.rm = TRUE),
            chinook0_wild_num = sum(chinook0_wild_num_day, 
                                    chinook0_wild_num_night, na.rm = TRUE),
            chinook0_hatchery_num = sum(chinook0_hatchery_num_day, 
                                        chinook0_hatchery_num_night, na.rm = TRUE)
            ) %>%
  pivot_longer(cols = c(chinook0_hatchery_num, chinook0_wild_num),
               names_to = c("species", "origin"),
               names_pattern = "(.*)_(.*)_num",
               values_to = "catch") %>%
  mutate(catch_per_unit_effort = catch/hours) %>%
  mutate(catch_per_unit_effort = ifelse(hours == 0, NA, catch_per_unit_effort))


dungeness_agg <- dungeness %>%
  mutate(river = "dungeness") %>%
  select(chinook0_hatchery_num, chinook0_wild_num,
         chinook1_hatchery_num, chinook1_wild_num,
         coho1_hatchery_num, coho1_wild_num,
         steelheadsmolt_hatchery_num, steelheadsmolt_wild_num,
         doy,year, river, In) %>%
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

#combine all the rivers into one dataframe
#keep only chinook0 and coho1

all_rivers <- rbind(skagit_agg, puyallup_agg, dungeness_agg)
all_rivers <- all_rivers %>%
  filter(species %in% c("chinook0", "coho1"))

#plot the catch per unit effort for chinook0 and coho1 for each river
#different lines for each year
#different colors for origin
#log the y axis

ggplot(all_rivers, aes(x = doy, y = catch+1, group = year))+
  xlim(0, 200) +
  geom_line(alpha = 0.5, linewidth = 0.5) +
  facet_grid(origin ~ species + river, scales = "free_y", space = "fixed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_log10()



ggplot(all_rivers, aes(x = doy, y = catch_per_unit_effort)) +
  geom_line() +
  facet_wrap(~river + species, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

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

#find the row in puyallup_agg dataframe that has chinook0_hatchery_num>3000

all_rivers %>%
  filter(catch > 3000)

#plot the catch per unit effort for chinook0 and coho1 for each river separately
#use cowplot to combine the plots
#filter for only chinook0 and coho1
#add slategray vertical line at doy = 150 and 225
skagit_chinook_plot <- ggplot(skagit_agg %>% filter(species == "chinook0"), 
                      aes(x = doy, y = catch, group = year, color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(subtitle = "Skagit River",
       title = "",
       x = "Day of year",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 150, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 200, color = "slategray", linetype = "dashed")

skagit_coho_plot <- ggplot(skagit_agg %>% filter(species == "coho1"), 
                           aes(x = doy, y = catch, group = year, color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14),
        strip.text.x.top = element_blank()
        )+
  labs(subtitle = "Skagit River",
       title = "",
       x = "Day of year",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 100, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 150, color = "slategray", linetype = "dashed")

puyallup_chinook_plot <- ggplot(puyallup_agg %>% filter(species == "chinook0"), 
                                aes(x = doy, y = catch, group = year, color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(subtitle = "Puyallup River",
       title = "",
       x = "Day of year",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 130, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 200, color = "slategray", linetype = "dashed")

dungeness_chinook_plot <- ggplot(dungeness_agg %>% filter(species == "chinook0"), 
                                 aes(x = doy, y = catch, group = year, color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(subtitle = "Dungeness River",
       title = "Chinook subyearlings",
       x = "Day of year",
       y = "Catch")+
  theme(legend.position="none") +
  geom_vline(xintercept = 130, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 200, color = "slategray", linetype = "dashed")


dungeness_coho_plot <- ggplot(dungeness_agg %>% filter(species == "coho1"), 
                              aes(x = doy, y = catch, group = year, color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(subtitle = "Dungeness River",
       title = "Coho yearlings",
       x = "Day of year",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 120, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 160, color = "slategray", linetype = "dashed")

#combine the plots

plot_grid(dungeness_chinook_plot, puyallup_chinook_plot,skagit_chinook_plot, 
          dungeness_coho_plot, skagit_coho_plot,  ncol = 5)

ggsave(here("output","chinook_coho_plots.jpeg"), width = 12, height = 6)

puyallup_plot <- ggplot(puyallup_agg %>% filter(species == "chinook0"), 
                        aes(x = doy, y = catch, group = year)) +
  xlim(25,200)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dungeness_plot <- ggplot(dungeness_agg %>% filter(species == "chinook0" | species == "coho1"), 
                         aes(x = doy, y = catch, group = year)) +
  xlim(25,200)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#combine the plots
#change species name to chinook subyearlings and coho yearlings
#combine the grid names
#remove gray background from grid
plot_grid(skagit_plot, puyallup_plot, dungeness_plot, ncol = 3)

ggplot(all_rivers, aes(x = doy, y = catch, group = year,
                       color = origin))+
  xlim(25, 200) +
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin ~ species + river, scales = "free_y", space = "fixed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 8))

all_rivers$species2 <- factor(all_rivers$species, 
                              labels = c("Chinook Subyearlings", "Coho Yearlings"))
all_rivers$river <- factor(all_rivers$river, 
                              labels = c("Skagit",
                                         "Puyallup",
                                         "Dungeness"))


ggplot(all_rivers, aes(x = doy, y = catch, group = year,
                       color = origin))+
  xlim(25, 200) +
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin ~ species2 + river, scales = "free_y", space = "fixed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 8))




