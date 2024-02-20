#Goal - edit the original paper_figures.R to include the puyallup coho
#make the figure two rwos - one for chinook and the other for coho

#also try to make it two columns, one for Chinook and the other for coho

#load packages

library(ggplot2)
library(tidyverse)
library(here)
library(cowplot)
library(ggh4x)



#readin file
dungeness <- read.csv(here("data",
                           "dungeness","dungeness_aggregated_w_trap_efficiency.csv"),
                      header=TRUE)

dungeness$river <- "dungeness"


puyallup <- read.csv(here("data",
                          "puyallup",
                          "puyallup_final.csv"),
                     header=TRUE)
puyallup$river <- "puyallup"

skagit <- read.csv(here("data",
                        "skagit",
                        "skagit_2010-2022_w_covariates.csv"), header=TRUE)

skagit$river <- "skagit"


skagit_agg <- skagit %>%
  mutate(river = "skagit") %>%
  select(chinook0_hatchery_num, chinook0_wild_num,
         coho1_hatchery_num, coho1_wild_num,
         doy,year, river, In) %>%
  pivot_longer(cols = c(chinook0_hatchery_num, chinook0_wild_num,
                        coho1_hatchery_num, coho1_wild_num),
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
         coho1_hatchery_num_day, coho1_wild_num_day,
         coho1_hatchery_num_night, coho1_wild_num_night,
         Date, In_day, In_night,
         doy, river) %>%
  mutate(year = year(Date), doy = yday(Date)) %>%
  group_by(year, doy, river) %>%
  summarise(hours = sum(In_day, In_night, na.rm = TRUE),
            chinook0_wild_num = sum(chinook0_wild_num_day, 
                                    chinook0_wild_num_night, na.rm = TRUE),
            
            chinook0_hatchery_num = sum(chinook0_hatchery_num_day, 
                                        chinook0_hatchery_num_night, na.rm = TRUE),
            
            coho1_wild_num = sum(coho1_wild_num_day,
                                 coho1_wild_num_night, na.rm = TRUE),
            
            coho1_hatchery_num = sum(coho1_hatchery_num_day,
                                     coho1_hatchery_num_night, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(chinook0_hatchery_num, chinook0_wild_num,
                        coho1_hatchery_num, coho1_wild_num),
               names_to = c("species", "origin"),
               names_pattern = "(.*)_(.*)_num",
               values_to = "catch") %>%
  mutate(catch_per_unit_effort = catch/hours) %>%
  mutate(catch_per_unit_effort = ifelse(hours == 0, NA, catch_per_unit_effort))


dungeness_agg <- dungeness %>%
  mutate(river = "dungeness") %>%
  select(chinook0_hatchery_num, chinook0_wild_num,
         coho1_hatchery_num, coho1_wild_num,
         doy,year, river, In) %>%
  pivot_longer(cols = c(chinook0_hatchery_num, chinook0_wild_num,
                        coho1_hatchery_num, coho1_wild_num),
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

skagit_chinook_plot <- ggplot(skagit_agg %>% filter(species == "chinook0"), 
                              aes(x = doy, y = catch, group = year, color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  labs(subtitle = "Skagit",
       title = " ",
       x = "Day of year",
       y = "")+
  
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  theme(legend.position="none") +
  geom_vline(xintercept = 150, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 189, color = "slategray", linetype = "dashed")+
  scale_y_continuous(n.breaks = 3)

skagit_coho_plot <- ggplot(skagit_agg %>% filter(species == "coho1"), 
                           aes(x = doy, y = catch, group = year, color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size  =14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 20),
        strip.text.x.top = element_blank()
  )+
  labs(subtitle = "Skagit",
       title = " ",
       x = "Day of year",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 100, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 150, color = "slategray", linetype = "dashed")+
  scale_y_continuous(n.breaks = 3)

skagit_chinook_plot
skagit_coho_plot


puyallup_chinook_plot <- ggplot(puyallup_agg %>% filter(species == "chinook0"), 
                                aes(x = doy, y = catch, group = year, color = origin)) +
  xlim(25,225) +
  
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  scale_y_continuous(n.breaks = 3)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(subtitle = "Puyallup",
       title = " ",
       x = "Day of year",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 130, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 218, color = "slategray", linetype = "dashed")

puyallup_chinook_plot


puyallup_coho_plot <- ggplot(puyallup_agg %>% filter(species == "coho1"), 
                             aes(x = doy, y = catch, group = year, color = origin)) +
  xlim(25,225) +
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  scale_y_continuous(n.breaks = 3)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 20),
        strip.text.x.top = element_blank()
  )+
  labs(subtitle = "Puyallup",
       title = " ",
       x = "Day of year",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 90, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 160, color = "slategray", linetype = "dashed")

puyallup_coho_plot


dungeness_chinook_plot <- ggplot(dungeness_agg %>% filter(species == "chinook0"), 
                                 aes(x = doy, y = catch, group = year, color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(subtitle = "Dungeness",
       title = "Chinook",
       x = "Day of year",
       y = "Catch")+
  theme(legend.position="none") +
  geom_vline(xintercept = 130, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 200, color = "slategray", linetype = "dashed") + 
  scale_y_continuous(n.breaks = 3)
dungeness_chinook_plot

dungeness_coho_plot <- ggplot(dungeness_agg %>% filter(species == "coho1"), 
                              aes(x = doy, y = catch, group = year, color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20)
  )+
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(subtitle = "Dungeness",
       title = "Coho",
       x = "Day of year",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 120, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 160, color = "slategray", linetype = "dashed")+
  scale_y_continuous(n.breaks = 3)

dungeness_coho_plot



#make one figure and facet wrap by species and by origin and by river

#first combine all the dataframes

all_data <- bind_rows(puyallup_agg, dungeness_agg, skagit_agg)
glimpse(all_data)

ggplot(all_data) +
  geom_line(aes(x = doy, y = catch, group = year, color = origin), alpha = 0.2, linewidth = 1) +
  facet_nested_wrap(river~origin~species, scales = "free_y", ncol = 2,
                    remove_labels = TRUE) +
  theme_classic() +
  xlim(25,225)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20)
  )+
  scale_color_manual(values = c("cadetblue","salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(
       x = "Day of year",
       y = "Catch")+
  theme(legend.position="none")+
  scale_y_continuous(n.breaks = 3)



dungeness_chinook_wild_plot <- ggplot(dungeness_agg %>% 
                                        filter(species == "chinook0", 
                                               origin == "wild"), 
                                      aes(x = doy, y = catch, group = year, 
                                          color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  
  scale_color_manual(values = c("salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(
       x = "",
       y = "Catch")+
  theme(legend.position="none") +
  geom_vline(xintercept = 130, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 200, color = "slategray", linetype = "dashed") + 
  scale_y_continuous(n.breaks = 3, limits = c(0, 800))
dungeness_chinook_wild_plot

dungeness_chinook_hatchery_plot <- ggplot(dungeness_agg %>% 
                                           filter(species == "chinook0", 
                                                  origin == "hatchery"), 
                                         aes(x = doy, y = catch, group = year, 
                                             color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  
  scale_color_manual(values = c("cadetblue"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(subtitle = "Chinook",
       x = "",
       y = "Catch")+
  theme(legend.position="none") +
  geom_vline(xintercept = 130, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 200, color = "slategray", linetype = "dashed") + 
  scale_y_continuous(n.breaks = 3, limits = c(0, 4000))+
  annotate("text", x = 55, y = 3500, label = "Dungeness", size = 6)

dungeness_chinook_hatchery_plot


puyallup_chinook_wild_plot <- ggplot(puyallup_agg %>% 
                                      filter(species == "chinook0", 
                                             origin == "wild"), 
                                    aes(x = doy, y = catch, group = year, 
                                        color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  
  scale_color_manual(values = c("salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(
       x = "",
       y = "Catch")+
  theme(legend.position="none") +
  geom_vline(xintercept = 130, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 218, color = "slategray", linetype = "dashed") + 
  scale_y_continuous(n.breaks = 3, limits = c(0, 500))
puyallup_chinook_wild_plot

puyallup_chinook_hatchery_plot <- ggplot(puyallup_agg %>% 
                                           filter(species == "chinook0", 
                                                  origin == "hatchery"), 
                                         aes(x = doy, y = catch, group = year, 
                                             color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  
  scale_color_manual(values = c("cadetblue"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(
       x = "",
       y = "Catch")+
  theme(legend.position="none") +
  geom_vline(xintercept = 130, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 218, color = "slategray", linetype = "dashed") + 
  scale_y_continuous(n.breaks = 3, limits = c(0, 8000))+
  annotate("text", x = 45, y = 7000, label = "Puyallup", size = 6)

puyallup_chinook_hatchery_plot

skagit_chinook_wild_plot <- ggplot(skagit_agg %>% 
                                      filter(species == "chinook0", 
                                             origin == "wild"), 
                                    aes(x = doy, y = catch, group = year, 
                                        color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  
  scale_color_manual(values = c("salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(
       x = "Day of year",
       y = "Catch")+
  theme(legend.position="none") +
  geom_vline(xintercept = 150, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 189, color = "slategray", linetype = "dashed") + 
  scale_y_continuous(n.breaks = 3, limits = c(0, 5000))
skagit_chinook_wild_plot


skagit_chinook_hatchery_plot <- ggplot(skagit_agg %>% 
                                           filter(species == "chinook0", 
                                                  origin == "hatchery"), 
                                         aes(x = doy, y = catch, group = year, 
                                             color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  
  scale_color_manual(values = c("cadetblue"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  labs(
       x = "",
       y = "Catch")+
  theme(legend.position="none") +
  geom_vline(xintercept = 150, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 189, color = "slategray", linetype = "dashed") + 
  scale_y_continuous(n.breaks = 3, limits = c(0, 3000), 
                     breaks = c(0, 1500, 3000))+
  annotate("text", x = 40, y = 2500, label = "Skagit", size = 6)

skagit_chinook_hatchery_plot


dungeness_coho_wild_plot <- ggplot(dungeness_agg %>% 
                                      filter(species == "coho1", 
                                             origin == "wild"), 
                                    aes(x = doy, y = catch, group = year, 
                                        color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  
  scale_color_manual(values = c("salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 20),
        strip.text.x.top = element_blank())+
  labs(
       x = "",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 120, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 160, color = "slategray", linetype = "dashed") + 
  scale_y_continuous(n.breaks = 3, limits = c(0, 400))
dungeness_coho_wild_plot


dungeness_coho_hatchery_plot <- ggplot(dungeness_agg %>% 
                                           filter(species == "coho1", 
                                                  origin == "hatchery"), 
                                         aes(x = doy, y = catch, group = year, 
                                             color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  
  scale_color_manual(values = c("cadetblue"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 20),
        strip.text.x.top = element_blank())+
  labs(subtitle = "Coho",
       x = "",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 120, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 160, color = "slategray", linetype = "dashed") + 
  scale_y_continuous(n.breaks = 3, limits = c(0, 6000))

dungeness_coho_hatchery_plot

plot_grid(dungeness_chinook_wild_plot, dungeness_coho_wild_plot,
          dungeness_chinook_hatchery_plot, dungeness_coho_hatchery_plot, 
          ncol = 2, nrow = 2)

puyallup_coho_wild_plot <- ggplot(puyallup_agg %>% 
                                      filter(species == "coho1", 
                                             origin == "wild"), 
                                    aes(x = doy, y = catch, group = year, 
                                        color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  
  scale_color_manual(values = c("salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 20),
        strip.text.x.top = element_blank())+
  labs(
       x = "",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 90, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 160, color = "slategray", linetype = "dashed") + 
  scale_y_continuous(n.breaks = 3, limits = c(0, 400))
puyallup_coho_wild_plot


puyallup_coho_hatchery_plot <- ggplot(puyallup_agg %>% 
                                           filter(species == "coho1", 
                                                  origin == "hatchery"), 
                                         aes(x = doy, y = catch, group = year, 
                                             color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  
  scale_color_manual(values = c("cadetblue"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 20),
        strip.text.x.top = element_blank())+
  labs(
       x = "",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 90, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 160, color = "slategray", linetype = "dashed") + 
  scale_y_continuous(n.breaks = 3, limits = c(0, 10000))

puyallup_coho_hatchery_plot


plot_grid(dungeness_chinook_wild_plot, dungeness_coho_wild_plot,
          dungeness_chinook_hatchery_plot, dungeness_coho_hatchery_plot,
          puyallup_chinook_wild_plot, puyallup_coho_wild_plot,
          puyallup_chinook_hatchery_plot, puyallup_coho_hatchery_plot, 
          ncol = 2, nrow = 4)

skagit_coho_wild_plot <- ggplot(skagit_agg %>% 
                                      filter(species == "coho1", 
                                             origin == "wild"), 
                                    aes(x = doy, y = catch, group = year, 
                                        color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  
  scale_color_manual(values = c("salmon"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 20),
        strip.text.x.top = element_blank())+
  labs(
       x = "Day of year",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 100, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 150, color = "slategray", linetype = "dashed") + 
  scale_y_continuous(n.breaks = 3, limits = c(0, 1000))

skagit_coho_wild_plot


#add text "Skagit" to the top, left of the plot
skagit_coho_hatchery_plot <- ggplot(skagit_agg %>% 
                                           filter(species == "coho1", 
                                                  origin == "hatchery"), 
                                         aes(x = doy, y = catch, group = year, 
                                             color = origin)) +
  xlim(25,225)+
  geom_line(alpha = 0.2, linewidth = 1) +
  facet_grid(origin~species, scales = "free_y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))+
  
  scale_color_manual(values = c("cadetblue"))+
  labs(color = "Origin") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 20),
        strip.text.x.top = element_blank())+
  labs(
       x = "",
       y = "")+
  theme(legend.position="none") +
  geom_vline(xintercept = 100, color = "slategray", linetype = "dashed") +
  geom_vline(xintercept = 150, color = "slategray", linetype = "dashed") + 
  scale_y_continuous(n.breaks = 3, limits = c(0, 4000))

skagit_coho_hatchery_plot

plot_grid(dungeness_chinook_hatchery_plot, dungeness_coho_hatchery_plot,
          dungeness_chinook_wild_plot, dungeness_coho_wild_plot,
          puyallup_chinook_hatchery_plot, puyallup_coho_hatchery_plot,
          puyallup_chinook_wild_plot, puyallup_coho_wild_plot, 
          skagit_chinook_hatchery_plot, skagit_coho_hatchery_plot,
          skagit_chinook_wild_plot, skagit_coho_wild_plot,
          ncol = 2, nrow = 6, align = "v")
ggsave(here("output","chinook_coho_plot_new.png"), width = 10, height = 14, units = "in")          
