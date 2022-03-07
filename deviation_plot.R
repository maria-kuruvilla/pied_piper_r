# Goal - Enter river, species, age and produce figure with deviation of wilf fish
# numbers from moving average as function of hatchery fish

library(RODBC)
library(here)
library(tidyverse)
library(tidyquant)



###function to return dataframe given river, species, age and type
extract_all_data <- function(species,age,type,river,file_type){
  data_filter = paste(species,age,type)
  file = paste0(river,file_type)
  year = c(2015,2016,2017,2018,2019,2020)
  
  for(i in 1:6){
    string_year = paste(toString(year[i]),file)
    print(string_year)
    channel <- odbcConnectAccess2007(here("Documents","data","pied_piper",string_year))
    df <- sqlFetch(channel, 'qry_AllCatch')
    #print(unique(df$WSPEName))
    df_species <- df[(df$WSPEName==data_filter & df$CaptureType==1),]# | df$WSPEName=="Coho 1+ H",]
    #print(df_chinook0[1,])
    if(i == 1){
      df_species_all_years = df_species
    }
    else{
      df_species_all_years <- rbind(df_species_all_years,df_species)
    }
  }
  
  df_species_all_years$day_of_year <- format(as.POSIXct(df_species_all_years$StartDate), format = "%m/%d")
  df_species_all_years$year <- format(as.POSIXct(df_species_all_years$StartDate), format = "%Y")
  df_species_all_years$start_datetime <- as.POSIXct(paste(df_species_all_years$StartDate, 
                                                          format(df_species_all_years$StartTime, format = "%H:%M:%S")))
  
  df_species_all_years$start_time <- format(as.POSIXct(df_species_all_years$start_datetime), format = "%H:%M:%S")
  
  
  df_species_all_years$end_datetime <- as.POSIXct(paste(df_species_all_years$EndDate, 
                                                        format(df_species_all_years$EndTime, format = "%H:%M:%S")))
  
  df_species_all_years$end_time <- format(as.POSIXct(df_species_all_years$end_datetime), format = "%H:%M:%S")
  
  df_species_all_years$fish_per_hour <- df_species_all_years$NumCaught/as.numeric(df_species_all_years$end_datetime - df_species_all_years$start_datetime)
  my_data_frame <- df_species_all_years %>%
    group_by(StartDate, EndDate,start_datetime,end_datetime,start_time, end_time, day_of_year, year) %>%
    summarise(sum_num_caught = sum(fish_per_hour))
  return(my_data_frame)
}



year_average <- function(df_new){
  my_data_frame <- df_new %>%
    group_by(day_of_year) %>%
    summarise(avg_num_caught = mean(fish_per_hour))
  f21 <- rep(1/21,21)
  my_data_frame$moving_average <- stats::filter(my_data_frame$avg_num_caught,f21,  sides = 2)
  my_data_frame$moving_average[is.na(my_data_frame$moving_average)] <- 
    my_data_frame$avg_num_caught[is.na(my_data_frame$moving_average)]
  for(i in 1:length(df_new$day_of_year)){
    #for loop for all entries in my_data_frame
    for(j in 1:length(my_data_frame$day_of_year)){
      ma_day_of_year = my_data_frame$day_of_year[j]
      all_data_day_of_year = df_new$day_of_year[i]
      #if both day of years are the same, add it to moving average column of df_new
      if(ma_day_of_year == all_data_day_of_year){
        df_new$moving_average[i] <- my_data_frame$moving_average[j]
      }
    }
  }
  return(df_new)
  
}



deviation <- function(coho_h){
  for(i in 1:length(coho_h$fish_per_hour)){
    for(j in 1:length(average_wildfish_per_hour$fish_per_hour)){
      if(coho_h$StartDate[i] == average_wildfish_per_hour$StartDate[j]){
        coho_h$wild_fish[i] <- average_wildfish_per_hour$fish_per_hour[j] 
        coho_h$wild_fish_ma[i] <- average_wildfish_per_hour$moving_average[j]
        coho_h$wild_fish_deviation[i] <- coho_h$wild_fish[i] - coho_h$wild_fish_ma[i]
        
      }
      
    }
  }
  return(coho_h)
}

coho_h <- deviation(coho_h)
plot_dev <- ggplot(data = coho_h,aes(x = fish_per_hour,y=wild_fish_deviation))+
  geom_point(alpha = 0.5) + theme_minimal() + ylab("Deviation of wild fish per hour") +
  xlab("Number of hatchery fish per hour") + ggtitle(paste(river,species,age))#+
  # xlim(0,0.25) + 
  # ylim(-0.15,0.15)
plot_dev

#log scale
plot_dev <- ggplot(data = coho_h,aes(x = fish_per_hour+1,y=wild_fish_deviation+1))+
  geom_point() + theme_minimal()  + ylab("Deviation of wild fish per hour") +
  xlab("Number of hatchery fish per hour") + ggtitle(paste(river,species,age))+
  scale_x_log10() + scale_y_log10()
plot_dev

river = "Dungeness"
age = "1+"
species="Coho"
filetype = ".accdb"
coho_w <- extract_all_data(species, age, "W", river, filetype)
coho_h <- extract_all_data(species, age, "H", river, filetype)

average_wildfish_per_hour <- year_average(coho_w)
coho_h <- deviation(coho_h)

plot_dev <- ggplot(data = coho_h,aes(x = fish_per_hour,y=wild_fish_deviation))+
  geom_point(alpha = 0.5) + theme_minimal() + ylab("Deviation of wild fish per hour") +
  xlab("Number of hatchery fish per hour") + ggtitle(paste(river,species,age))#+
# xlim(0,0.25) + 
# ylim(-0.15,0.15)
plot_dev

for(i in 1:length(coho_h$fish_per_hour)){
  for(j in 1:length(average_wildfish_per_hour$fish_per_hour)){
    if(coho_h$StartDate[i] == average_wildfish_per_hour$StartDate[j]){
      coho_h$wild_fish[i] <- average_wildfish_per_hour$fish_per_hour[j] 
      coho_h$wild_fish_ma[i] <- average_wildfish_per_hour$moving_average[j]
      coho_h$wild_fish_deviation[i] <- coho_h$wild_fish[i] - coho_h$wild_fish_ma[i]
      
    }
    
  }
}