#Goal - produce one figure with multiple panels to plot deviation of wild 
# fish from average as a function of number of hatchery fish out migrating 

#edit - each data point with for one start_datetime

#edit - make stack of plots with different lags


library(RODBC)
library(here)
library(tidyverse)
library(tidyquant)
library(RColorBrewer)
library(patchwork)

access_file <- function(file_name){
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      message("This is the 'try' part")
      
      channel <- odbcConnectAccess2007(here("Documents","data","pied_piper",
                                            file_name))
      df <- sqlFetch(channel, 'qry_AllCatch')
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped inside a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      message(paste("File does not exist"),file_name)
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message("Opening access file caused a warning:",file_name)
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      #message(paste("File successfully opened:", url))
      message("File successfully opened",file_name)
    }
  )    
  return(out)
  odbcClose(channel)
}

extract_species_data_error <- function(year, species,type,river,file_type){
  if(species == "Steelhead"){
    data_filter = paste(species,"smolt",type)
    file = paste0(river,file_type)
    string_year = paste(toString(year),file)
    print(string_year)
    year0 = year-1
    df <- access_file(string_year)
    df$StartDate_OG <- df$StartDate
    df_species_all_years <- df[((df$WSPEName==data_filter)
                                & df$CaptureType==1),]
    df_species_all_years$StartDate_OG <- df_species_all_years$StartDate
  }
  else {
    data_filter0 = paste(species,"0+",type)
    data_filter = paste(species,"1+",type)
    file = paste0(river,file_type)
    year0 = year-1
    string_year = paste(toString(year),file)
    string_year0 = paste(toString(year0),file)
    print(string_year)
    print(string_year0)
    
    df <- access_file(string_year)
    #print(unique(df$WSPEName))
    df_species <- df[((df$WSPEName==data_filter)
                      & df$CaptureType==1),]
    df_species$StartDate_OG <- df_species$StartDate
    
    
    df0 <- access_file(string_year0)
    df_species0 <- df0[((df0$WSPEName==data_filter0)
                        & df0$CaptureType==1),]
    df_species0$StartDate_OG <- df_species0$StartDate
    
    
    df_species_all_years <- rbind(df_species0,df_species)
  }
  
  df_species_all_years$day_of_year <- format(as.POSIXct(df_species_all_years$StartDate_OG), format = "%m/%d")
  df_species_all_years$year <- format(as.POSIXct(df_species_all_years$StartDate_OG), format = "%Y")
  df_species_all_years$start_datetime <- as.POSIXct(paste(df_species_all_years$StartDate_OG, 
                                                          format(df_species_all_years$StartTime, format = "%H:%M:%S")))
  
  df_species_all_years$start_time <- format(as.POSIXct(df_species_all_years$start_datetime), format = "%H:%M:%S")
  
  
  df_species_all_years$end_datetime <- as.POSIXct(paste(df_species_all_years$EndDate, 
                                                        format(df_species_all_years$EndTime, format = "%H:%M:%S")))
  
  df_species_all_years$end_time <- format(as.POSIXct(df_species_all_years$end_datetime), format = "%H:%M:%S")
  
  df_species_all_years$fish_per_hour <- (df_species_all_years$NumCaught
                                         /as.numeric(difftime(df_species_all_years$end_datetime,
                                                              df_species_all_years$start_datetime, units = "hours")))
  
  year(df_species_all_years$StartDate[year(df_species_all_years$StartDate) == year0]) <- 2000
  year(df_species_all_years$StartDate[year(df_species_all_years$StartDate) == year]) <- 2001
  return(df_species_all_years)
  
  
}

sum_species_data_deviation <- function(year, species,type,river,file_type){
  df <- extract_species_data_error(year, species,type,river,file_type)
  df_new_summarize <- df %>%
    group_by(WSPEName, StartDate, day_of_year, year, StartDate_OG, start_datetime) %>%
    summarise(sum_num_caught = sum(fish_per_hour))
  return(df_new_summarize)
}

data_all_years <- function(species,type,river,file_type){
  if(species == "Steelhead"){
    last_year = 2020
  }
  else {
    last_year = 2021
  }
  for(i in 2016:last_year){
    if(i==2016){
      df_combine <- sum_species_data_deviation(i, species,type,river,file_type)
    }
    else {
      df <- sum_species_data_deviation(i, species,type,river,file_type)
      df_combine <- rbind(df_combine,df)
    }
    
  }
  return(df_combine)
}

moving_average_added <- function(species,type,river,file_type){
  if(species == "Steelhead"){
    last_year = 2020
  }
  else {
    last_year = 2021
  }
  for(i in 2016:last_year){
    if(i==2016){
      df_combine <- sum_species_data_deviation(i, species,type,river,file_type)
    }
    else {
      df <- sum_species_data_deviation(i, species,type,river,file_type)
      df_combine <- rbind(df_combine,df)
    }
    
  }
  my_data_frame <- df_combine %>%
    group_by(StartDate) %>%
    summarise(avg_num_caught = mean(sum_num_caught), sd = sd(sum_num_caught))
  f21 <- rep(1/10,10)
  my_data_frame$moving_average <- stats::filter(my_data_frame$avg_num_caught,f21,  sides = 2)
  my_data_frame$moving_average[is.na(my_data_frame$moving_average)] <- 
    my_data_frame$avg_num_caught[is.na(my_data_frame$moving_average)]
  
  for(i in 1:length(df_combine$day_of_year)){
    #for loop for all entries in my_data_frame
    for(j in 1:length(my_data_frame$StartDate)){
      ma_day_of_year = my_data_frame$StartDate[j]
      all_data_day_of_year = df_combine$StartDate[i]
      #if both day of years are the same, add it to moving average column of df_new
      if(ma_day_of_year == all_data_day_of_year){
        df_combine$moving_average[i] <- my_data_frame$moving_average[j]
      }
    }
  }
  return(df_combine)
}

deviation_same_time <- function(coho_h,coho_w){
  for(i in 1:length(coho_h$sum_num_caught)){
    #print(i)
    for(j in 1:length(coho_w$sum_num_caught)){
      #print(j)
      
      if((coho_h$start_datetime[i] == coho_w$start_datetime[j]) && (coho_h$start_datetime[i] == coho_w$start_datetime[j])){
        #print(j)
        coho_h$wild_fish[i] <- coho_w$sum_num_caught[j] 
        coho_h$wild_fish_ma[i] <- coho_w$moving_average[j]
        coho_h$wild_fish_deviation[i] <- coho_h$wild_fish[i] - coho_h$wild_fish_ma[i]
        
        break
        
      }
      else {
        coho_h$wild_fish[i] <- NA
        coho_h$wild_fish_ma[i] <- NA
        coho_h$wild_fish_deviation[i] <- NA
      }
      
    }
  }
  return(coho_h)
}

deviation_same_day <- function(coho_h,coho_w){
  for(i in 1:length(coho_h$sum_num_caught)){
    #print(i)
    for(j in 1:length(coho_w$sum_num_caught)){
      #print(j)
      if((coho_h$start_datetime[i] == coho_w$start_datetime[j])){
        #print(j)
        coho_h$wild_fish[i] <- coho_w$sum_num_caught[j] 
        coho_h$wild_fish_ma[i] <- coho_w$moving_average[j]
        coho_h$wild_fish_deviation[i] <- coho_h$wild_fish[i] - coho_h$wild_fish_ma[i]
        
        break
        
      }
      if((coho_h$start_datetime[i] != coho_w$start_datetime[j])){
        coho_h$wild_fish[i] <- NA
        coho_h$wild_fish_ma[i] <- NA
        coho_h$wild_fish_deviation[i] <- NA
      }
      
      if((coho_h$StartDate_OG[i] == coho_w$StartDate_OG[j]) && (coho_h$start_datetime[i] < coho_w$start_datetime[j])){
        #print(j)
        coho_h$wild_fish_sameday[i] <- coho_w$sum_num_caught[j] 
        coho_h$wild_fish_ma_sameday[i] <- coho_w$moving_average[j]
        coho_h$wild_fish_deviation_sameday[i] <- coho_h$wild_fish_sameday[i] - coho_h$wild_fish_ma_sameday[i]
        
        break
        
      }
      if((coho_h$StartDate_OG[i] != coho_w$StartDate_OG[j]) || (coho_h$start_datetime[i] >= coho_w$start_datetime[j])){
        coho_h$wild_fish_sameday[i] <- NA
        coho_h$wild_fish_ma_sameday[i] <- NA
        coho_h$wild_fish_deviation_sameday[i] <- NA
      }
      
      if(((coho_h$StartDate_OG[i] + hours(24))== coho_w$StartDate_OG[j])){
        #print(j)
        coho_h$wild_fish_nextday[i] <- coho_w$sum_num_caught[j] 
        coho_h$wild_fish_ma_nextday[i] <- coho_w$moving_average[j]
        coho_h$wild_fish_deviation_nextday[i] <- coho_h$wild_fish_nextday[i] - coho_h$wild_fish_ma_nextday[i]
        
        break
        
      }
      if(((coho_h$StartDate_OG[i] + hours(24))!= coho_w$StartDate_OG[j])){
        coho_h$wild_fish_nextday[i] <- NA
        coho_h$wild_fish_ma_nextday[i] <- NA
        coho_h$wild_fish_deviation_nextday[i] <- NA
      }
      
    }
  }
  return(coho_h)
}

deviation_next_day <- function(coho_h,coho_w){
  for(i in 1:length(coho_h$sum_num_caught)){
    #print(i)
    for(j in 1:length(coho_w$sum_num_caught)){
      #print(j)
      
      if(((coho_h$StartDate_OG[i] + hours(24))== coho_w$StartDate_OG[j])){
        #print(j)
        coho_h$wild_fish_nextday[i] <- coho_w$sum_num_caught[j] 
        coho_h$wild_fish_ma_nextday[i] <- coho_w$moving_average[j]
        coho_h$wild_fish_deviation_nextday[i] <- coho_h$wild_fish_nextday[i] - coho_h$wild_fish_ma_nextday[i]
        
        break
        
      }
      else {
        coho_h$wild_fish_nextday[i] <- NA
        coho_h$wild_fish_ma_nextday[i] <- NA
        coho_h$wild_fish_deviation_nextday[i] <- NA
      }
      
    }
  }
  return(coho_h)
}

#Chinook 

species = "Chinook"
type = "W"
river = "Dungeness"
file_type = ".accdb"

chinook_w <- moving_average_added(species,type,river,file_type)

type = "H"
chinook_h <- data_all_years(species,type,river,file_type)

chinook_combine_nextday <- deviation_next_day(chinook_h,chinook_w)

#Chinook 

species = "Coho"
type = "W"
river = "Dungeness"
file_type = ".accdb"

coho_w <- moving_average_added(species,type,river,file_type)

type = "H"
coho_h <- data_all_years(species,type,river,file_type)

coho_combine_sameday <- deviation_same_day(coho_h,coho_w)