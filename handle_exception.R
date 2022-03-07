# Goal - to write a function that extracts data but can handle errors due to 
# first year not having year0

library(RODBC)
library(here)
library(tidyverse)
library(tidyquant)
library(RColorBrewer)


extract_species_data <- function(year, species,type,river,file_type){
  
  if(species == "Steelhead"){
    data_filter = paste(species,"smolt",type)
    file = paste0(river,file_type)
    string_year = paste(toString(year),file)
    print(string_year)
    channel <- odbcConnectAccess2007(here("Documents","data","pied_piper",string_year))
    df <- sqlFetch(channel, 'qry_AllCatch')
    df_species_all_years <- df[((df$WSPEName==data_filter)
                                & df$CaptureType==1),]
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
    channel <- odbcConnectAccess2007(here("Documents","data","pied_piper",string_year))
    df <- sqlFetch(channel, 'qry_AllCatch')
    #print(unique(df$WSPEName))
    df_species <- df[((df$WSPEName==data_filter)
                      & df$CaptureType==1),]
    channel0 <- odbcConnectAccess2007(here("Documents","data","pied_piper",string_year0))
    df0 <- sqlFetch(channel0, 'qry_AllCatch')
    df_species0 <- df0[((df0$WSPEName==data_filter0)
                        & df0$CaptureType==1),]
    
    df_species_all_years <- rbind(df_species0,df_species)
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
  
  return(df_species_all_years)
}



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
    
    df <- access_file(string_year)
    df_species_all_years <- df[((df$WSPEName==data_filter)
                                & df$CaptureType==1),]
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

sum_species_data <- function(df){
  df_new_summarize <- df %>%
    group_by(WSPEName, StartDate, day_of_year, year) %>%
    summarise(sum_num_caught = sum(fish_per_hour))
  return(df_new_summarize)
}

plot_single_year <- function(plot_empty, year, species,type,river,file_type){
  chinook_2018<- extract_species_data_error(year, species,type,river,file_type)
  sum_chinook_2018 <- sum_species_data(chinook_2018)
  
  plot2 <- plot_empty + geom_line(sum_chinook_2018, mapping = aes(StartDate, sum_num_caught))
  return(plot2)
}

chinook_2020 <- extract_species_data_error(2016,"Chinook","W","Dungeness",".accdb")
sum_chinook_2020 <- sum_species_data(chinook_2020)

plot_empty <- ggplot() + theme_minimal()

plot_empty <- plot_single_year(plot_empty, 2021,"Chinook","W","Dungeness",".accdb")

plot_empty

year = 2019
species = "Chinook"
type = "W"
river = "Dungeness"
file_type = ".accdb"

chinook_2019 <- extract_species_data_error(2019,"Chinook","W","Dungeness",".accdb")


plot_empty <- ggplot() + theme_minimal()

plot_empty <- plot_single_year(plot_empty, 2021,"Chinook","W","Dungeness",".accdb")

plot_empty
