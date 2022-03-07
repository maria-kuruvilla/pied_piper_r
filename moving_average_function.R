# Goal - to have one figure with all years data and moving average


library(RODBC)
library(here)
library(tidyverse)
library(tidyquant)
library(RColorBrewer)


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
    df_species$StartDate_OG <- df_species$StartDate
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

sum_species_data <- function(year, species,type,river,file_type){
  df <- extract_species_data_error(year, species,type,river,file_type)
  df_new_summarize <- df %>%
    group_by(WSPEName, StartDate, day_of_year, year) %>%
    summarise(sum_num_caught = sum(fish_per_hour))
  return(df_new_summarize)
}

moving_average <- function(species,type,river,file_type){
  if(species == "Steelhead"){
    last_year = 2020
  }
  else {
    last_year = 2021
  }
  for(i in 2016:last_year){
    if(i==2016){
      df_combine <- sum_species_data(i, species,type,river,file_type)
    }
    else {
      df <- sum_species_data(i, species,type,river,file_type)
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
  return(my_data_frame)
}

plot_all_years <- function(plot_empty, species,type,river,file_type){
  if(type == "H"){
    color_brewer = "#66c2a5"
  }
  else {
    color_brewer = "#fc8d62"
  }
  if(species == "Steelhead"){
    last_year = 2020
  }
  else {
    last_year = 2021
  }
  for(i in 2016:2021){
    df <- sum_species_data(i, species,type,river,file_type)
    plot_empty <- plot_empty + 
      geom_line(df, mapping = aes(StartDate, sum_num_caught), alpha = 0.3, color = color_brewer)
  }
  return(plot_empty)
}

plot_wild <- ggplot() + theme_minimal()

species = "Chinook"
type = "W"
river = "Dungeness"


plot_wild <- plot_all_years(plot_wild, species,type,river, ".accdb")
plot_wild <- plot_wild + 
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  labs(title = paste(river,paste(species,type)))+ 
  scale_x_datetime(expand = c(0,0),date_labels="%b")

ma <- moving_average(species,type,river, ".accdb")
plot_wild <- plot_wild + 
  geom_line(ma, mapping = aes(StartDate, moving_average), size = 1, color = "#fc8d62")
plot_wild


type = "H"
plot_both <- plot_all_years(plot_wild, species,type,river, ".accdb")
ma_h <- moving_average(species,type,river, ".accdb")
plot_both <- plot_both + 
  geom_line(ma, mapping = aes(StartDate, moving_average), size = 1, color = "#66c2a5")
plot_both


#plotting only the moving averages of wild with hatchery fish

species = "Chinook"
type = "W"
river = "Dungeness"
ma_w <- moving_average(species,type,river, ".accdb")

plot_wild <- ggplot() + theme_minimal()
plot_wild <- plot_wild + 
  geom_line(ma_w, mapping = aes(StartDate, moving_average), size = 1, color = "#fc8d62")

plot_wild

type = "H"
plot_both <- plot_all_years(plot_wild, species,type,river, ".accdb")
plot_both + scale_y_log10()

#plotting only moving oaverages of both wild and hatchery

species = "Chinook"
type = "W"
river = "Nisqually"
ma_w <- moving_average(species,type,river, ".accdb")
plot_wild <- ggplot() + theme_minimal()
plot_wild <- plot_wild + 
  geom_line(ma_w, mapping = aes(StartDate, moving_average), size = 1, color = "#fc8d62")

plot_wild
type = "H"
ma_h <- moving_average(species,type,river, ".accdb")
plot_both <- plot_wild + geom_line(ma_h, mapping = aes(StartDate, moving_average), size = 1, color = "#66c2a5")
  
plot_both
