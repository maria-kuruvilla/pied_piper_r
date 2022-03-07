# Goal - to have one function that takes in year, river and species and returns
# figure with number wild fish for each year, number of hatchery fish for each
# year and the moving average for wild fish

library(RODBC)
library(here)
library(tidyverse)
library(tidyquant)
library(RColorBrewer)

#function that takes in year, river and species and type
#returns dataframe

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

sum_species_data <- function(df){
  df_new_summarize <- df_species_all_years %>%
    group_by(WSPEName,StartDate, EndDate,start_datetime,end_datetime,
             start_time, end_time, day_of_year, year) %>%
    summarise(sum_num_caught = sum(fish_per_hour))
  return(df_new_summarize)
}

coho_w_2019<-extract_species_data(2019,"Steelhead","W","Dungeness",".accdb")

plot1 <- ggplot(data = coho_w_2019, aes(x = StartDate, y = sum_num_caught, group = 1)) +
  geom_line() + theme_minimal() +
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  labs(title = paste(river,paste(species,type)))+ 
  scale_x_datetime(expand = c(0,0),date_labels="%b")

print(plot1)

