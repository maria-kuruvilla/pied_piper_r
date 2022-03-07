#Goal - have one code to plot fish per hour for multiple years in Nisqually
#author - Maria Kuruvilla
#date - Feb 1 2022

library(RODBC)
library(here)
library(tidyverse)

year = c(2016,2017,2018,2019,2020)
#year = c(2015)

ggplot() +
  labs(title = "Wild Coho")
for(y in year){
  
  print(y)
  string_year = paste(toString(y),"Nisqually.accdb")
  print(string_year)
  channel <- odbcConnectAccess2007(here("Documents","data","pied_piper",string_year))
  df <- sqlFetch(channel, 'qry_AllCatch')
  #print(unique(df$WSPEName))
  df_coho <- df[df$WSPEName=="Coho 1+ W",]# | df$WSPEName=="Coho 1+ H",]
  print(df_coho[1,])
  # df_coho$start_datetime <- as.POSIXct(paste(df_coho$StartDate, 
  #                                            format(df_coho$StartTime, format = "%H:%M:%S")))
  # 
  # df_coho$start_time <- format(as.POSIXct(df_coho$start_datetime), format = "%H:%M:%S")
  # 
  # 
  # df_coho$end_datetime <- as.POSIXct(paste(df_coho$EndDate, 
  #                                          format(df_coho$EndTime, format = "%H:%M:%S")))
  # 
  # df_coho$end_time <- format(as.POSIXct(df_coho$end_datetime), format = "%H:%M:%S")
  # 
  # df_coho$fish_per_hour <- df_coho$NumCaught*60/as.numeric(df_coho$end_datetime - df_coho$start_datetime)
  # 
  # print(ggplot(data=df_coho, 
  #        aes(x=StartDate, y=fish_per_hour)) + 
  #   geom_line(lwd=1) + 
  #   ylab("Number of fish caught per hour") + xlab("Date") +
  #   labs(title = paste(toString(y),"Wild Coho")))
  # Sys.sleep(2)
  # odbcClose(channel)
  
}



