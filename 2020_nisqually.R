
# Goal - Read 2020 nisqually data file 
# author - Maria Kuruvilla
# date - 19th Jan 2022


library(RODBC)
library(here)
library(tidyverse)

channel <- odbcConnectAccess2007(here("Documents","data","pied_piper","2020 Nisqually.accdb"))

df <- sqlFetch(channel, 'qry_AllCatch')

unique(df$WSPEName)
#there seems to be chinook 0+ hatchery fish

df_chinook <- df[df$WSPEName=="Chinook 0+ W",]

ggplot(data=df_chinook, 
       aes(x=StartDate, y=NumCaught)) + 
  geom_col(alpha = 0.8) + 
  ylab("Number of fish caught") + xlab("Date") +
  labs(title = "Chinook 0+ 2020 Nisqually")

ggplot(data=df_chinook, 
       aes(x=StartTime, y=NumCaught)) + 
  geom_col(alpha = 0.8) + 
  ylab("Number of fish caught") + xlab("Time") +
  labs(title = "Chinook 0+ 2020 Nisqually")

ggplot(data=df_chinook, 
       aes(x=EndTime, y=NumCaught)) + 
  geom_col(alpha = 0.8) + 
  ylab("Number of fish caught") + xlab("End Time") +
  labs(title = "Chinook 0+ 2020 Nisqually")


df_steelhead <- df[df$WSPEName=="Steelhead smolt W",]

ggplot(data=df_steelhead, 
       aes(x=StartDate, y=NumCaught)) + 
  geom_col(alpha = 0.8) + 
  ylab("Number of fish caught") + xlab("Date") +
  labs(title = "Wild Steelhead smolt 2020 Nisqually")

time = as.POSIXct(paste(df_chinook$StartDate[1], 
                        format(df_chinook$StartTime[1], format = "%H:%M:%S")))

df_chinook$start_datetime <- as.POSIXct(paste(df_chinook$StartDate, 
                                    format(df_chinook$StartTime, format = "%H:%M:%S")))

df_chinook$start_time <- format(as.POSIXct(df_chinook$start_datetime), format = "%H:%M:%S")


df_chinook$end_datetime <- as.POSIXct(paste(df_chinook$EndDate, 
                                              format(df_chinook$EndTime, format = "%H:%M:%S")))

df_chinook$end_time <- format(as.POSIXct(df_chinook$end_datetime), format = "%H:%M:%S")



ggplot(data=df_chinook, 
       aes(x=start_datetime, y=NumCaught)) + 
  geom_col(alpha = 0.8) + 
  ylab("Number of fish caught") + xlab("Date") +
  labs(title = "Chinook 0+ 2020 Nisqually")

ggplot(data=df_chinook, 
       aes(x=start_time, y=NumCaught)) + 
  geom_col(alpha = 0.8) + 
  ylab("Number of fish caught") + xlab("Time") +
  labs(title = "Chinook 0+ 2020 Nisqually")+ coord_flip() 

ggplot(data=df_chinook, 
       aes(x=end_time, y=NumCaught)) + 
  geom_col(alpha = 0.8) + 
  ylab("Number of fish caught") + xlab("End Time") +
  labs(title = "Chinook 0+ 2020 Nisqually")+ coord_flip() 