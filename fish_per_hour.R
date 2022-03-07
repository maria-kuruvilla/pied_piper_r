# Goal - to write code to plot num fish caught per hour
#date - 28th Jan 2022

library(RODBC)
library(here)
library(tidyverse)

channel <- odbcConnectAccess2007(here("Documents","data","pied_piper","2019 Dungeness.accdb"))


df <- sqlFetch(channel, 'qry_AllCatch')

df_coho <- df[df$WSPEName=="Coho 1+ W",]# | df$WSPEName=="Coho 1+ H",]

df_coho$start_datetime <- as.POSIXct(paste(df_coho$StartDate, 
                                              format(df_coho$StartTime, format = "%H:%M:%S")))

df_coho$start_time <- format(as.POSIXct(df_coho$start_datetime), format = "%H:%M:%S")


df_coho$end_datetime <- as.POSIXct(paste(df_coho$EndDate, 
                                            format(df_coho$EndTime, format = "%H:%M:%S")))

df_coho$end_time <- format(as.POSIXct(df_coho$end_datetime), format = "%H:%M:%S")

df_coho$fish_per_hour <- df_coho$NumCaught*60/as.numeric(df_coho$end_datetime - df_coho$start_datetime)

ggplot(data=df_coho, 
       aes(x=StartDate, y=NumCaught, color=  WSPEName, fill = WSPEName)) + 
  geom_line(lwd=1) + 
  ylab("Number of fish caught") + xlab("Date") +
  labs(title = "Coho 2019 - Hatchery and Wild")


ggplot(data=df_coho, 
       aes(x=StartDate, y=fish_per_hour, color=  WSPEName, fill = WSPEName)) + 
  geom_line(lwd=1) + 
  ylab("Number of fish caught per hour") + xlab("Date") +
  labs(title = "Coho 2019 - Hatchery and Wild")


#chinook 0+

df_chinook <- df[df$WSPEName=="Chinook 0+ W" | df$WSPEName=="Chinook 0+ H",]
df_chinook$start_datetime <- as.POSIXct(paste(df_chinook$StartDate, 
                                           format(df_chinook$StartTime, format = "%H:%M:%S")))

df_chinook$start_time <- format(as.POSIXct(df_chinook$start_datetime), format = "%H:%M:%S")


df_chinook$end_datetime <- as.POSIXct(paste(df_chinook$EndDate, 
                                         format(df_chinook$EndTime, format = "%H:%M:%S")))

df_chinook$end_time <- format(as.POSIXct(df_chinook$end_datetime), format = "%H:%M:%S")

df_chinook$fish_per_hour <- df_chinook$NumCaught*60/as.numeric(df_chinook$end_datetime - df_chinook$start_datetime)



ggplot(data=df_chinook, 
       aes(x=StartDate, y=fish_per_hour, color=  WSPEName, fill = WSPEName)) + 
  geom_line(lwd=1) + 
  ylab("Number of fish caught per hour") + xlab("Date") +
  labs(title = "Chinook 0+ 2019 - Hatchery and Wild")



