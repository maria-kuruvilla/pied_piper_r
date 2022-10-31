#Goal - to try to reformat old data to read like the new data


library(RODBC)
library(here)
library(tidyverse)

channel <- odbcConnectAccess2007(here("Documents","data","pied_piper","2005-2006 Dungeness.mdb"))


df <- sqlFetch(channel, 'dungeness_chinook0_2006')

df$start_time <- format(as.POSIXct(df$Down), format = "%H:%M:%S")
df$end_time <- format(as.POSIXct(df$Up), format = "%H:%M:%S")

df$start_date <- format(as.POSIXct(df$Down), format="%Y/%m/%d")
df$end_date <- format(as.POSIXct(df$Up), format="%Y/%m/%d")
df$no_hours <- as.numeric(df$Up-df$Down)

df$wild_per_hour <- df$Chinook0/df$no_hours
df$hatchery_per_hour <- df$OtherCatch/df$no_hours


p1_w <- ggplot() + theme_minimal() + 
  geom_line(df, mapping = aes(as.Date(start_date), wild_per_hour), size = 1, color = "#fc8d62") +
  geom_line(df, mapping = aes(as.Date(start_date), hatchery_per_hour), size = 1, color = "#66c2a5") +
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  scale_x_date(expand = c(0,0))#,date_labels="%b")
                   #limits = c(as.POSIXct("2006-01-02"),as.POSIXct("2006-08-19")),
                   
p1_w

ggplot(df, aes(x=as.POSIXct(
  start_time, format="%H:%M:%S"))) + geom_bar() + scale_x_datetime(date_breaks = "2 hours")


h <- hist(as.POSIXct(df$start_time, format="%H:%M:%S"), breaks = "hours",
          xlim = c(as.POSIXct("00:00:00", format="%H:%M:%S"),as.POSIXct("24:00:00", format="%H:%M:%S")))

h$counts

p2 <- ggplot(df, aes(as.POSIXct(start_time, format="%H:%M:%S"), wild_per_hour)) + theme_minimal() +
  geom_col()
p2
p3 <- ggplot(df, aes(as.POSIXct(start_time, format="%H:%M:%S"), hatchery_per_hour)) + theme_minimal() +
  geom_col()
p3

h2 <- hist()


ggplot(df, aes(x=as.POSIXct(
  start_time, format="%H:%M:%S"), y = wild_per_hour)) + 
  geom_col(breaks = breaks_fixed) + scale_x_datetime(date_breaks = "2 hours")


plot1 <- ggplot(df, aes(x=as.POSIXct(
  start_time, format="%H:%M:%S")))+
  geom_histogram(breaks = breaks_fixed)

plot1

breaks_fixed = c(as.POSIXct("00:00:00", format="%H:%M:%S"),
  as.POSIXct("02:00:00", format="%H:%M:%S"),
  as.POSIXct("04:00:00", format="%H:%M:%S"),
  as.POSIXct("06:00:00", format="%H:%M:%S"),
  as.POSIXct("08:00:00", format="%H:%M:%S"),
  as.POSIXct("10:00:00", format="%H:%M:%S"),
  as.POSIXct("12:00:00", format="%H:%M:%S"),
  as.POSIXct("14:00:00", format="%H:%M:%S"),
  as.POSIXct("16:00:00", format="%H:%M:%S"),
  as.POSIXct("18:00:00", format="%H:%M:%S"),
  as.POSIXct("20:00:00", format="%H:%M:%S"),
  as.POSIXct("22:00:00", format="%H:%M:%S"),
  as.POSIXct("24:00:00", format="%H:%M:%S"))

h <- hist(as.POSIXct(df$start_time, format="%H:%M:%S"), breaks = breaks_fixed)
h$counts


ggplot(df, aes(x=as.POSIXct(
  start_time, format="%H:%M:%S"), y = wild_per_hour)) + 
  geom_col() + scale_x_datetime(breaks = breaks_fixed)







